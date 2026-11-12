//! Value layer for JSLT.
//!
//! `JsltValue` is an immutable JSON tree. Strings, arrays and objects are
//! reference counted, so cloning a value or taking a part of it does not copy.
//!  - deep equality (object key order-insensitive)
//!  - numeric extraction with NaN/Inf checks
//!  - array/string index & slice with negative indices and null propagation
//!
//! String index and slice by Unicode scalar values (code points), not bytes.

use indexmap::IndexMap;
use serde::ser::{SerializeMap, SerializeSeq};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{Number, Value};
use std::sync::Arc;

/// Object entries in insertion order. Keys are shared `Arc<str>`s, so copying a key
/// into another object does not allocate. Keys are hashed with foldhash, which is
/// much faster than SipHash on short keys and is randomly seeded per process.
pub type Map = IndexMap<Arc<str>, JsltValue, foldhash::fast::RandomState>;

/// An empty [`Map`] with room for `n` entries.
pub fn map_with_capacity(n: usize) -> Map {
    Map::with_capacity_and_hasher(n, Default::default())
}

#[derive(Clone, Default, PartialEq)]
pub enum JsltValue {
    #[default]
    Null,
    Bool(bool),
    Number(Number),
    String(Arc<str>),
    Array(Arc<Vec<JsltValue>>),
    Object(Arc<Map>),
}

impl std::fmt::Debug for JsltValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.to_json(), f)
    }
}

/// Compact JSON text.
impl std::fmt::Display for JsltValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&serde_json::to_string(self).map_err(|_| std::fmt::Error)?)
    }
}

impl JsltValue {
    // Constructors and type guards
    pub fn null() -> Self {
        JsltValue::Null
    }

    pub fn bool(b: bool) -> Self {
        JsltValue::Bool(b)
    }

    pub fn string(s: impl Into<Arc<str>>) -> Self {
        JsltValue::String(s.into())
    }

    /// Constructs a numeric value from an f64, but prefer an integer
    /// representation when the value is exactly integral and fits in i64.
    ///
    /// Note: caller must ensure the input is finite; NaN/Inf cannot be
    /// represented in serde_json::Number and will panic if passed to `number_f64`
    pub fn number(n: f64) -> Self {
        if n.is_finite() && n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
            return Self::number_i64(n as i64);
        }
        Self::number_f64(n)
    }

    pub fn number_f64(n: f64) -> Self {
        JsltValue::Number(Number::from_f64(n).unwrap())
    }

    pub fn number_i64(n: i64) -> Self {
        JsltValue::Number(Number::from(n))
    }

    pub fn array(a: Vec<JsltValue>) -> Self {
        JsltValue::Array(Arc::new(a))
    }

    pub fn object(m: Map) -> Self {
        JsltValue::Object(Arc::new(m))
    }

    pub fn is_null(&self) -> bool {
        matches!(self, JsltValue::Null)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, JsltValue::Bool(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, JsltValue::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, JsltValue::String(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self, JsltValue::Array(_))
    }

    pub fn is_object(&self) -> bool {
        matches!(self, JsltValue::Object(_))
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            JsltValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<&Number> {
        match self {
            JsltValue::Number(n) => Some(n),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            JsltValue::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[JsltValue]> {
        match self {
            JsltValue::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_object(&self) -> Option<&Map> {
        match self {
            JsltValue::Object(o) => Some(o),
            _ => None,
        }
    }

    /// The member `key` of an object; None for a missing key or a non-object.
    pub fn get(&self, key: &str) -> Option<&JsltValue> {
        self.as_object()?.get(key)
    }

    // Conversion helpers
    pub fn to_json(&self) -> Value {
        match self {
            JsltValue::Null => Value::Null,
            JsltValue::Bool(b) => Value::Bool(*b),
            JsltValue::Number(n) => Value::Number(n.clone()),
            JsltValue::String(s) => Value::String(s.to_string()),
            JsltValue::Array(a) => Value::Array(a.iter().map(JsltValue::to_json).collect()),
            JsltValue::Object(o) => {
                Value::Object(o.iter().map(|(k, v)| (k.to_string(), v.to_json())).collect())
            }
        }
    }

    pub fn into_json(self) -> Value {
        self.to_json()
    }

    pub fn from_json(value: Value) -> Self {
        match value {
            Value::Null => JsltValue::Null,
            Value::Bool(b) => JsltValue::Bool(b),
            Value::Number(n) => JsltValue::Number(n),
            Value::String(s) => JsltValue::String(s.into()),
            Value::Array(a) => JsltValue::array(a.into_iter().map(JsltValue::from_json).collect()),
            Value::Object(o) => JsltValue::object(
                o.into_iter().map(|(k, v)| (k.into(), JsltValue::from_json(v))).collect(),
            ),
        }
    }

    pub fn from_json_ref(value: &Value) -> Self {
        match value {
            Value::Null => JsltValue::Null,
            Value::Bool(b) => JsltValue::Bool(*b),
            Value::Number(n) => JsltValue::Number(n.clone()),
            Value::String(s) => JsltValue::String(s.as_str().into()),
            Value::Array(a) => JsltValue::array(a.iter().map(JsltValue::from_json_ref).collect()),
            Value::Object(o) => JsltValue::object(
                o.iter().map(|(k, v)| (k.as_str().into(), JsltValue::from_json_ref(v))).collect(),
            ),
        }
    }

    // Numeric helper: reject NaN/Inf as per spec intent
    /// Returns the value as `i64` if it is an integer number (not a float like `1.0`).
    pub fn as_i64(&self) -> Option<i64> {
        self.as_number()?.as_i64()
    }

    pub fn as_f64_checked(&self) -> Option<f64> {
        self.as_number()?.as_f64().filter(|f| f.is_finite())
    }

    // Deep structural equality
    // - numbers compare by finite f64 (treat -0.0 == 0.0)
    // - objects: key-order-insensitive, deep compare
    // - arrays: element-wise deep compare
    // - other same-type exact equality
    pub fn deep_eq(&self, other: &JsltValue) -> bool {
        use JsltValue::*;
        match (self, other) {
            (Null, Null) => true,
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => number_eq(a, b),
            (String(a), String(b)) => a == b,
            (Array(a), Array(b)) => {
                Arc::ptr_eq(a, b)
                    || (a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.deep_eq(y)))
            }
            (Object(a), Object(b)) => {
                Arc::ptr_eq(a, b)
                    || (a.len() == b.len()
                        && a.iter().all(|(k, va)| b.get(k).is_some_and(|vb| va.deep_eq(vb))))
            }
            _ => false,
        }
    }

    // Indexing:
    // - Arrays: zero-based; negative indices count from end; OOB -> null;
    // - Strings: by Unicode scalar index; negativ from end; OOB -> null; result is 1-char string
    // - Other types: null propagation
    pub fn index(&self, idx: i64) -> JsltValue {
        match self {
            JsltValue::Array(arr) => match normalize_index(idx, arr.len() as i64) {
                Some(i) => arr[i as usize].clone(),
                None => JsltValue::null(),
            },
            JsltValue::String(s) => {
                let len = s.chars().count() as i64;
                match normalize_index(idx, len) {
                    Some(i) => {
                        let ch = s.chars().nth(i as usize).expect("index within char count");
                        JsltValue::string(ch.to_string())
                    }
                    None => JsltValue::null(),
                }
            }
            _ => JsltValue::null(),
        }
    }

    // Slicing [start:end] with half-open sematics [start, end):
    // - Applies to arrays and strings
    // - Defaults: start=0, end=len
    // - Negative indices offset from end; indices clamped to [0, len]
    // - If start > end, empty ([}, "")
    // - Wrong types: null propagation
    pub fn slice(&self, start: Option<i64>, end: Option<i64>) -> JsltValue {
        match self {
            JsltValue::Array(arr) => {
                let (lo, hi) = compute_slice_bounds(start, end, arr.len());
                let slice = if lo <= hi { arr[lo..hi].to_vec() } else { Vec::new() };
                JsltValue::array(slice)
            }
            JsltValue::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let (lo, hi) = compute_slice_bounds(start, end, chars.len());
                let out: String =
                    if lo <= hi { chars[lo..hi].iter().collect() } else { String::new() };
                JsltValue::string(out)
            }
            _ => JsltValue::null(),
        }
    }

    // Helper function to find the type of a value.
    pub fn type_of(&self) -> &'static str {
        match self {
            JsltValue::Null => "null",
            JsltValue::Bool(_) => "boolean",
            JsltValue::Number(_) => "number",
            JsltValue::String(_) => "string",
            JsltValue::Array(_) => "array",
            JsltValue::Object(_) => "object",
        }
    }

    // Helper function to check if a value is truthy.
    pub fn truthy(&self) -> bool {
        match self {
            JsltValue::Null => false,
            JsltValue::Bool(b) => *b,
            JsltValue::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(true),
            JsltValue::String(s) => !s.is_empty(),
            JsltValue::Array(a) => !a.is_empty(),
            JsltValue::Object(o) => !o.is_empty(),
        }
    }

    // helper function to turn value to string representation
    pub fn stringify(&self) -> String {
        match self {
            JsltValue::Null => "null".to_string(),
            JsltValue::Bool(true) => "true".to_string(),
            JsltValue::Bool(false) => "false".to_string(),
            JsltValue::Number(n) => {
                // Prefer integer formatting when the numeric value is integral
                if let Some(i) = n.as_i64() {
                    i.to_string()
                } else if let Some(u) = n.as_u64() {
                    u.to_string()
                } else if let Some(f) = n.as_f64() {
                    // For float-backed number, supress trailing ".0"
                    if f.is_finite() && f.fract() == 0.0 {
                        // casting to i64 is fine for typical JSLT ranges, uses f64 internally
                        (f as i64).to_string()
                    } else {
                        // Use Rust's default shortest representation
                        f.to_string()
                    }
                } else {
                    n.to_string()
                }
            }
            JsltValue::String(s) => s.to_string(),
            // For arrays/objects: JSON rendering to match typical JSLT behavior for string()
            JsltValue::Array(_) | JsltValue::Object(_) => {
                serde_json::to_string(self).unwrap_or_default()
            }
        }
    }
}

impl Serialize for JsltValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            JsltValue::Null => serializer.serialize_unit(),
            JsltValue::Bool(b) => serializer.serialize_bool(*b),
            JsltValue::Number(n) => n.serialize(serializer),
            JsltValue::String(s) => serializer.serialize_str(s),
            JsltValue::Array(a) => {
                let mut seq = serializer.serialize_seq(Some(a.len()))?;
                for v in a.iter() {
                    seq.serialize_element(v)?;
                }
                seq.end()
            }
            JsltValue::Object(o) => {
                let mut map = serializer.serialize_map(Some(o.len()))?;
                for (k, v) in o.iter() {
                    map.serialize_entry(&**k, v)?;
                }
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for JsltValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_any(JsltValueVisitor)
    }
}

/// An object key, deserialized straight into an `Arc<str>`.
struct Key(Arc<str>);

impl<'de> Deserialize<'de> for Key {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct KeyVisitor;
        impl serde::de::Visitor<'_> for KeyVisitor {
            type Value = Key;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("a string key")
            }

            fn visit_str<E>(self, s: &str) -> Result<Key, E> {
                Ok(Key(s.into()))
            }
        }
        d.deserialize_str(KeyVisitor)
    }
}

struct JsltValueVisitor;

impl<'de> serde::de::Visitor<'de> for JsltValueVisitor {
    type Value = JsltValue;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("any JSON value")
    }

    fn visit_unit<E>(self) -> Result<JsltValue, E> {
        Ok(JsltValue::Null)
    }

    fn visit_none<E>(self) -> Result<JsltValue, E> {
        Ok(JsltValue::Null)
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<JsltValue, D::Error> {
        JsltValue::deserialize(d)
    }

    fn visit_bool<E>(self, b: bool) -> Result<JsltValue, E> {
        Ok(JsltValue::Bool(b))
    }

    fn visit_i64<E>(self, n: i64) -> Result<JsltValue, E> {
        Ok(JsltValue::Number(n.into()))
    }

    fn visit_u64<E>(self, n: u64) -> Result<JsltValue, E> {
        Ok(JsltValue::Number(n.into()))
    }

    fn visit_f64<E>(self, n: f64) -> Result<JsltValue, E> {
        Ok(Number::from_f64(n).map_or(JsltValue::Null, JsltValue::Number))
    }

    fn visit_str<E>(self, s: &str) -> Result<JsltValue, E> {
        Ok(JsltValue::String(s.into()))
    }

    fn visit_string<E>(self, s: String) -> Result<JsltValue, E> {
        Ok(JsltValue::String(s.into()))
    }

    fn visit_seq<A: serde::de::SeqAccess<'de>>(self, mut seq: A) -> Result<JsltValue, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0).min(4096));
        while let Some(v) = seq.next_element()? {
            items.push(v);
        }
        Ok(JsltValue::array(items))
    }

    fn visit_map<A: serde::de::MapAccess<'de>>(self, mut access: A) -> Result<JsltValue, A::Error> {
        let mut map = map_with_capacity(access.size_hint().unwrap_or(0).min(4096));
        while let Some((Key(k), v)) = access.next_entry::<Key, JsltValue>()? {
            map.insert(k, v);
        }
        Ok(JsltValue::object(map))
    }
}

fn number_eq(a: &Number, b: &Number) -> bool {
    match (a.as_f64(), b.as_f64()) {
        (Some(x), Some(y)) if x.is_finite() && y.is_finite() => {
            // Treat -0.0 == 0.0
            if x == 0.0 && y == 0.0 {
                true
            } else {
                x == y
            }
        }
        _ => false,
    }
}

// Normalize a possibly negative index into [0, len-1]. Returns None if OOB.
fn normalize_index(idx: i64, len: i64) -> Option<i64> {
    if len == 0 {
        return None;
    }
    let mut i = idx;
    if i < 0 {
        i += len; // negative from end
    }
    if i < 0 || i >= len {
        None
    } else {
        Some(i)
    }
}

// Compute slice bounds [lo, hi) clamped to [0, len], supporting negative indices.
// len is usize (array length or string char length).
fn compute_slice_bounds(start: Option<i64>, end: Option<i64>, len: usize) -> (usize, usize) {
    let len_i = len as i64;

    let raw_start = start.unwrap_or(0);
    let raw_end = end.unwrap_or(len_i);

    let mut lo = if raw_start < 0 { raw_start + len_i } else { raw_start };
    let mut hi = if raw_end < 0 { raw_end + len_i } else { raw_end };

    // Clamp to [0, len]
    lo = lo.clamp(0, len_i);
    hi = hi.clamp(0, len_i);

    // convert to uszie safely
    (lo as usize, hi as usize)
}

// Convenience From impl
impl From<Value> for JsltValue {
    fn from(value: Value) -> Self {
        JsltValue::from_json(value)
    }
}

impl From<JsltValue> for Value {
    fn from(value: JsltValue) -> Self {
        value.into_json()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn deserialize_matches_serde_json_value() {
        let text = r#"{"b":1,"a":[true,null,-3,18446744073709551615,1.5e300,0.1,"x\u00e9"],
            "o":{"k":{}},"b":2,"e":[]}"#;
        let direct: JsltValue = serde_json::from_str(text).unwrap();
        let via_value = JsltValue::from_json(serde_json::from_str::<Value>(text).unwrap());
        assert_eq!(direct.to_json(), via_value.to_json());
        assert_eq!(direct.to_string(), via_value.to_string());
    }

    #[test]
    fn as_f64_checked_rejects_nan_inf() {
        let v = JsltValue::from_json(json!(1.5));
        assert_eq!(v.as_f64_checked(), Some(1.5));

        // serde_json::Number cannot represent NaN/Inf directly; this tests the positive path only.
        // The evaluator should avoid constructing NaN/Inf; if it does, it must error at runtime.
    }

    #[test]
    fn deep_eq_objects_ignore_key_order() {
        let a = JsltValue::from_json(json!({"x": 1, "y": [true, null], "z": {"a": "b"}}));
        let b = JsltValue::from_json(json!({"z": {"a": "b"}, "y": [true, null], "x": 1}));
        assert!(a.deep_eq(&b));
    }

    #[test]
    fn deep_eq_arrays_and_numbers() {
        let a = JsltValue::from_json(json!([0.0, -0.0, 1, 2.5]));
        let b = JsltValue::from_json(json!([0, 0.0, 1.0, 2.5]));
        assert!(a.deep_eq(&b));
    }

    #[test]
    fn index_array_positive_negative_and_oob() {
        let v = JsltValue::from_json(json!([10, 20, 30]));
        assert_eq!(v.index(0).into_json(), json!(10));
        assert_eq!(v.index(2).into_json(), json!(30));
        assert!(v.index(3).is_null()); // OOB
        assert_eq!(v.index(-1).into_json(), json!(30));
        assert_eq!(v.index(-3).into_json(), json!(10));
        assert!(v.index(-4).is_null()); // OOB
    }

    #[test]
    fn index_string_by_unicode_scalar() {
        // "hé𝄞" → ['h', 'é', '𝄞'] where '𝄞' is U+1D11E (two UTF-16 code units, but one scalar)
        let s = "hé\u{1D11E}";
        let v = JsltValue::from_json(json!(s));

        assert_eq!(v.index(0).into_json(), json!("h"));
        assert_eq!(v.index(1).into_json(), json!("é"));
        assert_eq!(v.index(2).into_json(), json!("\u{1D11E}"));
        assert!(v.index(3).is_null());
        assert_eq!(v.index(-1).into_json(), json!("\u{1D11E}"));
        assert_eq!(v.index(-3).into_json(), json!("h"));
        assert!(v.index(-4).is_null());
    }

    #[test]
    fn slice_array_defaults_and_negatives() {
        let v = JsltValue::from_json(json!([0, 1, 2, 3, 4]));
        // Defaults
        assert_eq!(v.slice(None, None).into_json(), json!([0, 1, 2, 3, 4]));
        assert_eq!(v.slice(Some(2), None).into_json(), json!([2, 3, 4]));
        assert_eq!(v.slice(None, Some(3)).into_json(), json!([0, 1, 2]));

        // Negatives
        assert_eq!(v.slice(Some(-3), None).into_json(), json!([2, 3, 4]));
        assert_eq!(v.slice(None, Some(-2)).into_json(), json!([0, 1, 2]));

        // Mixed and clamped
        assert_eq!(v.slice(Some(-10), Some(2)).into_json(), json!([0, 1, 2][..2])); // -> [0,1]
        assert_eq!(v.slice(Some(3), Some(10)).into_json(), json!([3, 4]));

        // Empty when start > end
        assert_eq!(v.slice(Some(4), Some(2)).into_json(), json!([]));
    }

    #[test]
    fn slice_string_by_unicode_scalar() {
        let s = "hé\u{1D11E}X"; // ['h','é','𝄞','X']
        let v = JsltValue::from_json(json!(s));

        assert_eq!(v.slice(None, None).into_json(), json!(s));
        assert_eq!(v.slice(Some(1), Some(3)).into_json(), json!("é\u{1D11E}"));
        assert_eq!(v.slice(Some(-2), None).into_json(), json!("\u{1D11E}X"));
        assert_eq!(v.slice(None, Some(-1)).into_json(), json!("hé\u{1D11E}"));
        assert_eq!(v.slice(Some(3), Some(2)).into_json(), json!(""));
    }

    #[test]
    fn index_and_slice_wrong_types_null_propagation() {
        let v = JsltValue::from_json(json!(true));
        assert!(v.index(0).is_null());
        assert!(v.slice(Some(0), Some(1)).is_null());

        let v = JsltValue::from_json(json!({"a": 1}));
        assert!(v.index(0).is_null());
        assert!(v.slice(None, None).is_null());
    }

    #[test]
    fn members_and_elements_share_storage() {
        let v = JsltValue::from_json(json!({"a": {"b": [1, {"c": "x"}]}}));
        let (JsltValue::Object(a1), JsltValue::Object(a2)) =
            (v.get("a").unwrap().clone(), v.get("a").unwrap().clone())
        else {
            panic!("expected objects");
        };
        assert!(Arc::ptr_eq(&a1, &a2));

        let arr = v.get("a").unwrap().get("b").unwrap();
        let (JsltValue::Object(e1), JsltValue::Object(e2)) = (arr.index(1), arr.index(-1)) else {
            panic!("expected objects");
        };
        assert!(Arc::ptr_eq(&e1, &e2));
    }

    #[test]
    fn json_round_trip_keeps_key_order_and_numbers() {
        let src = json!({"z": 1, "a": [1.5, -0.0, 2, null, true], "m": {"y": "s", "b": {}}});
        let v = JsltValue::from_json_ref(&src);
        assert_eq!(v.to_json(), src);
        assert_eq!(v.to_string(), serde_json::to_string(&src).unwrap());
        assert_eq!(
            v.to_json().as_object().unwrap().keys().collect::<Vec<_>>(),
            vec!["z", "a", "m"]
        );
    }

    #[test]
    fn number_convert_integral_floats_to_integers() {
        let v = JsltValue::number(1.0);
        assert_eq!(v, JsltValue::number_i64(1));
    }
}
