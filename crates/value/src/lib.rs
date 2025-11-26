//! Value layer for JSLT: a tiny facade around `serde_json::Value`.
//!
//! This matches the spec's data model and provides helpers needed by the evaluator.
//!  - deep equality (object key order-insensitive)
//!  - numeric extraction with NaN/Inf checks
//!  - null helpers
//!  - array/string index & slice with negative indices and null propagation
//!
//! String index and slice by Unicode scalar values (code points), not bytes.

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_json::{Map, Number, Value};
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub struct JsltValue(pub Arc<Value>);

impl JsltValue {
    // Constructors and type guards
    pub fn null() -> Self {
        Self(Arc::new(Value::Null))
    }

    pub fn bool(b: bool) -> Self {
        Self(Arc::new(Value::Bool(b)))
    }

    pub fn string(s: String) -> Self {
        Self(Arc::new(Value::String(s)))
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
        Self(Arc::new(Value::Number(Number::from_f64(n).unwrap())))
    }

    pub fn number_i64(n: i64) -> Self {
        Self(Arc::new(Value::Number(Number::from(n))))
    }

    pub fn array(a: Vec<JsltValue>) -> Self {
        let items: Vec<Value> = a.into_iter().map(|v| v.into_json()).collect();
        Self(Arc::new(Value::Array(items)))
    }

    pub fn is_null(&self) -> bool {
        matches!(self.0.as_ref(), Value::Null)
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self.0.as_ref(), Value::Bool(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self.0.as_ref(), Value::Number(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self.0.as_ref(), Value::String(_))
    }

    pub fn is_array(&self) -> bool {
        matches!(self.0.as_ref(), Value::Array(_))
    }

    pub fn is_object(&self) -> bool {
        matches!(self.0.as_ref(), Value::Object(_))
    }

    // Conversion helpers
    pub fn as_json(&self) -> &Value {
        self.0.as_ref()
    }

    pub fn into_json(self) -> Value {
        Arc::try_unwrap(self.0).unwrap_or_else(|arc| (*arc).clone())
    }

    pub fn from_json(value: Value) -> Self {
        JsltValue(Arc::new(value))
    }

    // Numeric helper: reject NaN/Inf as per spec intent
    pub fn as_f64_checked(&self) -> Option<f64> {
        match self.0.as_ref() {
            Value::Number(n) => n.as_f64().and_then(|f| if f.is_finite() { Some(f) } else { None }),
            _ => None,
        }
    }

    // Deep structural equality
    // - numbers compare by finite f64 (treat -0.0 == 0.0)
    // - objects: key-order-insensitive, deep compare
    // - arrays: element-wise deep compare
    // - other same-type exact equality
    pub fn deep_eq(&self, other: &JsltValue) -> bool {
        use Value::*;
        match (self.0.as_ref(), other.0.as_ref()) {
            (Null, Null) => true,
            (Bool(a), Bool(b)) => a == b,
            (Number(a), Number(b)) => number_eq(a, b),
            (String(a), String(b)) => a == b,
            (Array(a), Array(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(x, y)| {
                    JsltValue::from_json(x.clone()).deep_eq(&JsltValue::from_json(y.clone()))
                })
            }
            (Object(a), Object(b)) => object_deep_eq(a, b),
            // Different types are never equal
            _ => false,
        }
    }

    // Indexing:
    // - Arrays: zero-based; negative indices count from end; OOB -> null;
    // - Strings: by Unicode scalar index; negativ from end; OOB -> null; result is 1-char string
    // - Other types: null propagation
    pub fn index(&self, idx: i64) -> JsltValue {
        match self.0.as_ref() {
            Value::Array(arr) => {
                let len = arr.len() as i64;
                if let Some(i) = normalize_index(idx, len) {
                    JsltValue::from_json(arr[i as usize].clone())
                } else {
                    JsltValue::null()
                }
            }
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let len = chars.len() as i64;
                if let Some(i) = normalize_index(idx, len) {
                    let ch = chars[i as usize];
                    JsltValue::from_json(Value::String(ch.to_string()))
                } else {
                    JsltValue::null()
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
        match self.0.as_ref() {
            Value::Array(arr) => {
                let (lo, hi) = compute_slice_bounds(start, end, arr.len());
                let slice = if lo <= hi { arr[lo..hi].to_vec() } else { Vec::new() };
                JsltValue::from_json(Value::Array(slice))
            }
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                let (lo, hi) = compute_slice_bounds(start, end, chars.len());
                let out: String =
                    if lo <= hi { chars[lo..hi].iter().collect() } else { String::new() };
                JsltValue::from_json(Value::String(out))
            }
            _ => JsltValue::null(),
        }
    }

    // Helper function to find the type of a value.
    pub fn type_of(&self) -> &'static str {
        match self.as_json() {
            Value::Null => "null",
            Value::Bool(_) => "boolean",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
        }
    }

    // Helper function to check if a value is truthy.
    pub fn truthy(&self) -> bool {
        match self.as_json() {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(true),
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Object(o) => !o.is_empty(),
        }
    }

    // helper function to turn value to string representation
    pub fn stringify(&self) -> String {
        match self.as_json() {
            Value::Null => "null".to_string(),
            Value::Bool(true) => "true".to_string(),
            Value::Bool(false) => "false".to_string(),
            Value::Number(n) => {
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
                    // fallback: default JSON rendering
                    self.as_json().to_string()
                }
            }
            Value::String(s) => s.clone(),
            // For arrays/objects: JSON rendering to match typical JSLT behavior for string()
            Value::Array(_) | Value::Object(_) => {
                serde_json::to_string(self.as_json()).unwrap_or_default()
            }
        }
    }
}

impl Serialize for JsltValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_json().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for JsltValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let v = Value::deserialize(deserializer)?;
        Ok(JsltValue::from_json(v))
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

fn object_deep_eq(a: &Map<String, Value>, b: &Map<String, Value>) -> bool {
    if a.len() != b.len() {
        return false;
    }

    // key order-insensitive: compare by keys
    for (ka, va) in a.iter() {
        let Some(vb) = b.get(ka) else { return false };
        if !JsltValue::from_json(va.clone()).deep_eq(&JsltValue::from_json(vb.clone())) {
            return false;
        }
    }

    true
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
    fn number_convert_integral_floats_to_integers() {
        let v = JsltValue::number(1.0);
        assert_eq!(v, JsltValue::number_i64(1));
    }
}
