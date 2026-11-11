//! stdlib v1: registry and all original JSLT stdlib functions

mod time;
mod url;

use crate::time::now_seconds_portable;
#[cfg(feature = "chrono")]
use crate::time::{JavaPattern, Zone};
#[cfg(feature = "regex")]
use regex::Regex;
use serde_json::{from_str, to_string, Value};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
#[cfg(feature = "regex")]
use std::sync::Arc;
use thiserror::Error;
use value::{JsltValue, Map};

#[derive(Debug, Clone, Copy)]
pub enum Arity {
    Exact(usize),
    Range { min: usize, max: Option<usize> },
}

impl Arity {
    pub fn check(&self, got: usize) -> Result<(), StdlibError> {
        match *self {
            Arity::Exact(n) => {
                if got == n {
                    Ok(())
                } else {
                    Err(StdlibError::Arity { expected: format!("exactly {}", n), got })
                }
            }
            Arity::Range { min, max } => {
                if got < min {
                    return Err(StdlibError::Arity { expected: format!("at least {}", min), got });
                }
                if let Some(m) = max {
                    if got > m {
                        return Err(StdlibError::Arity { expected: format!("at most {}", m), got });
                    }
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum StdlibError {
    #[error("arity mismatch: expected {expected}, got {got}")]
    Arity { expected: String, got: usize },

    #[error("type error: {0}")]
    Type(String),

    #[error("semantic error: {0}")]
    Semantic(String),
}

pub type StdResult = Result<JsltValue, StdlibError>;

pub trait JsltFunction {
    fn name(&self) -> &'static str;
    fn arity(&self) -> Arity;
    fn call(&self, args: &[JsltValue]) -> StdResult;
}

#[derive(Default)]
pub struct Registry {
    // Stable order to assign indices (function ids)
    order: Vec<Box<dyn JsltFunction + Send + Sync>>,
    // name -> index in order
    by_name: BTreeMap<String, usize>,
}

impl Registry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_default() -> Self {
        let mut r = Self::new();
        // General
        r.register(ContainsFn);
        r.register(SizeFn);
        r.register(ErrorFn);
        r.register(FallbackFn);
        r.register(MinFn);
        r.register(MaxFn);

        // Numeric
        r.register(IsNumberFn);
        r.register(IsIntegerFn);
        r.register(IsDecimalFn);
        r.register(NumberFn);
        r.register(RoundFn);
        r.register(FloorFn);
        r.register(CeilingFn);
        r.register(RandomFn);
        r.register(SumFn);
        r.register(ModFn);
        r.register(HashIntFn);

        // String
        r.register(IsStringFn);
        r.register(StringFn);
        r.register(LowercaseFn);
        r.register(UppercaseFn);
        r.register(Sha256HexFn);
        r.register(StartsWithFn);
        r.register(EndsWithFn);
        r.register(JoinFn);
        r.register(FromJsonFn);
        r.register(ToJsonFn);
        r.register(TrimFn);
        r.register(UuidV4JSLT);

        // Boolean
        r.register(BooleanFn);
        r.register(NotFn);
        r.register(IsBooleanFn);

        // Object
        r.register(IsObjectFn);
        r.register(KeysFn);
        r.register(ValuesFn);
        r.register(GetKeyFn);

        // Array
        r.register(ArrayFn);
        r.register(IsArrayFn);
        r.register(FlattenFn);
        r.register(AllFn);
        r.register(AnyFn);
        r.register(ZipFn);
        r.register(ZipWithIndexFn);
        r.register(IndexOfFn);

        // Time
        r.register(NowFn);
        // TODO: These are probably not the best working functions,
        // todo: and was added with help of AI to match the Java implementation.
        #[cfg(feature = "chrono")]
        {
            r.register(ParseTimeFn);
            r.register(FormatTimeFn);
        }

        // URL
        r.register(ParseUrlFn);

        // Regex
        #[cfg(feature = "regex")]
        {
            r.register(TestFn);
            r.register(CaptureFn);
            r.register(SplitFn);
            r.register(ReplaceFn);
        }

        r
    }

    /// The default registry, built once per process.
    pub fn shared() -> &'static Registry {
        static SHARED: std::sync::OnceLock<Registry> = std::sync::OnceLock::new();
        SHARED.get_or_init(Registry::with_default)
    }

    pub fn register<F: JsltFunction + Send + Sync + 'static>(&mut self, f: F) {
        let name = f.name();
        if self.by_name.contains_key(name) {
            // Overwrite? in general, avoid duplicates.
            // For simplicity here, prefer first registration and ignore later duplicates
            return;
        }
        let idx = self.order.len();
        self.order.push(Box::new(f));
        self.by_name.insert(name.to_string(), idx);
    }

    pub fn get_id(&self, name: &str) -> Option<usize> {
        self.by_name.get(name).copied()
    }

    pub fn get_by_id(&self, id: usize) -> Option<&(dyn JsltFunction + Send + Sync)> {
        self.order.get(id).map(|b| b.as_ref())
    }

    pub fn len(&self) -> usize {
        self.order.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.by_name.keys().map(|s| s.as_str())
    }

    pub fn call_by_id(&self, id: usize, args: &[JsltValue]) -> StdResult {
        let f =
            self.get_by_id(id).ok_or_else(|| StdlibError::Semantic("unknown function".into()))?;
        f.arity().check(args.len())?;
        f.call(args)
    }
}

/// Java `NodeUtils.toString(node, true)`: strings as-is, null as `None`, anything else as JSON.
fn to_string_or_null(v: &JsltValue) -> Option<std::borrow::Cow<'_, str>> {
    match v {
        JsltValue::Null => None,
        JsltValue::String(s) => Some(std::borrow::Cow::Borrowed(s)),
        other => Some(std::borrow::Cow::Owned(to_string(other).unwrap_or_default())),
    }
}

fn expect_string<'a>(v: &'a JsltValue, fname: &str, argn: usize) -> Result<&'a str, StdlibError> {
    v.as_str().ok_or_else(|| {
        StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be string, got {}",
            v.type_of()
        ))
    })
}

fn expect_object<'a>(v: &'a JsltValue, fname: &str, argn: usize) -> Result<&'a Map, StdlibError> {
    v.as_object().ok_or_else(|| {
        StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be object, got {}",
            v.type_of()
        ))
    })
}

fn expect_array<'a>(
    v: &'a JsltValue,
    fname: &str,
    argn: usize,
) -> Result<&'a [JsltValue], StdlibError> {
    v.as_array().ok_or_else(|| {
        StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be array, got {}",
            v.type_of()
        ))
    })
}

fn expect_int_i128(v: &JsltValue, fname: &str, argn: usize) -> Result<i128, StdlibError> {
    match v {
        JsltValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(i as i128)
            } else if let Some(u) = n.as_u64() {
                Ok(u as i128)
            } else if n.as_f64().is_some() {
                Err(StdlibError::Type(format!(
                    "{fname} expects argument #{argn} to be integer, got decimal"
                )))
            } else {
                Err(StdlibError::Type(format!(
                    "{fname} expects argument #{argn} to be integer number",
                )))
            }
        }
        _ => Err(StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be integer, got {}",
            v.type_of()
        ))),
    }
}

// Produce a canonical JSON string like serde_json/to_string() without whitespace,
// but with object keys sorted so that key order doesn't affect the result.
fn canonical_json_string(v: &JsltValue, out: &mut String) {
    match v {
        JsltValue::Null => out.push_str("null"),
        JsltValue::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        JsltValue::Number(n) => out.push_str(&n.to_string()),
        JsltValue::String(s) => {
            // reuse serde_json's escaping by serializing the string itself
            // this gives a quoted string with proper escapes
            out.push_str(&to_string(&**s).expect("string serialize"));
        }
        JsltValue::Array(a) => {
            out.push('[');
            for (i, elt) in a.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                canonical_json_string(elt, out);
            }
            out.push(']');
        }
        JsltValue::Object(m) => {
            out.push('{');
            // sort keys lexicographically
            let mut keys: Vec<&str> = m.keys().map(|k| k.as_str()).collect();
            keys.sort_unstable();
            for (i, k) in keys.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                // key must be JSON string with quotes/escapes
                out.push_str(&to_string(k).expect("key serialize"));
                out.push(':');
                let val = &m[*k];
                canonical_json_string(val, out);
            }
            out.push('}');
        }
    }
}

// Java's String.hashCode over UTF-16 code units:
// h = 31*h + code_unit, 32-bit signed with wrapping semantics.
fn java_string_hash(s: &str) -> i32 {
    let mut h: i32 = 0;
    for cu in s.encode_utf16() {
        h = h.wrapping_mul(31).wrapping_add(cu as i32);
    }
    h
}

// ---------------------- Functions ----------------------

struct StringFn;
impl JsltFunction for StringFn {
    fn name(&self) -> &'static str {
        "string"
    }

    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }

    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::string(args[0].stringify()))
    }
}

struct NumberFn;
impl JsltFunction for NumberFn {
    fn name(&self) -> &'static str {
        "number"
    }

    fn arity(&self) -> Arity {
        Arity::Range { min: 1, max: Some(2) }
    }

    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];
        let fallback = args.get(1);
        if v.is_number() {
            return Ok(v.clone());
        }
        if v.is_null() {
            return Ok(fallback.cloned().unwrap_or_else(JsltValue::null));
        }
        let Some(s) = v.as_str() else {
            return match fallback {
                Some(fb) => Ok(fb.clone()),
                None => Err(StdlibError::Type(format!(
                    "number: can't convert {} to number",
                    v.type_of()
                ))),
            };
        };
        match (parse_number(s), fallback) {
            (Some(n), _) => Ok(n),
            (None, Some(fb)) => Ok(fb.clone()),
            (None, None) => Err(StdlibError::Type(format!("number({s}) failed: not a number"))),
        }
    }
}

/// Parses a number with the same grammar as the Java implementation:
/// `-? digits* ('.' digits+)? ([eE] [+-]? digits+)?`. Digit-only input becomes an
/// integer; anything with a fraction or exponent becomes a float.
fn parse_number(s: &str) -> Option<JsltValue> {
    let b = s.as_bytes();
    let digits_end =
        |from: usize| from + b[from..].iter().take_while(|c| c.is_ascii_digit()).count();

    let mut pos = usize::from(b.first() == Some(&b'-'));
    let int_end = digits_end(pos);
    if int_end == b.len() {
        if int_end == pos {
            return None;
        }
        return Some(match s.parse::<i64>() {
            Ok(i) => JsltValue::number_i64(i),
            Err(_) => JsltValue::number_f64(s.parse::<f64>().ok()?),
        });
    }
    pos = int_end;
    if b[pos] == b'.' {
        let frac_end = digits_end(pos + 1);
        if frac_end == pos + 1 {
            return None;
        }
        pos = frac_end;
    }
    if pos < b.len() {
        if b[pos] != b'e' && b[pos] != b'E' {
            return None;
        }
        pos += 1;
        if matches!(b.get(pos), Some(b'+' | b'-')) {
            pos += 1;
        }
        let exp_end = digits_end(pos);
        if exp_end == pos || exp_end != b.len() {
            return None;
        }
    }
    let f = s.parse::<f64>().ok().filter(|f| f.is_finite())?;
    Some(JsltValue::number_f64(f))
}

struct BooleanFn;
impl JsltFunction for BooleanFn {
    fn name(&self) -> &'static str {
        "boolean"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(args[0].truthy()))
    }
}

struct SizeFn;
impl JsltFunction for SizeFn {
    fn name(&self) -> &'static str {
        "size"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        let v = &args[0];
        if v.is_null() {
            return Ok(JsltValue::null());
        }
        let n = if let Some(s) = v.as_str() {
            s.chars().count()
        } else if let Some(a) = v.as_array() {
            a.len()
        } else if let Some(o) = v.as_object() {
            o.len()
        } else {
            return Err(StdlibError::Type(format!("size: unsupported type {}", v.type_of())));
        };
        Ok(JsltValue::number_i64(n as i64))
    }
}

struct KeysFn;
impl JsltFunction for KeysFn {
    fn name(&self) -> &'static str {
        "keys"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        let v = &args[0];
        if v.is_null() {
            return Ok(JsltValue::null());
        }
        let obj = expect_object(v, "keys", 1)?;
        let mut out = Vec::with_capacity(obj.len());
        for k in obj.keys() {
            out.push(JsltValue::string(k.clone()));
        }
        Ok(JsltValue::array(out))
    }
}

struct ValuesFn;
impl JsltFunction for ValuesFn {
    fn name(&self) -> &'static str {
        "values"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        let v = &args[0];
        if v.is_null() {
            return Ok(JsltValue::null());
        }
        let obj = expect_object(v, "values", 1)?;
        let mut out = Vec::with_capacity(obj.len());
        for v in obj.values() {
            out.push(v.clone());
        }
        Ok(JsltValue::array(out))
    }
}

struct GetKeyFn;
impl JsltFunction for GetKeyFn {
    fn name(&self) -> &'static str {
        "get-key"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 2, max: Some(3) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let obj = &args[0];
        let Some(k) = to_string_or_null(&args[1]) else {
            return Ok(JsltValue::null());
        };
        if obj.is_null() {
            return Ok(JsltValue::null());
        }
        let m = expect_object(obj, "get-key", 1)?;
        match m.get(k.as_ref()) {
            Some(v) => Ok(v.clone()),
            None => Ok(args.get(2).cloned().unwrap_or(JsltValue::null())),
        }
    }
}

struct StartsWithFn;
impl JsltFunction for StartsWithFn {
    fn name(&self) -> &'static str {
        "starts-with"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        // null tested yields false
        if args[0].is_null() {
            return Ok(JsltValue::bool(false));
        }
        let s = expect_string(&args[0], "starts-with", 1)?;
        let p = expect_string(&args[1], "starts-with", 2)?;
        Ok(JsltValue::bool(s.starts_with(p)))
    }
}

struct EndsWithFn;
impl JsltFunction for EndsWithFn {
    fn name(&self) -> &'static str {
        "ends-with"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        if args[0].is_null() {
            return Ok(JsltValue::bool(false));
        }
        let s = expect_string(&args[0], "ends-with", 1)?;
        let p = expect_string(&args[1], "ends-with", 2)?;
        Ok(JsltValue::bool(s.ends_with(p)))
    }
}

struct UppercaseFn;
impl JsltFunction for UppercaseFn {
    fn name(&self) -> &'static str {
        "uppercase"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        if args[0].is_null() {
            return Ok(JsltValue::null());
        }
        let s = args[0].stringify();
        Ok(JsltValue::string(s.to_uppercase()))
    }
}

struct LowercaseFn;
impl JsltFunction for LowercaseFn {
    fn name(&self) -> &'static str {
        "lowercase"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        if args[0].is_null() {
            return Ok(JsltValue::null());
        }
        let s = args[0].stringify();
        Ok(JsltValue::string(s.to_lowercase()))
    }
}

struct TrimFn;
impl JsltFunction for TrimFn {
    fn name(&self) -> &'static str {
        "trim"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        if args[0].is_null() {
            return Ok(JsltValue::null());
        }
        let s = args[0].stringify();
        Ok(JsltValue::string(s.trim().to_string()))
    }
}

struct ContainsFn;
impl JsltFunction for ContainsFn {
    fn name(&self) -> &'static str {
        "contains"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let elt = &args[0];
        let seq = &args[1];

        match seq {
            JsltValue::Array(a) => {
                // deep equality
                let found = a.iter().any(|v| elt.deep_eq(&v.clone()));
                Ok(JsltValue::bool(found))
            }
            JsltValue::String(s) => {
                if elt.is_null() {
                    return Ok(JsltValue::bool(false));
                }
                let needle = elt.stringify();
                Ok(JsltValue::bool(s.contains(&needle)))
            }
            JsltValue::Object(o) => {
                if elt.is_null() {
                    return Ok(JsltValue::bool(false));
                }
                let k = elt.stringify();
                Ok(JsltValue::bool(o.contains_key(&k)))
            }
            JsltValue::Null => Ok(JsltValue::bool(false)),
            _ => Err(StdlibError::Type(format!("contains: unsupported type {}", seq.type_of()))),
        }
    }
}

struct JoinFn;
impl JsltFunction for JoinFn {
    fn name(&self) -> &'static str {
        "join"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let arr = &args[0];
        let sepv = &args[1];

        if arr.is_null() {
            return Ok(JsltValue::null());
        }
        let a = expect_array(arr, "join", 1)?;
        let sep = expect_string(sepv, "join", 2)?;

        let mut out = String::new();
        for (i, v) in a.iter().enumerate() {
            if i > 0 {
                out.push_str(sep);
            }
            out.push_str(v.clone().stringify().as_str());
        }
        Ok(JsltValue::string(out))
    }
}

struct ErrorFn;
impl JsltFunction for ErrorFn {
    fn name(&self) -> &'static str {
        "error"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let msg = expect_string(&args[0], "error", 1)?;
        Err(StdlibError::Semantic(msg.to_string()))
    }
}

// The evaluator calls `fallback` lazily (like the Java macro) and only uses this for its arity.
struct FallbackFn;
impl JsltFunction for FallbackFn {
    fn name(&self) -> &'static str {
        "fallback"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 2, max: Some(1024) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        for v in args {
            match v {
                JsltValue::Null => {}
                JsltValue::Array(a) if a.is_empty() => {}
                JsltValue::Object(o) if o.is_empty() => {}
                _ => return Ok(v.clone()),
            }
        }
        Ok(JsltValue::null())
    }
}

struct MinFn;
impl JsltFunction for MinFn {
    fn name(&self) -> &'static str {
        "min"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];
        let b = &args[1];

        if a.is_null() || b.is_null() {
            return Ok(JsltValue::null());
        }

        // Compare only when types are compatible. Otherwise: type error
        let ord = match (a, b) {
            (JsltValue::Number(na), JsltValue::Number(nb)) => {
                let fa = na.as_f64().unwrap_or(0.0);
                let fb = nb.as_f64().unwrap_or(0.0);
                fa.total_cmp(&fb)
            }
            (JsltValue::String(sa), JsltValue::String(sb)) => sa.cmp(sb),
            (JsltValue::Bool(ba), JsltValue::Bool(bb)) => ba.cmp(bb),
            // Mismatched or unsuported type
            _ => {
                return Err(StdlibError::Type(format!(
                    "min: incompatible types {} and {}",
                    a.type_of(),
                    b.type_of()
                )))
            }
        };

        Ok(match ord {
            std::cmp::Ordering::Less | std::cmp::Ordering::Equal => a.clone(),
            std::cmp::Ordering::Greater => b.clone(),
        })
    }
}

struct MaxFn;
impl JsltFunction for MaxFn {
    fn name(&self) -> &'static str {
        "max"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];
        let b = &args[1];

        if a.is_null() || b.is_null() {
            return Ok(JsltValue::null());
        }

        let ord = match (a, b) {
            (JsltValue::Number(na), JsltValue::Number(nb)) => {
                let fa = na.as_f64().unwrap_or(0.0);
                let fb = nb.as_f64().unwrap_or(0.0);
                fa.total_cmp(&fb)
            }
            (JsltValue::String(sa), JsltValue::String(sb)) => sa.cmp(sb),
            (JsltValue::Bool(ba), JsltValue::Bool(bb)) => ba.cmp(bb),
            _ => {
                return Err(StdlibError::Type(format!(
                    "max: incompatible types {} and {}",
                    a.type_of(),
                    b.type_of()
                )))
            }
        };

        Ok(match ord {
            std::cmp::Ordering::Less | std::cmp::Ordering::Equal => b.clone(),
            std::cmp::Ordering::Greater => a.clone(),
        })
    }
}

struct IsNumberFn;
impl JsltFunction for IsNumberFn {
    fn name(&self) -> &'static str {
        "is-number"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let is_num = args[0].is_number();
        Ok(JsltValue::bool(is_num))
    }
}

struct IsIntegerFn;
impl JsltFunction for IsIntegerFn {
    fn name(&self) -> &'static str {
        "is-integer"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];
        let is_int = match v {
            JsltValue::Number(n) => n.is_i64() || n.is_u64(),
            _ => false,
        };
        Ok(JsltValue::bool(is_int))
    }
}

struct IsDecimalFn;
impl JsltFunction for IsDecimalFn {
    fn name(&self) -> &'static str {
        "is-decimal"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];
        let is_dec = match v {
            JsltValue::Number(n) => n.is_f64(),
            _ => false,
        };
        Ok(JsltValue::bool(is_dec))
    }
}

struct RoundFn;
impl JsltFunction for RoundFn {
    fn name(&self) -> &'static str {
        "round"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        match v {
            JsltValue::Number(n) => {
                if n.is_i64() || n.is_u64() {
                    // Already integer
                    Ok(v.clone())
                } else if let Some(f) = n.as_f64() {
                    let r = f.round();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        // Prefer integer if in i64 range
                        Ok(JsltValue::number_i64(r as i64))
                    } else {
                        // Otherwise, return as f64 for extremely large numbers
                        Ok(JsltValue::number_f64(r))
                    }
                } else {
                    // Should not occur with serde_json::Number
                    Err(StdlibError::Type("round: unsupported numeric value".to_string()))
                }
            }
            _ => Err(StdlibError::Type("round: unsupported type".to_string())),
        }
    }
}

struct FloorFn;
impl JsltFunction for FloorFn {
    fn name(&self) -> &'static str {
        "floor"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        match v {
            JsltValue::Number(n) => {
                if n.is_i64() || n.is_u64() {
                    Ok(v.clone())
                } else if let Some(f) = n.as_f64() {
                    let r = f.floor();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        Ok(JsltValue::number_i64(r as i64))
                    } else {
                        Ok(JsltValue::number_f64(r))
                    }
                } else {
                    Err(StdlibError::Type("floor: unsupported numeric value".to_string()))
                }
            }
            _ => Err(StdlibError::Type("floor: unsupported type".to_string())),
        }
    }
}

struct CeilingFn;
impl JsltFunction for CeilingFn {
    fn name(&self) -> &'static str {
        "ceiling"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        match v {
            JsltValue::Number(n) => {
                if n.is_i64() || n.is_u64() {
                    Ok(v.clone())
                } else if let Some(f) = n.as_f64() {
                    let r = f.ceil();
                    if r >= i64::MIN as f64 && r <= i64::MAX as f64 {
                        Ok(JsltValue::number_i64(r as i64))
                    } else {
                        Ok(JsltValue::number_f64(r))
                    }
                } else {
                    Err(StdlibError::Type("floor: unsupported numeric value".to_string()))
                }
            }
            _ => Err(StdlibError::Type("floor: unsupported type".to_string())),
        }
    }
}

struct RandomFn;
impl JsltFunction for RandomFn {
    fn name(&self) -> &'static str {
        "random"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(0)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let x: f64 = rand::random();
        Ok(JsltValue::number_f64(x))
    }
}

struct SumFn;
impl JsltFunction for SumFn {
    fn name(&self) -> &'static str {
        "sum"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let arr = &args[0];

        if arr.is_null() {
            return Ok(JsltValue::null());
        }

        let a = expect_array(arr, "sum", 1)?;

        // Accumulate integers in i128 to reduce overflow risk.
        // If we encounter any decimal value we switch to f64 accumulation
        let mut int_acc: i128 = 0;
        let mut float_acc: f64 = 0.0;
        let mut any_float = false;

        for v in a {
            match v {
                JsltValue::Number(n) => {
                    if let Some(i) = n.as_i64() {
                        if any_float {
                            float_acc += i as f64;
                        } else {
                            int_acc = int_acc.saturating_add(i as i128)
                        }
                    } else if let Some(u) = n.as_u64() {
                        if any_float {
                            float_acc += u as f64;
                        } else {
                            int_acc = int_acc.saturating_add(u as i128)
                        }
                    } else if let Some(f) = n.as_f64() {
                        any_float = true;
                        float_acc += f;
                    } else {
                        return Err(StdlibError::Type(
                            "sum: unsupported numeric value".to_string(),
                        ));
                    }
                }
                _ => return Err(StdlibError::Type("sum: elements must be numbers".to_string())),
            }
        }

        if any_float {
            let total = float_acc + int_acc as f64;
            if !total.is_finite() {
                return Err(StdlibError::Type("sum: result is not a finite number".to_string()));
            }
            Ok(JsltValue::number_f64(total))
        } else if int_acc >= i64::MIN as i128 && int_acc <= i64::MAX as i128 {
            Ok(JsltValue::number_i64(int_acc as i64))
        } else {
            // fallback to f64 if outside i64 range
            Ok(JsltValue::number_f64(int_acc as f64))
        }
    }
}

struct ModFn;
impl JsltFunction for ModFn {
    fn name(&self) -> &'static str {
        "mod"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];
        let b = &args[1];

        if a.is_null() || b.is_null() {
            return Ok(JsltValue::null());
        }

        // Helper to extract
        let ai = expect_int_i128(a, "mod", 1)?;
        let di = expect_int_i128(b, "mod", 2)?;

        if di == 0 {
            return Err(StdlibError::Semantic("mod: divisor cannot be zero".to_string()));
        }

        // Euclidean modules: result in 0..abs(d)
        let d_abs = if di < 0 { -di } else { di };
        let r = ((ai % di) + d_abs) % d_abs;

        if r >= i64::MIN as i128 && r <= i64::MAX as i128 {
            Ok(JsltValue::number_i64(r as i64))
        } else {
            Ok(JsltValue::number_f64(r as f64))
        }
    }
}

struct HashIntFn;
impl JsltFunction for HashIntFn {
    fn name(&self) -> &'static str {
        "hash-int"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        // Build canonical JSON then compute java-like hash over UTF-16 code units
        let mut s = String::with_capacity(64);
        canonical_json_string(v, &mut s);
        let h = java_string_hash(&s) as i64;
        Ok(JsltValue::number_i64(h))
    }
}

// Helper: lower-hex encoded bytes without extra deps
fn to_hex_lower(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for &b in bytes {
        out.push(HEX[(b >> 4) as usize] as char);
        out.push(HEX[(b & 0x0f) as usize] as char);
    }
    out
}

struct Sha256HexFn;
impl JsltFunction for Sha256HexFn {
    fn name(&self) -> &'static str {
        "sha256-hex"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        // Spec: non-strings are stringified; strings are used as-is
        let message = v.stringify();
        let digest = Sha256::digest(message.as_bytes());
        let hex = to_hex_lower(&digest);
        Ok(JsltValue::string(hex))
    }
}

struct IsStringFn;
impl JsltFunction for IsStringFn {
    fn name(&self) -> &'static str {
        "is-string"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(args[0].is_string()))
    }
}

struct FromJsonFn;
impl JsltFunction for FromJsonFn {
    fn name(&self) -> &'static str {
        "from-json"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 1, max: Some(2) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];
        let fallback = args.get(1);

        let Some(s) = to_string_or_null(v) else {
            return Ok(JsltValue::null());
        };
        // Like Jackson, input of only JSON whitespace parses to nothing.
        if s.trim_matches([' ', '\t', '\r', '\n']).is_empty() {
            return Ok(JsltValue::null());
        }

        match from_str::<Value>(&s) {
            Ok(parsed) => Ok(JsltValue::from_json(parsed)),
            Err(_) => {
                if let Some(fb) = fallback {
                    Ok(fb.clone())
                } else {
                    Err(StdlibError::Type("from-json: failed to parse JSON".to_string()))
                }
            }
        }
    }
}

struct ToJsonFn;
impl JsltFunction for ToJsonFn {
    fn name(&self) -> &'static str {
        "to-json"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        // Serialize any JSON vlaue to compact JSON text
        let s =
            to_string(&args[0]).map_err(|e| StdlibError::Semantic(format!("to-json: {}", e)))?;
        Ok(JsltValue::string(s))
    }
}

struct NotFn;
impl JsltFunction for NotFn {
    fn name(&self) -> &'static str {
        "not"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(!args[0].truthy()))
    }
}

struct IsBooleanFn;
impl JsltFunction for IsBooleanFn {
    fn name(&self) -> &'static str {
        "is-boolean"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(args[0].is_boolean()))
    }
}

struct IsObjectFn;
impl JsltFunction for IsObjectFn {
    fn name(&self) -> &'static str {
        "is-object"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(args[0].is_object()))
    }
}

struct IsArrayFn;
impl JsltFunction for IsArrayFn {
    fn name(&self) -> &'static str {
        "is-array"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        Ok(JsltValue::bool(args[0].is_array()))
    }
}

#[cfg(feature = "regex")]
struct TestFn;
#[cfg(feature = "regex")]
impl JsltFunction for TestFn {
    fn name(&self) -> &'static str {
        "test"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let i = &args[0];
        let r = &args[1];

        if i.is_null() {
            return Ok(JsltValue::bool(false));
        }

        let input = str_or_stringify(i);
        let pattern = expect_string(r, "test", 2)?;

        let re = cached_regex(pattern)
            .map_err(|e| StdlibError::Semantic(format!("test: invalid regexp {}", e)))?;
        Ok(JsltValue::bool(re.is_match(&input)))
    }
}

#[cfg(feature = "regex")]
struct CaptureFn;
#[cfg(feature = "regex")]
impl JsltFunction for CaptureFn {
    fn name(&self) -> &'static str {
        "capture"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let i = &args[0];
        let r = &args[1];

        if i.is_null() {
            return Ok(JsltValue::null());
        }

        let input = str_or_stringify(i);
        let pattern = expect_string(r, "capture", 2)?;

        let re = cached_regex(pattern)
            .map_err(|e| StdlibError::Semantic(format!("capture: invalid regexp {}", e)))?;

        if let Some(caps) = re.captures(&input) {
            let mut obj = Map::new();
            for name in re.capture_names().flatten() {
                if let Some(m) = caps.name(name) {
                    obj.insert(name.to_string(), JsltValue::string(m.as_str()));
                }
            }
            Ok(JsltValue::object(obj))
        } else {
            // No match empty object
            Ok(JsltValue::object(Map::new()))
        }
    }
}

#[cfg(feature = "regex")]
struct SplitFn;
#[cfg(feature = "regex")]
impl JsltFunction for SplitFn {
    fn name(&self) -> &'static str {
        "split"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let i = &args[0];
        let r = &args[1];

        if i.is_null() {
            return Ok(JsltValue::null());
        }
        let input = str_or_stringify(i);
        let pattern = expect_string(r, "split", 2)?;

        let re = cached_regex(pattern)
            .map_err(|e| StdlibError::Semantic(format!("split: invalid regexp {}", e)))?;

        Ok(JsltValue::array(
            java_split(&re, &input).into_iter().map(|s| JsltValue::string(s.to_string())).collect(),
        ))
    }
}

/// Splits like Java's `String.split(regex)`: with no match the result is the whole input,
/// trailing empty strings are dropped, and a zero-width match at the start adds no leading "".
/// Strings as-is, anything else stringified.
#[cfg(feature = "regex")]
fn str_or_stringify(v: &JsltValue) -> std::borrow::Cow<'_, str> {
    match v.as_str() {
        Some(s) => std::borrow::Cow::Borrowed(s),
        None => std::borrow::Cow::Owned(v.stringify()),
    }
}

#[cfg(feature = "regex")]
const REGEX_CACHE_CAPACITY: usize = 256;

/// Compiles `pattern`, reusing earlier compilations. The cache is cleared when full.
///
/// Shared through `Arc`: cloning a `Regex` would give it a new, cold match cache.
#[cfg(feature = "regex")]
fn cached_regex(pattern: &str) -> Result<Arc<Regex>, regex::Error> {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};

    static CACHE: OnceLock<Mutex<HashMap<String, Arc<Regex>>>> = OnceLock::new();
    let cache = CACHE.get_or_init(Default::default);
    if let Some(re) = cache.lock().unwrap_or_else(|e| e.into_inner()).get(pattern) {
        return Ok(re.clone());
    }
    let re = Arc::new(Regex::new(pattern)?);
    let mut map = cache.lock().unwrap_or_else(|e| e.into_inner());
    if map.len() >= REGEX_CACHE_CAPACITY {
        map.clear();
    }
    map.insert(pattern.to_string(), re.clone());
    Ok(re)
}

#[cfg(feature = "regex")]
fn java_split<'a>(re: &Regex, input: &'a str) -> Vec<&'a str> {
    let mut parts = Vec::new();
    let mut last = 0;
    for m in re.find_iter(input) {
        if m.end() == 0 {
            continue;
        }
        parts.push(&input[last..m.start()]);
        last = m.end();
    }
    if parts.is_empty() {
        return vec![input];
    }
    parts.push(&input[last..]);
    while parts.last() == Some(&"") {
        parts.pop();
    }
    parts
}

#[cfg(feature = "regex")]
struct ReplaceFn;
#[cfg(feature = "regex")]
impl JsltFunction for ReplaceFn {
    fn name(&self) -> &'static str {
        "replace"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(3)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;

        if args[0].is_null() {
            return Ok(JsltValue::null());
        }

        let value = str_or_stringify(&args[0]);
        let pattern = expect_string(&args[1], "replace", 2)?;
        let out = expect_string(&args[2], "replace", 3)?;

        let re = cached_regex(pattern)
            .map_err(|e| StdlibError::Semantic(format!("replace: invalid regexp {}", e)))?;

        // Spec: error if the regexp EVER matches the empty string
        if re.find("").is_some() {
            return Err(StdlibError::Semantic(
                "replace: regexp must not match the empty string".to_string(),
            ));
        }

        let replaced = re.replace_all(&value, out).into_owned();
        Ok(JsltValue::string(replaced))
    }
}

// Helper: format a 16-byte UUID as 8-4-4-4-12 lower-hex with dashes.
fn format_uuid_hyphenated(bytes: &[u8; 16]) -> String {
    let mut out = String::with_capacity(16);
    let hex = to_hex_lower(bytes);
    // hex is 32 chars; insert dashes at 8, 12, 16, 20
    out.push_str(&hex[0..8]);
    out.push('-');
    out.push_str(&hex[8..12]);
    out.push('-');
    out.push_str(&hex[12..16]);
    out.push('-');
    out.push_str(&hex[16..20]);
    out.push('-');
    out.push_str(&hex[20..32]);
    out
}

struct UuidV4JSLT;
impl JsltFunction for UuidV4JSLT {
    fn name(&self) -> &'static str {
        "uuid"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 0, max: Some(2) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        match args.len() {
            0 => {
                // Random v4 (RFC 4122) UUID
                let mut b: [u8; 16] = rand::random();
                // Set version to 4 (bits 12..15 of time_hi_and_version)
                b[6] = (b[6] & 0x0F) | 0x40;
                // Set variant to RFC 4122 (10xx)
                b[8] = (b[8] & 0x3F) | 0x80;
                Ok(JsltValue::string(format_uuid_hyphenated(&b)))
            }
            2 => {
                // NIL UUID special case
                if args[0].is_null() && args[1].is_null() {
                    return Ok(JsltValue::string(
                        "00000000-0000-0000-0000-000000000000".to_string(),
                    ));
                }

                // Match original JSLT mask logic exactly (Java's UUID(long, long) semantics
                // Like Java, a null half counts as 0.
                let half = |i: usize| match args[i].is_null() {
                    true => Ok(0),
                    false => expect_int_i128(&args[i], "uuid", i + 1),
                };
                let msb_i = half(0)?;
                let lsb_i = half(1)?;

                // Preserve two's complement bit patterns
                let msb_raw = msb_i as u64;
                let lsb_raw = lsb_i as u64;

                // maskMSB: (msb & 0xFFFFFFFFFFFF0000) + (1 << 12) + ((msb & 0xFFFF) >> 4)
                let masked_msb =
                    (msb_raw & 0xFFFFFFFFFFFF0000u64) | (1u64 << 12) | ((msb_raw & 0xFFFF) >> 4);

                // maskLSB: (lsb & 0x3FFFFFFFFFFFFFFF) + 0x8000000000000000
                let masked_lsb = (lsb_raw & 0x3FFF_FFFF_FFFF_FFFFu64) | 0x8000_0000_0000_0000u64;

                // Pack big-endian into bytes
                let mut bytes = [0u8; 16];
                for (i, byte) in bytes.iter_mut().enumerate().take(8) {
                    *byte = ((masked_msb >> (8 * (7 - i))) & 0xFF) as u8;
                }
                for (i, byte) in bytes.iter_mut().skip(8).enumerate().take(8) {
                    *byte = ((masked_lsb >> (8 * (7 - i))) & 0xFF) as u8;
                }

                Ok(JsltValue::string(format_uuid_hyphenated(&bytes)))
            }
            _ => Err(StdlibError::Arity { expected: "0 or 2".to_string(), got: args.len() }),
        }
    }
}

struct ArrayFn;
impl JsltFunction for ArrayFn {
    fn name(&self) -> &'static str {
        "array"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        match v {
            JsltValue::Array(_) => Ok(v.clone()),
            JsltValue::Object(obj) => {
                let mut out: Vec<JsltValue> = Vec::with_capacity(obj.len());
                for (k, val) in obj.iter() {
                    let mut entry = Map::new();
                    entry.insert("key".to_string(), JsltValue::string(k.as_str()));
                    entry.insert("value".to_string(), val.clone());
                    out.push(JsltValue::object(entry));
                }
                Ok(JsltValue::array(out))
            }
            _ => Err(StdlibError::Type(format!("array: unsupported type {}", v.type_of()))),
        }
    }
}

struct FlattenFn;
impl JsltFunction for FlattenFn {
    fn name(&self) -> &'static str {
        "flatten"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        let arr = expect_array(v, "flatten", 1)?;

        fn flatten_value(val: &JsltValue, out: &mut Vec<JsltValue>) {
            match val {
                JsltValue::Array(arr) => {
                    for elt in arr.iter() {
                        flatten_value(elt, out);
                    }
                }
                _ => out.push(val.clone()),
            }
        }

        let mut flat: Vec<JsltValue> = Vec::new();
        for elt in arr {
            flatten_value(elt, &mut flat);
        }

        Ok(JsltValue::array(flat))
    }
}

struct AllFn;
impl JsltFunction for AllFn {
    fn name(&self) -> &'static str {
        "all"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        let arr = expect_array(v, "all", 1)?;

        let mut result = true;
        for elt in arr {
            if !elt.clone().truthy() {
                result = false;
                break;
            }
        }

        Ok(JsltValue::bool(result))
    }
}

struct AnyFn;
impl JsltFunction for AnyFn {
    fn name(&self) -> &'static str {
        "any"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        let arr = expect_array(v, "any", 1)?;

        let mut result = false;
        for elt in arr {
            if elt.clone().truthy() {
                result = true;
                break;
            }
        }

        Ok(JsltValue::bool(result))
    }
}

struct ZipFn;
impl JsltFunction for ZipFn {
    fn name(&self) -> &'static str {
        "zip"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];
        let b = &args[1];

        if a.is_null() || b.is_null() {
            return Ok(JsltValue::null());
        }

        let arr1 = expect_array(a, "zip", 1)?;
        let arr2 = expect_array(b, "zip", 2)?;

        if arr1.len() != arr2.len() {
            return Err(StdlibError::Semantic(
                "zip: input arrays must have the same length".to_string(),
            ));
        }

        let mut out: Vec<JsltValue> = Vec::with_capacity(arr1.len());
        for (v1, v2) in arr1.iter().zip(arr2.iter()) {
            let pair = JsltValue::array(vec![v1.clone(), v2.clone()]);
            out.push(pair);
        }

        Ok(JsltValue::array(out))
    }
}

struct ZipWithIndexFn;
impl JsltFunction for ZipWithIndexFn {
    fn name(&self) -> &'static str {
        "zip-with-index"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];

        if a.is_null() {
            return Ok(JsltValue::null());
        }

        let arr = expect_array(a, "zip-with-index", 1)?;

        let mut out: Vec<JsltValue> = Vec::with_capacity(arr.len());
        for (idx, val) in arr.iter().enumerate() {
            let mut obj = Map::new();
            obj.insert("value".to_string(), val.clone());
            obj.insert("index".to_string(), JsltValue::Number((idx as i64).into()));
            out.push(JsltValue::object(obj));
        }

        Ok(JsltValue::array(out))
    }
}

struct IndexOfFn;
impl JsltFunction for IndexOfFn {
    fn name(&self) -> &'static str {
        "index-of"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(2)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let a = &args[0];
        let n = &args[1];

        if a.is_null() {
            return Ok(JsltValue::null());
        }

        let arr = expect_array(a, "index-of", 1)?;

        for (i, elt) in arr.iter().enumerate() {
            if elt.clone().eq(n) {
                return Ok(JsltValue::number_i64(i as i64));
            }
        }

        Ok(JsltValue::number_i64(-1))
    }
}

struct NowFn;
impl JsltFunction for NowFn {
    fn name(&self) -> &'static str {
        "now"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(0)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        // Seconds since Unix epoch (UTC) as floating point, with sub-second precision
        let secs =
            now_seconds_portable().map_err(|e| StdlibError::Semantic(format!("now: {}", e)))?;
        Ok(JsltValue::number_f64(secs))
    }
}

/// Java `NodeUtils.toString(node, false)`: strings as-is, anything else (null too) as JSON.
#[cfg(feature = "chrono")]
fn to_java_string(v: &JsltValue) -> std::borrow::Cow<'_, str> {
    match v {
        JsltValue::String(s) => std::borrow::Cow::Borrowed(s),
        other => std::borrow::Cow::Owned(to_string(other).unwrap_or_default()),
    }
}

#[cfg(feature = "chrono")]
/// Start of Java's two-digit year window: 80 years before now, in epoch milliseconds.
fn two_digit_century_start_ms() -> i64 {
    now_seconds_portable()
        .ok()
        .and_then(|s| chrono::DateTime::from_timestamp(s as i64, 0))
        .and_then(|d| d.checked_sub_months(chrono::Months::new(80 * 12)))
        .map_or(0, |d| d.timestamp_millis())
}

#[cfg(feature = "chrono")]
struct ParseTimeFn;
#[cfg(feature = "chrono")]
impl JsltFunction for ParseTimeFn {
    fn name(&self) -> &'static str {
        "parse-time"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 2, max: Some(3) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let Some(text) = to_string_or_null(&args[0]) else {
            return Ok(JsltValue::null());
        };
        let fmt = to_java_string(&args[1]);
        let pattern = JavaPattern::compile(&fmt).map_err(|e| {
            StdlibError::Semantic(format!("parse-time: Couldn't parse format '{fmt}': {e}"))
        })?;
        match (pattern.parse(&text, two_digit_century_start_ms()), args.get(2)) {
            (Some(ms), _) => Ok(JsltValue::number_f64(ms as f64 / 1000.0)),
            (None, Some(fallback)) => Ok(fallback.clone()),
            (None, None) => {
                Err(StdlibError::Semantic(format!("parse-time: Unparseable date: \"{text}\"")))
            }
        }
    }
}

#[cfg(feature = "chrono")]
struct FormatTimeFn;
#[cfg(feature = "chrono")]
impl JsltFunction for FormatTimeFn {
    fn name(&self) -> &'static str {
        "format-time"
    }
    fn arity(&self) -> Arity {
        Arity::Range { min: 2, max: Some(3) }
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let seconds = match &args[0] {
            JsltValue::Number(n) => n.as_f64().unwrap_or(f64::NAN),
            JsltValue::String(s) => parse_number(s)
                .and_then(|n| n.as_f64_checked())
                .ok_or_else(|| StdlibError::Type(format!("number({s}) failed: not a number")))?,
            _ => return Ok(JsltValue::null()),
        };

        let fmt = to_java_string(&args[1]);
        let zone = match args.get(2) {
            Some(v) => {
                let name = to_java_string(v);
                Zone::from_id(&name).ok_or_else(|| {
                    StdlibError::Semantic(format!("format-time: Unknown timezone {name}"))
                })?
            }
            None => Zone::Utc,
        };
        let pattern = JavaPattern::compile(&fmt).map_err(|e| {
            StdlibError::Semantic(format!("format-time: Couldn't parse format '{fmt}': {e}"))
        })?;
        let epoch_ms = (seconds * 1000.0 + 0.5).floor();
        if !epoch_ms.is_finite() || epoch_ms.abs() > i64::MAX as f64 {
            return Err(StdlibError::Semantic("format-time: timestamp out of range".into()));
        }
        let out = pattern
            .format(epoch_ms as i64, &zone)
            .map_err(|e| StdlibError::Semantic(format!("format-time: {e}")))?;
        Ok(JsltValue::string(out))
    }
}

struct ParseUrlFn;
impl JsltFunction for ParseUrlFn {
    fn name(&self) -> &'static str {
        "parse-url"
    }
    fn arity(&self) -> Arity {
        Arity::Exact(1)
    }
    fn call(&self, args: &[JsltValue]) -> StdResult {
        self.arity().check(args.len())?;
        let v = &args[0];

        if v.is_null() {
            return Ok(JsltValue::null());
        }
        let s = expect_string(v, "parse-url", 0)?;

        let out =
            url::parse_url(s).map_err(|e| StdlibError::Semantic(format!("parse-url: {}", e)))?;

        Ok(JsltValue::from_json(Value::Object(out)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn j(v: Value) -> JsltValue {
        JsltValue::from_json(v)
    }

    #[test]
    fn registry_with_default_has_all_functions_and_is_stable() {
        let r = Registry::with_default();
        // Expect 14 built-ins as registered above
        assert_eq!(r.len(), 52);
        for name in [
            "string",
            "number",
            "boolean",
            "size",
            "keys",
            "values",
            "get-key",
            "starts-with",
            "ends-with",
            "uppercase",
            "lowercase",
            "trim",
            "contains",
            "join",
            "error",
            "fallback",
            "min",
            "max",
            "is-number",
            "is-integer",
            "is-decimal",
            "round",
            "floor",
            "ceiling",
            "random",
            "sum",
            "mod",
            "hash-int",
            "sha256-hex",
            "is-string",
            "from-json",
            "to-json",
            "not",
            "is-boolean",
            "is-object",
            "is-array",
            #[cfg(feature = "regex")]
            "test",
            #[cfg(feature = "regex")]
            "capture",
            #[cfg(feature = "regex")]
            "split",
            #[cfg(feature = "regex")]
            "replace",
            "uuid",
            "array",
            "flatten",
            "all",
            "any",
            "zip",
            "zip-with-index",
            "index-of",
            "now",
            #[cfg(feature = "chrono")]
            "parse-time",
            #[cfg(feature = "chrono")]
            "format-time",
            "parse-url",
        ] {
            assert!(r.get_id(name).is_some(), "missing function {}", name);
        }
    }

    #[test]
    fn call_by_id_dispatch_works_uppercase() {
        let r = Registry::with_default();
        let id = r.get_id("uppercase").unwrap();
        let out = r.call_by_id(id, &[j(json!("MiXeD"))]).unwrap();
        assert_eq!(out, JsltValue::string("MIXED".to_string()));
    }

    #[test]
    fn number_parses_and_respects_fallback() {
        let f = NumberFn;
        // parse numeric string; surrounding whitespace is not allowed
        let ok = f.call(&[j(json!("42"))]).unwrap();
        assert_eq!(ok, JsltValue::number_i64(42));
        assert!(f.call(&[j(json!("  42 "))]).is_err());

        // preserve numbers
        let ok2 = f.call(&[j(json!(4.14))]).unwrap();
        assert_eq!(ok2, JsltValue::number_f64(4.14));

        // null passthrough
        let n = f.call(&[j(json!(null))]).unwrap();
        assert!(n.is_null());

        // bad string without fallback => error
        let err = f.call(&[j(json!("nope"))]).unwrap_err();
        match err {
            StdlibError::Type(msg) => assert!(msg.contains("not a number")),
            _ => panic!("wrong err"),
        }

        // bad type with fallback => fallback
        let out = f.call(&[j(json!({"x":1})), j(json!(999))]).unwrap();
        assert!(out.deep_eq(&JsltValue::number_f64(999.0)));
    }

    #[test]
    fn size_on_string_array_object_and_errors() {
        let f = SizeFn;
        assert_eq!(f.call(&[j(json!("hé"))]).unwrap(), JsltValue::number_i64(2)); // unicode chars
        assert_eq!(f.call(&[j(json!([1, 2, 3]))]).unwrap(), JsltValue::number_i64(3));
        assert_eq!(f.call(&[j(json!({"a":1,"b":2}))]).unwrap(), JsltValue::number_i64(2));
        assert!(f.call(&[j(json!(true))]).is_err());
        assert!(f.call(&[j(json!(null))]).unwrap().is_null());
    }

    #[test]
    fn keys_values_and_get_key() {
        let obj = j(json!({"a":1,"b":2}));
        let keys = KeysFn.call(std::slice::from_ref(&obj)).unwrap();
        // Order of BTreeMap iteration is sorted by key
        assert_eq!(keys, JsltValue::array(vec![j(json!("a")), j(json!("b"))]));

        let values = ValuesFn.call(std::slice::from_ref(&obj)).unwrap();
        assert_eq!(values, JsltValue::array(vec![j(json!(1)), j(json!(2))]));

        // get existing key
        let got = GetKeyFn.call(&[obj.clone(), j(json!("b"))]).unwrap();
        assert_eq!(got, j(json!(2)));
        // missing key -> null
        assert!(GetKeyFn.call(&[obj.clone(), j(json!("z"))]).unwrap().is_null());
        // missing key with default
        let def = GetKeyFn.call(&[obj.clone(), j(json!("z")), j(json!(123))]).unwrap();
        assert_eq!(def, j(json!(123)));
    }

    #[test]
    fn starts_ends_upper_lower_trim() {
        assert_eq!(
            StartsWithFn.call(&[j(json!("hello")), j(json!("he"))]).unwrap(),
            j(json!(true))
        );
        assert_eq!(EndsWithFn.call(&[j(json!("hello")), j(json!("lo"))]).unwrap(), j(json!(true)));
        assert_eq!(UppercaseFn.call(&[j(json!("MiXeD"))]).unwrap(), j(json!("MIXED")));
        assert_eq!(LowercaseFn.call(&[j(json!("MiXeD"))]).unwrap(), j(json!("mixed")));
        assert_eq!(TrimFn.call(&[j(json!("  x \n"))]).unwrap(), j(json!("x")));
        // null propagation
        assert!(UppercaseFn.call(&[j(json!(null))]).unwrap().is_null());
    }

    #[test]
    fn contains_array_string_object_and_errors() {
        // array deep equality
        assert_eq!(
            ContainsFn.call(&[j(json!({"a":1})), j(json!([{"a":1},{"b":2}]))]).unwrap(),
            j(json!(true))
        );
        // string contains with stringify of needle
        assert_eq!(ContainsFn.call(&[j(json!(2)), j(json!("12x"))]).unwrap(), j(json!(true)));
        // object key contains using needle stringify
        assert_eq!(ContainsFn.call(&[j(json!(1)), j(json!({"1": true}))]).unwrap(), j(json!(true)));
        // null cases
        assert_eq!(ContainsFn.call(&[j(json!(null)), j(json!("ab"))]).unwrap(), j(json!(false)));
        assert_eq!(ContainsFn.call(&[j(json!(null)), j(json!({}))]).unwrap(), j(json!(false)));
        assert_eq!(ContainsFn.call(&[j(json!(1)), j(json!(null))]).unwrap(), j(json!(false)));
        // type error
        assert!(ContainsFn.call(&[j(json!(1)), j(json!(true))]).is_err());
    }

    #[test]
    fn join_happy_path_and_null_and_type_errors() {
        let arr = j(json!(["a", 1, true, null]));
        let out = JoinFn.call(&[arr, j(json!(","))]).unwrap();
        assert_eq!(out, j(json!("a,1,true,null")));
        // null array propagates null
        assert!(JoinFn.call(&[j(json!(null)), j(json!(","))]).unwrap().is_null());
        // type errors
        assert!(JoinFn.call(&[j(json!({})), j(json!(","))]).is_err());
        assert!(JoinFn.call(&[j(json!([])), j(json!(1))]).is_err());
    }

    #[test]
    fn sum_with_floats_returns_float_even_if_whole() {
        let arr = j(json!([1.5, 1.5]));
        let out = SumFn.call(&[arr]).unwrap();
        assert_eq!(out.to_json(), json!(3.0));
        assert!(out.as_i64().is_none());
    }

    #[cfg(feature = "regex")]
    #[test]
    fn regex_cache_reuses_compiled_patterns() {
        let a = cached_regex("^a+b$").unwrap();
        let b = cached_regex("^a+b$").unwrap();
        assert!(Arc::ptr_eq(&a, &b));
        assert!(a.is_match("aab"));
        assert!(cached_regex("(").is_err());
    }
}
