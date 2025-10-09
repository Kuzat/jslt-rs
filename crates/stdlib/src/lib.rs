//! Minimal stdlib v1: registry + a hanful of basic functions.
//! This crate defined:
//! - JsltFunction trait
//! - StdlibError (arity/type/semantic errors)
//! - Registry that registers built-ins and supports dispatch by function id
//! - a start set of functions: string, number, boolean, size, keys, values
//! get-key (and alias get), starts-with, ends-with, upper, lower, trim,
//! contains, join.

use serde_json::{from_str, to_string, Map, Value};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use thiserror::Error;
use value::JsltValue;

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

        // Boolean
        r.register(BooleanFn);
        r.register(NotFn);
        r.register(IsBooleanFn);

        // Object
        r.register(KeysFn);
        r.register(ValuesFn);
        r.register(GetKeyFn);

        // Array

        // Time

        // URL

        // Regexp

        r
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

fn expect_string<'a>(v: &'a JsltValue, fname: &str, argn: usize) -> Result<&'a str, StdlibError> {
    v.as_json().as_str().ok_or_else(|| {
        StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be string, got {}",
            v.type_of()
        ))
    })
}

fn expect_object<'a>(
    v: &'a JsltValue,
    fname: &str,
    argn: usize,
) -> Result<&'a Map<String, Value>, StdlibError> {
    v.as_json().as_object().ok_or_else(|| {
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
) -> Result<&'a Vec<Value>, StdlibError> {
    v.as_json().as_array().ok_or_else(|| {
        StdlibError::Type(format!(
            "{fname} expects argument #{argn} to be array, got {}",
            v.type_of()
        ))
    })
}

fn expect_int_i128(v: &JsltValue, fname: &str, argn: usize) -> Result<i128, StdlibError> {
    match v.as_json() {
        Value::Number(n) => {
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
fn canonical_json_string(v: &Value, out: &mut String) {
    match v {
        Value::Null => out.push_str("null"),
        Value::Bool(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Number(n) => out.push_str(&n.to_string()),
        Value::String(s) => {
            // reuse serde_json's escaping by serializing the string itself
            // this gives a quoted string with proper escapes
            out.push_str(&serde_json::to_string(s).expect("string serialize"));
        }
        Value::Array(a) => {
            out.push('[');
            for (i, elt) in a.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                canonical_json_string(elt, out);
            }
            out.push(']');
        }
        Value::Object(m) => {
            out.push('{');
            // sort keys lexicographically
            let mut keys: Vec<&str> = m.keys().map(|k| k.as_str()).collect();
            keys.sort_unstable();
            for (i, k) in keys.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                // key must be JSON string with quotes/escapes
                out.push_str(&serde_json::to_string(k).expect("key serialize"));
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
        // null => null
        if v.is_null() {
            return Ok(JsltValue::null());
        }
        // numbers => as-is
        if v.is_number() {
            return Ok(v.clone());
        }
        // strings => parse (allow leading zeros)
        if let Some(s) = v.0.as_str() {
            let s_trim = s.trim();
            match s_trim.parse::<f64>() {
                Ok(n) => Ok(JsltValue::number_f64(n)),
                Err(_) => {
                    if let Some(fb) = fallback {
                        Ok(fb.clone())
                    } else {
                        Err(StdlibError::Type("number: cannot parse string to number".to_string()))
                    }
                }
            }
        } else {
            // wrong type
            if let Some(fb) = fallback {
                Ok(fb.clone())
            } else {
                Err(StdlibError::Type(format!("number: unsupported type {}", v.type_of())))
            }
        }
    }
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
        let n = if let Some(s) = v.0.as_str() {
            s.chars().count()
        } else if let Some(a) = v.0.as_array() {
            a.len()
        } else if let Some(o) = v.0.as_object() {
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
            out.push(JsltValue::from_json(v.clone()));
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
        let key = &args[1];
        if obj.is_null() {
            return Ok(JsltValue::null());
        }
        let m = expect_object(obj, "get-key", 1)?;
        let k = expect_string(key, "get-key", 2)?;
        match m.get(k) {
            Some(v) => Ok(JsltValue::from_json(v.clone())),
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

        match seq.0.clone() {
            Value::Array(a) => {
                // deep equality
                let found = a.iter().any(|v| elt.deep_eq(&JsltValue::from_json(v.clone())));
                Ok(JsltValue::bool(found))
            }
            Value::String(s) => {
                if elt.is_null() {
                    return Ok(JsltValue::bool(false));
                }
                let needle = elt.stringify();
                Ok(JsltValue::bool(s.contains(&needle)))
            }
            Value::Object(o) => {
                if elt.is_null() {
                    return Ok(JsltValue::bool(false));
                }
                let k = elt.stringify();
                Ok(JsltValue::bool(o.contains_key(&k)))
            }
            Value::Null => Ok(JsltValue::bool(false)),
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
            out.push_str(JsltValue::from_json(v.clone()).stringify().as_str());
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

// TODO: Rework this to be lazy and work as macros in the original JSLT
// TODO: meaning it should take in Expressions and only evaluate them as needed.
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
            match v.as_json() {
                Value::Null => {}
                Value::Array(a) if a.is_empty() => {}
                Value::Object(o) if o.is_empty() => {}
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
        let ord = match (a.as_json(), b.as_json()) {
            (Value::Number(na), Value::Number(nb)) => {
                let fa = na.as_f64().unwrap_or(0.0);
                let fb = nb.as_f64().unwrap_or(0.0);
                fa.total_cmp(&fb)
            }
            (Value::String(sa), Value::String(sb)) => sa.cmp(sb),
            (Value::Bool(ba), Value::Bool(bb)) => ba.cmp(bb),
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

        let ord = match (a.as_json(), b.as_json()) {
            (Value::Number(na), Value::Number(nb)) => {
                let fa = na.as_f64().unwrap_or(0.0);
                let fb = nb.as_f64().unwrap_or(0.0);
                fa.total_cmp(&fb)
            }
            (Value::String(sa), Value::String(sb)) => sa.cmp(sb),
            (Value::Bool(ba), Value::Bool(bb)) => ba.cmp(bb),
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
        let is_int = match v.as_json() {
            Value::Number(n) => n.is_i64() || n.is_u64(),
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
        let is_dec = match v.as_json() {
            Value::Number(n) => n.is_f64(),
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

        match v.as_json() {
            Value::Number(n) => {
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

        match v.as_json() {
            Value::Number(n) => {
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

        match v.as_json() {
            Value::Number(n) => {
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
                Value::Number(n) => {
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
            Ok(JsltValue::number(total))
        } else {
            if int_acc >= i64::MIN as i128 && int_acc <= i64::MAX as i128 {
                Ok(JsltValue::number_i64(int_acc as i64))
            } else {
                // fallback to f64 if outside i64 range
                Ok(JsltValue::number_f64(int_acc as f64))
            }
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
        canonical_json_string(v.as_json(), &mut s);
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

        if v.is_null() {
            return Ok(JsltValue::null());
        }

        // Expect string; wrong type -> fallback or error
        let s = match v.as_json().as_str() {
            Some(s) => s,
            None => {
                if let Some(fb) = fallback {
                    return Ok(fb.clone());
                } else {
                    return Err(StdlibError::Type(format!(
                        "from-json: argument must be a string, got {}",
                        v.type_of()
                    )));
                }
            }
        };

        match from_str::<Value>(s) {
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
        let s = to_string(args[0].as_json())
            .map_err(|e| StdlibError::Semantic(format!("to-json: {}", e)))?;
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
        assert_eq!(r.len(), 34);
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
        // parse numeric string
        let ok = f.call(&[j(json!("  42 "))]).unwrap();
        assert_eq!(ok, JsltValue::number_f64(42.0));

        // preserve numbers
        let ok2 = f.call(&[j(json!(3.14))]).unwrap();
        assert_eq!(ok2, JsltValue::number_f64(3.14));

        // null passthrough
        let n = f.call(&[j(json!(null))]).unwrap();
        assert!(n.is_null());

        // bad string without fallback => error
        let err = f.call(&[j(json!("nope"))]).unwrap_err();
        match err {
            StdlibError::Type(msg) => assert!(msg.contains("cannot parse")),
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
        let keys = KeysFn.call(&[obj.clone()]).unwrap();
        // Order of BTreeMap iteration is sorted by key
        assert_eq!(keys, JsltValue::array(vec![j(json!("a")), j(json!("b"))]));

        let values = ValuesFn.call(&[obj.clone()]).unwrap();
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
    fn sum_with_float_summing_to_int_result_should_return_int() {
        let arr = j(json!([1.5, 1.5]));
        let out = SumFn.call(&[arr]).unwrap();
        assert_eq!(out, j(json!(3)));
    }
}
