//! Builtin edge cases checked against the Java implementation.

use serde_json::{json, Value};

fn run_jslt(src: &str) -> Result<Value, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    program.apply(&Value::Null, None).map_err(|e| e.to_string())
}

#[test]
fn fallback_stops_evaluating_at_first_value() {
    assert_eq!(run_jslt("fallback(22, true + not(false))"), Ok(json!(22)));
    assert_eq!(run_jslt(r#"fallback(null, [], {}, 0, error("x"))"#), Ok(json!(0)));
    assert_eq!(run_jslt("fallback(null, [])"), Ok(json!(null)));
    assert!(run_jslt(r#"fallback(null, error("boom"))"#).is_err());
    assert!(run_jslt("fallback(1)").is_err());
}

#[test]
fn user_defined_fallback_is_not_lazy() {
    assert_eq!(run_jslt("def fallback(a, b) $b fallback(1, 2)"), Ok(json!(2)));
}

#[test]
fn get_key_converts_key_like_java() {
    assert_eq!(run_jslt(r#"get-key({"a": 1}, null)"#), Ok(json!(null)));
    assert_eq!(run_jslt(r#"get-key({"1": 2}, 1)"#), Ok(json!(2)));
}

#[test]
fn from_json_converts_and_accepts_empty_input() {
    assert_eq!(run_jslt(r#"from-json("")"#), Ok(json!(null)));
    assert_eq!(run_jslt(r#"from-json("  ")"#), Ok(json!(null)));
    assert_eq!(run_jslt("from-json(5)"), Ok(json!(5)));
    assert_eq!(run_jslt(r#"from-json("[1", "bad")"#), Ok(json!("bad")));
}

#[test]
fn uuid_treats_one_null_half_as_zero() {
    assert_eq!(
        run_jslt("uuid(null, 9223372036854775807)"),
        Ok(json!("00000000-0000-1000-bfff-ffffffffffff"))
    );
    assert_eq!(run_jslt("uuid(null, null)"), Ok(json!("00000000-0000-0000-0000-000000000000")));
}

#[test]
fn parse_url_omits_missing_parts() {
    assert_eq!(
        run_jslt(r#"parse-url("https://example.com:8443")"#),
        Ok(json!({"host": "example.com", "port": 8443, "scheme": "https"}))
    );
    assert_eq!(
        run_jslt(r#"parse-url("http://x.com/#")"#),
        Ok(json!({"host": "x.com", "path": "/", "scheme": "http", "fragment": ""}))
    );
}

#[test]
fn parse_url_decodes_query_like_java() {
    let out = run_jslt(r#"parse-url("http://x.com/p?a+b=%41%20c&=z&&d=")"#).unwrap();
    assert_eq!(out["parameters"], json!({"a b": ["A c"], "=z": [null], "": [null], "d": [null]}));
    assert!(run_jslt(r#"parse-url("http://x.com/?a=%zz")"#).is_err());
}
