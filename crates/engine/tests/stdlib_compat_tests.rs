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
    assert_eq!(run_jslt(r#"from-json(" \t\r\n")"#), Ok(json!(null)));
    // Other whitespace is not valid JSON.
    assert_eq!(run_jslt(r#"from-json("\u00a0", "bad")"#), Ok(json!("bad")));
    assert_eq!(run_jslt(r#"from-json("\u000b", "bad")"#), Ok(json!("bad")));
    assert!(run_jslt(r#"from-json("\u00a0")"#).is_err());
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
    // Trailing empty pairs are dropped; empty pairs in the middle are kept.
    let out = run_jslt(r#"parse-url("http://x.com/?a=1&&")"#).unwrap();
    assert_eq!(out["parameters"], json!({"a": ["1"]}));
    let out = run_jslt(r#"parse-url("http://x.com/?&")"#).unwrap();
    assert_eq!(out["parameters"], json!({}));
}

#[test]
fn format_time_supports_millis_and_zone_names() {
    let t = "125641293.0";
    assert_eq!(
        run_jslt(&format!(r#"format-time({t}, "yyyy-MM-dd HH:mm:ssz")"#)),
        Ok(json!("1973-12-25 04:21:33UTC"))
    );
    assert_eq!(
        run_jslt(&format!(r#"format-time({t}, "yyyy-MM-dd HH:mm:ssz", "CET")"#)),
        Ok(json!("1973-12-25 05:21:33CET"))
    );
    assert_eq!(
        run_jslt(r#"format-time(1530000000, "z zzzz XXX", "Europe/Oslo")"#),
        Ok(json!("CEST Central European Summer Time +02:00"))
    );
    assert_eq!(run_jslt(r#"format-time(1514764800, "z", "PST")"#), Ok(json!("PST")));
    assert_eq!(
        run_jslt(r#"format-time(1514764800.005, "yyyy-MM-dd HH:mm:ss.SSS")"#),
        Ok(json!("2018-01-01 00:00:00.005"))
    );
    assert_eq!(run_jslt(r#"format-time("1514764800", "yyyy")"#), Ok(json!("2018")));
    assert_eq!(run_jslt(r#"format-time(true, "yyyy")"#), Ok(json!(null)));
}

#[test]
fn format_time_rejects_bad_formats_and_zones() {
    let err = run_jslt(r#"format-time(0, "yyyy-MM-dd dfkdfjkdf")"#).unwrap_err();
    assert!(err.contains("Couldn't parse format"), "{err}");
    assert!(err.contains("Illegal pattern character 'f'"), "{err}");
    let err = run_jslt(r#"format-time(0, "yyyy", "Foo")"#).unwrap_err();
    assert!(err.contains("Unknown timezone Foo"), "{err}");
    let err = run_jslt(r#"format-time(0, "yyyy", null)"#).unwrap_err();
    assert!(err.contains("Unknown timezone null"), "{err}");
}

#[test]
fn parse_time_supports_millis_and_zone_names() {
    assert_eq!(
        run_jslt(r#"round(parse-time("1973-12-25 05:21:33CET", "yyyy-MM-dd HH:mm:ssz"))"#),
        Ok(json!(125641293))
    );
    assert_eq!(
        run_jslt(r#"parse-time("2018-01-01 00:00:00.000", "yyyy-MM-dd HH:mm:ss.SSS")"#),
        Ok(json!(1514764800.0))
    );
    // `S` is a count of milliseconds, not a fraction.
    assert_eq!(
        run_jslt(r#"round(parse-time("1973-12-25 04:21:33.123", "yyyy-MM-dd HH:mm:ss.S") * 100)"#),
        Ok(json!(12564129312_i64))
    );
    assert_eq!(run_jslt(r#"parse-time(2018, "yyyy")"#), Ok(json!(1514764800.0)));
}

#[test]
fn parse_time_bad_format_errors_even_with_fallback() {
    let err = run_jslt(r#"parse-time("1973-12-25", "yyyy-MM-dd gnu", null)"#).unwrap_err();
    assert!(err.contains("Couldn't parse format"), "{err}");
    assert_eq!(run_jslt(r#"parse-time("1973-12-25", "yyyy-MM-dd HH", "fb")"#), Ok(json!("fb")));
    let err = run_jslt(r#"parse-time("1973-12-25", "yyyy-MM-dd HH")"#).unwrap_err();
    assert!(err.contains(r#"Unparseable date: "1973-12-25""#), "{err}");
}
