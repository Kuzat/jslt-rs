//! Operator semantics for null, mixed types and strings, matching the Java implementation.

use serde_json::Value;

fn run_jslt(src: &str) -> Result<String, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    let out = program.apply(&Value::Null, None).map_err(|e| e.to_string())?;
    Ok(serde_json::to_string(&out).unwrap())
}

fn assert_jslt(src: &str, expected: &str) {
    assert_eq!(run_jslt(src).as_deref(), Ok(expected), "program: {src}");
}

#[test]
fn arithmetic_with_null_is_null() {
    for src in ["2 + null", "null + 2", "2 - null", "2.5 * null", "null / 2", "2 % null"] {
        assert_jslt(src, "null");
    }
}

#[test]
fn null_plus_object_or_array_returns_the_other_side() {
    assert_jslt(r#"{"a": 1} + null"#, r#"{"a":1}"#);
    assert_jslt("null + [1, 2]", "[1,2]");
}

#[test]
fn string_concatenation_renders_other_side_as_json() {
    assert_jslt(r#""foo" + null"#, r#""foonull""#);
    assert_jslt(r#"null + "foo""#, r#""nullfoo""#);
    assert_jslt(r#""a" + true"#, r#""atrue""#);
    assert_jslt(r#""a" + 1.5"#, r#""a1.5""#);
    assert_jslt(r#""a" + [1, "b"]"#, r#""a[1,\"b\"]""#);
    assert_jslt(r#""a" + {"k": 1}"#, r#""a{\"k\":1}""#);
}

#[test]
fn string_multiplication_repeats() {
    assert_jslt(r#""foo" * 3"#, r#""foofoofoo""#);
    assert_jslt(r#"2 * "ab""#, r#""abab""#);
    assert_jslt(r#""ab" * 2.9"#, r#""abab""#);
    assert_jslt(r#""ab" * 0"#, r#""""#);
    assert_jslt(r#""ab" * -1"#, r#""""#);
    assert_jslt(r#""ab" * null"#, r#""""#);
    assert!(run_jslt(r#""a" * "b""#).is_err());
}

#[test]
fn null_is_smallest_in_comparisons() {
    assert_jslt("2 > null", "true");
    assert_jslt("2 >= null", "true");
    assert_jslt("null < 2", "true");
    assert_jslt("null > 2", "false");
    assert_jslt(r#"null < "a""#, "true");
    assert_jslt("null >= null", "true");
    assert_jslt("null > null", "false");
}

#[test]
fn incompatible_types_still_error() {
    assert!(run_jslt("true + 1").is_err());
    assert!(run_jslt(r#"1 < "a""#).is_err());
    assert!(run_jslt("[1] - 1").is_err());
}
