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
fn string_multiplication_is_size_limited() {
    // Overflowing the byte count must be an error, not a panic.
    assert!(run_jslt(r#""abc" * 9223372036854775807"#).is_err());
    // Over the default limit (64 MiB).
    let err = run_jslt(r#""abcd" * 20000000"#).unwrap_err();
    assert!(err.contains("too large"), "{err}");

    let cfg = interp::EvalConfig { max_string_repeat_bytes: Some(5), ..Default::default() };
    let program = engine::compile(r#""ab" * 3"#).unwrap();
    assert!(program.apply(&Value::Null, Some(cfg.clone())).is_err());
    let program = engine::compile(r#""ab" * 2"#).unwrap();
    assert_eq!(program.apply(&Value::Null, Some(cfg)).unwrap(), Value::from("abab"));
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

#[test]
fn and_or_return_booleans() {
    assert_jslt("1 and 2", "true");
    assert_jslt(r#""a" and []"#, "false");
    assert_jslt(r#""" and true"#, "false");
    assert_jslt(".foo and false", "false");
    assert_jslt(".foo or false", "false");
    assert_jslt(r#""a" or false"#, "true");
    assert_jslt("null or [1]", "true");
    assert_jslt("0 or {}", "false");
}

#[test]
fn and_or_short_circuit() {
    assert_jslt(r#"false and error("not evaluated")"#, "false");
    assert_jslt(r#"true or error("not evaluated")"#, "true");
}

fn run_on(src: &str, input: &str) -> String {
    let input: Value = serde_json::from_str(input).unwrap();
    let out = engine::compile(src).unwrap().apply(&input, None).unwrap();
    serde_json::to_string(&out).unwrap()
}

#[test]
fn pipe_binds_dot_to_left_value() {
    assert_jslt("1 + 3 | . * 5", "20");
    assert_jslt("1 | [., .] | {\"a\": .}", r#"{"a":[1,1]}"#);
    assert_eq!(run_on(".a | .b", r#"{"a": {"b": 2}}"#), "2");
    // `.` is restored after the pipe.
    assert_eq!(run_on("[.a | .b, .c]", r#"{"a": {"b": 2}, "c": 3}"#), "[2,3]");
}

#[test]
fn pipe_has_lowest_precedence() {
    assert_jslt("false or false | {\"a\": .}", r#"{"a":false}"#);
    assert_jslt("1 == 3 | {\"a\": .}", r#"{"a":false}"#);
    // The else branch extends over the pipe.
    assert_jslt("if (true) 1 else 2 | . * 10", "1");
    assert_jslt("if (false) 1 else 2 | . * 10", "20");
    assert_jslt("(if (false) 1 else 2) | . * 10", "20");
    assert_jslt("5 | if (. > 1) \"big\" else \"small\"", r#""big""#);
}

#[test]
fn pipe_feeds_object_matcher() {
    assert_eq!(run_on(".foo | {\"x\": 1, * : .}", r#"{"foo": {"a": 2}}"#), r#"{"a":2,"x":1}"#);
}

#[test]
fn matcher_on_left_of_pipe_uses_parent_key() {
    let out = run_on(r#"{"foo": {* : .} | {"n": 1, * : .}}"#, r#"{"foo": {"a": 2}, "bar": 3}"#);
    assert_eq!(out, r#"{"foo":{"a":2,"n":1}}"#);
}
