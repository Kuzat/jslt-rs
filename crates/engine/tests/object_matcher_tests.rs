//! Object matcher contexts and sequence indexing, matching the Java implementation.

use serde_json::{json, Value};

fn run_jslt(src: &str, input: Value) -> Result<Value, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    program.apply(&input, None).map_err(|e| e.to_string())
}

fn input() -> Value {
    json!({"a": {"b": 1, "c": 2}, "x": [{"q": 1}]})
}

#[test]
fn matcher_follows_the_enclosing_keys() {
    assert_eq!(run_jslt("{\"a\": {\"b\": 5, * : .}}", input()), Ok(json!({"a": {"b": 5, "c": 2}})));
    assert_eq!(run_jslt("{\"a\": if (true) {* : .}}", input()), Ok(json!({"a": {"b": 1, "c": 2}})));
    assert_eq!(
        run_jslt("{\"a\": string({* : .})}", input()),
        Ok(json!({"a": "{\"b\":1,\"c\":2}"}))
    );
}

#[test]
fn matcher_on_missing_or_non_object_context_copies_nothing() {
    assert_eq!(run_jslt("{\"x\": {* : .}}", input()), Ok(json!({})));
    assert_eq!(run_jslt("{\"a\": {\"zz\": {* : .}}}", input()), Ok(json!({})));
}

#[test]
fn for_and_pipe_restart_the_context() {
    assert_eq!(run_jslt("{\"a\": [for (.x) {* : .}]}", input()), Ok(json!({"a": [{"q": 1}]})));
    assert_eq!(run_jslt("{\"a\": .x[0] | {* : .}}", input()), Ok(json!({"a": {"q": 1}})));
}

#[test]
fn matcher_in_array_or_function_is_an_error() {
    for (src, msg) in [
        ("def f(o) { * : . } f(.)", "Object matcher used inside function declaration"),
        ("def f(o) let x = {* : .} $x f(.)", "Object matcher used inside function declaration"),
        ("{\"x\": [{* : .}]}", "Object matcher used inside array"),
    ] {
        let err = run_jslt(src, input()).unwrap_err();
        assert!(err.contains(msg), "{src}: {err}");
    }
    assert_eq!(run_jslt("def f(o) [for (.) {* : .}] f(.)", input()).map(|_| ()), Ok(()));
}

#[test]
fn bad_indexes_are_errors() {
    for (src, inp, msg) in [
        (".[7]", json!("12345"), "String index out of range: 7"),
        (".[\"a\"]", json!([1, 2]), "Can't index array/string with \"a\""),
        (".[1 : null]", json!("12345"), "Can't index array/string with null"),
    ] {
        let err = run_jslt(src, inp).unwrap_err();
        assert!(err.contains(msg), "{src}: {err}");
    }
    assert_eq!(run_jslt(".[7]", json!([1, 2])), Ok(json!(null)));
    assert_eq!(run_jslt(".[-1]", json!("12345")), Ok(json!("5")));
}
