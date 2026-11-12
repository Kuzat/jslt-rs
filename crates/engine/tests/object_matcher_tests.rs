//! Object keys, matcher contexts and sequence indexing, matching the Java implementation.

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

#[test]
fn computed_keys_evaluate_to_strings() {
    assert_eq!(run_jslt("{\"foo\" + \"_bar\" : 25}", json!(null)), Ok(json!({"foo_bar": 25})));
    assert_eq!(
        run_jslt("{ \"bar\" : { * : . }, string(.foo) : \"hey\" }", json!({"foo": 1})),
        Ok(json!({"1": "hey"}))
    );
    assert_eq!(run_jslt("{sum: 1, \"a\" + \"b\": 2}", json!(null)), Ok(json!({"sum": 1, "ab": 2})));
    assert_eq!(run_jslt("{1 + 1 : null}", json!(null)), Ok(json!({})));
    let src = "{{ // comment\n let x = 1 \"k\": $x} | string(.) : 1}";
    assert_eq!(run_jslt(src, json!(null)), Ok(json!({"{\"k\":1}": 1})));
}

#[test]
fn bad_object_keys_are_errors() {
    for (src, msg) in [
        ("{1 : 2}", "Object key must be string"),
        ("{\"a\" : 1, \"a\" + \"\" : 2}", "Duplicate key 'a' in object"),
        ("{\"a\" : 1, \"b\" : 2, \"a\" : 3}", "Invalid object declaration, duplicate key 'a'"),
        ("{(\"a\") : 1, \"a\" : 2}", "Invalid object declaration, duplicate key 'a'"),
        (
            "{\"a\" + \"\" : 1, * : .}",
            "Object matcher not allowed in objects which have dynamic keys",
        ),
        ("{\"a\" + \"\" : {* : .}}", "Object matcher used inside dynamic object"),
    ] {
        let err = run_jslt(src, input()).unwrap_err();
        assert!(err.contains(msg), "{src}: {err}");
    }
}

#[test]
fn matcher_keeps_empty_and_null_values() {
    let input = json!({"a": null, "b": [], "c": {}, "d": 1});
    assert_eq!(
        run_jslt("{*: if (. == 1) null else .}", input),
        Ok(json!({"a": null, "b": [], "c": {}, "d": null}))
    );
}

#[test]
fn matcher_skips_explicit_and_excluded_keys() {
    let src = r#"{"a": null, "c": {"d": null, * : .}, * - b : .}"#;
    let input = json!({"a": 1, "b": 2, "c": {"d": 3, "e": 4}, "f": 5});
    assert_eq!(run_jslt(src, input), Ok(json!({"c": {"e": 4}, "f": 5})));
}
