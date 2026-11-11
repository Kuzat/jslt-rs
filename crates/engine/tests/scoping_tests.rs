//! Let scoping and expression boundaries, matching the Java implementation.

use serde_json::{json, Value};

fn run_jslt(src: &str, input: Value) -> Result<Value, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    program.apply(&input, None).map_err(|e| e.to_string())
}

#[test]
fn let_value_ends_before_next_expression() {
    assert_eq!(run_jslt("let x = 1 [\"h\" + $x]", json!(null)), Ok(json!(["h1"])));
    assert_eq!(run_jslt("let x = \"a\" {\"k\": $x}", json!(null)), Ok(json!({"k": "a"})));
}

#[test]
fn comprehension_lets_are_visible_in_filter() {
    let src = "[for ([1, 2, 3, 4]) let v = . * 10 $v if ($v > 20)]";
    assert_eq!(run_jslt(src, json!(null)), Ok(json!([30, 40])));
    let src = "{for (.) let v = (.value) .key : $v if ($v > 1)}";
    assert_eq!(run_jslt(src, json!({"a": 1, "b": 2})), Ok(json!({"b": 2})));
}

#[test]
fn object_comprehension_lets_are_evaluated_once_per_element() {
    let out = run_jslt("{for ([1]) let r = random() string($r) : $r}", json!(null)).unwrap();
    let (k, v) = out.as_object().unwrap().iter().next().unwrap();
    assert_eq!(k, &v.as_f64().unwrap().to_string());
}

#[test]
fn inner_scopes_may_shadow() {
    let src = "let v = 1 [for ([1, 2]) let v = 10 . + $v]";
    assert_eq!(run_jslt(src, json!(null)), Ok(json!([11, 12])));
}

#[test]
fn duplicate_let_in_same_scope_is_an_error() {
    for src in [
        "let a = 1 let a = 2 $a",
        "def f() let a = 1 let a = 2 $a f()",
        "[for ([1]) let a = 1 let a = 2 $a]",
    ] {
        let err = run_jslt(src, json!(null)).unwrap_err();
        assert!(err.contains("`a`"), "{src}: {err}");
    }
}

#[test]
fn comprehension_lets_do_not_carry_over_between_elements() {
    // The inner `v` is in scope in its own initializer (as in Java), so `$v` there is the
    // unassigned inner variable, never the previous element's value.
    let src = "let v = (0) [for ([1, 2, 3]) let v = ($v + 1) $v]";
    assert_eq!(run_jslt(src, json!(null)), Ok(json!([null, null, null])));
    let src = "let v = 1 [for ([1, 2]) let v = . * 10 $v] + [$v]";
    assert_eq!(run_jslt(src, json!(null)), Ok(json!([10, 20, 1])));
}

#[test]
fn function_let_may_not_redeclare_parameter() {
    let err = run_jslt("def f(a) let a = 1 $a f(2)", json!(null)).unwrap_err();
    assert!(err.contains("`a`"), "{err}");
}
