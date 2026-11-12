//! `apply_value` gives the same results as `apply`.

use engine::JsltValue;
use serde_json::json;
use std::collections::HashMap;

#[test]
fn apply_value_matches_apply() {
    let input = json!({"a": 1, "b": [1, 2, 3], "c": {"d": "x"}});
    let programs = [
        ".",
        "{\"n\": .a + 1, * : .}",
        "def f(x) $x * 2 [for (.b) f(.)]",
        "let k = .c {for (.) .key : .value}",
        "[.c.d, string(.b), is-object(.c)]",
    ];
    for src in programs {
        let program = engine::compile(src).unwrap();
        let expected = program.apply(&input, None).unwrap();
        let got = program.apply_value(&JsltValue::from_json_ref(&input), None).unwrap();
        assert_eq!(got.to_json(), expected, "{src}");
    }
}

#[test]
fn apply_value_with_variables() {
    let program = engine::compile("{\"v\": $v, \"in\": .}").unwrap();
    let vars = HashMap::from([("v".to_string(), JsltValue::from_json(json!([1])))]);
    let out = program.apply_value_with_variables(&JsltValue::from_json(json!(2)), vars, None);
    assert_eq!(out.unwrap().to_json(), json!({"v": [1], "in": 2}));
}

#[test]
fn apply_value_reports_errors() {
    let program = engine::compile("error(\"boom\")").unwrap();
    assert!(program.apply_value(&JsltValue::from_json(json!(null)), None).is_err());
}
