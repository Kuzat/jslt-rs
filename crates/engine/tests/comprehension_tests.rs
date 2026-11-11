//! For-comprehension sequences, matching the Java implementation.

use serde_json::{json, Value};

fn run_jslt(src: &str, input: Value) -> Result<Value, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    program.apply(&input, None).map_err(|e| e.to_string())
}

#[test]
fn null_sequence_gives_null() {
    assert_eq!(run_jslt("[for (null) number(.)]", json!(null)), Ok(json!(null)));
    assert_eq!(run_jslt("{for (.foo) .key : .value}", json!({})), Ok(json!(null)));
}

#[test]
fn scalar_sequence_is_an_error() {
    for (src, msg) in [
        ("[for (32323) .]", "For loop can't iterate over 32323"),
        ("[for (\"abc\") .]", "For loop can't iterate over \"abc\""),
        ("{for (true) \"a\" : .}", "Object comprehension can't loop over true"),
    ] {
        let err = run_jslt(src, json!(null)).unwrap_err();
        assert!(err.contains(msg), "{src}: {err}");
    }
}

#[test]
fn empty_sequences_give_empty_results() {
    assert_eq!(run_jslt("[for ([]) .]", json!(null)), Ok(json!([])));
    assert_eq!(run_jslt("{for ({}) .key : .value}", json!(null)), Ok(json!({})));
}
