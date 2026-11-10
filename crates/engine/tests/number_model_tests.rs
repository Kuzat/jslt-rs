//! Integer/decimal typing of numbers, matching the Java implementation.
//! Outputs are compared as serialized JSON so that `10` and `10.0` differ.

use serde_json::Value;

fn eval(src: &str) -> Result<String, String> {
    let program = engine::compile(src).map_err(|e| e.to_string())?;
    let out = program.apply(&Value::Null, None).map_err(|e| e.to_string())?;
    Ok(serde_json::to_string(&out).unwrap())
}

fn assert_eval(src: &str, expected: &str) {
    assert_eq!(eval(src).as_deref(), Ok(expected), "program: {src}");
}

#[test]
fn integer_arithmetic_stays_integer() {
    assert_eval("1 + 2", "3");
    assert_eval("5 - 7", "-2");
    assert_eval("6 * 7", "42");
    assert_eval("-(3)", "-3");
}

#[test]
fn decimal_arithmetic_stays_decimal() {
    assert_eval("2.5 * 4", "10.0");
    assert_eval("2.2 + 1.8", "4.0");
    assert_eval("1.5 - 0.5", "1.0");
    assert_eval("-(2.0)", "-2.0");
}

#[test]
fn division_is_integer_only_when_exact() {
    assert_eval("6 / 3", "2");
    assert_eval("7 / 2", "3.5");
    assert_eval("6.0 / 3", "2.0");
    assert!(eval("1 / 0").is_err());
}

#[test]
fn number_parses_integers_and_decimals() {
    assert_eval(r#"number("42")"#, "42");
    assert_eval(r#"number("-10")"#, "-10");
    assert_eval(r#"number("01")"#, "1");
    assert_eval(r#"number("1534663342000")"#, "1534663342000");
    assert_eval(r#"number("2.5")"#, "2.5");
    assert_eval(r#"number(".5")"#, "0.5");
    assert_eval(r#"number("1e3")"#, "1000.0");
    assert_eval(r#"number("-72915539138.234933")"#, "-72915539138.23494");
}

#[test]
fn number_rejects_invalid_strings() {
    for s in ["123.", "Infinity", "-NaN", " 1", "1 ", "", "-", "+1", "1e", "1e+"] {
        assert!(eval(&format!("number({s:?})")).is_err(), "number({s:?}) should fail");
        assert_eval(&format!("number({s:?}, 0)"), "0");
    }
}

#[test]
fn number_null_uses_fallback() {
    assert_eval("number(null)", "null");
    assert_eval("number(null, 243)", "243");
}

#[test]
fn sum_keeps_decimal_type() {
    assert_eval("sum([1, 2, 3])", "6");
    assert_eval("sum([1.5, 2.5])", "4.0");
}
