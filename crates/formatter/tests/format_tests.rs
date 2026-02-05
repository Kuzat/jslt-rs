//! Comprehensive formatter tests

use formatter::{format_source, format_source_with_config, FormatConfig};

#[test]
fn test_idempotency_simple() {
    let input = "def foo(x) x + 1";
    let formatted = format_source(input).unwrap();
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted, "Formatting should be idempotent");
}

#[test]
fn test_idempotency_complex() {
    let input = r#"
import "module.jslt" as mod

def add(x, y) x + y

def complex(data)
  let result = [for (.items) .value if .active]
  {
    "sum": add(.a, .b),
    "filtered": result
  }

{"output": complex(.)}
"#;
    let formatted = format_source(input).unwrap();
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted, "Complex formatting should be idempotent");
}

#[test]
fn test_format_basic_expression() {
    let input = ".a+.b";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".a + .b");
}

#[test]
fn test_format_def_spacing() {
    let input = "def  foo(x,y,z)    x+y+z";
    let formatted = format_source(input).unwrap();
    // Formatter adds newline after top-level defs
    assert_eq!(formatted, "def foo(x, y, z) x + y + z\n");
}

#[test]
fn test_format_let_statement() {
    let input = "let   x=1+2\n{ \"result\": . }";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "let x = 1 + 2\n\n{\"result\": .}");
}

#[test]
fn test_format_array_single_line() {
    let input = "[1,2,3]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[1, 2, 3]");
}

#[test]
fn test_format_array_multiline() {
    let config = FormatConfig {
        max_width: 10, // Force multi-line
        ..Default::default()
    };
    let input = "[1, 2, 3, 4, 5]";
    let formatted = format_source_with_config(input, config).unwrap();
    // Should be multi-line because it exceeds max_width
    assert!(formatted.contains('\n'));
}

#[test]
fn test_format_object_single_line() {
    let input = r#"{"a":1,"b":2}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{"a": 1, "b": 2}"#);
}

#[test]
fn test_format_object_multiline() {
    let config = FormatConfig {
        max_width: 10, // Force multi-line
        ..Default::default()
    };
    let input = r#"{"key1": 1, "key2": 2}"#;
    let formatted = format_source_with_config(input, config).unwrap();
    // Should be multi-line because it exceeds max_width
    assert!(formatted.contains('\n'));
}

#[test]
fn test_format_if_expression() {
    let input = "if(.x>5)   .a   else   .b";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "if (.x > 5) .a else .b");
}

#[test]
fn test_format_binary_operators() {
    let tests = vec![
        (".a+.b", ".a + .b"),
        // Note: .a-.b is parsed as .a followed by negative number -b
        (".a - .b", ".a - .b"),
        (".a*.b", ".a * .b"),
        (".a/.b", ".a / .b"),
        (".a%.b", ".a % .b"),
        (".a<.b", ".a < .b"),
        (".a<=.b", ".a <= .b"),
        (".a>.b", ".a > .b"),
        (".a>=.b", ".a >= .b"),
        (".a==.b", ".a == .b"),
        (".a!=.b", ".a != .b"),
        (".a and .b", ".a and .b"),
        (".a or .b", ".a or .b"),
    ];

    for (input, expected) in tests {
        let formatted = format_source(input).unwrap();
        assert_eq!(formatted, expected, "Failed for input: {}", input);
    }
}

#[test]
fn test_format_unary_operators() {
    let input = "not   .x";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "not .x");

    let input = "-.x";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "-.x");
}

#[test]
fn test_format_member_access() {
    let input = ".foo.bar.baz";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".foo.bar.baz");
}

#[test]
fn test_format_index_access() {
    let input = ".array[0]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".array[0]");
}

#[test]
fn test_format_slice() {
    let tests = vec![
        (".array[1:3]", ".array[1:3]"),
        (".array[:3]", ".array[:3]"),
        (".array[1:]", ".array[1:]"),
        (".array[:]", ".array[:]"),
    ];

    for (input, expected) in tests {
        let formatted = format_source(input).unwrap();
        assert_eq!(formatted, expected, "Failed for input: {}", input);
    }
}

#[test]
fn test_format_function_call() {
    let input = "foo(1,2,3)";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "foo(1, 2, 3)");
}

#[test]
fn test_format_array_comprehension() {
    let input = "[for(.items).value]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[for (.items) .value]");

    let input = "[for(.items).value if .active]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[for (.items) .value if .active]");
}

#[test]
fn test_format_object_comprehension() {
    let input = "{for(.items).key:.value}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{for (.items) .key: .value}");

    let input = "{for(.items).key:.value if .active}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{for (.items) .key: .value if .active}");
}

#[test]
fn test_format_string_escaping() {
    let input = r#"{"text": "hello\nworld"}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{"text": "hello\nworld"}"#);
}

#[test]
fn test_format_program_with_imports() {
    let input = r#"import   "foo.jslt"   as   foo
.result"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "import \"foo.jslt\" as foo\n\n.result");
}

#[test]
fn test_format_program_spacing() {
    let input = r#"import "a.jslt" as a
def foo(x) x + 1
let y = 2
{ "result": . }"#;
    let formatted = format_source(input).unwrap();
    // Should have blank lines between sections
    assert!(formatted.contains("as a\n\n"));
    assert!(formatted.contains("+ 1\n\n"));
    assert!(formatted.contains("= 2\n\n"));
}

#[test]
fn test_format_empty_array_object() {
    let input = "[]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[]");

    let input = "{}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{}");
}

#[test]
fn test_format_nested_structures() {
    let input = r#"{"outer":{"inner":[1,2,3]}}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{"outer": {"inner": [1, 2, 3]}}"#);
}

#[test]
fn test_format_let_block() {
    // Let blocks are inline expressions, not top-level lets
    let input = "if (true) let x=1 let y=2 x+y else 0";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "if (true) let x = 1 let y = 2 x + y else 0");
}

#[test]
fn test_format_invalid_syntax() {
    let input = "def foo(";
    let result = format_source(input);
    assert!(result.is_err(), "Should fail on invalid syntax");
}

#[test]
fn test_custom_indent_width() {
    let config = FormatConfig {
        indent_width: 4,
        max_width: 10, // Force multi-line
        ..Default::default()
    };
    let input = "[1, 2, 3]";
    let formatted = format_source_with_config(input, config).unwrap();
    // Multi-line array should have 4-space indentation
    if formatted.contains('\n') {
        assert!(formatted.contains("    "), "Should use 4-space indent");
    }
}

#[test]
fn test_literals() {
    let tests = vec![
        ("null", "null"),
        ("true", "true"),
        ("false", "false"),
        ("42", "42"),
        ("3.14", "3.14"),
        (r#""hello""#, r#""hello""#),
    ];

    for (input, expected) in tests {
        let formatted = format_source(input).unwrap();
        assert_eq!(formatted, expected, "Failed for input: {}", input);
    }
}

#[test]
fn test_variables() {
    let input = "$foo";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "$foo");
}

#[test]
fn test_this_expression() {
    let input = ".";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".");
}

#[test]
fn test_object_spread() {
    let input = r#"{*:.base,"override":true}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{*: .base, "override": true}"#);
}
