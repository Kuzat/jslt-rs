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
    assert_eq!(formatted, ".a + .b\n");
}

#[test]
fn test_format_def_spacing() {
    let input = "def  foo(x,y,z)    x+y+z";
    let formatted = format_source(input).unwrap();
    // def bodies are always on new line with indentation
    assert_eq!(formatted, "def foo(x, y, z)\n  x + y + z\n");
}

#[test]
fn test_format_let_statement() {
    let input = "let   x=1+2\n{ \"result\": . }";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "let x = 1 + 2\n\n{\"result\": .}\n");
}

#[test]
fn test_format_array_single_line() {
    let input = "[1,2,3]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[1, 2, 3]\n");
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
    assert_eq!(formatted, r#"{"a": 1, "b": 2}"#.to_string() + "\n");
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
    // if/else branches are on new lines
    assert_eq!(formatted, "if (.x > 5)\n  .a\nelse\n  .b\n");

    let input_no_else = "if(.x>5)   .a";
    let formatted_no_else = format_source(input_no_else).unwrap();
    assert_eq!(formatted_no_else, "if (.x > 5)\n  .a\n");
}

#[test]
fn test_format_binary_operators() {
    let tests = vec![
        (".a+.b", ".a + .b\n"),
        // Note: .a-.b is parsed as .a followed by negative number -b
        (".a - .b", ".a - .b\n"),
        (".a*.b", ".a * .b\n"),
        (".a/.b", ".a / .b\n"),
        (".a%.b", ".a % .b\n"),
        (".a<.b", ".a < .b\n"),
        (".a<=.b", ".a <= .b\n"),
        (".a>.b", ".a > .b\n"),
        (".a>=.b", ".a >= .b\n"),
        (".a==.b", ".a == .b\n"),
        (".a!=.b", ".a != .b\n"),
        (".a and .b", ".a and .b\n"),
        (".a or .b", ".a or .b\n"),
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
    assert_eq!(formatted, "not .x\n");

    let input = "-.x";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "-.x\n");
}

#[test]
fn test_format_member_access() {
    let input = ".foo.bar.baz";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".foo.bar.baz\n");
}

#[test]
fn test_format_index_access() {
    let input = ".array[0]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".array[0]\n");
}

#[test]
fn test_format_slice() {
    let tests = vec![
        (".array[1:3]", ".array[1:3]\n"),
        (".array[:3]", ".array[:3]\n"),
        (".array[1:]", ".array[1:]\n"),
        (".array[:]", ".array[:]\n"),
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
    assert_eq!(formatted, "foo(1, 2, 3)\n");
}

#[test]
fn test_format_array_comprehension() {
    let input = "[for(.items).value]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[for (.items) .value]\n");

    let input = "[for(.items).value if .active]";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "[for (.items) .value if .active]\n");
}

#[test]
fn test_format_object_comprehension() {
    let input = "{for(.items).key:.value}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{for (.items) .key: .value}\n");

    let input = "{for(.items).key:.value if .active}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{for (.items) .key: .value if .active}\n");
}

#[test]
fn test_format_string_escaping() {
    let input = r#"{"text": "hello\nworld"}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{"text": "hello\nworld"}"#.to_string() + "\n");
}

#[test]
fn test_format_program_with_imports() {
    let input = r#"import   "foo.jslt"   as   foo
.result"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "import \"foo.jslt\" as foo\n\n.result\n");
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
    assert_eq!(formatted, "[]\n");

    let input = "{}";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "{}\n");
}

#[test]
fn test_format_nested_structures() {
    let input = r#"{"outer":{"inner":[1,2,3]}}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{"outer": {"inner": [1, 2, 3]}}"#.to_string() + "\n");
}

#[test]
fn test_format_let_block() {
    // Let blocks are inline expressions, not top-level lets
    let input = "if (true) let x=1 let y=2 x+y else 0";
    let formatted = format_source(input).unwrap();
    // if/else now on new lines, but let blocks within branches stay inline
    assert_eq!(formatted, "if (true)\n  let x = 1 let y = 2 x + y\nelse\n  0\n");
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
    let input = "[1, 2, 3, 4, 5]"; // Longer array to exceed max_width
    let formatted = format_source_with_config(input, config).unwrap();
    // Multi-line array should have 4-space indentation
    // The output should be multi-line due to max_width
    assert!(formatted.contains('\n'));
    assert!(formatted.contains("    "), "Should use 4-space indent");
}

#[test]
fn test_literals() {
    let tests = vec![
        ("null", "null\n"),
        ("true", "true\n"),
        ("false", "false\n"),
        ("42", "42\n"),
        ("3.14", "3.14\n"),
        (r#""hello""#, "\"hello\"\n"),
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
    assert_eq!(formatted, "$foo\n");
}

#[test]
fn test_this_expression() {
    let input = ".";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, ".\n");
}

#[test]
fn test_object_spread() {
    let input = r#"{*:.base,"override":true}"#;
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, r#"{*: .base, "override": true}"#.to_string() + "\n");
}

#[test]
fn test_comment_preservation() {
    let input = r#"// File header comment
import "foo.jslt" as foo

// Function to add
def add(x, y)
  x + y

// Compute result
let x = 1

{"sum": add(x, 2)}"#;

    let formatted = format_source(input).unwrap();

    // Verify all comments are preserved
    assert!(formatted.contains("// File header comment"));
    assert!(formatted.contains("// Function to add"));
    assert!(formatted.contains("// Compute result"));

    // Verify idempotency
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted);
}

#[test]
fn test_no_blank_line_after_header_comments_before_def() {
    let input = r#"// Convert any type to a number.
// Numbers are returned as is.
// Strings are parsed as numbers, if possible.
// In other cases, null is returned.
def asNumber(input)
  number($input,null)"#;
    let formatted = format_source(input).unwrap();

    assert_eq!(
        formatted,
        r#"// Convert any type to a number.
// Numbers are returned as is.
// Strings are parsed as numbers, if possible.
// In other cases, null is returned.
def asNumber(input)
  number($input, null)
"#
    );
}

#[test]
fn test_no_leading_newline_at_start_of_file() {
    let input = "\n\n\n// Top comment\ndef f(x) x";
    let formatted = format_source(input).unwrap();
    assert!(!formatted.starts_with('\n'));
}

#[test]
fn test_comment_before_top_level_object_stays_outside_object() {
    let input = r#"def f(x)
  x
// Keep this outside the object
{"a":1}"#;
    let formatted = format_source(input).unwrap();

    assert!(formatted.contains("x\n\n// Keep this outside the object\n{\"a\": 1}\n"));
}

#[test]
fn test_preserve_comment_before_closing_object_brace() {
    let input = r#"{"a": 1 // keep
}"#;
    let formatted = format_source(input).unwrap();

    assert!(formatted.contains("\"a\": 1\n  // keep\n}"));
}

#[test]
fn test_no_extra_blank_line_before_consecutive_object_comments() {
    let input = r#"{
  "a": 1,
  // first
  // second
  "b": 2
}"#;
    let formatted = format_source(input).unwrap();

    assert!(formatted.contains("\"a\": 1,\n  // first\n  // second\n  \"b\": 2"));
    assert!(!formatted.contains("\"a\": 1,\n\n  // first\n\n  // second"));
}

#[test]
fn test_preserve_def_body_and_inline_comments() {
    let input = r#"// Convert any type to a string.
def asString(input)
  // some comment here as well
  if ($input == null)
    null
  else
    string($input) // Some end of line comment"#;
    let formatted = format_source(input).unwrap();

    let expected = r#"// Convert any type to a string.
def asString(input)
  // some comment here as well
  if ($input == null)
    null
  else
    string($input)  // Some end of line comment
"#;
    assert_eq!(formatted, expected);
}

#[test]
fn test_object_literal_comments() {
    let input = r#"{
  // For building ip-geo lookup model.
  "ipAddress": asString(.actor."spt:remoteAddress"),
  "latitude": asNumber(.location.latitude),
  "longitude": asNumber(.location.longitude),

  // For analysis/debugging.
  "eventType": asString(."@type"),
  "trackerType": asString(.tracker.type),
  "client": asString(get-client(.)),

  // Used to infer quality/age of location
  "locationAccuracy": asNumber(.location.accuracy),
  "creationDate": asString(.creationDate),
  "locationTimestamp": asString(.location.timestamp),
  "published": asString(.published),

  // To join back enrichment onto event later.
  "eventId": asString(."@id")
}"#;

    let formatted = format_source(input).unwrap();

    // Verify all comments are preserved
    assert!(formatted.contains("// For building ip-geo lookup model."));
    assert!(formatted.contains("// For analysis/debugging."));
    assert!(formatted.contains("// Used to infer quality/age of location"));
    assert!(formatted.contains("// To join back enrichment onto event later."));

    // Verify idempotency
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted);
}

#[test]
fn test_object_inline_comments() {
    let input = r#"{
  "field1": "value",
  // This is a comment between fields
  "field2": "another value"
}"#;

    let formatted = format_source(input).unwrap();

    // Verify comment is preserved
    assert!(formatted.contains("// This is a comment between fields"));

    // Verify idempotency
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted);
}

#[test]
fn test_preserve_blank_lines_in_objects() {
    let input = r#"{
  "section1": 1,

  "section2": 2,


  "section3": 3
}"#;
    let formatted = format_source(input).unwrap();

    // Should preserve single blank line
    assert!(formatted.contains("\"section1\": 1,\n\n  \"section2\""));

    // Should preserve two blank lines
    assert!(formatted.contains("\"section2\": 2,\n\n\n  \"section3\""));

    // Verify idempotency
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted);
}

#[test]
fn test_trailing_newline() {
    let input = "def foo(x) x";
    let formatted = format_source(input).unwrap();
    assert!(formatted.ends_with('\n'), "Should have trailing newline");
    assert!(!formatted.ends_with("\n\n"), "Should have exactly one trailing newline");
}

#[test]
fn test_def_body_on_newline() {
    let input = "def simple(x) x + 1";
    let formatted = format_source(input).unwrap();
    assert_eq!(formatted, "def simple(x)\n  x + 1\n");
}

#[test]
fn test_nested_if_else() {
    let input = "if (.a) if (.b) 1 else 2 else 3";
    let formatted = format_source(input).unwrap();
    // Verify proper multi-level indentation
    assert!(formatted.contains("if (.a)\n  if (.b)"));
}

#[test]
fn test_complex_formatting_example() {
    // Use the user's actual example
    let input = r#"
def asNumber(input)
  number($input,null)

def asString(input)
  if ($input == null)
    null
  else
    string($input)

{
  // For building ip-geo lookup model.
  "ipAddress": asString(.actor."spt:remoteAddress"),

  // For analysis/debugging.
  "eventType": asString(."@type")
}
"#;
    let formatted = format_source(input).unwrap();

    // Verify all requirements met
    assert!(formatted.ends_with('\n'));
    assert!(formatted.contains("def asNumber(input)\n  number("));
    assert!(formatted.contains("if ($input == null)\n"));
    assert!(formatted.contains("\"ipAddress\": asString(.actor"));

    // Idempotency check
    let reformatted = format_source(&formatted).unwrap();
    assert_eq!(formatted, reformatted);
}
