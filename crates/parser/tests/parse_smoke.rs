// Bring the parser entry points into scope.
// Adjust paths if your crate names differ.
use parser::Parser;

// Pretty helper: parse a whole program and return its pretty-printed form.
// We prefer checking the Display output (AST pretty-printer) to avoid
// hard-coding every AST node shape in tests.
fn parse_fmt(src: &str) -> String {
    let mut p = Parser::new(src).expect("lexer error during Parser::new");
    let prog = p.parse_program().expect("parse_program failed");
    format!("{prog}")
}

// Pretty helper where we expect an error.
fn parse_err(src: &str) -> String {
    let mut p = Parser::new(src).expect("lexer error during Parser::new");
    match p.parse_program() {
        Ok(prog) => panic!("expected error, but parsed: {prog}"),
        Err(e) => format!("{:?}", e), // stable-ish debug string
    }
}

#[test]
fn literals_and_variables() {
    assert_eq!(parse_fmt("null"), "null");
    assert_eq!(parse_fmt("true"), "true");
    assert_eq!(parse_fmt("false"), "false");
    assert_eq!(parse_fmt("123"), "123");
    assert_eq!(parse_fmt("3.14"), "3.14");
    assert_eq!(parse_fmt("\"a\\\\b\\\"c\\n\""), "\"a\\\\b\\\"c\\n\"");
    assert_eq!(parse_fmt("."), ".");
    assert_eq!(parse_fmt(".a"), ".a");
    assert_eq!(parse_fmt("$foo"), "$foo");
}

#[test]
fn precedence_and_grouping() {
    // 1 + 2 * 3 => "1 + 2 * 3"
    assert_eq!(parse_fmt("1 + 2 * 3"), "1 + 2 * 3");

    // (1 + 2) * 3 => "(1 + 2) * 3"
    assert_eq!(parse_fmt("(1 + 2) * 3"), "(1 + 2) * 3");

    // -(1 + 2)
    assert_eq!(parse_fmt("-(1 + 2)"), "-(1 + 2)");

    // not $a and ($b or $c)
    assert_eq!(parse_fmt("not $a and ($b or $c)"), "not $a and ($b or $c)");

    // comparison binds lower than add: (1 + 2) < 3 -> "1 + 2 < 3"
    assert_eq!(parse_fmt("1 + 2 < 3"), "1 + 2 < 3");
}

#[test]
fn if_expression_lowest_precedence() {
    // if (a) b else c and d => if(a) b else (c and d)
    assert_eq!(parse_fmt("if ($a) $b else $c and $d"), "if ($a) $b else $c and $d");

    // (if (true) 1 else 2) + 1 gets parenthesized when needed
    assert_eq!(parse_fmt("(if (true) 1 else 2) + 1"), "(if (true) 1 else 2) + 1");
}

#[test]
fn postfix_chain_member_index_call_and_quoted_member() {
    // Member with ident and quoted string
    assert_eq!(parse_fmt("$a.b"), "$a.b");
    assert_eq!(parse_fmt("$a.\"x y\""), "$a.\"x y\"");
    assert_eq!(parse_fmt("$a.\"quote:\\\"\""), "$a.\"quote:\\\"\"");

    // Indexing
    assert_eq!(parse_fmt("$a[0]"), "$a[0]");

    // Slicing variants
    assert_eq!(parse_fmt("$a[1:2]"), "$a[1:2]");
    assert_eq!(parse_fmt("$a[:2]"), "$a[:2]");
    assert_eq!(parse_fmt("$a[1:]"), "$a[1:]");
    assert_eq!(parse_fmt("$a[:]"), "$a[:]");

    // Call + chaining
    assert_eq!(parse_fmt("$f.g(1, \"s\", $x)"), "$f.g(1, \"s\", $x)");

    // Mixed chain
    assert_eq!(parse_fmt(".a[1:3](42).\"weird-key\"[0]"), ".a[1:3](42).\"weird-key\"[0]");
}

#[test]
fn arrays_and_objects_and_spread() {
    assert_eq!(parse_fmt("[1, \"two\", false]"), "[1, \"two\", false]");

    // Object literal with ident key, quoted key, and spread
    assert_eq!(parse_fmt("{ a: 1, \"x y\": \"v\", *: $rest }"), "{a: 1, \"x y\": \"v\", *: $rest}");
}

#[test]
fn for_comprehensions() {
    // [for (seq) body]
    assert_eq!(parse_fmt("[for ($seq) $x]"), "[for ($seq) $x]");

    // [for (seq) body if cond]
    assert_eq!(parse_fmt("[for ($seq) 1 if true]"), "[for ($seq) 1 if true]");

    // {for (seq) key: value}
    assert_eq!(parse_fmt("{for ($seq) \"k\": $v}"), "{for ($seq) \"k\": $v}");

    // {for (seq) key: value if cond}
    assert_eq!(parse_fmt("{for ($seq) $a.b: 2 if ($x == 3)}"), "{for ($seq) $a.b: 2 if ($x == 3)}");
}

#[test]
fn normal_let_binding_with_single_value() {
    assert_eq!(parse_fmt("let a = 1\n$a"), "let a = 1\n$a");
}

#[test]
fn multiple_let_bindings_in_program() {
    assert_eq!(
        parse_fmt("let a = 1\nlet b = 2\nlet c = a + b\n$c"),
        "let a = 1\nlet b = 2\nlet c = a + b\n$c"
    )
}

#[test]
fn single_def_with_single_value() {
    assert_eq!(parse_fmt("def foo(a, b) a + b\nfoo(1, 2)"), "def foo(a, b) a + b\nfoo(1, 2)");
}

#[test]
fn multiple_defs_in_program() {
    assert_eq!(
        parse_fmt("def foo(a, b) a + b\ndef bar(a, b) a * b\nfoo(1, 2) + bar(2, 3)"),
        "def foo(a, b) a + b\ndef bar(a, b) a * b\nfoo(1, 2) + bar(2, 3)"
    )
}

#[test]
fn function_refs_and_calls() {
    // Bare function name (resolved in binder later), then call
    assert_eq!(parse_fmt("foo"), "foo");
    assert_eq!(parse_fmt("foo()"), "foo()");
    assert_eq!(parse_fmt("foo(1, 2, $x)"), "foo(1, 2, $x)");

    // Chained calls and mix with member/index
    assert_eq!(parse_fmt("foo()(.)"), "foo()(.)");
}

#[test]
fn full_program_with_multiple_def_and_let() {}

#[test]
fn errors_are_helpful() {
    // Missing closing ]
    let e = parse_err("[1, 2");
    assert!(
        e.contains("Unterminated") || e.contains("expected") || e.contains("']'"),
        "unexpected error: {e}"
    );

    // Member without a key after '.'
    let e = parse_err("$a.");
    assert!(
        e.contains("identifier") || e.contains("string") || e.contains("after '.'"),
        "unexpected error: {e}"
    );

    // Variable lacking ident after $
    let e = parse_err("$");
    assert!(e.contains("ExpectedIdent") || e.contains("identifier"), "unexpected error: {e}");

    // let binding missing '='
    let e = parse_err("let a 1; .");
    assert!(e.contains("=") || e.contains("binding"), "unexpected error: {e}");
}
