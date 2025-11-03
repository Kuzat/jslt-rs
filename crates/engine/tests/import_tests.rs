use engine::{compile_with_import_path, CompiledProgram, EngineError};
use serde_json::json;
use std::fs;
use std::io::Write;
use tempfile::TempDir;

// Helper: write a file relative to dir
fn write_file(dir: &TempDir, rel: &str, content: &str) -> std::path::PathBuf {
    let path = dir.path().join(rel);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    let mut f = fs::File::create(&path).unwrap();
    f.write_all(content.as_bytes()).unwrap();
    path
}

// Helper: compile a main file from disk and then evaluate
fn compile_file_with_imports(path: &std::path::Path) -> Result<CompiledProgram, EngineError> {
    let src = fs::read_to_string(path).unwrap();
    compile_with_import_path(&src, &path.to_string_lossy())
}

#[test]
fn import_namespace_and_call_function() {
    // module.jslt defines a function that we call as ns:inc(41)
    // main imports it and calls the namespaced function.
    let td = TempDir::new().unwrap();

    write_file(
        &td,
        "module.jslt",
        r#"
                def inc(x) $x + 1
            "#,
    );

    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "module.jslt" as ns
                ns:inc(41)
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    assert_eq!(out, json!(42));
}

#[test]
fn import_callable_module_and_invoke() {
    // callable.jslt has a final expression → callable module.
    // Calling alias(.) should evaluate the final expression using '.' = provided arg.
    //
    // Module: returns .a + 10
    let td = TempDir::new().unwrap();

    write_file(
        &td,
        "callable.jslt",
        r#"
                // no defs/lets, just final expression relying on '.'
                .a + 10
            "#,
    );

    // main calls the callable module with an object argument for dot.
    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "callable.jslt" as calc
                calc({ "a": 5 })
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    // 5 + 10 = 15
    assert_eq!(out, json!(15));
}

#[test]
fn imported_module_can_itself_import() {
    // transitive import: main -> a.jslt -> b.jslt
    // b.jslt provides a function; a.jslt re-exports by calling it; main calls a:foo
    let td = TempDir::new().unwrap();

    write_file(
        &td,
        "b.jslt",
        r#"
                def add2(x) $x + 2
            "#,
    );

    write_file(
        &td,
        "a.jslt",
        r#"
                import "b.jslt" as b
                // re-export behavior via namespaced call
                def foo(x) b:add2($x)
            "#,
    );

    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "a.jslt" as a
                a:foo(40)
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    assert_eq!(out, json!(42));
}

#[test]
fn cyclic_import_is_rejected() {
    // a imports b, b imports a → cycle
    let td = TempDir::new().unwrap();

    write_file(
        &td,
        "a.jslt",
        r#"
                import "b.jslt" as b
                // Any body
                1
            "#,
    );

    write_file(
        &td,
        "b.jslt",
        r#"
                import "a.jslt" as a
                2
            "#,
    );

    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "a.jslt" as a
                0
            "#,
    );

    // compile should error due to cycle
    let err = compile_file_with_imports(&main_path).unwrap_err();
    let msg = format!("{}", err);
    assert!(msg.contains("cyclic import detected"), "msg={}", msg);
}

#[test]
fn namespace_and_callable_in_one_program() {
    // Import both a namespace module and a callable module and use both.
    let td = TempDir::new().unwrap();

    write_file(
        &td,
        "ns_mod.jslt",
        r#"
                def mul10(x) $x * 10
            "#,
    );

    write_file(
        &td,
        "call_mod.jslt",
        r#"
                // returns . + 3
                . + 3
            "#,
    );

    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "ns_mod.jslt" as ns
                import "call_mod.jslt" as adder

                // ns:mul10(3) = 30
                // adder(7)    = 10
                ns:mul10(3) + adder(7)
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    assert_eq!(out, json!(40));
}

#[test]
fn imported_grandchild_function() {
    // Deep transitive import: main -> a.jslt -> b.jslt -> c.jslt
    // c.jslt provides a function; b.jslt calls it; a.jslt calls b; main calls a:top
    let td = TempDir::new().unwrap();

    // c.jslt: base function add3
    write_file(
        &td,
        "c.jslt",
        r#"
                def add3(x) $x + 3
            "#,
    );

    // b.jslt: imports c and defines a function that uses c:add3
    write_file(
        &td,
        "b.jslt",
        r#"
                import "c.jslt" as c
                def via_c(x) c:add3($x)
            "#,
    );

    // a.jslt: imports b and defines top that calls b:via_c
    write_file(
        &td,
        "a.jslt",
        r#"
                import "b.jslt" as b
                def top(x) b:via_c($x)
            "#,
    );

    // main imports a and calls a:top(39) expecting 42
    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "a.jslt" as a
                a:top(39)
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    assert_eq!(out, json!(42));
}

#[test]
fn imported_function_with_lets_in_def() {
    let td = TempDir::new().unwrap();

    // a.jslt: defines `add` with a let statement in the def
    write_file(
        &td,
        "a.jslt",
        r#"
                def add(x)
                    let y = 10
                    $x + $y
            "#,
    );

    // main imports a and calls a:add(39) expecting 49
    let main_path = write_file(
        &td,
        "main.jslt",
        r#"
                import "a.jslt" as a
                a:add(39)
            "#,
    );

    let prog = compile_file_with_imports(&main_path).unwrap();
    let out = prog.apply(&json!({}), None).unwrap();
    assert_eq!(out, json!(49));
}
