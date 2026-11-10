//! Runs the data-driven test suites from the Java reference implementation
//! (`conformance/java-jslt/core/src/test/resources`) against this engine.
//!
//! Cases listed in `conformance/upstream-known-failures.txt` are allowed to fail.
//! Any other failure fails the test, and so does a listed case that now passes,
//! so the list can only shrink.
//!
//! Regenerate the list with:
//! `UPDATE_UPSTREAM_KNOWN_FAILURES=1 cargo test -p engine --test upstream_tests`

use serde_json::Value;
use std::collections::BTreeSet;
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};

const KNOWN_FAILURES_FILE: &str = "conformance/upstream-known-failures.txt";
const UPDATE_ENV: &str = "UPDATE_UPSTREAM_KNOWN_FAILURES";

/// Mirrors the suites loaded by Java's `QueryTest`, `QueryErrorTest`,
/// `JsonParseTest` and `JsonParseErrorTest`.
const QUERY_SUITES: &[&str] = &[
    "query-tests.json",
    "query-tests.yaml",
    "function-tests.json",
    "experimental-tests.json",
    "function-declaration-tests.yaml",
    "query-error-tests.json",
    "function-error-tests.json",
];
const JSON_PARSE_SUITE: &str = "json-parse-tests.json";
const JSON_PARSE_ERROR_SUITE: &str = "json-parse-error-tests.json";

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..").canonicalize().unwrap()
}

fn resources_dir() -> PathBuf {
    repo_root().join("conformance/java-jslt/core/src/test/resources")
}

struct Case {
    id: String,
    query: String,
    outcome: Result<(), String>,
}

fn load_suite(name: &str) -> Vec<Value> {
    let text = fs::read_to_string(resources_dir().join(name))
        .unwrap_or_else(|e| panic!("failed to read {name}: {e}"));
    let doc: Value = if name.ends_with(".yaml") {
        yaml_serde::from_str(&text).unwrap_or_else(|e| panic!("invalid YAML in {name}: {e}"))
    } else {
        serde_json::from_str(&text).unwrap_or_else(|e| panic!("invalid JSON in {name}: {e}"))
    };
    doc["tests"].as_array().unwrap_or_else(|| panic!("{name} has no `tests` array")).clone()
}

/// Upstream stores JSON documents as strings, but YAML suites sometimes use
/// plain values (`input: {}`, `output: 5`).
fn json_field(v: &Value) -> Result<Value, String> {
    match v {
        Value::String(s) => serde_json::from_str(s).map_err(|e| format!("bad JSON {s:?}: {e}")),
        other => Ok(other.clone()),
    }
}

/// Serializes and reparses, like the Java runner does, so `1.0` != `1`.
fn normalize(v: &Value) -> Value {
    serde_json::from_str(&serde_json::to_string(v).unwrap()).unwrap()
}

fn run_jslt(query: &str, input: &Value) -> Result<Result<Value, String>, String> {
    panic::catch_unwind(AssertUnwindSafe(|| {
        // Upstream fixtures import modules relative to the resources directory.
        let main_path = resources_dir().join("main.jslt");
        engine::compile_with_import_path(query, &main_path.to_string_lossy())
            .and_then(|p| p.apply(input, None))
            .map_err(|e| e.to_string())
    }))
    .map_err(|p| {
        let msg = p
            .downcast_ref::<String>()
            .cloned()
            .or_else(|| p.downcast_ref::<&str>().map(|s| s.to_string()))
            .unwrap_or_default();
        format!("panic: {msg}")
    })
}

fn check_query_case(test: &Value) -> Result<(), String> {
    let query = test["query"].as_str().ok_or("missing query")?;
    if test.get("variables").is_some() {
        return Err("external variables are not supported".into());
    }
    let input = json_field(&test["input"])?;
    let result = run_jslt(query, &input)?;

    if let Some(expected_err) = test.get("error") {
        return match result {
            Err(_) => Ok(()),
            Ok(v) => Err(format!("expected error containing {expected_err}, got {v}")),
        };
    }
    let expected = json_field(&test["output"])?;
    match result {
        Ok(v) if normalize(&v) == expected => Ok(()),
        Ok(v) => Err(format!("expected {expected}, got {v}")),
        Err(e) => Err(format!("expected {expected}, got error: {e}")),
    }
}

fn check_json_parse_case(json: &str, expect_error: bool) -> Result<(), String> {
    let result = run_jslt(json, &Value::Null)?;
    match (result, expect_error) {
        (Err(_), true) => Ok(()),
        (Ok(v), true) => Err(format!("expected parse error, got {v}")),
        (Err(e), false) => Err(format!("got error: {e}")),
        (Ok(v), false) => {
            let expected: Value = serde_json::from_str(json).map_err(|e| e.to_string())?;
            if normalize(&v) == expected {
                Ok(())
            } else {
                Err(format!("expected {expected}, got {v}"))
            }
        }
    }
}

fn run_all() -> Vec<Case> {
    let mut cases = Vec::new();
    for suite in QUERY_SUITES {
        for (ix, test) in load_suite(suite).iter().enumerate() {
            cases.push(Case {
                id: format!("{suite}#{ix}"),
                query: test["query"].as_str().unwrap_or_default().to_string(),
                outcome: check_query_case(test),
            });
        }
    }
    for (suite, expect_error) in [(JSON_PARSE_SUITE, false), (JSON_PARSE_ERROR_SUITE, true)] {
        for (ix, test) in load_suite(suite).iter().enumerate() {
            let json = test.as_str().unwrap_or_default();
            cases.push(Case {
                id: format!("{suite}#{ix}"),
                query: json.to_string(),
                outcome: check_json_parse_case(json, expect_error),
            });
        }
    }
    cases
}

fn one_line(s: &str, max: usize) -> String {
    let flat = s.split_whitespace().collect::<Vec<_>>().join(" ");
    if flat.chars().count() > max {
        format!("{}…", flat.chars().take(max).collect::<String>())
    } else {
        flat
    }
}

fn summary(cases: &[Case]) -> Vec<String> {
    let mut suites: Vec<&str> = QUERY_SUITES.to_vec();
    suites.extend([JSON_PARSE_SUITE, JSON_PARSE_ERROR_SUITE]);
    let mut lines = Vec::new();
    for suite in suites {
        let prefix = format!("{suite}#");
        let in_suite: Vec<_> = cases.iter().filter(|c| c.id.starts_with(&prefix)).collect();
        let passed = in_suite.iter().filter(|c| c.outcome.is_ok()).count();
        lines.push(format!("{suite:<34} {passed:>4}/{:<4}", in_suite.len()));
    }
    let passed = cases.iter().filter(|c| c.outcome.is_ok()).count();
    let pct = 100.0 * passed as f64 / cases.len() as f64;
    lines.push(format!("{:<34} {passed:>4}/{:<4} ({pct:.1}%)", "TOTAL", cases.len()));
    lines
}

fn read_known_failures() -> BTreeSet<String> {
    let text = fs::read_to_string(repo_root().join(KNOWN_FAILURES_FILE)).unwrap_or_default();
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .filter_map(|l| l.split_whitespace().next())
        .map(str::to_string)
        .collect()
}

fn write_known_failures(cases: &[Case]) {
    let mut out = String::from(
        "# Upstream Java JSLT test cases that currently fail. Generated by\n\
         # crates/engine/tests/upstream_tests.rs; regenerate with:\n\
         #   UPDATE_UPSTREAM_KNOWN_FAILURES=1 cargo test -p engine --test upstream_tests\n\
         #\n",
    );
    for line in summary(cases) {
        out += &format!("# {line}\n");
    }
    out += "\n";
    for c in cases {
        if let Err(reason) = &c.outcome {
            out +=
                &format!("{}  # {} => {}\n", c.id, one_line(&c.query, 60), one_line(reason, 100));
        }
    }
    fs::write(repo_root().join(KNOWN_FAILURES_FILE), out).unwrap();
}

#[test]
fn upstream_java_jslt_suites() {
    if !resources_dir().exists() {
        let msg = "upstream test resources missing; run `git submodule update --init`";
        if std::env::var_os("CI").is_some() {
            panic!("{msg}");
        }
        eprintln!("skipping: {msg}");
        return;
    }

    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(|_| {}));
    let cases = run_all();
    panic::set_hook(default_hook);

    println!("Upstream Java JSLT coverage:");
    for line in summary(&cases) {
        println!("  {line}");
    }

    if std::env::var_os(UPDATE_ENV).is_some() {
        write_known_failures(&cases);
        println!("updated {KNOWN_FAILURES_FILE}");
        return;
    }

    let known = read_known_failures();
    let mut regressions = Vec::new();
    let mut fixed = Vec::new();
    for c in &cases {
        match (&c.outcome, known.contains(&c.id)) {
            (Err(reason), false) => regressions.push(format!(
                "  {}: {} => {}",
                c.id,
                one_line(&c.query, 80),
                one_line(reason, 200)
            )),
            (Ok(()), true) => fixed.push(format!("  {}", c.id)),
            _ => {}
        }
    }
    let ids: BTreeSet<_> = cases.iter().map(|c| c.id.as_str()).collect();
    let stale: Vec<_> =
        known.iter().filter(|k| !ids.contains(k.as_str())).map(|k| format!("  {k}")).collect();

    let mut problems = String::new();
    if !regressions.is_empty() {
        problems +=
            &format!("New failures (not in {KNOWN_FAILURES_FILE}):\n{}\n", regressions.join("\n"));
    }
    if !fixed.is_empty() {
        problems += &format!(
            "Now passing; remove from {KNOWN_FAILURES_FILE} (or rerun with {UPDATE_ENV}=1):\n{}\n",
            fixed.join("\n")
        );
    }
    if !stale.is_empty() {
        problems += &format!("Unknown ids in {KNOWN_FAILURES_FILE}:\n{}\n", stale.join("\n"));
    }
    assert!(problems.is_empty(), "\n{problems}");
}
