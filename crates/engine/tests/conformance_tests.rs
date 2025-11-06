use engine::compile;
use interp::EvalConfig;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Deserialize, Serialize)]
struct TestCase {
    name: String,
    description: Option<String>,
    program: String,
    input: Value,
    expected: Value,
}

#[derive(Debug)]
struct TestResult {
    name: String,
    passed: bool,
    rust_output: Option<Value>,
    java_output: Option<Value>,
    error: Option<String>,
}

fn get_conformance_cases_dir() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir).parent().unwrap().parent().unwrap().join("conformance").join("cases")
}

fn get_java_jslt_script() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    Path::new(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("conformance")
        .join("scripts")
        .join("run_java_jslt.sh")
}

fn load_test_cases() -> Result<Vec<TestCase>, Box<dyn std::error::Error>> {
    let cases_dir = get_conformance_cases_dir();

    // If an explicit file is provided via env var, load only that test file
    if let Ok(file_spec) = env::var("CONFORMANCE_TEST_FILE") {
        let mut candidate_paths: Vec<PathBuf> = Vec::new();
        let p = PathBuf::from(&file_spec);
        if p.is_absolute() {
            candidate_paths.push(p);
        } else {
            // try relative to current dir first, then within conformance/cases
            candidate_paths.push(p.clone());
            candidate_paths.push(cases_dir.join(&p));
            // if only a bare filename was provided, also try within cases
            if p.extension().is_none() {
                candidate_paths.push(cases_dir.join(format!("{}.json", p.display())));
            }
        }

        let existing = candidate_paths
            .into_iter()
            .find(|x| x.exists())
            .ok_or_else(|| format!("Specified CONFORMANCE_TEST_FILE not found: {}", file_spec))?;

        let content = fs::read_to_string(&existing)?;
        let test_case: TestCase = serde_json::from_str(&content)
            .map_err(|e| format!("Failed to parse {}: {}", existing.display(), e))?;
        return Ok(vec![test_case]);
    }

    // Otherwise, load all test files from the directory
    let mut test_cases = Vec::new();

    for entry in fs::read_dir(&cases_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("json") {
            let content = fs::read_to_string(&path)?;
            let test_case: TestCase = serde_json::from_str(&content)
                .map_err(|e| format!("Failed to parse {}: {}", path.display(), e))?;
            test_cases.push(test_case);
        }
    }

    test_cases.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(test_cases)
}

fn run_java_jslt(program: &str, input: &Value) -> Result<Value, Box<dyn std::error::Error>> {
    let temp_dir = tempfile::tempdir()?;
    let program_file = temp_dir.path().join("program.jslt");
    let input_file = temp_dir.path().join("input.json");

    fs::write(&program_file, program)?;
    fs::write(&input_file, serde_json::to_string_pretty(input)?)?;

    let java_script = get_java_jslt_script();
    let output = Command::new(&java_script).arg(&program_file).arg(&input_file).output()?;

    if !output.status.success() {
        return Err(format!("Java JSLT failed: {}", String::from_utf8_lossy(&output.stderr)).into());
    }

    let result_str = String::from_utf8(output.stdout)?;
    let result: Value = serde_json::from_str(result_str.trim())?;
    Ok(result)
}

fn run_rust_jslt(program: &str, input: &Value) -> Result<Value, Box<dyn std::error::Error>> {
    let compiled = compile(program)?;
    let config = EvalConfig { max_steps: Some(1_000_000_000), ..Default::default() };
    let result: Value = compiled.apply(input, Some(config))?;
    Ok(result)
}

fn run_differential_test(test_case: &TestCase) -> TestResult {
    let java_result = run_java_jslt(&test_case.program, &test_case.input);
    let rust_result = run_rust_jslt(&test_case.program, &test_case.input);

    match (java_result, rust_result) {
        (Ok(java_output), Ok(rust_output)) => {
            let java_matches_expected = java_output == test_case.expected;
            let rust_matches_java = rust_output == java_output;
            let passed = java_matches_expected && rust_matches_java;

            let error_msg = if !passed {
                Some(format!(
                    "Mismatch - Expected: {}, Java: {}, Rust: {}",
                    test_case.expected, java_output, rust_output
                ))
            } else {
                None
            };

            TestResult {
                name: test_case.name.clone(),
                passed,
                rust_output: Some(rust_output),
                java_output: Some(java_output),
                error: error_msg,
            }
        }
        (Err(java_err), _) => TestResult {
            name: test_case.name.clone(),
            passed: false,
            rust_output: None,
            java_output: None,
            error: Some(format!("Java JSLT error: {}", java_err)),
        },
        (_, Err(rust_err)) => TestResult {
            name: test_case.name.clone(),
            passed: false,
            rust_output: None,
            java_output: None,
            error: Some(format!("Rust JSLT error: {}", rust_err)),
        },
    }
}

#[test]
fn test_conformance_rust_only() {
    let result = std::thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(|| {
            let test_cases = load_test_cases().expect("Failed to load test cases");

            if test_cases.is_empty() {
                panic!("No test cases found in conformance/cases/");
            }

            let mut failed: Vec<(String, String)> = Vec::new();

            for tc in &test_cases {
                let rust_res = run_rust_jslt(&tc.program, &tc.input);
                match rust_res {
                    Ok(out) if out == tc.expected => {
                        // ok
                    }
                    Ok(out) => failed.push((
                        tc.name.clone(),
                        format!("Expected: '{}', Got: '{}'", tc.expected, out),
                    )),
                    Err(e) => failed.push((tc.name.clone(), format!("Error: {}", e))),
                }
            }

            if !failed.is_empty() {
                eprintln!("❌ Rust-only conformance failures:");
                for (name, msg) in &failed {
                    eprintln!("  - {}: {}", name, msg);
                }
                panic!("Some Rust-only conformance tests failed");
            }
        })
        .expect("Failed to spawn thread")
        .join();

    if let Err(e) = result {
        std::panic::resume_unwind(e);
    }
}

#[ignore = "Run manually to verify conformance against Java JSLT"]
#[test]
fn test_conformance_against_java_jslt() {
    let test_cases = load_test_cases().expect("Failed to load test cases");

    if test_cases.is_empty() {
        panic!("No test cases found in conformance/cases/");
    }

    let mut results = Vec::new();
    let mut passed_count = 0;

    for test_case in &test_cases {
        println!("Running test: {}", test_case.name);
        let result = run_differential_test(test_case);

        if result.passed {
            passed_count += 1;
            println!("  ✅ PASSED");
        } else {
            println!(
                "  ❌ FAILED: {}",
                result.error.as_ref().unwrap_or(&"Unknown error".to_string())
            );
        }

        results.push(result);
    }

    println!("\n=== DIFFERENTIAL TEST RESULTS ===");
    println!("Total tests: {}", test_cases.len());
    println!("Passed: {}", passed_count);
    println!("Failed: {}", test_cases.len() - passed_count);

    if passed_count == test_cases.len() {
        println!("🎉 All tests passed!");
    } else {
        println!("\n❌ Failed tests:");
        for result in &results {
            if !result.passed {
                println!("  - {}: {}", result.name, result.error.as_ref().unwrap());
                println!("    Java: {}", result.java_output.as_ref().unwrap());
                println!("    Rust: {}", result.rust_output.as_ref().unwrap());
            }
        }
        panic!("Some conformance tests failed");
    }
}
