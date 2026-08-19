//! Performance regression harness for the language server.
//!
//! These are wall-clock assertions, so they are `#[ignore]`d by default and
//! meant to be run deliberately:
//!
//! ```text
//! cargo test -p jslt-lsp --test perf_tests --release -- --ignored --nocapture
//! ```
//!
//! The budgets are deliberately loose — they exist to catch an accidental
//! quadratic blowup or a reparse-per-request regression, not to police
//! millisecond drift. Timings are printed so a run can be compared by eye.

mod harness;

use std::fmt::Write as _;
use std::time::{Duration, Instant};

use harness::TestServer;
use serde_json::json;

/// Defs in the single large document.
const LARGE_FILE_DEFS: usize = 500;
/// Modules importing the shared one in the synthetic workspace.
const WORKSPACE_MODULES: usize = 60;
/// Defs exported by the shared module.
const SHARED_DEFS: usize = 20;

/// Budget for a request answered from one document's cached snapshot.
const SINGLE_FILE_BUDGET: Duration = Duration::from_millis(250);
/// Budget for a request that fans out over the whole module graph.
const WORKSPACE_BUDGET: Duration = Duration::from_millis(1_500);
/// Budget for the initial disk crawl of the synthetic workspace.
const DISCOVERY_BUDGET: Duration = Duration::from_secs(5);

/// A file with many top-level defs and a body that calls all of them.
fn large_document() -> String {
    let mut out = String::new();
    for i in 0..LARGE_FILE_DEFS {
        let _ = writeln!(out, "def helper_{i}(x)\n  let scaled = $x * {i}\n  $scaled + {i}\n");
    }

    let _ = writeln!(out, "{{");
    for i in 0..LARGE_FILE_DEFS {
        let comma = if i + 1 == LARGE_FILE_DEFS { "" } else { "," };
        let _ = writeln!(out, "  \"k{i}\": helper_{i}(.value){comma}");
    }
    let _ = writeln!(out, "}}");
    out
}

/// One shared module plus many modules importing and calling it.
fn workspace_files() -> Vec<(String, String)> {
    let mut shared = String::new();
    for i in 0..SHARED_DEFS {
        let _ = writeln!(shared, "def shared_{i}(x)\n  $x + {i}\n");
    }
    let _ = writeln!(shared, "shared_0(.value)");

    let mut files = vec![("shared.jslt".to_string(), shared)];
    for module in 0..WORKSPACE_MODULES {
        let mut text = String::from("import \"shared.jslt\" as shared\n\n{\n");
        for i in 0..SHARED_DEFS {
            let comma = if i + 1 == SHARED_DEFS { "" } else { "," };
            let _ = writeln!(text, "  \"k{i}\": shared:shared_{i}(.value){comma}");
        }
        text.push_str("}\n");
        files.push((format!("module_{module}.jslt"), text));
    }
    files
}

/// Time `work`, report it, and fail if it blew the budget.
async fn measure<F, T>(label: &str, budget: Duration, work: F) -> T
where
    F: std::future::Future<Output = T>,
{
    let started = Instant::now();
    let result = work.await;
    let elapsed = started.elapsed();

    println!("{label:<32} {:>8.1?} (budget {budget:?})", elapsed);
    assert!(elapsed <= budget, "{label} took {elapsed:?}, over the {budget:?} budget");

    result
}

#[tokio::test]
#[ignore = "wall-clock timings; run explicitly in release"]
async fn single_large_document_stays_responsive() {
    let text = large_document();
    let mut server = TestServer::with_files(&[("large.jslt", &text)]).await;

    measure("didOpen + analysis", WORKSPACE_BUDGET, server.open("large.jslt", &text)).await;

    // Inside the body of the first def, where the scope chain is deepest.
    let completion = measure(
        "completion",
        SINGLE_FILE_BUDGET,
        server.position_request("textDocument/completion", "large.jslt", 2, 2),
    )
    .await;
    assert!(completion.is_array(), "completion returned {completion:?}");

    for method in ["textDocument/hover", "textDocument/definition"] {
        measure(method, SINGLE_FILE_BUDGET, server.position_request(method, "large.jslt", 0, 5))
            .await;
    }

    // References for a def called once from the body of a 500-def file.
    let references = measure(
        "references",
        SINGLE_FILE_BUDGET,
        server.position_request("textDocument/references", "large.jslt", 0, 5),
    )
    .await;
    assert!(references.is_array(), "references returned {references:?}");
}

#[tokio::test]
#[ignore = "wall-clock timings; run explicitly in release"]
async fn medium_workspace_cross_file_operations_stay_responsive() {
    let files = workspace_files();
    let borrowed: Vec<(&str, &str)> =
        files.iter().map(|(name, text)| (name.as_str(), text.as_str())).collect();

    let started = Instant::now();
    let mut server = TestServer::with_files(&borrowed).await;
    let discovery = started.elapsed();
    println!("{:<32} {:>8.1?} (budget {DISCOVERY_BUDGET:?})", "initialize + discovery", discovery);
    assert!(discovery <= DISCOVERY_BUDGET, "discovery took {discovery:?}");

    let shared = files[0].1.clone();
    server.open("shared.jslt", &shared).await;

    // `def shared_0` is called by every module in the workspace.
    let references = measure(
        "workspace references",
        WORKSPACE_BUDGET,
        server.position_request("textDocument/references", "shared.jslt", 0, 5),
    )
    .await;
    let locations = references.as_array().expect("locations");
    assert!(
        locations.len() > WORKSPACE_MODULES,
        "expected a call site per module, got {}",
        locations.len()
    );

    let uri = server.uri("shared.jslt");
    let rename = measure(
        "workspace rename",
        WORKSPACE_BUDGET,
        server.request(
            "textDocument/rename",
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 5 },
                "newName": "renamed_0",
            }),
        ),
    )
    .await;
    let changes = rename["changes"].as_object().expect("changes");
    assert_eq!(changes.len(), WORKSPACE_MODULES + 1, "every importer must be edited");
}
