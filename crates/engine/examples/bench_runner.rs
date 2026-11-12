//! Times the benchmark workloads in-process with the same method as
//! `conformance/scripts/JsltBench.java`, so the numbers can be compared with Java JSLT.
//! Like Java, which applies to a parsed `JsonNode`, it applies to a parsed `JsltValue`.
//!
//! Usage: bench_runner [--dump DIR] [--out DIR] [--warmup-ms N] [--measure-ms N] [NAME...]
//!   --dump DIR  write `<name>.jslt` and `<name>.json` for each workload, then exit
//!   --out DIR   also write each workload's output to `<name>.out.json`
//!
//! Prints one `name<TAB>median_ns<TAB>min_ns` line per workload.

#[path = "../benches/workloads/mod.rs"]
mod workloads;

use std::hint;
use std::path::PathBuf;
use std::time::{Duration, Instant};

const BATCH_TARGET: Duration = Duration::from_millis(20);

fn main() {
    let mut dump: Option<PathBuf> = None;
    let mut out: Option<PathBuf> = None;
    let mut warmup = Duration::from_millis(1_000);
    let mut measure = Duration::from_millis(3_000);
    let mut names = Vec::new();
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        let mut value = || args.next().unwrap_or_else(|| panic!("{a} needs a value"));
        match a.as_str() {
            "--dump" => dump = Some(value().into()),
            "--out" => out = Some(value().into()),
            "--warmup-ms" => warmup = Duration::from_millis(value().parse().expect("number")),
            "--measure-ms" => measure = Duration::from_millis(value().parse().expect("number")),
            _ => names.push(a),
        }
    }

    let selected: Vec<_> = workloads::all()
        .into_iter()
        .filter(|w| names.is_empty() || names.iter().any(|n| n == w.name))
        .collect();

    if let Some(dir) = dump {
        std::fs::create_dir_all(&dir).expect("create dump dir");
        for w in &selected {
            std::fs::write(dir.join(format!("{}.jslt", w.name)), w.program).unwrap();
            std::fs::write(dir.join(format!("{}.json", w.name)), (w.input)().to_string()).unwrap();
        }
        return;
    }

    for w in &selected {
        let compiled = engine::compile(w.program).expect("compile workload");
        let input = engine::JsltValue::from_json((w.input)());
        let run = || compiled.apply_value(hint::black_box(&input), None).expect("apply workload");

        if let Some(dir) = &out {
            std::fs::create_dir_all(dir).expect("create out dir");
            std::fs::write(dir.join(format!("{}.out.json", w.name)), run().to_string()).unwrap();
        }

        let (median, min) = time(run, warmup, measure);
        println!("{}\t{}\t{}", w.name, median, min);
    }
}

/// Runs `f` for `warmup`, then in batches of about `BATCH_TARGET` for `measure`.
/// Returns the median and minimum nanoseconds per call over the batches.
fn time<T>(mut f: impl FnMut() -> T, warmup: Duration, measure: Duration) -> (u64, u64) {
    let start = Instant::now();
    let mut calls = 0u64;
    while start.elapsed() < warmup || calls == 0 {
        hint::black_box(f());
        calls += 1;
    }
    let per_call = start.elapsed().as_nanos() as u64 / calls;
    let batch = (BATCH_TARGET.as_nanos() as u64 / per_call.max(1)).max(1);

    let mut samples = Vec::new();
    let start = Instant::now();
    while start.elapsed() < measure || samples.len() < 5 {
        let t = Instant::now();
        for _ in 0..batch {
            hint::black_box(f());
        }
        samples.push(t.elapsed().as_nanos() as u64 / batch);
    }
    samples.sort_unstable();
    (samples[samples.len() / 2], samples[0])
}
