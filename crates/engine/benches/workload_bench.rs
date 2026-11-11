//! Criterion benchmarks over the shared workloads (see `workloads/mod.rs`).
//!
//! Compare against a saved baseline:
//!   cargo bench -p engine --bench workload_bench -- --save-baseline main
//!   cargo bench -p engine --bench workload_bench -- --baseline main

use criterion::{criterion_group, criterion_main, Criterion, SamplingMode};
use std::hint;
use std::time::Duration;

mod workloads;

fn bench_apply(c: &mut Criterion) {
    let mut g = c.benchmark_group("apply");
    // Flat sampling and few samples keep slow workloads like queens (~1 s per call) practical.
    g.sampling_mode(SamplingMode::Flat).sample_size(20).measurement_time(Duration::from_secs(5));
    for w in workloads::all() {
        let compiled = engine::compile(w.program).expect("compile workload");
        let input = (w.input)();
        g.bench_function(w.name, |b| {
            b.iter(|| hint::black_box(compiled.apply(hint::black_box(&input), None).unwrap()))
        });
    }
    g.finish();
}

fn bench_compile(c: &mut Criterion) {
    let mut g = c.benchmark_group("compile");
    for w in workloads::all().into_iter().filter(|w| matches!(w.name, "transform" | "queens")) {
        g.bench_function(w.name, |b| {
            b.iter(|| hint::black_box(engine::compile(hint::black_box(w.program)).unwrap()))
        });
    }
    g.finish();
}

criterion_group!(benches, bench_apply, bench_compile);
criterion_main!(benches);
