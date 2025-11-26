use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use std::hint;

fn bench_eval_queens(c: &mut Criterion) {
    // Use the queens conformance case; input is an empty object {}
    let fixture: serde_json::Value =
        serde_json::from_str(include_str!("../../../conformance/cases/208_queens.json"))
            .expect("valid queens fixture JSON");
    let program =
        fixture.get("program").and_then(|v| v.as_str()).expect("fixture has program string");
    let input = fixture.get("input").cloned().unwrap_or_else(|| serde_json::json!({}));

    let compiled = engine::compile(program).expect("compile queens");

    let mut g = c.benchmark_group("eval_queens");
    g.measurement_time(std::time::Duration::from_secs(10));
    g.sample_size(100);
    g.bench_function("apply_only", |b| {
        b.iter(|| {
            let out = compiled.apply(hint::black_box(&input), None).expect("apply");
            hint::black_box(out);
        })
    });
    g.finish();
}

fn bench_eval_identity_throughput(c: &mut Criterion) {
    // Simple throughput test of applying identity program over a moderately large input
    let program = ".";
    let mut arr = Vec::with_capacity(10_000);
    for i in 0..10_000u32 {
        arr.push(serde_json::json!({"i": i, "s": format!("x{}", i)}));
    }
    let input = serde_json::json!({"items": arr});
    let compiled = engine::compile(program).expect("compile identity");

    let mut g = c.benchmark_group("eval_identity_large_input");
    g.throughput(Throughput::Elements(10_000));
    g.bench_function("apply_only", |b| {
        b.iter(|| {
            let out = compiled.apply(hint::black_box(&input), None).expect("apply");
            hint::black_box(out);
        })
    });
    g.finish();
}

criterion_group!(
    name = eval_group;
    config = Criterion::default();
    targets = bench_eval_queens, bench_eval_identity_throughput
);
criterion_main!(eval_group);
