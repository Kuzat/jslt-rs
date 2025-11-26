use criterion::{criterion_group, criterion_main, Criterion};
use std::hint;

// A synthetic map-like comprehension to exercise array/object comprehensions
// Example program applies a transformation over an array field `.items`
const COMPREHENSION_PROG: &str = r#"
[
  for (.items)
    { "idx": .i, "s": .s, "pair": (.i * 2) + (size(.s) - 1) }
]
"#;

fn build_large_input(n: usize) -> serde_json::Value {
    let mut arr = Vec::with_capacity(n);
    for i in 0..n {
        arr.push(serde_json::json!({"i": i as i64, "s": format!("x{}", i)}));
    }
    serde_json::json!({"items": arr})
}

fn bench_comprehension(c: &mut Criterion) {
    let compiled = engine::compile(COMPREHENSION_PROG).expect("compile comprehension");
    for &n in &[100, 1_000, 10_000] {
        let input = build_large_input(n);
        c.bench_function(&format!("comprehension_n={}", n), |b| {
            b.iter(|| {
                let out = compiled.apply(hint::black_box(&input), None).expect("apply");
                hint::black_box(out);
            })
        });
    }
}

criterion_group!(name = comp_group; config = Criterion::default(); targets = bench_comprehension);
criterion_main!(comp_group);
