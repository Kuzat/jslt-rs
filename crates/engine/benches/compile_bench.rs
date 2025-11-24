use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn bench_compile_queens(c: &mut Criterion) {
    // Load the conformance queens fixture and extract the program string
    let fixture: serde_json::Value = serde_json::from_str(include_str!(
        "../../../conformance/cases/208_queens.json"
    ))
    .expect("valid queens fixture JSON");
    let program = fixture
        .get("program")
        .and_then(|v| v.as_str())
        .expect("fixture has program string");

    c.bench_function("compile_queens_program", |b| {
        b.iter(|| {
            let compiled = engine::compile(black_box(program)).expect("compile queens");
            criterion::black_box(compiled);
        })
    });
}

fn bench_compile_small_programs(c: &mut Criterion) {
    let progs = [
        ".",                                     // identity
        "{sum: .a + .b, greet: \"hi \" + .name}", // small object
        "[1,2,3,4,5]",                          // literal array
    ];
    for (i, p) in progs.iter().enumerate() {
        c.bench_function(&format!("compile_small_{}", i), |b| {
            b.iter(|| {
                let compiled = engine::compile(black_box(p)).expect("compile small");
                criterion::black_box(compiled);
            })
        });
    }
}

fn benches(c: &mut Criterion) {
    bench_compile_queens(c);
    bench_compile_small_programs(c);
}

criterion_group!(name = compile_group; config = Criterion::default(); targets = benches);
criterion_main!(compile_group);
