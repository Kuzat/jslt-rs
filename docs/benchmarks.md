# Benchmarks

The benchmarks run a fixed set of workloads in-process, so they measure the engine and not
process or JVM startup.

## Workloads

The workloads are in `crates/engine/benches/workloads/`. Each one is a `.jslt` program plus an
input that `mod.rs` generates. The inputs are deterministic, so the numbers are comparable from
run to run and with Java JSLT.

| Workload | Exercises | Input |
|---|---|---|
| `transform` | Record mapping: field access, string functions, nested comprehensions, a user function | 1 000 events |
| `matcher` | Object matchers (`*: .`) that copy most of each record | 1 000 events |
| `aggregate` | Filters, lets, `sum`/`any`/`all`, object comprehensions, experimental `group-by` | 1 000 events |
| `strings` | Regex functions (`test`, `capture`, `replace`, `split`), `sha256-hex`, `to-json` | 1 000 events |
| `time` | `parse-time` and `format-time` with a time zone | 1 000 events |
| `large_object` | Building, iterating and looking up a 1 000-key object | 1 000 events |
| `identity` | `.` on a large input (cost of passing values in and out) | 1 000 events |
| `numeric` | Arithmetic and number functions | 10 000 numbers |
| `fib` | Recursive user function calls (`fib(20)`) | `{"n": 20}` |
| `queens` | The N-Queens program from `conformance/cases/208_queens.json` | `{}` |
| `cdp` | The upstream `user-external2cdp.jslt` test (small input, per-call overhead) | `user-external.json` |

To add a workload, put a program in that directory and add it to `all()` in `mod.rs`. Use only
syntax and functions that Java JSLT 0.1.14 supports (for example `mod()`, not `%`), so the
comparison can run it.

## Rust vs Java

```bash
conformance/scripts/bench_compare.sh            # about 2 minutes
conformance/scripts/bench_compare.sh --quick    # shorter warmup and measurement
conformance/scripts/bench_compare.sh fib queens # only some workloads
```

The script runs each workload with `bench_runner` (`crates/engine/examples/bench_runner.rs`) and
with `JsltBench` (`conformance/scripts/JsltBench.java`) on a warm JVM. Both use the same method:
a warmup, then batches of about 20 ms, reporting the median time per call. The script also checks
that both give the same output. It writes `report.md`, `rust.tsv` and `java.tsv` to
`target/bench-compare/`. It needs a JDK and `jq`, and builds the Java JSLT jar if it is missing.

## Tracking changes

To see the effect of a change quickly, save a run and compare with it:

```bash
conformance/scripts/bench_compare.sh --rust-only --out /tmp/before
# ... make the change ...
conformance/scripts/bench_compare.sh --rust-only --baseline /tmp/before/rust.tsv
```

With `--quick`, expect 5 to 15% noise between runs. Use the full run, or criterion, for small
differences.

Criterion gives statistically checked results for the same workloads, plus compile times:

```bash
cargo bench -p engine --bench workload_bench -- --save-baseline main
# ... make the change ...
cargo bench -p engine --bench workload_bench -- --baseline main
cargo bench -p engine --bench workload_bench -- apply/transform   # one workload
```

`cargo test --workspace --all-targets` also runs each workload once, so CI catches a workload
that stops compiling.

`conformance/scripts/bench_suite.sh` is a different tool. It times whole CLI runs of both
implementations with hyperfine, so the results include process and JVM startup.

## Results

Rust and Java side by side on an Apple M3 Pro, from `bench_compare.sh --quick`. All
outputs match.

| Workload | Rust (median) | Java (median) | Rust / Java |
|---|---:|---:|---:|
| transform | 9.53 ms | 1.23 ms | 7.72× |
| matcher | 5.58 ms | 350.53 µs | 15.91× |
| aggregate | 2.25 ms | 222.53 µs | 10.11× |
| strings | 47.25 ms | 1.52 ms | 31.09× |
| time | 5.02 ms | 2.65 ms | 1.89× |
| large_object | 2.33 ms | 168.78 µs | 13.80× |
| identity | 3.41 ms | 9 ns | 379110.33× |
| numeric | 5.12 ms | 3.69 ms | 1.39× |
| fib | 1.75 ms | 1.19 ms | 1.48× |
| queens | 906.35 ms | 699.22 ms | 1.30× |
| cdp | 11.72 µs | 849 ns | 13.81× |
