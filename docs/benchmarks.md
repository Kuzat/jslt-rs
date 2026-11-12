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
a warmup, then batches of about 20 ms, reporting the median time per call. Java applies to a
parsed `JsonNode`, and Rust applies to a parsed `JsltValue` with `CompiledProgram::apply_value`.
`apply` takes and returns `serde_json::Value` and converts both ways. For the 1 000-event input
that adds about 3 ms per call (criterion group `apply_json`). The script also checks
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
| transform | 5.50 ms | 1.22 ms | 4.51× |
| matcher | 1.99 ms | 336.99 µs | 5.91× |
| aggregate | 455.60 µs | 219.71 µs | 2.07× |
| strings | 4.12 ms | 1.43 ms | 2.88× |
| time | 3.09 ms | 2.54 ms | 1.22× |
| large_object | 552.70 µs | 158.27 µs | 3.49× |
| identity | 178 ns | 8 ns | 22.25× |
| numeric | 5.12 ms | 3.56 ms | 1.44× |
| fib | 1.78 ms | 1.20 ms | 1.49× |
| queens | 886.65 ms | 686.05 ms | 1.29× |
| cdp | 4.33 µs | 830 ns | 5.21× |
