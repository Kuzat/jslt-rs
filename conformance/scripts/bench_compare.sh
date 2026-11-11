#!/usr/bin/env bash
# Times the benchmark workloads (crates/engine/benches/workloads) in-process with Rust and with
# Java JSLT, checks that both give the same output, and prints a Markdown table.
#
# Usage: bench_compare.sh [--quick] [--rust-only] [--baseline FILE] [--out DIR] [NAME...]
#   --quick          shorter warmup and measurement
#   --rust-only      skip Java (no JDK needed)
#   --baseline FILE  add a column comparing with an earlier run's rust.tsv
#   --out DIR        where to write results (default: target/bench-compare)
#
# Each run writes DIR/rust.tsv (and DIR/java.tsv): name, median ns, min ns per call.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
OUT="$REPO_ROOT/target/bench-compare"
WARMUP=2000
MEASURE=5000
JAVA=1
BASELINE=""
NAMES=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    --quick) WARMUP=500; MEASURE=1500; shift;;
    --rust-only) JAVA=0; shift;;
    --baseline) BASELINE="$(cd "$(dirname "$2")" && pwd)/$(basename "$2")"; shift 2;;
    --out) OUT="$2"; shift 2;;
    -h|--help) sed -n '2,12p' "$0" | sed 's/^# \{0,1\}//'; exit 0;;
    *) NAMES+=("$1"); shift;;
  esac
done

mkdir -p "$OUT"
OUT="$(cd "$OUT" && pwd)"
# Copy the baseline first: it may be the rust.tsv this run overwrites.
if [[ -n "$BASELINE" ]]; then
  BASELINE_COPY="$(mktemp)"
  trap 'rm -f "$BASELINE_COPY"' EXIT
  cp "$BASELINE" "$BASELINE_COPY"
  BASELINE="$BASELINE_COPY"
fi
cd "$REPO_ROOT"

echo "Building Rust bench runner (release)..." >&2
cargo build -q --release -p engine --example bench_runner
RUNNER="$REPO_ROOT/target/release/examples/bench_runner"
"$RUNNER" --dump "$OUT/workloads" ${NAMES[@]+"${NAMES[@]}"}

echo "Running Rust workloads..." >&2
"$RUNNER" --warmup-ms "$WARMUP" --measure-ms "$MEASURE" --out "$OUT/rust-out" \
  ${NAMES[@]+"${NAMES[@]}"} | tee "$OUT/rust.tsv" >&2

if [[ $JAVA -eq 1 ]]; then
  JAR=$(ls "$REPO_ROOT"/conformance/java-jslt/core/build/libs/core-*-all.jar 2>/dev/null | head -n1 || true)
  if [[ -z "$JAR" ]]; then
    echo "Building Java JSLT..." >&2
    (cd "$REPO_ROOT/conformance/java-jslt" && ./gradlew -q shadowJar >&2)
    JAR=$(ls "$REPO_ROOT"/conformance/java-jslt/core/build/libs/core-*-all.jar | head -n1)
  fi
  mkdir -p "$OUT/java-classes"
  javac -cp "$JAR" -d "$OUT/java-classes" "$SCRIPT_DIR/JsltBench.java"
  echo "Running Java workloads..." >&2
  java -cp "$JAR:$OUT/java-classes" JsltBench "$OUT/workloads" \
    --warmup-ms "$WARMUP" --measure-ms "$MEASURE" --out "$OUT/java-out" \
    ${NAMES[@]+"${NAMES[@]}"} | tee "$OUT/java.tsv" >&2
fi

# Formats nanoseconds with a readable unit.
fmt() {
  awk -v ns="$1" 'BEGIN {
    if (ns == "") { print "-"; exit }
    if (ns >= 1e9) printf "%.2f s", ns / 1e9
    else if (ns >= 1e6) printf "%.2f ms", ns / 1e6
    else if (ns >= 1e3) printf "%.2f µs", ns / 1e3
    else printf "%d ns", ns
  }'
}
lookup() { [[ -f "$1" ]] && awk -F'\t' -v n="$2" '$1 == n { print $2 }' "$1" || true; }
ratio() { awk -v a="$1" -v b="$2" 'BEGIN { if (a == "" || b == "" || b == 0) print "-"; else printf "%.2f×", a / b }'; }

header="| Workload | Rust (median) |"
divider="|---|---:|"
if [[ -n "$BASELINE" ]]; then header+=" Baseline | vs baseline |"; divider+="---:|---:|"; fi
if [[ $JAVA -eq 1 ]]; then header+=" Java (median) | Rust / Java | Same output |"; divider+="---:|---:|:---:|"; fi

{
  echo "$header"
  echo "$divider"
  while IFS=$'\t' read -r name rust _; do
    row="| $name | $(fmt "$rust") |"
    if [[ -n "$BASELINE" ]]; then
      base=$(lookup "$BASELINE" "$name")
      row+=" $(fmt "$base") | $(ratio "$rust" "$base") |"
    fi
    if [[ $JAVA -eq 1 ]]; then
      java=$(lookup "$OUT/java.tsv" "$name")
      if cmp -s <(jq -S . "$OUT/rust-out/$name.out.json") <(jq -S . "$OUT/java-out/$name.out.json"); then
        same="yes"
      else
        same="**NO**"
      fi
      row+=" $(fmt "$java") | $(ratio "$rust" "$java") | $same |"
    fi
    echo "$row"
  done < "$OUT/rust.tsv"
} | tee "$OUT/report.md"
