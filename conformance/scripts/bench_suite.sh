#!/usr/bin/env bash

# One-command automated benchmark suite comparing Rust jslt vs Java JSLT.
# It runs several scenarios and produces a single Markdown report.
#
# Usage:
#   ./bench_suite.sh [--out DIR] [--quick]
#
# Options:
#   --out DIR   Directory to place artifacts (default: target/bench-suite-YYYYmmdd-HHMMSS)
#   --quick     Run a lighter suite (fewer iterations, smaller datasets)
#
# Prereqs:
#   - Rust toolchain (stable)
#   - Java 11–17 (for Java JSLT build)
#   - hyperfine (recommended for pretty tables); falls back to /usr/bin/time
#   - jq (recommended)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../" && pwd)"

OUT_DIR=""
QUICK=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --out) OUT_DIR="$2"; shift 2;;
    --quick) QUICK=1; shift;;
    -h|--help)
      sed -n '1,60p' "$0" | sed 's/^# \{0,1\}//'; exit 0;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

ts() { date +%Y%m%d-%H%M%S; }

if [[ -z "$OUT_DIR" ]]; then
  OUT_DIR="$REPO_ROOT/target/bench-suite-$(ts)"
fi
mkdir -p "$OUT_DIR"

HAVE_HYPERFINE=0
if command -v hyperfine >/dev/null 2>&1; then HAVE_HYPERFINE=1; fi

echo "Benchmark artifacts will be written to: $OUT_DIR" >&2

echo "Building Rust CLI (release)..." >&2
pushd "$REPO_ROOT" >/dev/null
cargo build -p cli --release >/dev/null
popd >/dev/null

JSLT_BIN="$REPO_ROOT/target/release/jslt"

# Ensure Java jar exists by doing a quick no-op build if needed (build happens inside the runner)
JAVA_RUN_SH="$SCRIPT_DIR/run_java_jslt.sh"

# Helper: write environment section
env_md="$OUT_DIR/00_environment.md"
{
  echo "### Environment"
  echo
  echo "- Date: $(date -Iseconds)"
  echo "- Host: $(uname -a)"
  if command -v sysctl >/dev/null 2>&1; then
    CPU="$(sysctl -n machdep.cpu.brand_string 2>/dev/null || true)"
    [[ -n "$CPU" ]] && echo "- CPU: $CPU"
  fi
  if [[ -f /proc/cpuinfo ]]; then
    echo "- CPU: $(grep -m1 'model name' /proc/cpuinfo | sed 's/.*: //')" || true
  fi
  echo "- rustc: $(rustc --version 2>/dev/null || echo 'n/a')"
  echo "- cargo: $(cargo --version 2>/dev/null || echo 'n/a')"
  echo "- java: $(java -version 2>&1 | head -n1 | sed 's/"//g')"
  echo
} > "$env_md"

# Helper to run a single transform benchmark via hyperfine
run_transform_md() {
  local name="$1"; shift
  local program_file="$1"; shift
  local input_file="$1"; shift
  local out_md="$OUT_DIR/$name.md"

  if [[ $HAVE_HYPERFINE -eq 1 ]]; then
    echo "Running $name with hyperfine..." >&2
    hyperfine \
      --warmup "$([ $QUICK -eq 1 ] && echo 1 || echo 3)" \
      --min-runs "$([ $QUICK -eq 1 ] && echo 3 || echo 10)" \
      --export-markdown "$out_md" \
      --command-name "rust jslt" \
      "$JSLT_BIN -p $program_file -i $input_file > /dev/null" \
      --command-name "java jslt" \
      "$JAVA_RUN_SH $program_file $input_file > /dev/null" \
      --show-output
  else
    echo "hyperfine not found"
    exit 1
  fi
}

# Prepare queens fixture program and input
QUEENS_FIX="$REPO_ROOT/conformance/cases/208_queens.json"
QUEENS_PROG="$OUT_DIR/queens.jslt"
QUEENS_IN="$OUT_DIR/queens.json"
if command -v jq >/dev/null 2>&1; then
  jq -r .program "$QUEENS_FIX" > "$QUEENS_PROG"
  jq -c .input "$QUEENS_FIX" > "$QUEENS_IN"
else
  # fallback: assume {} input, extract program as best-effort
  awk '/"program"/ {p=1; next} p {print; if (/,?$/) exit}' "$QUEENS_FIX" | sed 's/^\s*"\|",\?$//g' > "$QUEENS_PROG" || true
  echo '{}' > "$QUEENS_IN"
fi

# Scenarios: 1) Queens; 2) Micro identities; 3) Batch over generated dirs
run_transform_md "01_queens" "$QUEENS_PROG" "$QUEENS_IN"

# Micro: inline expressions — we need to create temp program files for Java
make_prog_file() {
  local expr="$1"; local path="$2"; echo "$expr" > "$path"; }

MICRO_DIR="$OUT_DIR/micro"
mkdir -p "$MICRO_DIR"
echo '{}' > "$MICRO_DIR/input.json"
make_prog_file '.' "$MICRO_DIR/identity.jslt"
make_prog_file '{"sum": .a + .b, "greet": "hi " + .name}' "$MICRO_DIR/small_obj.jslt"
make_prog_file '[for ([1,2,3,4,5]) . * 2]' "$MICRO_DIR/comprehension.jslt"

run_transform_md "02_micro_identity" "$MICRO_DIR/identity.jslt" "$MICRO_DIR/input.json"
echo '{"a":2,"b":3,"name":"alice"}' > "$MICRO_DIR/input2.json"
run_transform_md "03_micro_small_obj" "$MICRO_DIR/small_obj.jslt" "$MICRO_DIR/input2.json"
run_transform_md "04_micro_comprehension" "$MICRO_DIR/comprehension.jslt" "$MICRO_DIR/input.json"

# Batch throughput: generate dirs and run loop commands via hyperfine
GEN_SH="$SCRIPT_DIR/gen_json_dir.sh"

gen_and_batch() {
  local tag="$1"; local num="$2"; local size="$3"
  local dir="$OUT_DIR/inputs_${tag}_${size}"
  "$GEN_SH" -o "$dir" -n "$num" -S "$size"

  local prog="$MICRO_DIR/small_obj.jslt"
  local rust_loop="bash -c \"for f in '$dir'/*.json; do [[ -f \\\"\$f\\\" ]] || continue; '$JSLT_BIN' -p '$prog' -i \\\"\$f\\\" >/dev/null; done\""
  local java_loop="bash -c \"for f in '$dir'/*.json; do [[ -f \\\"\$f\\\" ]] || continue; '$JAVA_RUN_SH' '$prog' \\\"\$f\\\" >/dev/null; done\""

  local out_md="$OUT_DIR/10_batch_${tag}_${size}.md"
  if [[ $HAVE_HYPERFINE -eq 1 ]]; then
    echo "Running batch ${tag}/${size} with hyperfine..." >&2
    hyperfine \
      --warmup "$([ $QUICK -eq 1 ] && echo 1 || echo 2)" \
      --export-markdown "$out_md" \
      --command-name "rust jslt batch ($tag/$size)" \
      "${rust_loop}" \
      --command-name "java jslt batch ($tag/$size)" \
      "${java_loop}" \
      --show-output
  else
    echo "hyperfine not found; measuring batch ${tag}/${size} with /usr/bin/time" >&2
    {
      echo "### batch ${tag}/${size}"
      echo
      echo '```text'
      echo "== rust jslt batch =="; /usr/bin/time -lp "${rust_loop[@]}"
      echo
      echo "== java jslt batch =="; /usr/bin/time -lp "${java_loop[@]}"
      echo '```'
    } > "$out_md"
  fi
}

if [[ $QUICK -eq 1 ]]; then
  gen_and_batch "q" 100 small
else
  gen_and_batch "n1" 500 small
  gen_and_batch "n2" 1000 small
  gen_and_batch "n3" 500 medium
fi

# Compose final report
REPORT="$OUT_DIR/benchmark_suite_$(ts).md"
{
  echo "### JSLT Rust vs Java — Automated Benchmark Suite"
  echo
  cat "$env_md"
  echo
  echo "### Queens (N-Queens heavy program)"
  echo
  if [[ -f "$OUT_DIR/01_queens.md" ]]; then cat "$OUT_DIR/01_queens.md"; fi
  echo
  echo "### Micro-benchmarks (single transform)"
  echo
  [[ -f "$OUT_DIR/02_micro_identity.md" ]] && echo "#### Identity" && echo && cat "$OUT_DIR/02_micro_identity.md" && echo
  [[ -f "$OUT_DIR/03_micro_small_obj.md" ]] && echo "#### Small object" && echo && cat "$OUT_DIR/03_micro_small_obj.md" && echo
  [[ -f "$OUT_DIR/04_micro_comprehension.md" ]] && echo "#### Comprehension" && echo && cat "$OUT_DIR/04_micro_comprehension.md" && echo
  echo
  echo "### Batch throughput"
  echo
  for f in "$OUT_DIR"/10_batch_*.md; do
    [[ -f "$f" ]] || continue
    base="$(basename "$f" .md)"
    tag="${base#10_batch_}"
    echo "#### $tag"; echo; cat "$f"; echo
  done
} > "$REPORT"

echo "Benchmark suite completed. Report: $REPORT" >&2
echo "$REPORT"
