#!/usr/bin/env bash

# Benchmark throughput for processing many JSON inputs with a single JSLT program
# comparing Rust CLI vs Java JSLT.
#
# Usage:
#   ./bench_batch.sh -p program.jslt -d inputs_dir [--pretty]
#   ./bench_batch.sh -e '.expr' -d inputs_dir
#
# The script will iterate over all *.json files in the given directory.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../" && pwd)"
JAVA_RUN_SH="$SCRIPT_DIR/run_java_jslt.sh"

PROGRAM=""
EVAL=""
DIR=""
PRETTY=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -p|--program) PROGRAM="$2"; shift 2;;
    -e|--eval) EVAL="$2"; shift 2;;
    -d|--dir) DIR="$2"; shift 2;;
    --pretty) PRETTY="--pretty"; shift;;
    -h|--help)
      echo "Usage: $0 (-p FILE | -e EXPR) -d DIR [--pretty]"; exit 0;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$DIR" ]]; then echo "Error: --dir is required" >&2; exit 1; fi
if [[ ! -d "$DIR" ]]; then echo "Error: dir not found: $DIR" >&2; exit 1; fi
if [[ -z "$PROGRAM" && -z "$EVAL" ]]; then echo "Error: one of --program or --eval is required" >&2; exit 1; fi
if [[ -n "$PROGRAM" && ! -f "$PROGRAM" ]]; then echo "Error: program file not found: $PROGRAM" >&2; exit 1; fi

echo "Building Rust CLI (release)..." >&2
pushd "$REPO_ROOT" >/dev/null
cargo build -p cli --release >/dev/null
popd >/dev/null

JSLT_BIN="$REPO_ROOT/target/release/jslt"

TMP_PROG=""
if [[ -n "$EVAL" ]]; then
  TMP_PROG=$(mktemp /tmp/jslt-prog-XXXX.jslt)
  echo "$EVAL" > "$TMP_PROG"
  trap 'rm -f "$TMP_PROG"' EXIT
fi

quote() { printf '%q' "$1"; }

if [[ -n "$PROGRAM" ]]; then
  PROG_ARG_RUST="-p $(quote "$PROGRAM")"
  PROG_ARG_JAVA="$(quote "$PROGRAM")"
else
  PROG_ARG_RUST="-e $(quote "$EVAL")"
  PROG_ARG_JAVA="$(quote "$TMP_PROG")"
fi

PRETTY_PART=""
if [[ -n "$PRETTY" ]]; then
  PRETTY_PART=" $(quote "$PRETTY")"
fi

# Build fully inlined loop bodies so subshells don't depend on exported vars.
RUST_LOOP="for f in $(quote "$DIR")/*.json; do [[ -f \"\$f\" ]] || continue; $(quote "$JSLT_BIN") $PROG_ARG_RUST -i \"\$f\"${PRETTY_PART} >/dev/null; done"
JAVA_LOOP="for f in $(quote "$DIR")/*.json; do [[ -f \"\$f\" ]] || continue; $(quote "$JAVA_RUN_SH") $PROG_ARG_JAVA \"\$f\" >/dev/null; done"


if command -v hyperfine >/dev/null 2>&1; then
  echo "Running batch with hyperfine over $(ls "$DIR"/*.json 2>/dev/null | wc -l | tr -d ' ') files..." >&2
  hyperfine \
    --warmup 2 \
    --export-markdown benchmark_batch.md \
    --command-name "rust jslt batch" \
    "bash -c '$RUST_LOOP'" \
    --command-name "java jslt batch" \
    "bash -c '$JAVA_LOOP'"
  echo "Results saved to benchmark_batch.md"
else
  echo "hyperfine not found; using /usr/bin/time for overall loop" >&2
  echo "=== rust jslt batch ==="
  /usr/bin/time -lp bash -c "$RUST_LOOP"
  echo "=== java jslt batch ==="
  /usr/bin/time -lp bash -c "$JAVA_LOOP"
fi
