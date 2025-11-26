#!/usr/bin/env bash

# Compare end-to-end performance of Rust jslt CLI vs Java JSLT
# Usage:
#   ./bench_transform.sh -p program.jslt -i input.json [--pretty]
#   ./bench_transform.sh -e '.expr' -i input.json
#
# Notes:
# - Uses hyperfine if available, otherwise falls back to /usr/bin/time
# - Builds Rust CLI in release mode, and builds Java JSLT via run_java_jslt.sh when needed

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../" && pwd)"
JAVA_RUN_SH="$SCRIPT_DIR/run_java_jslt.sh"

PROGRAM=""
EVAL=""
INPUT=""
PRETTY=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    -p|--program)
      PROGRAM="$2"; shift 2;;
    -e|--eval)
      EVAL="$2"; shift 2;;
    -i|--input)
      INPUT="$2"; shift 2;;
    --pretty)
      PRETTY="--pretty"; shift;;
    -h|--help)
      echo "Usage: $0 (-p FILE | -e EXPR) -i INPUT.json [--pretty]"; exit 0;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$INPUT" ]]; then
  echo "Error: --input is required" >&2; exit 1
fi
if [[ -z "$PROGRAM" && -z "$EVAL" ]]; then
  echo "Error: one of --program or --eval is required" >&2; exit 1
fi
if [[ -n "$PROGRAM" && ! -f "$PROGRAM" ]]; then
  echo "Error: program file not found: $PROGRAM" >&2; exit 1
fi
if [[ ! -f "$INPUT" ]]; then
  echo "Error: input file not found: $INPUT" >&2; exit 1
fi

echo "Building Rust CLI (release)..." >&2
pushd "$REPO_ROOT" >/dev/null
cargo build -p cli --release >/dev/null
popd >/dev/null

JSLT_BIN="$REPO_ROOT/target/release/jslt"

join_cmd() {
  local out=""
  for arg in "$@"; do
    out+="$(printf '%q' "$arg") "
  done
  echo "${out% }"
}

# Prepare commands as strings for both hyperfine and time
if [[ -n "$PROGRAM" ]]; then
  RUST_CMD_STR=$(join_cmd "$JSLT_BIN" -p "$PROGRAM" -i "$INPUT" ${PRETTY:+$PRETTY})
  JAVA_CMD_STR=$(join_cmd "$JAVA_RUN_SH" "$PROGRAM" "$INPUT")
else
  # For Java CLI we need a file; create a temp file with the expression
  TMP_PROG=$(mktemp /tmp/jslt-prog-XXXX.jslt)
  echo "$EVAL" > "$TMP_PROG"
  trap 'rm -f "$TMP_PROG"' EXIT

  RUST_CMD_STR=$(join_cmd "$JSLT_BIN" -e "$EVAL" -i "$INPUT" ${PRETTY:+$PRETTY})
  JAVA_CMD_STR=$(join_cmd "$JAVA_RUN_SH" "$TMP_PROG" "$INPUT")
fi

if command -v hyperfine >/dev/null 2>&1; then
  echo "RUST hyperfine command string:"
  printf '  %s\n' "${RUST_CMD_STR} > /dev/null"

  echo "Running with hyperfine..." >&2
  hyperfine \
    --warmup 3 \
    --export-markdown benchmark_transform.md \
    --command-name "rust jslt" \
    "${RUST_CMD_STR} > /dev/null" \
    --command-name "java jslt" \
    "${JAVA_CMD_STR} > /dev/null"
  echo "Results saved to benchmark_transform.md"
else
  echo "hyperfine not found; using /usr/bin/time for a few runs" >&2
  for name in "rust jslt" "java jslt"; do
    echo "=== $name ==="
    for i in {1..5}; do
      if [[ "$name" == "rust jslt" ]]; then
        /usr/bin/time -lp bash -c "${RUST_CMD_STR} >/dev/null"
      else
        /usr/bin/time -lp bash -c "${JAVA_CMD_STR} >/dev/null"
      fi
    done
  done
fi
