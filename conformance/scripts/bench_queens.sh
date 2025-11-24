#!/usr/bin/env bash

# Convenience wrapper: benchmark the queens conformance case (208_queens.json)
# against Rust CLI vs Java CLI.
#
# Usage:
#   ./bench_queens.sh [--pretty]
#
# Optionally you can pass --case PATH to point to another fixture JSON

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../" && pwd)"

PRETTY=""
CASE_JSON="$REPO_ROOT/conformance/cases/208_queens.json"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --pretty) PRETTY="--pretty"; shift;;
    --case) CASE_JSON="$2"; shift 2;;
    -h|--help) echo "Usage: $0 [--pretty] [--case conformance/cases/XXX.json]"; exit 0;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ ! -f "$CASE_JSON" ]]; then
  echo "Fixture not found: $CASE_JSON" >&2; exit 1
fi

PROGRAM_FILE=$(mktemp /tmp/jslt-queens-prog-XXXX.jslt)
INPUT_FILE=$(mktemp /tmp/jslt-queens-input-XXXX.json)
trap 'rm -f "$PROGRAM_FILE" "$INPUT_FILE"' EXIT

# Extract fields using jq if present; otherwise use simple sed/awk fallback
if command -v jq >/dev/null 2>&1; then
  jq -r .program "$CASE_JSON" > "$PROGRAM_FILE"
  jq -c .input "$CASE_JSON" > "$INPUT_FILE"
else
  # crude fallback: assumes the JSON strings are in single line (they are in repo via tooling)
  awk '/"program"/ {p=1; next} p {print; if (/",?$/) exit}' "$CASE_JSON" | sed 's/^\s*"\|",\?$//g' > "$PROGRAM_FILE" || true
  awk '/"input"/ {print; exit}' "$CASE_JSON" | sed 's/^.*: \(.*\)$/\1/' > "$INPUT_FILE" || echo '{}' > "$INPUT_FILE"
fi

"$SCRIPT_DIR/bench_transform.sh" -p "$PROGRAM_FILE" -i "$INPUT_FILE" $PRETTY
