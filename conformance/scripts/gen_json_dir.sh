#!/usr/bin/env bash

# Generate a directory with many JSON files for batch benchmarking.
#
# Usage:
#   ./gen_json_dir.sh -o out_dir -n 1000 [-S small|medium|large]
#
# Options:
#   -o, --out       Output directory to (re)create
#   -n, --num       Number of files to generate (default: 100)
#   -S, --size      Payload size per file: small|medium|large (default: small)
#
# Each JSON file is a compact object containing predictable fields so that
# JSLT programs can operate on them in a stable way.

set -euo pipefail

OUT=""
NUM=100
SIZE="small"

while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--out) OUT="$2"; shift 2;;
    -n|--num) NUM="$2"; shift 2;;
    -S|--size) SIZE="$2"; shift 2;;
    -h|--help)
      sed -n '1,40p' "$0" | sed 's/^# \{0,1\}//'; exit 0;;
    *) echo "Unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$OUT" ]]; then
  echo "Error: --out is required" >&2; exit 1
fi

case "$SIZE" in
  small|medium|large) ;;
  *) echo "Error: --size must be small|medium|large" >&2; exit 1;;
esac

rm -rf "$OUT"
mkdir -p "$OUT"

payload_for() {
  local i="$1"
  case "$SIZE" in
    small)
      # ~ a few fields
      printf '{"i":%d,"a":1,"b":2,"s":"x%d"}' "$i" "$i"
      ;;
    medium)
      # Add an array of 20 simple objects
      printf '{"i":%d,"a":1,"b":2,"s":"x%d","items":[' "$i" "$i"
      local j
      for j in $(seq 0 19); do
        printf '{"k":%d,"v":"%d-%d"}' "$j" "$i" "$j"
        if [[ $j -lt 19 ]]; then printf ','; fi
      done
      printf ']}'
      ;;
    large)
      # Add an array of 200 simple objects
      printf '{"i":%d,"a":1,"b":2,"s":"x%d","items":[' "$i" "$i"
      local j
      for j in $(seq 0 199); do
        printf '{"k":%d,"v":"%d-%d","arr":[1,2,3,4,5]}' "$j" "$i" "$j"
        if [[ $j -lt 199 ]]; then printf ','; fi
      done
      printf ']}'
      ;;
  esac
}

echo "Generating $NUM JSON files in $OUT (size=$SIZE) ..." >&2
for i in $(seq 1 "$NUM"); do
  payload_for "$i" > "$OUT/$(printf "%06d" "$i").json"
done

echo "Done: $(ls -1 "$OUT"/*.json 2>/dev/null | wc -l | tr -d ' ') files" >&2
