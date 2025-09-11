#!/bin/bash

# Test script to validate Java JSLT setup with a simple case
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TEMP_DIR=$(mktemp -d)

# Create test files
echo "." > "$TEMP_DIR/identity.jslt"
echo '{"hello": "world"}' > "$TEMP_DIR/input.json"

echo "Testing Java JSLT with identity transformation..."
result=$("$SCRIPT_DIR/run_java_jslt.sh" "$TEMP_DIR/identity.jslt" "$TEMP_DIR/input.json")

echo "Result: $result"

# Cleanup
rm -rf "$TEMP_DIR"

echo "Java JSLT test completed successfully!"