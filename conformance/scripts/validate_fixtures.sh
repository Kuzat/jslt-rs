#!/bin/bash

# Script to validate that all test fixture files follow the correct JSON schema
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CASES_DIR="$SCRIPT_DIR/../cases"

echo "Validating test fixture files..."

error_count=0
total_count=0

for case_file in "$CASES_DIR"/*.json; do
    if [ ! -f "$case_file" ]; then
        continue
    fi
    
    total_count=$((total_count + 1))
    filename=$(basename "$case_file")
    echo "Validating: $filename"
    
    # Check if file is valid JSON
    if ! jq . "$case_file" > /dev/null 2>&1; then
        echo "  ❌ Invalid JSON syntax"
        error_count=$((error_count + 1))
        continue
    fi
    
    # Check required fields
    if ! jq -e '.name' "$case_file" > /dev/null; then
        echo "  ❌ Missing required field: name"
        error_count=$((error_count + 1))
    fi
    
    if ! jq -e '.program' "$case_file" > /dev/null; then
        echo "  ❌ Missing required field: program"
        error_count=$((error_count + 1))
    fi
    
    if ! jq -e '.input' "$case_file" > /dev/null; then
        echo "  ❌ Missing required field: input"
        error_count=$((error_count + 1))
    fi
    
    if ! jq 'has("expected")' "$case_file" | grep -q true; then
        echo "  ❌ Missing required field: expected"
        error_count=$((error_count + 1))
    fi
    
    # Check field types
    if ! jq -e '.name | type == "string"' "$case_file" > /dev/null; then
        echo "  ❌ Field 'name' must be a string"
        error_count=$((error_count + 1))
    fi
    
    if ! jq -e '.program | type == "string"' "$case_file" > /dev/null; then
        echo "  ❌ Field 'program' must be a string"
        error_count=$((error_count + 1))
    fi
    
    # Optional description field type check
    if jq -e '.description' "$case_file" > /dev/null; then
        if ! jq -e '.description | type == "string"' "$case_file" > /dev/null; then
            echo "  ❌ Field 'description' must be a string"
            error_count=$((error_count + 1))
        fi
    fi
    
    if [ $error_count -eq 0 ]; then
        echo "  ✅ Valid"
    fi
done

echo ""
echo "=== VALIDATION SUMMARY ==="
echo "Total files: $total_count"
echo "Errors: $error_count"

if [ $error_count -eq 0 ]; then
    echo "🎉 All test fixtures are valid!"
    exit 0
else
    echo "❌ Some test fixtures have errors. Please fix them before continuing."
    exit 1
fi