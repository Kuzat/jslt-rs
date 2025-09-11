#!/bin/bash

# Script to regenerate expected outputs by running Java JSLT on all test cases
# This is useful when updating test fixtures or syncing with upstream Java JSLT

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CASES_DIR="$SCRIPT_DIR/../cases"
JAVA_JSLT_SCRIPT="$SCRIPT_DIR/run_java_jslt.sh"

echo "Regenerating expected outputs for all test cases..."

updated_count=0
for case_file in "$CASES_DIR"/*.json; do
    if [ ! -f "$case_file" ]; then
        continue
    fi
    
    echo "Processing: $(basename "$case_file")"
    
    # Extract program and input from test case
    program=$(jq -r '.program' "$case_file")
    input=$(jq -c '.input' "$case_file")
    
    # Create temporary files
    temp_dir=$(mktemp -d)
    program_file="$temp_dir/program.jslt"
    input_file="$temp_dir/input.json"
    
    echo "$program" > "$program_file"
    echo "$input" > "$input_file"
    
    # Run Java JSLT to get expected output
    if java_output=$("$JAVA_JSLT_SCRIPT" "$program_file" "$input_file" 2>/dev/null); then
        # Update the test case file with new expected output
        jq --argjson expected "$java_output" '.expected = $expected' "$case_file" > "$temp_dir/updated.json"
        mv "$temp_dir/updated.json" "$case_file"
        updated_count=$((updated_count + 1))
        echo "  ✅ Updated"
    else
        echo "  ❌ Failed to run Java JSLT"
    fi
    
    # Cleanup
    rm -rf "$temp_dir"
done

echo ""
echo "✅ Regeneration complete. Updated $updated_count test cases."
echo "💡 Don't forget to review and commit the changes!"