#!/bin/bash

# Script to build and run Java JSLT reference implementation
# Usage: ./run_java_jslt.sh <jslt_program_file> <input_json_file>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAVA_JSLT_DIR="$SCRIPT_DIR/../java-jslt"
# Find the shadowJar file (it includes version number)
JAR_FILE="$JAVA_JSLT_DIR/core/build/libs/core-*-all.jar"

# Check arguments
if [ $# -ne 2 ]; then
    echo "Usage: $0 <jslt_program_file> <input_json_file>" >&2
    exit 1
fi

JSLT_PROGRAM="$1"
INPUT_JSON="$2"

# Check if files exist
if [ ! -f "$JSLT_PROGRAM" ]; then
    echo "Error: JSLT program file not found: $JSLT_PROGRAM" >&2
    exit 1
fi

if [ ! -f "$INPUT_JSON" ]; then
    echo "Error: Input JSON file not found: $INPUT_JSON" >&2
    exit 1
fi

# Build Java JSLT if jar doesn't exist or is older than source
# Use globbing to find the actual JAR file
JAR_ACTUAL=$(ls $JAR_FILE 2>/dev/null | head -n1 || echo "")
if [ -z "$JAR_ACTUAL" ] || [ "$JAVA_JSLT_DIR" -nt "$JAR_ACTUAL" ]; then
    echo "Building Java JSLT..." >&2
    cd "$JAVA_JSLT_DIR"
    
    # Try to build with current Java version
    if ! ./gradlew :core:shadowJar --quiet; then
        echo "Build failed. If you're using Java 21+, try switching to Java 17:" >&2
        echo "  sdk use java 17.0.16-amzn  # if using sdkman" >&2
        echo "  or set JAVA_HOME to a Java 11-17 installation" >&2
        exit 1
    fi
    
    cd - > /dev/null
    
    # Update JAR_ACTUAL after building
    JAR_ACTUAL=$(ls $JAR_FILE 2>/dev/null | head -n1 || echo "")
fi

# Run Java JSLT
if [ -z "$JAR_ACTUAL" ]; then
    echo "Error: Could not find built JAR file" >&2
    exit 1
fi

java -cp "$JAR_ACTUAL" com.schibsted.spt.data.jslt.cli.JSLT "$JSLT_PROGRAM" "$INPUT_JSON"