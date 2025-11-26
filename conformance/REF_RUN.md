# JSLT Reference Implementation Differential Testing

This directory contains the infrastructure for comparing our Rust JSLT implementation against the official Java JSLT reference implementation to ensure 100% compatibility.

## Directory Structure

```
conformance/
├── cases/                    # JSON test fixture files
│   ├── 001_identity.json     # Identity transformation test
│   ├── 002_member_access.json # Basic member access test  
│   └── ...                   # More test cases
├── java-jslt/               # Java JSLT reference (git submodule)
├── scripts/                 # Test automation scripts
│   ├── run_java_jslt.sh     # Wrapper to build and run Java JSLT
│   ├── test_java_jslt.sh    # Test Java JSLT setup
│   ├── regenerate_expected.sh # Update expected outputs
│   ├── validate_fixtures.sh  # Validate test fixture format
│   └── sync_java_tests.sh   # Sync with upstream Java repo
└── REF_RUN.md              # This documentation
```

## Test Fixture Format

Each test case is a JSON file with the following structure:

```json
{
  "name": "test_name",
  "description": "Optional description of what this test validates",
  "program": ".member",
  "input": {"member": "value", "other": 42},
  "expected": "value"
}
```

### Required Fields
- `name`: Unique identifier for the test case
- `program`: The JSLT transformation program
- `input`: JSON input to transform
- `expected`: Expected output from the transformation

### Optional Fields  
- `description`: Human-readable description of the test

## Running Tests

### Prerequisites

1. **Java 11-17**: Required to build and run the Java JSLT reference implementation
   - **Note**: Java 21+ may not work due to Gradle 7.4.1 compatibility issues
   - Recommended: Java 17 for best compatibility
   - Install with `brew install openjdk@17` (macOS) or `sudo apt install openjdk-17-jdk` (Ubuntu)
2. **Rust**: Required to run our Rust implementation
3. **jq**: Required for test fixture validation and maintenance scripts
4. Fetch the `java-jslt` git submodule
   5. `git submodule update --init`

### Local Testing

```bash
# Test Java JSLT setup
./conformance/scripts/test_java_jslt.sh

# Validate test fixture format
./conformance/scripts/validate_fixtures.sh

# Run differential tests (requires working Rust JSLT implementation and Java JSLT setup)
cargo test -p engine --test conformance_tests -- --ignored

# Run just Rust JSLT tests (no Java and Java JSLT submodule needed)
cargo test -p engine --test conformance_tests

# Update expected outputs from Java JSLT
./conformance/scripts/regenerate_expected.sh
```

## Performance Benchmarks

This repo includes lightweight performance checks to compare the Rust implementation against the Java reference.

### Rust micro-benchmarks (Criterion)

We use `criterion` benches under `crates/engine/benches/` to measure hot paths:

- `compile_bench.rs` — measures parse+bind compile time for small programs and the queens program.
- `eval_bench.rs` — measures `apply()` cost for a compiled program (queens and a large identity case).
- `comprehension_bench.rs` — synthetic map-like comprehension over arrays of different sizes.

Run them with:

```bash
cargo bench -p engine

# Or list without executing to confirm they build
cargo bench -p engine --no-run
```

Notes:
- Benches reference the `conformance/cases/208_queens.json` fixture for a realistic heavy program.
- Results are printed to stdout and Criterion's report directory (`target/criterion`).

### CLI end-to-end comparisons: Rust vs Java

In `conformance/scripts/` there are helper scripts that compare the Rust CLI (`jslt`) with the Java reference CLI. They prefer `hyperfine` when available, and fall back to `/usr/bin/time`.

Prereqs:
- Rust toolchain (stable)
- Java 11–17 (for Java JSLT)
- `hyperfine` (recommended) and `jq` (optional but recommended)

Scripts:

1. `bench_transform.sh`
   - Compare one transform end-to-end (compile + apply) for both Rust and Java.
   - Usage examples:
     ```bash
     # Program file + input file
     ./conformance/scripts/bench_transform.sh -p examples/foo.jslt -i examples/input.json

     # Inline expression + input file
     ./conformance/scripts/bench_transform.sh -e '.name' -i examples/input.json
     ```

2. `bench_queens.sh`
   - Convenience wrapper to run the queens case from `conformance/cases/208_queens.json`:
     ```bash
     ./conformance/scripts/bench_queens.sh
     ```
   - You can point it to a different fixture: `--case conformance/cases/XXX.json`.

3. `bench_batch.sh`
   - Measures throughput over many JSON input files in a directory for a given program.
   - Usage:
     ```bash
     # Using a program file
     ./conformance/scripts/bench_batch.sh -p examples/foo.jslt -d path/to/inputs

     # Using an inline expression
     ./conformance/scripts/bench_batch.sh -e '.data | size(.)' -d path/to/inputs
     ```

4. `bench_suite.sh`
   - One-command automated suite that runs several scenarios and produces a single consolidated Markdown report comparing Rust vs Java.
   - Usage:
     ```bash
     # Quick mode (smaller datasets, fewer runs)
     ./conformance/scripts/bench_suite.sh --quick

     # Full mode with custom output directory
     ./conformance/scripts/bench_suite.sh --out ./target/my-bench-$(date +%s)
     ```
   - Output: a timestamped directory under `target/bench-suite-*/` containing per-scenario Markdown files and a combined `benchmark_suite_*.md` report.
   - Scenarios covered:
     - Queens heavy program from `conformance/cases/208_queens.json`
     - Micro single-transform cases (identity, small object arithmetic, small comprehension)
     - Batch throughput across generated directories of JSON files (uses `gen_json_dir.sh`)

Under the hood these scripts:
- Build the Rust CLI in release mode: `cargo build -p cli --release`.
- Build and run Java via `conformance/scripts/run_java_jslt.sh` (which builds the fat jar if needed).

### What to look for

- Compile time: `compile_bench` measures parser+binder; useful for LSP-like workloads.
- Evaluation time: `eval_bench` and CLI scripts measure runtime apply cost.
- Throughput: `bench_batch.sh` approximates a real-life scenario with one compiled program processing many inputs.

### CI/CD Testing

The GitHub Actions workflow automatically:

1. Checks out the repository with git submodules
2. Sets up Java 11 and Rust stable
3. Builds the Java JSLT reference implementation
4. Validates all test fixture files
5. Tests the Java JSLT setup
6. Runs standard Rust tests (formatting, clippy, unit tests)
7. Runs differential tests (currently allowed to fail)

## Maintenance

### Adding New Test Cases

1. Create a new JSON file in `conformance/cases/` following the naming convention `NNN_description.json`
2. Use the test fixture format described above
3. Run `./conformance/scripts/validate_fixtures.sh` to ensure the format is correct
4. Run `./conformance/scripts/regenerate_expected.sh` to generate expected output from Java JSLT

### Syncing with Upstream

```bash
# Pull latest changes from Java JSLT repository
./conformance/scripts/sync_java_tests.sh

# After syncing, consider regenerating expected outputs
./conformance/scripts/regenerate_expected.sh
```

### Updating Expected Outputs

If the Java JSLT reference implementation behavior changes or you fix a bug in a test case:

```bash
# Regenerate all expected outputs from current Java JSLT
./conformance/scripts/regenerate_expected.sh

# Review the changes
git diff conformance/cases/

# Commit if the changes are correct
git add conformance/cases/
git commit -m "Update expected outputs from Java JSLT"
```

## Java JSLT Reference Implementation

The Java reference implementation is included as a git submodule pointing to `https://github.com/schibsted/jslt`. 

### Manual Usage

```bash
# Build the Java implementation
cd conformance/java-jslt
./gradlew jar

# Run a transformation
echo '{"name": "John"}' | java -cp build/libs/jslt.jar com.schibsted.spt.data.jslt.cli.JSLT program.jslt input.json
```

### Wrapper Script Usage

The `run_java_jslt.sh` script handles building and provides a simpler interface:

```bash
# Create test files
echo ".name" > program.jslt
echo '{"name": "John", "age": 30}' > input.json

# Run transformation
./conformance/scripts/run_java_jslt.sh program.jslt input.json
# Output: "John"
```

## Test Categories

Our conformance test suite covers:

### Core Language Features
- Identity transformation (`.`)
- Member access (`.field`, `.["field"]`)
- Array indexing (`.[0]`, `.[-1]`)
- Null propagation for missing members
- Nested access (`.user.name`)

### Control Flow
- Conditional expressions (`if-then-else`)
- Let bindings (`let $x = .field`)
- Function definitions (`def func(param)`)

### Standard Library Functions
- Type conversion: `string()`, `number()`, `boolean()`
- String functions: `starts-with()`, `ends-with()`, `contains()`, `split()`, `join()`
- Array/Object functions: `size()`, `keys()`, `values()`, `get()`
- Math functions: arithmetic operators, comparisons

### Advanced Features
- Array comprehensions (`[for .items expr]`)
- Object comprehensions (`{for .items key: value}`)
- Function calls with multiple arguments
- Complex nested transformations

### Error Handling
- Invalid syntax handling
- Runtime errors (type mismatches, division by zero)
- Null input handling
- Unicode string processing

## Implementation Status

- ✅ **Infrastructure**: Complete test harness and CI integration
- 🚧 **Basic Tests**: 5 test cases covering core functionality  
- ⏳ **Rust Implementation**: Pending - currently returns placeholder values
- ⏳ **Comprehensive Coverage**: Need ~50-100 more test cases
- ⏳ **Advanced Features**: Comprehensions, functions, etc.

## Contributing

When adding new test cases:

1. Follow the JSON fixture format exactly
2. Use descriptive names and include descriptions
3. Start with simple cases and build complexity gradually
4. Validate fixtures before committing
5. Test against Java JSLT to ensure expected outputs are correct

The goal is to have comprehensive coverage of the JSLT language specification to ensure our Rust implementation is fully compatible with the reference Java implementation.