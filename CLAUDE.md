# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JSLT-RS is a Rust implementation of JSLT (JSON Selection and Transformation Language). The project includes a full parser, binder, evaluator, comprehensive stdlib (45+ built-in functions), CLI tool with formatting support, WASM bindings with browser demo, and a Language Server Protocol implementation with formatting capability.

## Build and Test Commands

### Building

```bash
# Check all crates for compilation errors
cargo check --workspace

# Build all crates
cargo build --workspace

# Build specific crate
cargo build -p engine
cargo build -p cli --release

# Run CLI tool
cargo run -p cli -- <args>
```

### Testing

```bash
# Run all tests
cargo test --workspace

# Run tests for specific crate
cargo test -p engine
cargo test -p parser

# Run a single test
cargo test --test test_name

# Run conformance tests (comparing against Java JSLT reference)
cargo test -p engine --test conformance_tests

# Run differential tests (requires Java JSLT setup)
cargo test -p engine --test conformance_tests -- --ignored
```

### Benchmarking

```bash
# Run criterion benchmarks for engine crate
cargo bench -p engine

# Run benchmark suite comparing Rust vs Java implementation
./conformance/scripts/bench_suite.sh

# Run quick benchmark suite
./conformance/scripts/bench_suite.sh --quick

# Benchmark specific transformation
./conformance/scripts/bench_transform.sh -p examples/foo.jslt -i examples/input.json

# Benchmark the queens case
./conformance/scripts/bench_queens.sh
```

### Linting and Formatting

```bash
# Format Rust code
cargo fmt --all

# Run clippy lints
cargo clippy --workspace

# Fix clippy warnings automatically
cargo clippy --workspace --fix

# Format JSLT code
jslt format <file.jslt>
jslt format --write <file.jslt>  # Format in-place
jslt format --check <file.jslt>  # Check if formatted (CI mode)
```

### WASM Development

```bash
# Build WASM bundle
wasm-pack build crates/wasm --release --target web --out-dir ../../examples/wasm/pkg

# Serve WASM demo locally
npx http-server examples/wasm -p 8080
# or
python3 -m http.server -d examples/wasm 8080
```

## Architecture

### Workspace Structure

This is a Rust workspace with multiple crates organized by responsibility:

**Core Pipeline:**
- `crates/lexer/` → `crates/parser/` → `crates/ast/` → `crates/interp/` → `crates/engine/`

**Data Model:**
- `crates/value/` - `JsltValue` wrapper around `serde_json::Value` with JSLT-specific semantics

**Standard Library:**
- `crates/stdlib/` - Built-in function registry and 45+ function implementations (string, numeric, array, object, boolean, time, regex, URL)

**User-Facing:**
- `crates/cli/` - Command-line interface with `run` and `format` subcommands
- `crates/formatter/` - JSLT code formatter with configurable style
- `crates/wasm/` - Browser WASM bindings (deployed to GitHub Pages)
- `crates/lsp/` - Language Server Protocol implementation with formatting support

**Testing Infrastructure:**
- `conformance/` - Differential testing against Java JSLT reference implementation
- `conformance/cases/` - JSON test fixtures (program + input + expected output)
- `conformance/scripts/` - Test automation and benchmarking scripts

### Processing Pipeline

1. **Lexing** (`lexer`): Source text → Token stream with spans
2. **Parsing** (`parser`): Token stream → AST (using Pratt parser for operator precedence)
3. **Binding** (`interp::binder`): AST → BoundProgram (resolves names, handles imports, creates closures)
4. **Evaluation** (`interp`): BoundProgram + input JSON → output JSON (with environment stack, budget checking, recursion guards)
5. **Engine API** (`engine`): Public compile/apply interface that orchestrates the above

### Key Architectural Patterns

**Module System:**
- Modules can be imported with `import "path.jslt" as name`
- Module resolution supports cyclic import detection
- Modules without final expression act as namespaces
- Modules with final expression can be called as functions
- Imported functions accessed via `prefix:function-name(...)`

**Name Resolution:**
- Two-pass binding: first pass collects all definitions, second pass resolves references
- Variables (`$x`) and functions (`foo`) resolved to indices/handles
- Lexical scoping with environment stacks
- Closures capture values for `def` functions

**Value Model:**
- All values are `JsltValue` wrapping `serde_json::Value`
- Null propagation: member/index access on null returns null (no errors)
- Negative array indices supported (e.g., `[-1]` is last element)
- Out-of-bounds access returns null

**Evaluation Safety:**
- Budget tracking: configurable max steps (default 100M)
- Recursion depth limits: configurable max call depth (default 10K)
- Short-circuit evaluation for `and`/`or`
- Non-finite number detection (NaN/Inf)

## Conformance Testing

The project uses differential testing against the official Java JSLT implementation:

- Test fixtures are JSON files in `conformance/cases/` with format: `{"name": "...", "program": "...", "input": {...}, "expected": ...}`
- Java JSLT reference is in `conformance/java-jslt/` (git submodule)
- Scripts in `conformance/scripts/` automate testing and benchmarking
- Run `./conformance/scripts/regenerate_expected.sh` to update expected outputs from Java JSLT
- Requires Java 11-17 (not Java 21+ due to Gradle compatibility)

## Development Workflow

### Adding New Features

1. Start with AST modifications in `crates/ast/` if new syntax is needed
2. Update lexer (`crates/lexer/`) for new tokens
3. Update parser (`crates/parser/`) using Pratt parser pattern for expressions
4. Update binder (`crates/interp/binder.rs`) for name resolution
5. Implement evaluation logic in `crates/interp/` evaluator
6. Add tests in the relevant crate and add conformance test cases
7. Update stdlib if adding built-in functions

### Adding Standard Library Functions

1. Add function signature to `crates/stdlib/src/lib.rs`
2. Implement in appropriate module (e.g., `string.rs`, `array.rs`)
3. Register in `Registry::new()` constructor
4. Add conformance test cases
5. Document in `docs/stdlib.md`

### Working with Spans

All AST nodes and errors carry `Span { byte_off, line, col }` for accurate error reporting. Preserve spans when constructing AST nodes.

## Common Patterns

### Error Handling

- Parse errors: `ParseError` with span and kind
- Bind errors: `BindErrors` collecting multiple issues
- Runtime errors: `RuntimeError` with span and typed variants (TypeError, DivByZero, ArityMismatch, etc.)
- Engine errors: `EngineError` wrapping all error types

### Testing

- Unit tests in `#[cfg(test)] mod tests` within each crate
- Integration tests in `crates/*/tests/`
- Conformance tests compare against Java JSLT expected outputs
- Benchmarks use criterion in `crates/engine/benches/`

### CLI Examples

```bash
# Transform with program file
cargo run -p cli -- -p examples/files/programs/add-a-b.jslt -i examples/files/inputs/a-b-numbers.json

# Inline expression
echo '{"name": "alice"}' | cargo run -p cli -- -e '.name'

# Pretty-print output
cargo run -p cli -- -e '{"sum": .a + .b}' -i input.json --pretty

# Format JSLT code
jslt format program.jslt
jslt format --write program.jslt  # Format in-place
jslt format --check program.jslt  # Check if formatted
```

## Formatter

The formatter (`crates/formatter/`) provides consistent code formatting for JSLT programs.

### Formatter Architecture

```
crates/formatter/
├── src/
│   ├── lib.rs           # Public API (format, format_with_config)
│   ├── config.rs        # Configuration types and .jsltfmt loading
│   ├── writer.rs        # Output buffer with indentation tracking
│   ├── format_program.rs # Top-level program formatting
│   ├── format_expr.rs   # Expression formatting
│   ├── format_stmt.rs   # Statement formatting (def, let, import)
│   └── format_trivia.rs # Comment formatting (stub - not yet implemented)
└── tests/
    └── format_tests.rs  # 30+ comprehensive tests
```

### Configuration

Create a `.jsltfmt` file in your project root:

```toml
indent_width = 2
max_width = 100
indent_style = "spaces"  # or "tabs"
trailing_comma = "never"  # or "always"
```

The formatter searches for `.jsltfmt` in the current directory and parent directories.

### Formatter Features

- **Idempotent**: Running the formatter multiple times produces the same output
- **Smart line breaking**: Arrays and objects break to multi-line when exceeding `max_width`
- **Configurable indentation**: Spaces or tabs, with configurable width
- **LSP integration**: Format Document command works in VS Code with JSLT LSP
- **Comment preservation**: Infrastructure ready (trivia in AST), but parser doesn't collect comments yet

### Usage in Code

```rust
use formatter::{format_source, format_source_with_config, FormatConfig};

// Format with default config
let formatted = format_source("def  foo(x,y)    x+y")?;

// Format with custom config
let config = FormatConfig {
    indent_width: 4,
    max_width: 80,
    ..Default::default()
};
let formatted = format_source_with_config(source, config)?;
```

## Important Notes

- The workspace forbids `unsafe` code (workspace lint)
- Regex and time functions are always enabled (not feature-gated in current implementation)
- Parser uses Pratt parsing for operator precedence
- The project maintains compatibility with Java JSLT reference implementation
- WASM demo is deployed to GitHub Pages at https://kuzat.github.io/jslt-rs/
