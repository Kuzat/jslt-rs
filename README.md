# JSLT-RS

A Rust implementation of the JSLT (JSON Selection and Transformation Language) specification.

## Building

This is a workspace-based Rust project. To build:

```bash
# Check all crates for compilation errors
cargo check --workspace

# Build all crates
cargo build --workspace

# Run tests for all crates
cargo test --workspace

# Build specific crate
cargo build -p engine

# Run CLI tool
cargo run -p cli -- <args>
```

## Project Structure

This project is organized as a Rust workspace with the following crates:

- `crates/ast/` - AST types with spans and pretty-printer
- `crates/lexer/` - Hand-rolled lexer, no dependencies
- `crates/parser/` - Pratt parser that produces AST
- `crates/value/` - JsltValue facade over serde_json::Value
- `crates/interp/` - Evaluator with environments and binding/linking
- `crates/stdlib/` - Built-in functions registry and implementations
- `crates/engine/` - Public API for compile/apply operations
- `crates/cli/` - Command-line interface

## Development Status

This project is in early development. See `TODO.md` for the detailed implementation plan.

## License

MIT License - see [LICENSE](LICENSE) file for details.