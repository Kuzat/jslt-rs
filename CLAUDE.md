# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

JSLT-RS is a Rust implementation of the JSLT (JSON Selection and Transformation Language) specification. This is a workspace-based Rust project implementing a complete JSLT interpreter with multiple crates for different components.

## Architecture

This is a multi-crate Rust workspace with the following structure:

### Core Crates
- `crates/ast/` - AST types with spans and pretty-printer
- `crates/lexer/` - Hand-rolled lexer, no dependencies
- `crates/parser/` - Pratt parser that produces AST
- `crates/engine/` - Public API for compile/apply operations
- `crates/cli/` - Command-line interface

### Additional Planned Crates
- `crates/value/` - JsltValue facade over serde_json::Value
- `crates/interp/` - Evaluator with environments and binding/linking
- `crates/stdlib/` - Built-in functions registry and implementations
- Optional: `crates/capi/`, `crates/node/`, `crates/python/`, `crates/wasm/`, `crates/lsp/`

## Development Commands

### Basic Commands
- `cargo check` - Check for compilation errors
- `cargo check --workspace` - Check all workspace crates
- `cargo build` - Build the project
- `cargo build --workspace` - Build all workspace crates
- `cargo test` - Run tests
- `cargo test --workspace` - Run tests for all workspace crates

### Package-Specific Commands
- `cargo check -p lexer` - Check specific crate
- `cargo test -p lexer` - Test specific crate
- `cargo build -p cli` - Build specific crate

### Running the CLI
- `cargo run -p cli -- <args>` - Run the CLI tool
- `cargo run -- <args>` - Run main binary (from src/main.rs)

## Key Implementation Details

### Language Specification
- Full EBNF grammar defined in `docs/spec.md`
- JSLT operates on JSON-like values (null, boolean, number, string, array, object)
- Supports expressions, declarations (`def`, `let`), comprehensions, and built-in functions
- Null propagation for member access and indexing

### Design Principles
- Minimal dependencies (serde, serde_json, thiserror)
- Feature flags for optional functionality (regex, time, FFI bindings)
- Hand-rolled lexer for precise `.` vs `.5` disambiguation
- Pratt parser for precedence handling
- Workspace structure for clean separation of concerns

### Current Status
The project appears to be in early development stage with basic workspace structure set up but minimal implementation completed. The TODO.md contains a comprehensive implementation plan with 16 major phases from tokens/lexer through to documentation and release.

## Testing Strategy
- Golden file tests in `conformance/cases/` directory (planned)
- Differential testing against Java JSLT reference implementation
- Unit tests for each crate component
- Property-based testing recommended for evaluator

## Key Files to Reference
- `docs/spec.md` - Complete JSLT language specification
- `TODO.md` - Detailed implementation roadmap and architecture decisions
- `Cargo.toml` - Workspace configuration