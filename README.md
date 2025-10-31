# JSLT-RS

A Rust implementation of the [JSLT](https://github.com/schibsted/jslt) (JSON Selection and Transformation Language) with a small engine API and a browser WASM demo.

**Current status:** Full parser, binder, and evaluator with comprehensive stdlib support. All core JSLT features are implemented, including expressions, functions, comprehensions, and 45+ built-in functions. A live WASM demo is available at [GitHub Pages](https://kuzat.github.io/jslt-rs/).

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

# Build CLI tool
cargo build -p cli --release

# Run CLI tool
cargo run -p cli -- <args>
```

## CLI Usage

The CLI tool allows you to transform JSON data using JSLT programs.

### Basic Usage

```bash
# Transform JSON using a JSLT program file and JSON input file
cargo run -p cli -- -p <program.jslt> -i <input.json>

# Use inline expressions with --eval
cargo run -p cli -- -e '.name' -i <input.json>

# Read input from stdin
echo '{"name": "alice"}' | cargo run -p cli -- -e '.name'

# Pretty-print output
cargo run -p cli -- -p <program.jslt> -i <input.json> --pretty
```

### CLI Options

- `-p, --program <FILE>` - Path to a JSLT program file (use `-` to read from stdin)
- `-e, --eval <EXPR>` - Inline JSLT expression to evaluate (conflicts with `--program`)
- `-i, --input <FILE>` - JSON input file (use `-` to read from stdin, or omit to read from stdin)
- `--pretty` - Pretty-print the output JSON

### Examples with Sample Files

The repository includes example programs and inputs in the `examples/files/` directory:

```
examples/files/
├── inputs/
│   ├── a-b-numbers.json    # {"a": 1, "b": 2}
│   └── empty.json          # {}
└── programs/
    ├── add-a-b.jslt        # {"sum": .a + .b}
    └── hello.jslt          # {"hello": "world"}
```

**Example 1: Add two numbers**
```bash
cargo run -p cli -- -p examples/files/programs/add-a-b.jslt -i examples/files/inputs/a-b-numbers.json
# Output: {"sum":3}
```

**Example 2: Simple hello world transformation**
```bash
cargo run -p cli -- -p examples/files/programs/hello.jslt -i examples/files/inputs/empty.json
# Output: {"hello":"world"}
```

**Example 3: Using inline expressions**
```bash
# Extract a field
echo '{"name": "alice", "age": 30}' | cargo run -p cli -- -e '.name'
# Output: "alice"

# Transform with object construction
echo '{"a": 2, "b": 3}' | cargo run -p cli -- -e '{"sum": .a + .b, "product": .a * .b}'
# Output: {"sum":5,"product":6}
```

**Example 4: Pretty-printed output**
```bash
cargo run -p cli -- -p examples/files/programs/add-a-b.jslt -i examples/files/inputs/a-b-numbers.json --pretty
# Output:
# {
#   "sum": 3
# }
```

## Browser/WASM Quickstart

The repository includes a WASM wrapper crate and a minimal browser demo.

### Prerequisites

- Rust stable and wasm32 target:
```shell script
rustup target add wasm32-unknown-unknown
```

- wasm-pack (builds the web-friendly JS/WASM bundle):
```shell script
cargo install wasm-pack
```

- A static HTTP server to host the demo directory (choose one):
  - Node (no install): npx http-server
  - Python 3: python3 -m http.server

### Build the WASM bundle

This produces a JS glue file and a .wasm module under examples/wasm/pkg/.

```shell script
# From the repo root
  wasm-pack build crates/wasm --release --target web --out-dir ../../examples/wasm/pkg
```


Notes:
- --target web outputs an ES module you can import from a <script type="module">.
- Re-run this command any time you change Rust code in the WASM wrapper or engine.

### Run the demo page

Serve the demo directory to avoid CORS issues and import the generated module.

```shell script
# From the repo root, serve the examples/wasm directory:
  npx http-server examples/wasm -p 8080
  # or
  python3 -m http.server -d examples/wasm 8080
```


Open http://localhost:8080 in your browser.

Try this example:
- Program:
```
{ "greet": .name }
```

- Input:
```json
{ "a": 2, "b": 3, "name": "alice" }
```


Expected output:
```json
{
    "greet": "alice"
  }
```


### Troubleshooting

- Seeing {} instead of your object?
  - Hard refresh to bust the browser cache so the fresh pkg/ is used.
  - Ensure you built with --target web and are serving over HTTP, not file://.
- “WASM not loaded” or module import errors:
  - Confirm the server is running from examples/wasm/ and pkg/ exists.
  - Check the browser console for network errors (404 for pkg/jslt_wasm.js or the .wasm file).

## Project Structure

This project is organized as a Rust workspace with the following crates and directories:

**Crates:**
- `crates/ast/` - AST types with spans and pretty-printer
- `crates/lexer/` - Hand-rolled lexer, no dependencies
- `crates/parser/` - Pratt parser that produces AST
- `crates/value/` - JsltValue facade over serde_json::Value
- `crates/interp/` - Evaluator with environments and binding/linking
- `crates/stdlib/` - Built-in functions registry and implementations (45+ functions)
- `crates/engine/` - Public API for compile/apply operations
- `crates/cli/` - Command-line interface
- `crates/wasm/` - wasm-bindgen wrapper for browser/WASM deployment

**Additional directories:**
- `examples/wasm/` - Browser demo with HTML/JS frontend (deployed to GitHub Pages)
- `conformance/` - Test cases and reference test harness
- `docs/` - Language specification and stdlib documentation

## Development Status

This project is in active development with most core features completed:

✅ **Completed:**
- Lexer, parser, and AST with full JSLT grammar support
- Expression evaluator with proper null propagation and short-circuiting
- Name resolution and binding for variables and functions
- All 45+ standard library functions (string, numeric, array, object, boolean, time, regex, URL)
- Conformance test suite with reference implementation comparison
- WASM bindings with live browser demo deployed to GitHub Pages
- CI/CD pipeline with automated builds and deployments
- Module import system design 
- CLI improvements (pretty printing, eval mode)

🚧 **In Progress:**
- Error message polish and code frame display
- Language Server Protocol (LSP) support

📋 **Planned:**
- Node.js, JVM, Python bindings
- Extension function system

See `TODO.md` for the detailed implementation plan.

## License

Apache License, Version 2.0 - see [LICENSE](LICENSE) file for details.