Here’s a lean repo layout and a brutally practical TODO that gets you from zero → usable Rust JSLT with room for LSP/FFI later—without drowning in deps.

# Repo structure (workspace)

```
jslt-rs/
├─ Cargo.toml                 # workspace + shared dev-deps
├─ rust-toolchain.toml        # pin stable toolchain
├─ .rustfmt.toml              # formatting
├─ .clippy.toml               # lints (pedantic off; deny unwrap in lib)
├─ LICENSE
├─ README.md
├─ docs/
│  ├─ spec.md                 # the EBNF spec (our canvas doc)
│  └─ stdlib.md               # stdlib signatures & behavior
├─ conformance/
│  ├─ cases/                  # test fixtures (json program + input + expected)
│  │  ├─ 000_minimal.json
│  │  └─ ...
│  └─ REF_RUN.md              # how to run Java JSLT to generate expected outputs
├─ crates/
│  ├─ ast/                    # AST types + spans + pretty-printer
│  ├─ lexer/                  # hand-rolled lexer, no deps
│  ├─ parser/                 # Pratt parser → AST
│  ├─ value/                  # JsltValue facade (wraps serde_json::Value)
│  ├─ interp/                 # evaluator + env + binding/linking
│  ├─ stdlib/                 # built-ins registry + implementations
│  ├─ engine/                 # public API: compile/apply, errors; depends on ↑
│  ├─ cli/                    # tiny CLI (read program, read json, print result)
│  ├─ capi/                   # C ABI + cbindgen config (optional feature)
│  ├─ node/                   # napi-rs bindings (optional)
│  ├─ python/                 # pyo3 bindings (optional)
│  ├─ wasm/                   # wasm-bindgen wrapper (optional)
│  └─ lsp/                    # tower-lsp server (optional)
└─ xtask/                     # optional: dev automation (codegen headers, pack)
```

## Workspace `Cargo.toml` (sketch)

```toml
[workspace]
members = [
  "crates/ast", "crates/lexer", "crates/parser", "crates/value",
  "crates/interp", "crates/stdlib", "crates/engine", "crates/cli",
  # optional bindings:
  # "crates/capi", "crates/node", "crates/python", "crates/wasm", "crates/lsp",
  # "xtask",
]
resolver = "2"

[workspace.dependencies]
serde = { version = "1", features = ["derive"] }
serde_json = "1"
thiserror = "1"            # tiny, improves error ergonomics (can drop if you want)
regex = { version = "1", optional = true }  # gate under feature
# dev-only
criterion = { version = "0.5", optional = true }
proptest = { version = "1", optional = true }

[workspace.lints.rust]
unsafe_code = "forbid"
```

## Crate responsibilities (one-liners)

* `ast`: Lossless AST with `Span { byte_off, line, col }` + `Display`/pretty-print.
* `lexer`: UTF-8 scanner; emits tokens, handles `.` vs `.5` lookahead; comments/whitespace.
* `parser`: Pratt parser; produces AST; no eval logic.
* `value`: `JsltValue` newtype over `serde_json::Value`, plus utilities (borrowed string views later).
* `interp`: name resolution (let/def), environments, evaluator, eval budget/recursion guard.
* `stdlib`: `JsltFunction` trait + registry; string/array/object/regex/time (regex/time behind features).
* `engine`: public API: `compile(&str) -> Program`, `Program::apply(&self, &JsltValue) -> Result<JsltValue>`.
* `cli`: `jslt` binary: `jslt -p program.jslt -i input.json`.
* `capi`/`node`/`python`/`wasm`: thin wrappers over `engine`.
* `lsp`: language server using `parser` and `engine` for diagnostics/completions.

# Feature flags (keep base tiny)

* In `engine`:

  * default features: `["serde_json"]` (i.e., **no regex/time by default**)
  * `regex` → pulls in `regex` and enables regex stdlib fns
  * `time` → adds datetime funcs (off by default)
  * `ffi-c`, `ffi-node`, `ffi-python`, `wasm`, `lsp` → opt-in
  * `bench`, `fuzz` → dev-only helpers

---

# Step-by-step TODO (with acceptance criteria)

## 0) Bootstrap

* [x] Initialize workspace, add crates, set licenses, CI skeleton (fmt + clippy + unit tests).

  * **Accept:** `cargo check -p engine` and `cargo test` succeed; `README` explains build.

## 1) Tokens & Lexer (no deps)

* [x] Define token enum (keywords, identifiers, numbers, strings, punctuators).
* [x] Implement scanner with:

  * JSON escapes `\n`, `\uXXXX` (reject invalid)
  * comments `// ...`
* [x] Emit spans for every token.

  * **Accept:** golden tests in `crates/lexer/tests/` for tricky lexemes (`.`, `.5`, quoted member keys).

## 2) AST

* [x] Define AST per spec (Expr variants; `Let`, `Def`, `If`, comprehensions, calls, member, index/slice).
* [x] Add `Span` to each node; implement `fmt::Display` for pretty-print.

  * **Accept:** round-trip pretty-print → parse → pretty-print is stable (ignoring whitespace).

## 3) Parser (Pratt)

* [x] Implement precedence table from spec.
* [x] Grammar features:

  * members with quoted keys, calls, index/slice
  * `if (...) a else b` expression
  * `let` with **semicolon** separators
  * comprehensions `[for (seq) expr [if cond]]` and `{for (...) key : value [if cond]}`
* [x] Good error messages: unexpected token, unterminated string, etc.

  * **Accept:** parse a corpus in `conformance/cases` without panics, with useful errors on broken inputs.

## 4) Binder / Name resolution

* [x] Walk AST to resolve variables (`$x`) and functions (`foo`) to indices/handles.
* [x] Environments: lexical scope stacks; capture closure values for `def`.

  * **Accept:** compile reports unknown names with nearest suggestions (optional Levenshtein later).

## 5) Import statements

* [ ] Implement module import system:
  * `import "module.jslt" as name` — imports module and binds to namespace prefix.
  * `import "module.jslt" as func` — imports module with final expression as callable function.
  * Module resolution (initially from classpath/filesystem).
  * Cyclic import detection.
  * Import statements must appear before variable/function declarations.
  * Imported functions accessed via `prefix:function-name(...)`.
  * Imported modules can import other modules.

  * **Accept:** can import and use functions from external modules; cyclic imports are rejected with clear error; modules without final expression work as namespaces; modules with final expression work as functions.

## 6) Value model

* [x] `JsltValue(serde_json::Value)` facade with helpers:

  * numeric ops (`as_f64_checked`), deep equality, object key order-insensitive eq.
  * null helpers, array/string index & slice with negative indices and bounds → `null`.
  * **Accept:** unit tests for index/slice semantics.

## 7) Evaluator (interp)

* [x] Expression evaluation, short-circuit `and`/`or`, truthiness rules (start strict; adjust later if ref impl differs).
* [x] Null propagation for member/index/slice per spec.
* [x] Evaluation budget + recursion depth guard (config on `Program::apply`).

  * **Accept:** run simple programs end to end.

## 8) Stdlib v1 (minimal)

* [ ] Functions (from `docs/stdlib.md`) — check off as implemented in `crates/stdlib/src/lib.rs`:

  - General
    - [x] contains(element, sequence)
    - [x] size(sequence)
    - [x] error(message)
    - [x] fallback(arg1, arg2, ...)
    - [x] min(a, b)
    - [x] max(a, b)

  - Numeric
    - [x] is-number(value)
    - [x] is-integer(value)
    - [x] is-decimal(value)
    - [x] number(value, fallback?)
    - [x] round(x)
    - [x] floor(x)
    - [x] ceiling(x)
    - [x] random()
    - [x] sum(array)
    - [x] mod(a, d)
    - [x] hash-int(value)

  - String
    - [x] is-string(value)
    - [x] string(value)
    - [x] test(input, regexp)
    - [x] capture(input, regexp)
    - [x] split(input, regexp)
    - [x] join(array, separator)
    - [x] lowercase(string)
    - [x] uppercase(string)
    - [x] sha256-hex(value)
    - [x] starts-with(tested, prefix)
    - [x] ends-with(tested, suffix)
    - [x] from-json(string, fallback?)
    - [x] to-json(value)
    - [x] replace(value, regexp, out)
    - [x] trim(value)
    - [x] uuid(msb?, lsb?)

  - Boolean
    - [x] boolean(value)
    - [x] not(value)
    - [x] is-boolean(value)

  - Object
    - [x] is-object(value)
    - [x] get-key(object, key, fallback?)

  - Array
    - [x] array(value)
    - [x] is-array(value)
    - [x] flatten(array)
    - [x] all(array)
    - [x] any(array)
    - [x] zip(array1, array2)
    - [x] zip-with-index(array)
    - [x] index-of(array, value)

  - Time
    - [x] now()
    - [ ] parse-time(time, format, fallback?)
    - [ ] format-time(timestamp, format, timezone?)

  - URL
    - [ ] parse-url(url)

* [x] Regex-gated (feature = "regex"): implement using `regex` crate where applicable
  - [x] test
  - [x] capture
  - [x] replace

  * **Accept:** conformance tests for each function; wrong arity → runtime error with span.

## 9) Conformance harness

* [x] Define fixture format (JSON keeps deps minimal):

  ```json
  { "program": "def f(x) x+1\nf(.a)",
    "input": { "a": 1 },
    "expected": 2 }
  ```
* [x] Test runner in `engine/tests/conformance.rs` that loads each fixture and runs compile+apply.
* [x] Differential test script (optional dev tool) that:

  * runs Java JSLT on the same fixtures to regenerate `expected`.
  * stores outputs next to cases (so we can see deltas).
  * **Accept:** N canonical cases pass; any failing case is either spec OPEN or a true bug.

## 10) CLI

* [ ] `jslt -p program.jslt -i input.json` → prints result JSON.
* [ ] `--eval 'expr'` shortcut; `--pretty`.

  * **Accept:** used in README examples; handles stdin/stdout.

## 11) Error polish

* [ ] One error type with parse/type/runtime variants; include spans; nice `Display`.
* [ ] Show a code frame on CLI errors (point to span).

  * **Accept:** user-friendly messages; no panics for user mistakes.

## 12) Performance checks (lightweight)

* [ ] Add a handful of `criterion` benches (compile hot path, map-like comprehension).
* [ ] Avoid cloning large values; use `Rc` where helpful.

  * **Accept:** compile+eval of medium templates stays within sane allocations.

## 13) C ABI (optional but recommended for interop)

* [ ] `capi` crate exposing:

  ```c
  typedef struct jslt_prog jslt_prog_t;
  jslt_prog_t* jslt_compile(const char* src, char** err_json);
  int jslt_apply(jslt_prog_t*, const char* input_json, char** output_json, char** err_json);
  void jslt_free(void*);
  ```
* [ ] `cbindgen` to generate `include/jslt.h`.

  * **Accept:** tiny C example compiles and runs.

## 14) Node/Python bindings (optional)

* [ ] `node`: napi-rs; export `compile()` returning an opaque handle + `apply(handle, value)`.
* [ ] `python`: pyo3; expose `Program` class.

  * **Accept:** smoke tests calling both from their runtimes.

## 15) WASM (optional)

* [x] `wasm`: `wasm-bindgen`; export `compile/apply` with JS `Any` ↔ `JsltValue` conversions via `serde-wasm-bindgen`.

  * **Accept:** browser demo in `examples/wasm/`.

## 16) LSP (optional but nice)

* [ ] `lsp`: tower-lsp; features:

  * parse-on-type diagnostics
  * completion (built-ins + in-scope names)
  * hover (types + doc from stdlib.md)
  * format (pretty-printer)
  * **Accept:** works in VS Code via simple client config.

## 17) Docs & release hygiene

* [ ] Fill `docs/stdlib.md` with signatures and semantics.
* [ ] Update `docs/spec.md` to remove OPENs as we confirm via diff tests.
* [ ] README: quickstart, API examples, CLI usage, feature flags.
* [ ] Tag `v0.1.0` crates; publish `engine` and `cli` (bindings optional).

## 18) Add extension functions (optional)

* [ ] Add the possibility for users to define their own functions.
  * [ ] Make sure this works as with original JSLT language.
* [ ] `wasm` bindings should be updated to support this.
* [ ] `lsp` should be updated to support this.
---

## Minimal dev tooling

* **CI:** GitHub Actions with matrix (ubuntu/mac/windows), steps: `fmt`, `clippy`, `test`.
* **xtask (optional):** `cargo xtask gen-headers`, `cargo xtask regen-fixtures`, `cargo xtask bench`.

---


