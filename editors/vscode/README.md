# JSLT for VS Code

Syntax highlighting plus a client for the JSLT language server (`crates/lsp`).

## Trying it out

Open the **repository root** in VS Code and press <kbd>F5</kbd>. The
`Run JSLT VS Code extension` launch configuration builds the server binary,
compiles the extension, and opens `examples/lsp-sample` in an Extension
Development Host. See that directory's README for a walkthrough of what to
exercise.

Reload the host window (<kbd>Cmd/Ctrl</kbd>+<kbd>R</kbd>) after rebuilding the
server to pick up a new binary.

## Building by hand

```bash
bun install          # or: npm install

bun run build:server # cargo build -p jslt-lsp, copies into bin/
bun run compile      # tsc -p ./
```

`bun run build:server` produces a debug binary; `bash scripts/build.sh release`
produces an optimized one.

## Settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `jslt.serverPath` | `""` | Path to a `jslt-lsp` executable. Falls back to the binary bundled in `bin/`. |
| `jslt.trace.server` | `off` | LSP message tracing. Set to `verbose` to see traffic in the *JSLT Language Server* output channel. |

The server logs at `RUST_LOG=info`. For per-request latency, point
`jslt.serverPath` at a binary you launch yourself with `RUST_LOG=jslt_lsp=debug`.
