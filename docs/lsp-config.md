# JSLT LSP Config

You can configure module resolution roots for the language server by placing either
`jslt-lsp.toml` or `.jslt-lsp.toml` in your project (the server searches upward from
the currently opened file).

## Supported keys

Top-level:

```toml
module_root = "src/main/resources"
module_roots = ["src/main/resources", "shared/jslt"]
```

Nested under `imports`:

```toml
[imports]
root = "src/main/resources"
roots = ["src/main/resources", "shared/jslt"]
```

Relative paths are resolved from the directory containing the config file.

The LSP also keeps the opened file's own directory as a fallback resolution root.
