# JSLT language server sample workspace

A small multi-module workspace for exercising the language server by hand. It is
the fixture the VS Code launch configuration opens (`.vscode/launch.json` at the
repo root), but any LSP client pointed at this directory works.

```
main.jslt              entry point; imports both modules
modules/strings.jslt   slug, shout
modules/numbers.jslt   clamp, percent
broken.jslt            deliberately broken, for diagnostics
input.json             sample input for the CLI
```

The same program runs under the CLI, so you can check that the transformation is
actually correct and not just that the editor is happy:

```bash
cargo run -p cli -- -p examples/lsp-sample/main.jslt -i examples/lsp-sample/input.json --pretty
```

Note that a module used as a namespace must not end in a final expression: a
module with a body is callable as `str(...)` rather than exporting `str:slug`.

## What to try

1. **Diagnostics** — open `broken.jslt`. The missing import and the unclosed
   object both report. Fix them and the squiggles clear.
2. **Formatting** — Format Document on `main.jslt`. Running it twice changes
   nothing the second time.
3. **Completion** — inside the object in `main.jslt`, type `str:` and then `.`
   and `$` in a value position.
4. **Hover** — hover `str` in an import, `num:clamp` in a call, and a stdlib
   function like `round` inside `modules/numbers.jslt`.
5. **Signature help** — start typing `num:clamp(` and watch the active parameter
   advance as you add commas.
6. **Go to definition** — on `slug` in `str:slug` (jumps into the module), then
   on the `str` half (opens the module file), then on a local `$name`.
7. **Find references** — on `def slug` in `modules/strings.jslt`. Call sites in
   `main.jslt` are listed alongside the local ones.
8. **Rename** — rename that `slug`. Both files update, and only the function
   half of `str:slug` changes. Then rename the `str` alias: only the alias
   identifier moves, never the module path.
9. **Unsaved edits** — change `modules/strings.jslt` without saving and re-run
   Find References. Results reflect the buffer, not the file on disk.

Module resolution roots are configurable if you would rather import bare module
names; see [docs/lsp-config.md](../../docs/lsp-config.md).
