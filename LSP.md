# JSLT LSP Roadmap (Full-Featured)

## Goal

Deliver a modern, full-featured JSLT language server with the same baseline developer experience users expect from
mature LSP servers (navigation, editing intelligence, refactoring, semantic highlighting, and workspace-aware tooling).

## Current Baseline (as of 2026-02-18)

- Implemented: push diagnostics (`didOpen`/`didChange`), import-not-found diagnostics, binder/parser diagnostics, full
  document formatting.
- Not yet implemented: completion, hover, signature help, go-to-definition, references, rename, symbols, semantic
  tokens, code actions, inlay hints, folding, selection ranges, call hierarchy, workspace search.

## Design Principles

- Be parser-error tolerant: features should still work in partially typed files.
- Prefer one shared analysis pipeline over per-feature custom logic.
- Keep behavior deterministic and low-latency (<50ms typical single-file operations).
- Align capability flags with real behavior and progressive feature rollout.
- Add protocol conformance tests per feature before editor-integration tests.

## Foundation Work (must land first)

### F0: Analysis Snapshot + Symbol Table

- Priority: P0
- User value: enables all language-intelligence features to be consistent.
- Protocol surface: internal only.
- Implementation details:
- Build a reusable `AnalysisSnapshot` for a document: parsed AST, best-effort binder result, diagnostics, symbol table,
  scope tree, import list.
- Track symbol kinds: function, parameter, let variable, import alias, module path.
- Store byte-span and LSP range for both declaration and references.
- Acceptance criteria:
- All features consume the same snapshot object.
- Snapshot build is test-covered for valid and invalid source.

### F1: Workspace Index + Module Graph

- Priority: P0
- User value: cross-file navigation, rename, references, and workspace symbols.
- Protocol surface: internal only.
- Implementation details:
- Maintain workspace file index keyed by URI.
- Build import graph from `import` headers and configured module roots.
- Cache per-file analysis with invalidation on change and file ops.
- Acceptance criteria:
- Can resolve symbols across imported JSLT modules.
- Re-index is incremental (changed file + impacted dependents only).

## Feature Backlog

### 1) Context-Aware Completion

- Priority: P0
- LSP methods/capabilities: `textDocument/completion`, `completionItem/resolve`, `completionProvider` with trigger
  chars (`.`, `$`, `"`, `:`).
- Scope:
- Keywords/snippets (`def`, `let`, `if/else`, `for`, object/array comprehensions).
- In-scope identifiers (params, lets, function names).
- Built-in stdlib functions with arity and short docs.
- Imported module aliases and qualified function names.
- Implementation details:
- Use scope tree from analysis snapshot.
- Add `CompletionItemKind`, detail text, insert text format (snippets where useful).
- Use lazy doc loading via `completionItem/resolve`.
- Acceptance criteria:
- Correctly ranks local symbols above globals/builtins.
- Works in incomplete code and inside function-call contexts.

### 2) Hover Documentation

- Priority: P0
- LSP methods/capabilities: `textDocument/hover`, `hoverProvider`.
- Scope:
- Show declaration signature for local defs/lets/params.
- Show stdlib function docs (name, arity, summary, examples).
- Show import/module information for aliases.
- Implementation details:
- Add symbol lookup by cursor position.
- For stdlib docs, generate metadata table from `stdlib` registry and optionally `docs/stdlib.md`.
- Acceptance criteria:
- Hover on any symbol resolves to useful info with no crashes on syntax errors.

### 3) Signature Help

- Priority: P0
- LSP methods/capabilities: `textDocument/signatureHelp`, `signatureHelpProvider` with trigger chars (`(`, `,`).
- Scope:
- Active parameter highlighting for function calls.
- Works for user-defined functions and stdlib functions.
- Implementation details:
- Locate nearest call expression around cursor.
- Compute active argument index from commas/parens in tolerant parse.
- Acceptance criteria:
- Active parameter index is correct in nested calls.

### 4) Go To Definition

- Priority: P0
- LSP methods/capabilities: `textDocument/definition`, `definitionProvider`.
- Scope:
- Jump to `def`, `let`, parameter, and import alias definition.
- Cross-file jump to imported module files and imported functions.
- Implementation details:
- Resolve symbol at cursor from snapshot.
- Use workspace index for cross-file symbols.
- Acceptance criteria:
- Definition jumps are stable and deterministic for same input.

### 5) Find References

- Priority: P0
- LSP methods/capabilities: `textDocument/references`, `referencesProvider`.
- Scope:
- Find all usages of functions, lets, params, and import aliases.
- Include or exclude declaration per request context.
- Implementation details:
- Use symbol IDs (not text matching) across index.
- Acceptance criteria:
- Results are accurate for shadowed names and nested scopes.

### 6) Rename Symbol

- Priority: P0
- LSP methods/capabilities: `textDocument/rename`, `textDocument/prepareRename`, `renameProvider`.
- Scope:
- Safe rename for defs/lets/params/import aliases across workspace.
- Block rename for builtins/keywords and invalid locations.
- Implementation details:
- Resolve canonical symbol ID.
- Emit `WorkspaceEdit` with all changed ranges.
- Acceptance criteria:
- No accidental edits to unrelated shadowed symbols.
- `prepareRename` validates target and rename range.

### 7) Document Symbols (Outline)

- Priority: P1
- LSP methods/capabilities: `textDocument/documentSymbol`, `documentSymbolProvider`.
- Scope:
- Show imports, defs, top-level lets, major object keys.
- Implementation details:
- Build hierarchical `DocumentSymbol` tree from AST.
- Acceptance criteria:
- Outline updates correctly after edits.

### 8) Workspace Symbols

- Priority: P1
- LSP methods/capabilities: `workspace/symbol`, `workspaceSymbolProvider`.
- Scope:
- Search defs/lets/modules across workspace.
- Implementation details:
- Query workspace index; rank by exact/prefix/fuzzy match.
- Acceptance criteria:
- Fast response on medium projects.

### 9) Semantic Tokens

- Priority: P1
- LSP methods/capabilities: `textDocument/semanticTokens/full`, optionally `/range`.
- Scope:
- Tokenize keywords, variables, parameters, functions, strings, numbers, operators, namespaces (imports).
- Implementation details:
- Derive token stream from AST + binder for semantic categories.
- Respect LSP relative encoding rules.
- Acceptance criteria:
- Stable token output and correct delta behavior if delta mode is later added.

### 10) Code Actions (Quick Fixes)

- Priority: P1
- LSP methods/capabilities: `textDocument/codeAction`, optionally `codeAction/resolve`.
- Scope:
- Fix unknown function/variable using binder suggestions.
- Add missing import for unresolved module path (where resolvable).
- Organize imports (sort/group/remove unused aliases).
- Implementation details:
- Map diagnostics codes to action producers.
- Use minimal edits and preserve formatting where possible.
- Acceptance criteria:
- Quick fixes are only shown when safe and relevant.

### 11) Document Highlights

- Priority: P1
- LSP methods/capabilities: `textDocument/documentHighlight`, `documentHighlightProvider`.
- Scope:
- Highlight declaration + references in current file for symbol under cursor.
- Acceptance criteria:
- Works for shadowed scopes without bleed-through.

### 12) Folding Ranges

- Priority: P1
- LSP methods/capabilities: `textDocument/foldingRange`, `foldingRangeProvider`.
- Scope:
- Fold imports block, function bodies, object/array literals, if/else blocks, comments.
- Acceptance criteria:
- Folding regions are syntactically meaningful and non-overlapping.

### 13) Selection Ranges

- Priority: P2
- LSP methods/capabilities: `textDocument/selectionRange`, `selectionRangeProvider`.
- Scope:
- Expand selection expression -> parent expression -> statement/block -> full document.
- Acceptance criteria:
- Predictable parent chain for nested expressions.

### 14) Range + On-Type Formatting

- Priority: P1
- LSP methods/capabilities: `textDocument/rangeFormatting`, `textDocument/onTypeFormatting`.
- Scope:
- Format selected range and smart newline/brace formatting while typing.
- Implementation details:
- Reuse existing formatter with range mapping fallback to full-file when required.
- Acceptance criteria:
- Produces stable edits and no destructive reflow surprises.

### 15) Inlay Hints

- Priority: P2
- LSP methods/capabilities: `textDocument/inlayHint`, optionally `inlayHint/resolve`.
- Scope:
- Optional hints for inferred callable targets, parameter names for long call sites, and captured variables.
- Acceptance criteria:
- Hints are concise, low-noise, and configurable.

### 16) Call Hierarchy

- Priority: P2
- LSP methods/capabilities: `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`,
  `callHierarchy/outgoingCalls`.
- Scope:
- Show callers/callees for `def` functions across workspace.
- Acceptance criteria:
- Graph is correct for imported functions and local functions.

### 17) Document Links for Imports

- Priority: P2
- LSP methods/capabilities: `textDocument/documentLink`, optional `/resolve`.
- Scope:
- Clickable import path links that open module files.
- Acceptance criteria:
- Links resolve with configured `module_roots` and fallback root.

### 18) File Operation Awareness

- Priority: P2
- LSP methods/capabilities: `workspace/didRenameFiles`, `workspace/didCreateFiles`, `workspace/didDeleteFiles`.
- Scope:
- Refresh index and optionally offer import-path update code actions after file moves.
- Acceptance criteria:
- No stale cross-file navigation after rename/move operations.

### 19) Improved Diagnostics (Static Analysis)

- Priority: P1
- LSP methods/capabilities: existing push diagnostics.
- Scope:
- Add warnings/hints: unused `let`, unused `def`, unreachable `if` branch (constant condition), duplicate object keys.
- Add `relatedInformation` for cross-file import errors.
- Implementation details:
- Extend binder + AST passes with severity mapping and stable diagnostic codes.
- Acceptance criteria:
- New diagnostics are accurate and low false-positive.

### 20) Workspace Configuration + Dynamic Settings

- Priority: P1
- LSP methods/capabilities: `workspace/didChangeConfiguration`, `workspace/configuration`.
- Scope:
- Client-configurable settings: enable/disable diagnostics groups, inlay hints, semantic tokens, completion snippets,
  module roots overrides.
- Acceptance criteria:
- Settings changes apply without restart.

## Suggested Delivery Milestones

### Milestone A (Core IDE, must-have)

- F0, F1
- Completion
- Hover
- Signature help
- Go to definition
- Find references
- Rename

### Milestone B (Daily-driver parity)

- Document symbols
- Workspace symbols
- Semantic tokens
- Code actions (quick fixes + organize imports)
- Document highlight
- Range/on-type formatting
- Improved diagnostics
- Dynamic settings

### Milestone C (Advanced polish)

- Folding ranges
- Selection ranges
- Inlay hints
- Call hierarchy
- Document links
- File-operation awareness

## Testing Strategy

- Unit tests for symbol extraction, scope resolution, reference maps, rename edit sets.
- Protocol tests for each LSP endpoint (request/response fixtures).
- Golden tests for semantic tokens and code actions.
- Workspace integration tests with multi-file import graphs and rename scenarios.
- Performance checks on large synthetic workspace (latency and memory thresholds).

## Observability and Quality Gates

- Add tracing spans per request type and analysis phase.
- Track per-feature latency metrics and cache hit rates.
- Add panic-safety guards so feature handlers return partial/empty results instead of failing server-wide.

## Definition of Done for “Full LSP”

- All Milestone A and B features shipped and enabled by default.
- Milestone C features implemented or intentionally documented as opt-in.
- Cross-file navigation and rename are reliable in real multi-module projects.
- No major editor (Neovim, VS Code, JetBrains LSP client) compatibility blockers.

## Milestone A Implementation Checklist (PR Order + Crate Tasks)

### Planning assumptions

- Team can land 1 PR at a time on mainline.
- Keep each PR vertically useful and testable.
- Prefer internal refactors before adding new protocol handlers.

### PR sequence overview

| PR | Outcome | Main crates | Size estimate |
| --- | --- | --- | --- |
| 1 | LSP architecture refactor + request context plumbing | `crates/lsp` | M (2-3 days) |
| 2 | F0: Analysis snapshot + symbol table | `crates/lsp`, `crates/ast` | L (3-5 days) |
| 3 | Completion v1 (scopes + stdlib + imports) | `crates/lsp`, `crates/stdlib` | M (2-4 days) |
| 4 | Hover + signature help | `crates/lsp`, `crates/stdlib` | M (2-3 days) |
| 5 | Go-to-definition (single-file first, then cross-file hooks) | `crates/lsp` | M (2-3 days) |
| 6 | F1: Workspace index + module graph | `crates/lsp`, `crates/engine` | L (3-5 days) |
| 7 | References (workspace-aware) | `crates/lsp` | M (2-4 days) |
| 8 | Rename + prepareRename (workspace edits) | `crates/lsp` | M (2-4 days) |
| 9 | Stabilization: perf, conformance fixtures, editor sanity checks | `crates/lsp` | M (2-3 days) |

### PR 1 checklist: LSP architecture refactor

- `crates/lsp`:
- Split `src/lib.rs` into modules: `state.rs`, `analysis.rs`, `handlers/*.rs`, `convert.rs`.
- Introduce shared request context (`document`, `snapshot`, `workspace view`) for handlers.
- Add capability builder utility so feature flags are explicit and testable.
- Add internal error type for non-fatal handler failures (return empty/partial responses).
- Verification:
- `cargo test -p jslt-lsp`
- No behavior regressions in existing diagnostics/formatting tests.

### PR 2 checklist: F0 analysis snapshot + symbol table

- `crates/lsp`:
- Implement `AnalysisSnapshot` with: AST parse result, binder diagnostics, symbol table, scope tree, call-site index, import header index.
- Define stable `SymbolId` and `SymbolKind` enums (function, let, param, import alias, module path).
- Build cursor-position lookup API (`symbol_at`, `scope_at`, `call_at`).
- Add snapshot cache keyed by URI + version.
- `crates/ast`:
- Add minimal AST walking helpers if needed to avoid duplicating traversal logic in LSP.
- Verification:
- Unit tests for scope nesting, symbol shadowing, and tolerant parsing scenarios.
- `cargo test -p jslt-lsp`

### PR 3 checklist: Completion v1

- `crates/lsp`:
- Add `textDocument/completion` handler with context-aware candidate sources:
- local scope symbols from snapshot,
- import aliases and qualified names,
- keywords/snippets.
- Add completion ranking heuristics (local > imported > stdlib > keywords).
- Add `completionItem/resolve` for lazy docs/details.
- `crates/stdlib`:
- Expose iterable builtin metadata API (name + arity string + short summary seed).
- Verification:
- Completion fixture tests at cursor markers for top-level, function body, call args, member/object contexts.
- `cargo test -p jslt-lsp`

### PR 4 checklist: Hover + signature help

- `crates/lsp`:
- Add `textDocument/hover` for local symbols, imports, and stdlib entries.
- Add `textDocument/signatureHelp` with active-parameter computation for nested calls.
- Reuse snapshot call-site index to avoid per-request reparsing.
- `crates/stdlib`:
- Provide canonical signature text for each builtin for hover/signature output.
- Verification:
- Tests for hover content shape and signature active param correctness.
- `cargo test -p jslt-lsp`

### PR 5 checklist: Go-to-definition

- `crates/lsp`:
- Add `textDocument/definition` handler.
- Implement local definition resolution for params/lets/defs/import aliases via `SymbolId`.
- Add cross-file resolution hook points that delegate to workspace index once PR 6 lands.
- Verification:
- Single-file definition tests for shadowing and aliasing.
- `cargo test -p jslt-lsp`

### PR 6 checklist: F1 workspace index + module graph

- `crates/lsp`:
- Implement workspace file discovery (JSLT files) and URI-to-snapshot index.
- Build import graph using parsed import headers + module root config resolution.
- Add incremental invalidation on `didChange` and file operations.
- Implement cross-file symbol lookup APIs for definition/references/rename.
- `crates/engine` (optional helper):
- Expose or reuse module path normalization helper to keep index resolution consistent with compiler semantics.
- Verification:
- Integration tests with multi-file fixtures and imported module chains.
- `cargo test -p jslt-lsp`

### PR 7 checklist: References

- `crates/lsp`:
- Add `textDocument/references` handler using canonical `SymbolId`.
- Support include-declaration toggle.
- Ensure shadowed names do not cross-match.
- Verification:
- Multi-file and shadowing-heavy tests.
- `cargo test -p jslt-lsp`

### PR 8 checklist: Rename

- `crates/lsp`:
- Add `textDocument/prepareRename` target validation and precise rename range.
- Add `textDocument/rename` to emit deterministic `WorkspaceEdit` across files.
- Guardrails: reject builtins/keywords and invalid symbol kinds.
- Verification:
- Rename tests for local, cross-file import alias, and function symbols.
- Edit-application tests to ensure syntactically valid output.
- `cargo test -p jslt-lsp`

### PR 9 checklist: Stabilization and release readiness

- `crates/lsp`:
- Add request latency tracing around completion/hover/definition/references/rename.
- Add simple performance regression harness (large synthetic file + medium workspace).
- Add editor smoke checklist (Neovim + VS Code baseline flows).
- Update docs with supported capabilities and known gaps.
- Verification:
- `cargo test -p jslt-lsp`
- Manual smoke run against at least one multi-module sample workspace.

### Cross-PR quality gates

- No panics on malformed/incomplete source.
- Handlers should return empty/partial results rather than transport errors where possible.
- Every new handler must include:
- Unit tests.
- At least one integration/fixture test.
- Capability flag wiring in `initialize`.
- Keep existing diagnostics + formatting behavior unchanged unless explicitly intended.
