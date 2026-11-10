# Repository Guidelines

## Project Structure & Module Organization
- Core crates live under `crates/` (lexer, parser, ast, interp, engine, stdlib, value, cli, wasm, lsp). Tests often colocated in each crate’s `src/` with unit tests.
- Conformance fixtures and benchmark scripts are under `conformance/` (cases, scripts). Benchmarks read `conformance/cases/*.json` and scripts emit markdown reports.
- Top-level `examples/` contains sample programs; `docs/` holds reference material.

## Build, Test, and Development Commands
- Build all crates: `cargo build` (release: `cargo build --release`).
- Run unit tests workspace-wide: `cargo test`.
- Benchmark engine (local): `cargo bench -p engine --bench eval_bench`.
- CLI demo: `cargo run -p cli -- --help` (use `--program file.jslt -i input.json` or `--eval '.expr'`).
- Conformance bench suite: `conformance/scripts/bench_suite.sh` (writes markdown report path to stdout).

## Coding Style & Naming Conventions
- Rust 2021 edition; follow `rustfmt` defaults (4-space indents). Keep code ASCII unless required.
- Prefer expressive function/var names; modules snake_case; types CamelCase. Avoid unnecessary clones; favor borrowing.
- Add brief comments only when logic is non-obvious. Avoid long inline explanations.

## Testing Guidelines
- Unit tests live beside code (`#[cfg(test)]`); run with `cargo test`.
- Benchmarks use Criterion in `crates/engine/benches/*`; expect gnuplot fallback to plotters when missing.
- Conformance fixtures: `conformance/cases/*.json`; use bench scripts for performance comparisons, not for CI gating.

## Commit & Pull Request Guidelines
- Commit messages: short imperative summary (e.g., “Optimize evaluator closures”), optional body for rationale. Keep unrelated changes separate.
- PRs: describe intent, surface risks/regressions, and note how to reproduce/verify (commands run). Include links to related issues and benchmark snippets when relevant.

## Agent-Specific Tips
- Use `rg` for searches (`rg pattern crates/interp/src`); prefer `cargo fmt` only if style diverges.
- Avoid destructive git commands; don’t revert user changes.
- Bench scripts emit markdown reports—capture and share their paths in PR discussion. If running benches in CI, ensure hyperfine/jq/Java are available.
