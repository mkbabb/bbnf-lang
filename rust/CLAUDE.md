# CLAUDE.md — rust/

Cargo workspace containing the core BBNF compiler and language server.

## Structure

```
rust/
├── Cargo.toml          Workspace manifest (resolver = "2")
├── bbnf/               Core library: grammar parsing, analysis, Rust codegen
├── bbnf-derive/        Proc-macro crate: #[derive(Parser)]
└── lsp/                Language server binary (bbnf-lsp)
```

## Build

```bash
cargo build --workspace                    # Debug build, all crates
cargo build --release -p bbnf-lsp          # Release LSP binary
cargo test --workspace                     # All tests
cargo clippy --all-targets -- -D warnings  # Lint (CI enforces this)
```

## Dependencies

- **parse_that**: Local path dep — parser combinator library
- **pprint**: Local path dep — pretty printing (derives `Pretty`)
- **indexmap**: Insertion-order HashMap (AST rule ordering)
- **syn/quote/proc-macro2**: Rust code generation
- **tower-lsp-server**: LSP protocol (lsp crate only)
- **tokio**: Async runtime (lsp crate only)
- **self_cell**: Self-referential structs for AST caching (lsp crate only)

## Architecture

```
.bbnf file
  → BBNFGrammar parser (bbnf/grammar.rs)
  → AST: IndexMap<Expression, Expression>
  → Analysis: SCC, FIRST sets, dispatch tables (bbnf/analysis.rs)
  → Codegen: Rust TokenStream via syn/quote (bbnf/generate.rs)
  → #[derive(Parser)] emits enum + parser methods (bbnf-derive/lib.rs)
```

The LSP reuses the parser and analysis from `bbnf/` but does its own
diagnostic generation and feature dispatch.

## Conventions

- Edition 2024, nightly toolchain.
- Borrowed lifetimes (`'a`) throughout AST types.
- `Box::leak()` for import module graphs (arena-style ownership).
- Fixed-point iteration for FIRST sets; Tarjan's algorithm for SCC.
