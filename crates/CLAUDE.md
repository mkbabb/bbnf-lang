# CLAUDE.md — crates/

Cargo workspace containing the core BBNF compiler and language server.

## Structure

```
crates/
├── Cargo.toml          Workspace manifest (resolver = "2")
├── core/               bbnf — grammar parsing, IR lowering, multi-target codegen
├── ir/                 bbnf-ir — canonical grammar IR, CSP passes, bytecode compiler, VM, grammar-tier e-graph
├── egraph/             egraph — general-purpose e-graph substrate (shared by bbnf-ir + bbnf-regex)
├── egraph-derive/      egraph-derive — #[derive(Language)] proc-macro
├── ser/                bbnf-ser — Serializer/Deserializer traits (leaf crate)
├── derive/             bbnf-derive — #[derive(Parser)] proc-macro; delegates to `bbnf::pipeline`
├── analysis/           bbnf-analysis — LSP analysis engine (DocumentState, 14 feature providers)
├── gorgeous/           gorgeous — grammar-driven formatters (workspace member)
├── bootstrap/          bootstrap — self-hosting grammar tooling
└── lsp/                bbnf-lsp — Language server binary (+ DAP adapter)
```

## Build

```bash
cargo build --workspace                    # Debug build, all crates
cargo build --release -p bbnf-lsp          # Release LSP binary
cargo test --workspace                     # All tests
cargo clippy --all-targets -- -D warnings  # Lint (CI enforces this)
```

## Dependencies

- **parse_that**: External path dep (via `.cargo/config.toml` patch) — parser combinator library, re-exports `bbnf-regex`
- **pprint**: External path dep — pretty printing (derives `Pretty`)
- **csp-solver**: External path dep — generalized CSP/COP substrate used by IR passes and the e-graph
- **egraph**: Workspace crate — general-purpose e-graph substrate (`EGraph`, `Language`, `Rewrite`, `CostModel`, `CostWeights`, `BackoffScheduler`)
- **egraph-derive**: Workspace crate — `#[derive(Language)]` proc-macro
- **bbnf-ser**: Workspace crate — Serializer/Deserializer traits, no bbnf deps
- **bbnf-ir**: Workspace crate — canonical grammar IR, CSP passes, bytecode VM, grammar-tier e-graph
- **bbnf-analysis**: Workspace crate — LSP analysis engine, feature providers
- **indexmap**: Insertion-order HashMap (AST rule ordering)
- **syn/quote/proc-macro2**: Rust code generation
- **tower-lsp-server**: LSP protocol (lsp crate only)
- **tokio**: Async runtime (lsp crate only)
- **self_cell**: Self-referential structs for AST caching (bbnf-analysis crate)
- **ls-types**: LSP type definitions (bbnf-analysis crate)

## Architecture

```
.bbnf file
  → BBNFGrammar bootstrap parser (core/src/grammar/)
  → AST: ParsedGrammar (IndexMap<Expression, Expression> + imports + directives)
  → lower/: AST → bbnf-ir GrammarIR
  → ir/src/passes/: SCC, FIRST/FOLLOW, dispatch tables, e-graph-based regex rewriting,
                    CSP type projection, DAG build, pattern recognition
  → core/src/backend/: target-agnostic Emitter trait + per-target emitters
      ├── driver/    shared compilation driver (NodeId-keyed decisions)
      ├── patterns/  pre-solved pattern detection (delim_scan, key_dispatch)
      ├── strategy/  CSP-solved emission strategies
      ├── prettify/  @pretty analysis + plan
      ├── rust/      Rust backend (→ #[derive(Parser)] via bbnf-derive)
      ├── ts/        TypeScript backend
      └── wasm/      WASM backend
```

The LSP reuses the parser, IR pipeline, and analysis through `bbnf-analysis`,
but does its own diagnostic generation and feature dispatch. `bbnf-derive`
delegates to `bbnf::pipeline::compile_paths_request` so the proc-macro and
library paths share one pass ordering.

## Conventions

- Edition 2024, nightly toolchain.
- Borrowed lifetimes (`'a`) throughout AST types.
- `Box::leak()` for import module graphs (arena-style ownership).
- Fixed-point iteration for FIRST sets; Tarjan's algorithm for SCC.
- `ParsedGrammar.recovers: Vec<RecoverDirective>` — stores `@recover` directives alongside the rule map.
- `RecoverDirective { rule: &str, sync: Expression }` — sync expression is any parsed BBNF `Expression`.
- `unescape_literal()` in `backend/util.rs` (and per-target `backend/{ts,wasm}/helpers.rs`) — converts raw BBNF escape sequences (`\n`, `\t`, etc.) to actual chars before emission.
- `Expression::Minus` emits `.minus()` (parse-that set-difference), not `.not()` (negative lookahead)
- Sub-variant coercion — heterogeneous alternation branches generate anonymous enum variants (e.g., `factor_4`). Global type-matching lookup handles the two-pass codegen architecture.
