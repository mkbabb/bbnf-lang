# CLAUDE.md — bbnf-lang

Monorepo for the BBNF (Better Backus-Naur Form) grammar ecosystem.
BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

## Architecture state (post-Tranche H & I)

BBNF's optimizer follows a **four-layer separation**:
**e-graph** (equivalence generation + cost-guided extraction) /
**facts** (monotone legality propagation via `NodeFacts` /
`ContextFacts` / `RegexInfo`) / **CSP/COP** (choice under
constraints) / **backend** (emission-only code generation). Every
transformation belongs to exactly one layer — mixing is a design
smell.

The optimizer is **representation-separated** from the data model:
`IrNode` (compiler-centric) and `Hir` (regex-centric) stay
unconstrained by e-graph substrate needs. Parallel optimizer-
centric enums `GrammarENode` and `HirENode` live inside `egraph/`
modules, generated their `Language` impl via `#[derive(Language)]`
from `egraph-derive`, and are hand-maintained. Translation glue
between owning trees and parallel e-nodes (`build_egraph.rs`,
`write_back.rs`, `insert_hir`, `extract_hir`) is small and
localized.

The **grammar tier** (`crates/ir/src/egraph/`) and the
**bbnf-regex HIR tier**
(`parse-that/rust/regex/src/egraph/`) are **isomorphic**: same
`egraph` substrate crate, same `Rewrite` trait, same
`BackoffScheduler`, mirrored rule files, and shared `CostWeights`
in `egraph::cost_weights`. `GrammarCostModel` and
`RegexExtractionCost` embed the same weights struct so branch-
factoring and dispatch incentives stay in sync across tiers.
A new optimization lands as a mirrored rule pair — one file per
tier — or as a single tier if its domain is exclusive.

The **permanent grammar-tier regex layer**
(`crates/ir/src/egraph/rules/regex.rs`) owns `Alt`-level rewrites
across sibling branches: dedup, superset absorption, charclass
union, heterogeneous fusion. The **HIR tier** owns intra-pattern
simplification (flatten, dedup, superset, union, repetition
absorption) upstream of `RegexInfo::analyze_from_hir`, so every
downstream analysis (FIRST sets, nullable, width, DFA sizing)
sees canonicalized HIR with zero caller awareness. The legacy
destructive `simplify_regex_algebra` / `merge_regex_alts` passes
(802 LOC) were deleted in Tranche H-7 after parity with the
retained e-graph rules was proven across the full test suite.

The **durable DAG** (`crates/ir/src/dag/mod.rs`) is built exactly
once per compile at a single well-defined pipeline step
(`crates/core/src/pipeline/compile.rs:409`), enforced by a
`debug_assert!` and a `cargo test`-time grep invariant in
`crates/ir/tests/dag_invariant.rs`. Tests and benches that
exercise a single pass in isolation call the `bbnf_ir::dag::ensure_dag`
helper. `project_types` asserts `ir.dag.is_some()` at entry.
`GrammarDag::node_for`'s `HashMap<*const IrNode, NodeId>`
reverse-pointer map is correct by design (valid for the lifetime
of the borrowed `&GrammarIR`) and is NOT part of any residual
pointer-identity cleanup.

## Structure

```
bbnf-lang/                              Repo root = Cargo workspace root
├── Cargo.toml                          Workspace manifest
├── crates/                             Rust crates
│   ├── core/                           bbnf — grammar parser, IR lowering, codegen
│   │   └── src/
│   │       ├── backend/
│   │       │   ├── driver/             Parse codegen orchestration
│   │       │   ├── patterns/           Pattern detection (decisions, key_dispatch, delim_scan)
│   │       │   ├── emitter.rs          Emitter trait (target-agnostic)
│   │       │   ├── rust/               Rust backend emitter
│   │       │   ├── ts/                 TypeScript backend emitter
│   │       │   ├── wasm/              WASM backend emitter
│   │       │   └── prettify/           Prettify analysis + plan
│   │       ├── generate/
│   │       │   ├── serialize/          Grammar-guided serialization codegen
│   │       │   └── regex/              Regex → TokenStream codegen (DFA, HIR, generalized)
│   │       ├── lower/                  AST → IR lowering
│   │       └── pipeline/               Compilation orchestrator + directives + validation
│   ├── ir/                             bbnf-ir — canonical grammar IR, CSP passes, VM
│   ├── ser/                            bbnf-ser — Serializer/Deserializer traits
│   ├── derive/                         bbnf-derive — #[derive(Parser)] proc-macro
│   ├── analysis/                       bbnf-analysis — LSP analysis engine
│   ├── lsp/                            bbnf-lsp — language server binary
│   └── gorgeous/                       gorgeous — grammar-driven formatters
├── wasm/                               bbnf-wasm (standalone, not workspace member)
├── playground/                         Vue 3 + Monaco playground
├── extension/                          VS Code extension (LSP client)
├── grammar/                            Example grammars + language specification
├── docs/                               Documentation
├── data/                               Benchmark datasets
├── Makefile                            Build automation
└── package.json                        NPM workspaces (playground)
```

## Build Commands

```bash
make build          # Release LSP + extension
make dev            # Debug LSP + extension (fast iteration)
make test           # All Rust tests
make bench          # LSP performance benchmarks
make install        # Build, package .vsix, install into VS Code
make package        # Build + create bbnf-lang.vsix
make watch          # Continuous rebuild (cargo watch)
make clean          # Remove artifacts
```

## Manual Builds

```bash
# Rust (requires nightly) — from repo root
cargo test --workspace && cargo build --release -p bbnf-lsp

# Extension
cd extension && npm ci && npm run build

# WASM (builds into playground/src/wasm/)
cd wasm && wasm-pack build --target web --out-dir ../playground/src/wasm
```

## Development

- **F5 workflow**: `make dev` then F5 in VS Code
- **Install locally**: `make install` then reload VS Code
- **Integration tests**: `cargo test -p bbnf-lsp --test integration -- --nocapture` (from repo root)
- **LSP binary path resolution** (priority order):
  1. VS Code setting `BBNF.server.path`
  2. Environment variable `BBNF_SERVER_PATH`
  3. Bundled binary `extension/server/bbnf-lsp`

## Release

```bash
make bump-patch     # 1.0.x
make bump-minor     # 1.x.0
make bump-major     # x.0.0
make release        # Push tags → GitHub Actions builds 5 platforms → Marketplace
```

Platforms: linux-x64, linux-arm64, darwin-x64, darwin-arm64, win32-x64.
Requires `VSCE_PAT` secret in GitHub repo settings.

## Dependency Graph

```
bbnf-ser  ────────────────────────────────────── leaf crate (itoa, ryu)
   ↑
pprint  (impl Serializer for FmtBuilder)
   ↑
bbnf-regex  ──────────────────────────────────── leaf crate (smallvec)
   ↑        HIR, NFA/DFA, classify, algebra, charset, first
parse_that  (re-exports bbnf-regex)
   ↑
csp-solver ──→ bbnf-ir ──→ bbnf (core) ──→ bbnf-ser
                  ↑             ↑
              bbnf-derive ──────┘
                  ↑
              bbnf-analysis ──→ bbnf-lsp
                  ↑
              gorgeous (workspace member)
                  ↑
              bbnf-wasm (standalone, wasm-pack → playground)
```

**Leaf crates** (`bbnf-ser`, `bbnf-regex`) have zero bbnf dependencies.
Generated parser code references `::bbnf_ser::Serializer` directly.
External deps (`parse_that`, `pprint`, `csp-solver`) resolved via `.cargo/config.toml` patches for local dev.

## Conventions

- **Rust**: Nightly toolchain, edition 2024. Clippy with `-D warnings`.
- **Extension**: esbuild, CommonJS output (Node.js), `vscode` external.
- **Grammars**: `.bbnf` extension. `@import` for composition. `@recover` for error recovery. `;` terminators.
- **Crate deps**: `parse_that` and `pprint` from crates.io; local dev via `.cargo/config.toml` `[patch.crates-io]`.
- **Lifetimes**: Borrowed `'a` throughout Rust AST; `Box::leak()` for import module graphs.
- **Import system**: Cyclic imports handled via partial-init before recursion. Selective imports expand transitive local deps automatically. `@import` directives can appear at any position in a file.
- **Recovery**: `@recover rule syncExpr ;` — per-rule annotation specifying a sync expression for multi-error parsing. Any valid BBNF expression (regex, alternation, concatenation, etc.) is valid as the sync. Emits `.recover(syncParser, null)` in TS codegen and a `Recovered` enum variant in Rust proc-macro codegen.
- **Analysis pipeline**: Tarjan SCC → topological sort → IR lowering → CSP FIRST sets (128-bit `CharSet128` from `bbnf-regex`) → dispatch tables. All fixed-point analyses go through the CSP solver.
- **Recursive SpanParser codegen**: `try_generate_span_parser()` handles all expression types (concat, alt, many, skip/next, minus, nonterminal refs). Iterative fixed-point loop on `sp_method_rules` — start empty, try generating for all eligible rules, add successes, repeat until convergence (2–3 iterations). Literal unescape via `unescape_literal()` + `proc_macro2::Literal::string()`.
- **Codegen**: Grammar → `lower/` (5 sub-modules: `mod.rs`, `string_interner.rs`, `fn_table.rs`, `expression.rs`, `metadata.rs`) → bbnf-ir `Module` → `codegen/` (single monolithic path + optional prettify) + `ir_enums.rs`/`ir_types.rs`. `pipeline.rs` orchestrates the full lowering + codegen sequence.
- **Vec unboxing**: `in_vec` parameter threading through codegen, `ir_node_to_tokens_vec`, `project_node_in_vec`. Transparent rule `_unboxed()` generation for zero-cost enum extraction.
- **`try_flatten_pair`**: Extension for `(BoxedEnum, Vec<Enum>)` patterns — flattens pair into unboxed Vec.
- **Regex algebra is e-graph-native**: The grammar-tier e-graph (`crates/ir/src/egraph/rules/regex.rs`) owns `Alt([Regex, ...])` fusion, dedup, superset absorption, and union merge via `DeduplicateAltBranches`, `SupersetAbsorbAlt`, `UnionMergeAlt`, `FuseAltRegexBranches`. The bbnf-regex HIR e-graph (`parse-that/rust/regex/src/egraph/rules/`) handles intra-pattern simplification (flatten, dedup, superset, union, repetition absorption) upstream of `RegexInfo::analyze_from_hir`. Both tiers share `CostWeights`. The destructive `merge_regex_alts` / `simplify_regex_algebra` passes were deleted in Tranche H-7.
- **Pipeline synchronization**: The IR pass ordering in `pipeline.rs` and `derive/src/lib.rs` must be kept in sync—both run the same 16-operation sequence (14 unique passes, including `sort_alt_branches`, `factor_regex_with_lookahead`, and `fuse_token_dispatch` after `compute_follow_sets`).
- **`fuse_single_use` pass**: Inlines single-use rules at their call site regardless of body size, guarded by SCC membership. Runs after `inline_acyclic` + prune, before `eliminate_epsilon`.
- **`fuse_token_dispatch` pass**: Fuses `@token`-marked rules at dispatch call sites—inlines the token body for direct matching while preserving the enum variant. Runs after `factor_regex_with_lookahead`, before `generate_dispatch_tables`.
- **`no_collapse` gating**: Rules annotated with `@no_collapse` are excluded from inlining and fusing passes to preserve their identity in the generated AST.
- **`emit_discarded` for Skip/Next**: Skip and Next codegen emits the discarded side for its side effects (e.g., whitespace consumption) even though the value is unused.
- **mimalloc in bench files**: Benchmark binaries use `#[global_allocator] static GLOBAL: mimalloc::MiMalloc` for consistent, high-performance allocation during benchmarking.
- **Prettify codegen**: `@pretty` directives control Doc emission. Hint vocabulary: `group`, `indent`, `dedent`, `block`, `blankline`, `nobreak`, `softbreak`, `hardbreak`, `compact`, `fast`, `off`. `generate_prettify()` produces `to_doc()` + `source_range()` impls. Sub-variant coercion for heterogeneous alternation branches. Shared hint definitions in `analysis/src/directives/hints.rs` — single source of truth for codegen + LSP.
- **Wrapped vec formatting**: Delimiter-wrapped repetitions (e.g. `"{" >> items << "}"`) emit IfBreak concat — one item per line when Group breaks, comma-separated inline when it fits.
- **`skip_recover`**: Parser attribute that suppresses `@recover` codegen and the `Recovered` enum variant. Used by formatting-only parsers that assume well-formed input.
- **Type comparison**: `types_eq()` compares `syn::Type` structurally via per-token-tree comparison—no string serialization.
- **Sub-variant validation**: `validate_sub_variant_uniqueness()` rejects cross-rule type collisions at compile time.
- **Monolithic codegen**: `codegen/` is the sole codegen path. Generates direct recursive functions with slab allocation—zero combinator overhead. `MonoCtx` tracks fusion eligibility, single-site inline eligibility, dispatch-guaranteed-byte, `current_rule_name`, and hoisted leaf-parser bindings. Unified `SepByConfig` + `emit_mono_sep_by_core` handles all three sep_by variants (bare, ws-aware, delimited-with-terminator). Optional `prettify/` sub-path emits fused parse+format for `#[parser(prettify)]`.
- **Span-only codegen**: `codegen/span/` (4 sub-modules: `mod.rs`, `alt.rs`, `expr.rs`, `repeat.rs`). Triggered by `#[parser(span)]`. Generates `fn __rule_span(state) -> Option<Span>` for every rule—zero allocations, no enum variants, no Vec.
- **Inline optional Span codegen**: Optional Span expressions (Literal/Regex with fast-path) in `codegen/repeat.rs` and `codegen/span/repeat.rs` emit inline byte checks instead of constructing a SpanParser per call.
- **Delimiter-driven flat scanning**: `codegen/delim_scan.rs`. Grammar-agnostic codegen optimization for `Wrap(Repeat(Alt))` patterns where the Alt's FIRST sets overlap. Detects single-byte "pivot" Literals that distinguish branches, emits a forward `memchr` scanner loop. Span path replaces the descent entirely; slab path uses speculative dispatch (scan selects branch, then calls existing recursive descent for typed construction). Pseudo-class guard: if value after pivot terminates at the open byte, falls back to block branch. When the pivot rule returns Span (`is_token` or `TypeDesc::Span`), constructs the result directly from scanner offsets, eliminating speculative rewind + re-parse.
- **`factor_regex_with_lookahead` pass**: `ir/src/passes/factor_lookahead.rs`. Detects Alt branches with overlapping regex FIRST sets but disjoint continuation FIRST sets. Factors common prefix and builds dispatch table on continuation. Runs after `compute_follow_sets`, before `generate_dispatch_tables`.
- **Direct memchr emission**: `fast_paths::emit_regex_direct_call` handles negated character class patterns `[^XYZ]+` and `[^XYZ]*`, emitting inline `memchr::memchr1/2/3` calls that bypass SpanParser enum dispatch. Also handles positive character classes (`[a-z]`, `[abc]`) and their repetitions (`[a-z]+`, `[a-z]*`) via direct inline byte-range/set checks.
- **BumpSlab**: `parse_that::BumpSlab`—byte-based bump allocator (no type parameter). Generic methods: `alloc<T>`, `alloc_slice_clone<T>`, `alloc_slice_copy<T>`. Zero RefCell borrow tracking per alloc. Used via `parse_with_context(&input, &slab)`.
- **`@ws` directive**: `@ws /regex/ ;` overrides what `?w` compiles to. Stored as `GrammarIR::ws_pattern: Option<StringId>`. Codegen uses `emit_ws_trim()` which checks `ir.ws_pattern` and emits `fast_paths::emit_regex_direct_call` for known SIMD fast paths (e.g., CSS comment-aware whitespace → `css_ws_comment_fast`).
- **`@token` directive**: `@token ruleName ;` marks a rule as a lexical token. Stored as `RuleMeta::is_token: bool`. Implies `span_eligible`. Uses fusion-style inlining (body inlined at call sites, enum variant preserved) for `@pretty` compatibility.
- **`@debug` directive**: `@debug ruleName ;` / `@debug * ;` instruments rules for trace output. Stored as `RuleMeta::debug: bool` and `GrammarIR::debug_all: bool`. Trace codegen: `codegen/trace.rs` emits `#[cfg(feature = "parser-trace")]` calls for monolithic paths.
- **DAP server**: `lsp/src/dap/` (4 sub-modules: `mod.rs`, `adapter.rs`, `protocol.rs`, `mapping.rs`). `bbnf-lsp --dap` speaks Debug Adapter Protocol over stdin/stdout.
- **Import item spans**: `ImportedName<'a>` preserves byte spans for selective imports. `ImportedItem` in analysis layer.
- **IR-backed LSP analysis**: `try_compile_ir()` in `diagnostics.rs` runs the full IR pipeline per document change, extracts `IrRuleMeta` (FOLLOW sets, dispatch, memo, span eligibility, projected type) for hover enrichment.
- **`collapse_simple_spans`**: `GrammarIR::collapse_simple_spans: bool`. When true (prettify disabled), Seq nodes where all children are simple Span leaves collapse to a single Span, eliminating slab allocation. Gated to prevent type cascading in prettify grammars.
- **Cold benchmarks only**: All bench macros construct fresh BumpSlab + Parser per iteration. Warm/cached benchmarks (reusing a pre-constructed Parser) removed—they measure combinator cache throughput, not parse throughput.
- **JSON pattern detection**: Exact-match against canonical regex patterns (no substring heuristics). `is_json_string_regex()` / `is_json_number_regex()` use `const` pattern arrays.
- **WASM**: `wasm/` crate (`bbnf-wasm`) — 31 exports total: 5 formatters (json/css/bnf/ebnf/bbnf) + `analyze_grammar` + 17 LSP features + 7 VM functions (compile, compile_grammar_debug, parse, parse_check, format, debug_step, free) + `init_panic_hook`. Decomposed into `analysis.rs`, `gorgeous.rs`, `lsp.rs`, `vm.rs`.
- **Playground composables**: `composables/wasm/{types,index,loader}.ts` + `usePlaygroundQuery.ts`, `useSplitPane.ts`, `usePipeline.ts`, `useExamples.ts`, `useWalkthrough.ts`, `useDocs.ts`, `useHeroState.ts`, `useTypewriter.ts`, `useMouseParallax.ts`, `useScrollTimeline.ts`, `useScrollMorph.ts`, `useMarkdownComponents.ts`, `useChartData.ts`, `useDebugSession.ts`. 15 Monaco providers: hover, completion, semantic tokens, inlay hints, definition, document symbols, folding, selection ranges, code actions, code lens, references, rename, document formatting, range formatting, on-type formatting.
- **Per-expression mapping `->` syntax**: `expr -> mapper` where mapper is a closure (`|s: Span| -> f64 { ... }`), function path (`crate::func`), or constant (`0u8`). Postfix operator at `?w` precedence. `=>` is a legacy alias applying `->` to the entire rule RHS. Lowered via `Expression::MappedExpression` in the AST; `lower/expression.rs` calls `try_specialize_map_fn` to detect conversion patterns.
- **`FnDescriptor::NumberConvert`**: Fused numeric regex scan + f64 conversion. Codegen emits `css_number_scan_f64(state)` — Eisel-Lemire fast path in `parse_that`. Zero regex overhead.
- **`FnDescriptor::HexConvert`**: Inline char-class loop + user conversion function. Codegen emits a hand-rolled `[0-9a-fA-F]` scanner that feeds accumulated bytes to the user's function path.
- **`FnDescriptor::Constant`**: Direct value emission, skips Span construction entirely. `"px" -> 0u8` lowers to `Map(Literal("px"), Constant { value: "0u8", return_type })`.
- **`css_number_scan_f64`**: Fused CSS number scanner in `parse_that`. Handles sign, fraction, exponent via Eisel-Lemire algorithm. Used by `NumberConvert` codegen path.
- **Literal prefix factoring**: `ir/src/passes/prefix.rs` performs trie-style byte-level splitting of Literal alternation branches. `Alt([Literal("rem"), Literal("rlh")])` becomes `Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))`, enabling dispatch tables for branches sharing a first byte.
- **Map-transparent all-literal detection**: `codegen/alt/` sees through `Map(Literal, Constant)` wrappers. An alternation of mapped literals (`"px" -> 0u8 | "em" -> 1u8`) uses the sequential literal fast path — byte-compare first, then emit the constant.
- **Regex structural classifier**: `generate/hir_classify.rs` uses `regex-syntax` HIR for structural classification — detects Numeric/HexDigits/Identifier/QuotedString patterns (aliased as `regex_classify` for backward compat). Drives `FnDescriptor` specialization in `try_specialize_map_fn` and inline scanner selection in `fast_paths.rs`.
- **Inline byte scanners**: `fast_paths.rs` emits direct inline code for patterns beyond JSON: `css_ident_fast` for `[a-zA-Z_][\w-]*`, `css_string_fast` for quoted strings, `css_ws_comment_fast` for comment-aware whitespace, comma-or-whitespace `,|\s+`, and generalized char-class/negated-class loops via `emit_generalized_regex_direct`.

## Roadmap

### Landing Page — COMPLETE
Landing page at `/` with HeroSection, DemoCards, FeatureCards, LivePreviewStrip, CodeCardFan, CodeCardGrid, FooterSection, TypewriterText. NavBar in `layout/`.
Routes: `/` = landing, `/playground` = playground, `/docs` = docs.

### Documentation Page
- Dynamic docs rendered from markdown at `/docs/:slug`
- File-system or frontmatter-based routing (`docs/*.md` → slug)
- Sidebar nav auto-generated from markdown headings/structure
- Code blocks with syntax highlighting (reuse Monaco)
- API reference: grammar syntax, directives (`@pretty`, `@recover`, `@no_collapse`, `@import`, `@ws`, `@token`, `skip_recover`), hint vocabulary

### Playground Walk-Through Demos
- Guided interactive tutorials that load grammar + input pairs
- Step-by-step progression with tooltip/overlay annotations per feature
- Progress tracking through demo steps
- Starter demos: "Build a JSON parser", "Add error recovery", "Format with `@pretty`"
