# CLAUDE.md — bbnf-lang

Monorepo for the BBNF (Better Backus-Naur Form) grammar ecosystem.
BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

## Structure

```
bbnf-lang/
├── rust/                       Rust workspace (Cargo)
│   ├── bbnf/                   Core grammar parser, IR lowering, codegen (lib)
│   │   └── generate/codegen/trace.rs  Debug trace codegen (#[cfg(feature = "parser-trace")])
│   ├── bbnf-ir/                Canonical grammar IR, bytecode compiler, interpreter
│   ├── bbnf-derive/            Proc-macro: #[derive(Parser)] from .bbnf files
│   ├── bbnf-analysis/          LSP analysis engine (DocumentState, 14 feature providers, state management)
│   └── lsp/                    Language server (bbnf-lsp binary)
│       └── src/dap/            DAP server (mod, adapter, protocol, mapping)
├── wasm/                       bbnf-wasm crate (wasm-pack → playground)
│   └── src/                    lib.rs + analysis.rs, gorgeous.rs, lsp.rs, vm.rs
├── typescript/                 @mkbabb/bbnf-lang — runtime parser + codegen
├── prettier-plugin-bbnf/       Prettier plugin for .bbnf formatting
├── playground/                 Vue 3 + Monaco playground (uses bbnf-wasm)
│   ├── src/components/debug/   Debug pane components (breakpoints, call stack, parse state)
│   └── src/composables/        wasm/, usePlaygroundQuery, useSplitPane, usePipeline, useExamples, useWalkthrough, useDocs, useHeroState, useTypewriter, useMouseParallax, useScrollTimeline, useScrollMorph, useMarkdownComponents, useChartData, useDebugSession
├── extension/                  VS Code extension (LSP client)
├── docs/                       Documentation (markdown, rendered by playground)
├── grammar/                    Example grammars + language specification
│   └── lang/                   Language grammars (json, css, bnf, ebnf, bbnf, google-sheets)
├── data/                       Benchmark datasets
├── scripts/                    Build automation scripts
├── server/                     Compiled LSP binary (copied by Makefile)
├── .github/workflows/          CI (ci.yml) + release pipeline (release.yml)
├── .vscode/                    Launch configs, tasks, settings
├── Makefile                    Build automation
└── package.json                NPM workspaces root
```

## Build Commands

```bash
make build          # Release LSP + extension
make dev            # Debug LSP + extension (fast iteration)
make test           # All Rust + TypeScript tests
make bench          # LSP performance benchmarks
make install        # Build, package .vsix, install into VS Code
make package        # Build + create bbnf-lang.vsix
make watch          # Continuous rebuild (cargo watch)
make clean          # Remove artifacts
```

## Manual Builds

```bash
# Rust (requires nightly)
cd rust && cargo test --workspace && cargo build --release -p bbnf-lsp

# TypeScript
cd typescript && npm ci && npm test

# Prettier plugin (build TS first)
npm ci && cd typescript && npm run build && cd ../prettier-plugin-bbnf && npm test

# Extension
cd extension && npm ci && npm run build

# WASM (builds into playground/src/wasm/)
cd wasm && wasm-pack build --target web --out-dir ../playground/src/wasm
```

## Development

- **F5 workflow**: `make dev` then F5 in VS Code
- **Install locally**: `make install` then reload VS Code
- **Integration tests**: `cargo test -p bbnf-lsp --test integration -- --nocapture`
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
Rust:                                 NPM:
  pprint → parse_that → bbnf           @mkbabb/parse-that → @mkbabb/bbnf-lang
                          ↓
                      bbnf_derive
                          ↓
                       gorgeous
                          ↓
                      bbnf_wasm (wasm-pack → playground)
```

bbnf and bbnf_derive sit in the middle of the Rust crate graph.
bbnf-lsp uses workspace-relative paths to bbnf; cross-repo deps are version-only.

## Conventions

- **Rust**: Nightly toolchain, edition 2024. Clippy with `-D warnings`.
- **TypeScript**: ES2022 target, strict mode, ESM. Vite for bundling, vitest for tests.
- **Extension**: esbuild, CommonJS output (Node.js), `vscode` external.
- **Grammars**: `.bbnf` extension. `@import` for composition. `@recover` for error recovery. `;` terminators.
- **Crate deps**: `parse_that` and `pprint` from crates.io; local dev via `.cargo/config.toml` `[patch.crates-io]`.
- **Lifetimes**: Borrowed `'a` throughout Rust AST; `Box::leak()` for import module graphs.
- **Import system**: Cyclic imports handled via partial-init before recursion. Selective imports expand transitive local deps automatically. `@import` directives can appear at any position in a file.
- **Recovery**: `@recover rule syncExpr ;` — per-rule annotation specifying a sync expression for multi-error parsing. Any valid BBNF expression (regex, alternation, concatenation, etc.) is valid as the sync. Emits `.recover(syncParser, null)` in TS codegen and a `Recovered` enum variant in Rust proc-macro codegen.
- **Analysis pipeline**: Tarjan SCC → topological sort → FIRST sets (128-bit `CharSet`) → dispatch tables (constant-time alternation selection by leading character).
- **Recursive SpanParser codegen**: `try_generate_span_parser()` handles all expression types (concat, alt, many, skip/next, minus, nonterminal refs). Iterative fixed-point loop on `sp_method_rules` — start empty, try generating for all eligible rules, add successes, repeat until convergence (2–3 iterations). Literal unescape via `unescape_literal()` + `proc_macro2::Literal::string()`.
- **Codegen**: Grammar → `lower/` (5 sub-modules: `mod.rs`, `string_interner.rs`, `fn_table.rs`, `expression.rs`, `metadata.rs`) → bbnf-ir `Module` → `codegen/` (single monolithic path + optional prettify) + `ir_enums.rs`/`ir_types.rs`. `pipeline.rs` orchestrates the full lowering + codegen sequence.
- **Vec unboxing**: `in_vec` parameter threading through codegen, `ir_node_to_tokens_vec`, `project_node_in_vec`. Transparent rule `_unboxed()` generation for zero-cost enum extraction.
- **`try_flatten_pair`**: Extension for `(BoxedEnum, Vec<Enum>)` patterns — flattens pair into unboxed Vec.
- **`merge_regex_alts` pass**: Fuses `Alt([Regex, Regex, ...])` into a single combined regex pattern. Runs after `merge_literals` and before `factor_common_prefixes` in the IR pipeline.
- **Pipeline synchronization**: The IR pass ordering in `pipeline.rs` and `bbnf-derive/src/lib.rs` must be kept in sync—both run the same 18-operation sequence (16 unique passes, including `sort_alt_branches`, `factor_regex_with_lookahead`, and `fuse_token_dispatch` after `compute_follow_sets`).
- **`fuse_single_use` pass**: Inlines single-use rules at their call site regardless of body size, guarded by SCC membership. Runs after `inline_acyclic` + prune, before `eliminate_epsilon`.
- **`fuse_token_dispatch` pass**: Fuses `@token`-marked rules at dispatch call sites—inlines the token body for direct matching while preserving the enum variant. Runs after `factor_regex_with_lookahead`, before `generate_dispatch_tables`.
- **`no_collapse` gating**: Rules annotated with `@no_collapse` are excluded from inlining and fusing passes to preserve their identity in the generated AST.
- **`emit_discarded` for Skip/Next**: Skip and Next codegen emits the discarded side for its side effects (e.g., whitespace consumption) even though the value is unused.
- **mimalloc in bench files**: Benchmark binaries use `#[global_allocator] static GLOBAL: mimalloc::MiMalloc` for consistent, high-performance allocation during benchmarking.
- **Prettify codegen**: `@pretty` directives control Doc emission. Hint vocabulary: `group`, `indent`, `dedent`, `block`, `blankline`, `nobreak`, `softbreak`, `hardbreak`, `compact`, `fast`, `off`. `generate_prettify()` produces `to_doc()` + `source_range()` impls. Sub-variant coercion for heterogeneous alternation branches. Shared hint definitions in `bbnf-analysis/src/directives/hints.rs` — single source of truth for codegen + LSP.
- **Wrapped vec formatting**: Delimiter-wrapped repetitions (e.g. `"{" >> items << "}"`) emit IfBreak concat — one item per line when Group breaks, comma-separated inline when it fits.
- **`skip_recover`**: Parser attribute that suppresses `@recover` codegen and the `Recovered` enum variant. Used by formatting-only parsers that assume well-formed input.
- **Type comparison**: `types_eq()` compares `syn::Type` structurally via per-token-tree comparison—no string serialization.
- **Sub-variant validation**: `validate_sub_variant_uniqueness()` rejects cross-rule type collisions at compile time.
- **Monolithic codegen**: `codegen/` is the sole codegen path. Generates direct recursive functions with slab allocation—zero combinator overhead. `MonoCtx` tracks fusion eligibility, single-site inline eligibility, dispatch-guaranteed-byte, `current_rule_name`, and hoisted leaf-parser bindings. Unified `SepByConfig` + `emit_mono_sep_by_core` handles all three sep_by variants (bare, ws-aware, delimited-with-terminator). Optional `prettify/` sub-path emits fused parse+format for `#[parser(prettify)]`.
- **Span-only codegen**: `codegen/span/` (4 sub-modules: `mod.rs`, `alt.rs`, `expr.rs`, `repeat.rs`). Triggered by `#[parser(span)]`. Generates `fn __rule_span(state) -> Option<Span>` for every rule—zero allocations, no enum variants, no Vec.
- **Inline optional Span codegen**: Optional Span expressions (Literal/Regex with fast-path) in `codegen/repeat.rs` and `codegen/span/repeat.rs` emit inline byte checks instead of constructing a SpanParser per call.
- **Delimiter-driven flat scanning**: `codegen/delim_scan.rs`. Grammar-agnostic codegen optimization for `Wrap(Repeat(Alt))` patterns where the Alt's FIRST sets overlap. Detects single-byte "pivot" Literals that distinguish branches, emits a forward `memchr` scanner loop. Span path replaces the descent entirely; slab path uses speculative dispatch (scan selects branch, then calls existing recursive descent for typed construction). Pseudo-class guard: if value after pivot terminates at the open byte, falls back to block branch. When the pivot rule returns Span (`is_token` or `TypeDesc::Span`), constructs the result directly from scanner offsets, eliminating speculative rewind + re-parse.
- **`factor_regex_with_lookahead` pass**: `bbnf-ir/src/passes/factor_lookahead.rs`. Detects Alt branches with overlapping regex FIRST sets but disjoint continuation FIRST sets. Factors common prefix and builds dispatch table on continuation. Runs after `compute_follow_sets`, before `generate_dispatch_tables`.
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
- **Literal prefix factoring**: `bbnf-ir/src/passes/prefix.rs` performs trie-style byte-level splitting of Literal alternation branches. `Alt([Literal("rem"), Literal("rlh")])` becomes `Seq(Literal("r"), Alt([Literal("em"), Literal("lh")]))`, enabling dispatch tables for branches sharing a first byte.
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
