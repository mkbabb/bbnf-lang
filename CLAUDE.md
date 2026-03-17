# CLAUDE.md — bbnf-lang

Monorepo for the BBNF (Better Backus-Naur Form) grammar ecosystem.
BBNF extends EBNF for defining context-free grammars, used by the
[`parse-that`](https://github.com/mkbabb/parse-that) parser combinator library.

## Structure

```
bbnf-lang/
├── rust/                       Rust workspace (Cargo)
│   ├── bbnf/                   Core grammar parser, IR lowering, codegen (lib)
│   ├── bbnf-ir/                Canonical grammar IR, bytecode compiler, interpreter
│   ├── bbnf-derive/            Proc-macro: #[derive(Parser)] from .bbnf files
│   ├── bbnf-analysis/          LSP analysis engine (DocumentState, 14 feature providers, state management)
│   └── lsp/                    Language server (bbnf-lsp binary)
├── wasm/                       bbnf-wasm crate (wasm-pack → playground)
│   └── src/                    lib.rs + analysis.rs, gorgeous.rs, lsp.rs, vm.rs
├── typescript/                 @mkbabb/bbnf-lang — runtime parser + codegen
├── prettier-plugin-bbnf/       Prettier plugin for .bbnf formatting
├── playground/                 Vue 3 + Monaco playground (uses bbnf-wasm)
│   └── src/composables/        wasm/, usePlaygroundQuery, useSplitPane, usePipeline, useExamples, useWalkthrough, useDocs, useHeroState, useTypewriter, useMouseParallax, useScrollTimeline, useScrollMorph, useMarkdownComponents, useChartData
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
- **IR-based codegen**: Grammar → `lower/` (5 sub-modules: `mod.rs`, `string_interner.rs`, `fn_table.rs`, `expression.rs`, `metadata.rs`) → bbnf-ir `Module` → `ir_codegen/` (7 sub-modules: `mod.rs`, `alt.rs`, `seq.rs`, `repeat.rs`, `wrap.rs`, `infer.rs`, `inline.rs`) + `ir_enums.rs`/`ir_types.rs`/`ir_span.rs`/`ir_pretty/` (5 sub-modules: `mod.rs`, `patterns.rs`, `heuristics.rs`, `codegen.rs`, `utils.rs`). `pipeline.rs` orchestrates the full lowering + codegen sequence.
- **Inline codegen**: `ir_codegen/inline.rs` — `InlineCtx`, `emit_rule_body_inline` for flat match-arm dispatch without combinator overhead.
- **Vec unboxing**: `in_vec` parameter threading through codegen, `ir_node_to_tokens_vec`, `infer_node_type_in_vec`. Transparent rule `_unboxed()` generation for zero-cost enum extraction.
- **`try_flatten_pair`**: Extension for `(BoxedEnum, Vec<Enum>)` patterns — flattens pair into unboxed Vec.
- **`merge_regex_alts` pass**: Fuses `Alt([Regex, Regex, ...])` into a single combined regex pattern. Runs after `merge_literals` and before `factor_common_prefixes` in the IR pipeline.
- **Pipeline synchronization**: The IR pass ordering in `pipeline.rs` and `bbnf-derive/src/lib.rs` must be kept in sync—both run the same 12-pass sequence.
- **Prettify codegen**: `@pretty` directives control Doc emission. Hint vocabulary: `group`, `indent`, `dedent`, `block`, `blankline`, `nobreak`, `softbreak`, `hardbreak`, `compact`, `fast`, `off`. `generate_prettify()` produces `to_doc()` + `source_range()` impls. Sub-variant coercion for heterogeneous alternation branches. Heuristic inference (`heuristics.rs`) auto-applies hints for un-annotated rules (toplevel, brace-delimited, large compound). Shared hint definitions in `hints.rs` — single source of truth for codegen + LSP.
- **Wrapped vec formatting**: Delimiter-wrapped repetitions (e.g. `"{" >> items << "}"`) emit IfBreak concat — one item per line when Group breaks, comma-separated inline when it fits.
- **`skip_recover`**: Parser attribute that suppresses `@recover` codegen and the `Recovered` enum variant. Used by formatting-only parsers that assume well-formed input.
- **Type comparison**: `types_eq()` compares `syn::Type` structurally via per-token-tree comparison—no string serialization.
- **Sub-variant validation**: `validate_sub_variant_uniqueness()` rejects cross-rule type collisions at compile time.
- **JSON pattern detection**: Exact-match against canonical regex patterns (no substring heuristics). `is_json_string_regex()` / `is_json_number_regex()` use `const` pattern arrays.
- **WASM**: `wasm/` crate (`bbnf-wasm`) — 29 exports total: 5 formatters (json/css/bnf/ebnf/bbnf) + `analyze_grammar` + 17 LSP features + 5 VM functions (compile, parse, parse_check, format, free) + `init_panic_hook`. Decomposed into `analysis.rs`, `gorgeous.rs`, `lsp.rs`, `vm.rs`.
- **Playground composables**: `composables/wasm/{types,index,loader}.ts` + `usePlaygroundQuery.ts`, `useSplitPane.ts`, `usePipeline.ts`, `useExamples.ts`, `useWalkthrough.ts`, `useDocs.ts`, `useHeroState.ts`, `useTypewriter.ts`, `useMouseParallax.ts`, `useScrollTimeline.ts`, `useScrollMorph.ts`, `useMarkdownComponents.ts`, `useChartData.ts`. 15 Monaco providers: hover, completion, semantic tokens, inlay hints, definition, document symbols, folding, selection ranges, code actions, code lens, references, rename, document formatting, range formatting, on-type formatting.

## Roadmap

### Landing Page — COMPLETE
Landing page at `/` with HeroSection, DemoCards, FeatureCards, LivePreviewStrip, CodeCardFan, CodeCardGrid, FooterSection, TypewriterText. NavBar in `layout/`.
Routes: `/` = landing, `/playground` = playground, `/docs` = docs.

### Documentation Page
- Dynamic docs rendered from markdown at `/docs/:slug`
- File-system or frontmatter-based routing (`docs/*.md` → slug)
- Sidebar nav auto-generated from markdown headings/structure
- Code blocks with syntax highlighting (reuse Monaco)
- API reference: grammar syntax, directives (`@pretty`, `@recover`, `@no_collapse`, `@import`, `skip_recover`), hint vocabulary

### Playground Walk-Through Demos
- Guided interactive tutorials that load grammar + input pairs
- Step-by-step progression with tooltip/overlay annotations per feature
- Progress tracking through demo steps
- Starter demos: "Build a JSON parser", "Add error recovery", "Format with `@pretty`"
