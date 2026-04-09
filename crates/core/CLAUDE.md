# CLAUDE.md — crates/core/

Core BBNF library: grammar parsing, static analysis, IR lowering, and
multi-target code generation (Rust / TypeScript / WASM).

## Structure

```
bbnf/
├── Cargo.toml
├── src/
│   ├── lib.rs            Re-exports every top-level module. `parse_that::Span` is also re-exported
│   │                     for generated parser code.
│   ├── types.rs          `Expression`, `Token`, `AST`, `Comment`, `Comments`, `ImportDirective`, `ParsedGrammar`.
│   ├── grammar/          BBNF grammar parser (bootstrap).
│   │   ├── mod.rs        `parse` / `parse_with_state` — single-call parse through the bootstrap enum parser.
│   │   ├── generated.rs  Auto-generated bootstrap parser (from `bbnf.bbnf`; checked in).
│   │   ├── host.rs       `extract_grammar` — `BbnfBootstrapEnum → ParsedGrammar`.
│   │   └── schema/       `CstSchema` — first-class CST description for the grammar.
│   │       ├── mod.rs    Schema public surface.
│   │       ├── model.rs  `CstSchema`, `FieldRole`, target-agnostic types.
│   │       ├── build.rs  `CstSchema::from_ir` — role assignment from IR rule bodies.
│   │       └── emit/     Per-target CST emitters.
│   │           ├── mod.rs     Dispatch.
│   │           ├── runtime.rs Shared runtime helpers.
│   │           ├── rust/      Rust CST helper codegen.
│   │           └── ts.rs      TS CST helper codegen (placeholder).
│   ├── graph/            Grammar dependency graph.
│   │   ├── mod.rs        Re-exports.
│   │   ├── deps.rs       `calculate_ast_deps`, `traverse_ast`.
│   │   ├── scc.rs        Tarjan SCC, `SccResult`, topological sort.
│   │   └── metadata.rs   AST-level alias detection (`find_aliases`).
│   ├── imports/          Module system — `@import` resolution.
│   │   ├── mod.rs        Re-exports `ImportError`, `ModuleData`, `ResolvedImport`,
│   │   │                  `ImportCycle`, `ModuleRegistry`, `load_module_graph`.
│   │   ├── errors.rs     `ImportError` enum + impls.
│   │   ├── registry.rs   `ModuleRegistry` struct + accessors + the `ModuleData` /
│   │   │                  `ResolvedImport` / `ImportCycle` types.
│   │   ├── loader.rs     `load_module_graph` + `load_recursive` (DFS, partial-init under cycles).
│   │   └── resolve.rs    `resolve_imports_for`, `transitive_local_deps`, `resolve_import_path`.
│   ├── lower/            Bootstrap tree → `GrammarIR`.
│   │   ├── mod.rs        Entry point (`lower_to_ir`), `LowerCtx`, orchestration.
│   │   ├── string_interner.rs  `StringInterner` — dedup string literals.
│   │   ├── fn_table.rs   `FnTable` — host function descriptors.
│   │   ├── expression.rs Recursive expression/node lowering + `try_specialize_map_fn`.
│   │   ├── metadata.rs   Rule metadata lowering (recover, pretty, token).
│   │   └── value_expr.rs `MapExpr` value lowering.
│   ├── pipeline.rs       Thin facade over `pipeline/`: `CompileRequest`, `CompileOutput`,
│   │                     `CompileTarget`, `CompileError`, `PipelineOptions`; re-exports
│   │                     `compile_grammar` / `compile_ast` / `compile_paths_request`.
│   ├── pipeline/         Compilation sub-modules.
│   │   ├── compile.rs    Full lowering + pass pipeline (16-op sequence) + DAG build site.
│   │   ├── directives.rs Directive collection (`DirectiveMaps`, `load_merged_paths`).
│   │   └── validate.rs   `validate_ast` + `validate_pretty_directives`.
│   ├── backend/          Multi-target code generation infrastructure.
│   │   ├── mod.rs        Re-exports `Emitter`, `PreparedGrammar`, `prepare_grammar`, shared types.
│   │   ├── emitter.rs    `Emitter` trait — target-agnostic emission surface.
│   │   ├── types.rs      Shared driver types (`SepByConfig`, `DelimScanConfig`,
│   │   │                  `AltBranchInfo`, `FlattenStrategy`, `KeyDispatchBranch`,
│   │   │                  `SeqChildGroup`, `TokenDispatchArmCompiled`, `ValuePlacement`).
│   │   ├── util.rs       `unescape_literal` and other shared helpers.
│   │   ├── driver/       Shared compilation driver.
│   │   │   ├── mod.rs    `DriverState`, public entry points.
│   │   │   ├── analysis.rs  `BackendAnalysis`, `BackendPreparation`, `prepare_grammar`.
│   │   │   ├── node.rs   Per-node dispatcher (`compile_node`).
│   │   │   ├── seq.rs    Seq + operator-chain emission decisions.
│   │   │   ├── alt.rs    Alt strategy dispatch (all-literal, dispatch table, key dispatch, checkpoint).
│   │   │   ├── repeat.rs Repeat (sep_by / optional / many).
│   │   │   ├── reference.rs  Ref (inline vs call).
│   │   │   ├── map.rs    Map (FnDescriptor classification + fusion).
│   │   │   ├── wrap.rs   `open >> middle << close` (delim sep_by, delim scan, generic wrap).
│   │   │   └── prettify.rs   Prettify-mode specialization.
│   │   ├── kernels/      Tranche V.7 — recognizer-family kernel emission.
│   │   │   ├── mod.rs              Module map.
│   │   │   ├── quoted_string.rs    QuotedString / JsonString / CssQuotedString.
│   │   │   ├── number.rs           Numeric / JsonNumber (fused + span).
│   │   │   ├── identifier.rs       CSS / generic identifier.
│   │   │   ├── comment_ws.rs       Comment-aware whitespace.
│   │   │   ├── charclass.rs        CharClassQuantified, HexDigits.
│   │   │   ├── prefix_class.rs     PrefixThenClass / Anchored / AccelDriven.
│   │   │   ├── balanced_wrap.rs    Balanced delimiter scan (V.8 wires).
│   │   │   └── sep_list.rs         Separator-list element loop (V.8 wires).
│   │   ├── patterns/     Legacy pattern detection (Tranche F).
│   │   │   ├── mod.rs              Re-exports.
│   │   │   ├── cache.rs            `NodeId`-keyed lookup tables consumed by
│   │   │   │                        the driver. Populated by `install_pattern_caches`.
│   │   │   ├── decisions.rs        Shared decision functions (decide_seq /
│   │   │   │                        decide_alt) — type-resolution layer, kept.
│   │   │   ├── delim_scan.rs       Delimiter-scan detection.
│   │   │   └── key_dispatch.rs     Key-dispatch detection + config.
│   │   │   Note: detection halves of delim_scan/key_dispatch are deletion
│   │   │   candidates for the follow-up tranche once strategy solvers
│   │   │   migrate to consume `ir.recognizer_decisions` (V.6 / V.8
│   │   │   accessor) directly.
│   │   ├── strategy/     CSP-solved emission strategies (NodeId-keyed).
│   │   │   ├── mod.rs    `solve_alt_strategies` entry + re-exports.
│   │   │   ├── alt_strategy.rs  Alt strategy solver.
│   │   │   ├── ref_strategy.rs  Ref strategy solver.
│   │   │   ├── repeat_strategy.rs  Repeat strategy solver.
│   │   │   ├── seq_strategy.rs  Seq strategy solver.
│   │   │   └── wrap_strategy.rs  Wrap strategy solver.
│   │   ├── prettify/     Prettify analysis + plan.
│   │   │   ├── mod.rs    Re-exports.
│   │   │   ├── analysis.rs  Prettify eligibility + policy inference.
│   │   │   ├── plan.rs   `PrettyRulePlan` planning.
│   │   │   └── types.rs  `PrettyPolicy`, `SeparatorPolicy`, `WrapperPolicy`.
│   │   ├── rust/         Rust backend.
│   │   │   ├── mod.rs    Re-exports + sub-module wiring.
│   │   │   ├── analysis/ Rust-specific analysis.
│   │   │   │   ├── mod.rs       Entry point.
│   │   │   │   ├── inline.rs    Inline call-strategy analysis.
│   │   │   │   └── specialize.rs  Specialization analysis.
│   │   │   ├── alloc_emit.rs    Slab scratch emission + context generation.
│   │   │   ├── emitter_types.rs `RustEmitter`, `RustEmitCtx` struct definitions.
│   │   │   ├── ir_enums.rs      Enum type generation from IR alternations.
│   │   │   ├── ir_types.rs      IR-level type projection + `ParserAttributes`.
│   │   │   ├── trace.rs         Debug trace codegen (`#[cfg(feature = "parser-trace")]`).
│   │   │   └── emitter/         `impl Emitter for RustEmitter` — one trait impl in mod.rs,
│   │   │       │                 delegating to `xxx_impl` methods on kind-grouped siblings.
│   │   │       ├── mod.rs       The trait impl block.
│   │   │       ├── leaves.rs    literal / regex / epsilon.
│   │   │       ├── seq.rs       seq_all_span / seq_grouped.
│   │   │       ├── alt.rs       alt checkpoint / all-literal / sub-variant.
│   │   │       ├── dispatch.rs  alt dispatch table + token dispatch.
│   │   │       ├── repeat.rs    many / optional / sep-by.
│   │   │       ├── binary.rs    skip / next / minus / negate.
│   │   │       ├── operator_chain.rs  Chained binary flattening.
│   │   │       ├── map_value.rs enum wrap / number convert / constant / map expr / span capture / hex convert / fused.
│   │   │       ├── grammar.rs   rule_function / type_definitions / emit_grammar.
│   │   │       ├── ws.rs        whitespace trim emission.
│   │   │       └── prettify/    Prettify-mode emission (one `_impl` pattern per file).
│   │   │           ├── mod.rs       Shared helpers: `prettify_fn_ident`, `emit_separator_ops`,
│   │   │           │                 `emit_rule_wrapper`, `emit_whitespace_segment`,
│   │   │           │                 `split_compile_error`.
│   │   │           ├── literal.rs   Literal prettify.
│   │   │           ├── seq.rs       Sequence prettify.
│   │   │           ├── alt.rs       Alternation prettify.
│   │   │           ├── repeat.rs    Repetition prettify.
│   │   │           ├── attempt.rs   Attempt / fallback prettify.
│   │   │           └── grammar.rs   Rule + grammar prettify.
│   │   ├── ts/           TypeScript backend.
│   │   │   ├── mod.rs    Re-exports `TsCode`, `TsEmitCtx`, `TsEmitter`.
│   │   │   ├── code.rs   `TsCode { stmts, expr }` + ctx.
│   │   │   ├── helpers.rs  `ts_escape`, `type_desc_to_ts`, `compile_map_expr_to_js`, etc.
│   │   │   ├── alt.rs    Alt emission (sibling of emitter/).
│   │   │   ├── repeat.rs Repeat emission.
│   │   │   ├── dispatch.rs  Dispatch emission.
│   │   │   ├── ws.rs     Whitespace emission.
│   │   │   └── emitter/  The `impl Emitter for TsEmitter` block + `xxx_impl` groups.
│   │   │       ├── mod.rs   Trait impl, delegating to siblings and the alt/repeat/dispatch/ws
│   │   │       │             siblings in the enclosing `ts/` directory (whose helper methods are
│   │   │       │             `pub(in crate::backend::ts)` so the new emitter sub-module can call them).
│   │   │       ├── leaves.rs   literal / regex / epsilon.
│   │   │       ├── binary.rs   call / inline_wrap / operator_chain / skip / next / minus / negate.
│   │   │       ├── value.rs    enum_wrap / number_convert / constant / map_expr / span_capture / hex_convert / fused_map.
│   │   │       └── grammar.rs  rule_function / type_definitions / emit_grammar.
│   │   └── wasm/         WASM backend (same layout as ts/).
│   │       ├── mod.rs    Re-exports `WasmEmitCtx`, `WasmEmitter`.
│   │       ├── code.rs   Code state.
│   │       ├── helpers.rs `unescape_literal`.
│   │       ├── alt.rs / repeat.rs / dispatch.rs / ws.rs    Siblings of emitter/.
│   │       └── emitter/  The `impl Emitter for WasmEmitter` block + `xxx_impl` groups.
│   │           ├── mod.rs   Trait impl.
│   │           ├── leaves.rs / binary.rs / value.rs / grammar.rs
│   └── generate/         Two-track codegen surface.
│       ├── mod.rs        Track 1 (CST helpers via `grammar::schema::emit::rust::generate`)
│       │                  + Track 2 (backend parser code via `backend::rust::emitter::generate`).
│       ├── serialize/    Grammar-guided serialization codegen.
│       │   ├── mod.rs    Entry point.
│       │   └── serialize.rs  Serializer impl generation.
│       └── regex/        Regex → TokenStream codegen.
│           ├── mod.rs    Public surface (`emit_regex`, `solve_regex_strategy`, cost model).
│           ├── cost_model.rs  `CostModel`, `EmitOpts`, `LengthHint`.
│           ├── patterns/ Pattern detection.
│           │   ├── mod.rs       Re-exports.
│           │   ├── char_class.rs Character class detection.
│           │   └── shorthand.rs  Shorthand detection.
│           └── emit/     Tiered emission via pluggable pattern registry.
│               ├── mod.rs    `RegexPattern` trait + registry.
│               ├── negated_class.rs  Negated char-class memchr emission.
│               ├── scanner_plan.rs   Scanner plan construction.
│               ├── simd.rs    SIMD-accelerated fast paths.
│               ├── generalized/  Generalized regex-to-scanner emission.
│               │   ├── mod.rs
│               │   └── class_segments.rs
│               ├── hir/          HIR-based emission.
│               │   ├── mod.rs
│               │   ├── leaf.rs
│               │   ├── alternation.rs
│               │   └── repetition.rs
│               └── dfa/          DFA-based emission.
│                   ├── mod.rs
│                   ├── helpers.rs
│                   └── table.rs
└── tests/                Integration tests (all under tests/, never inline).
```

## Key Types

- **`Expression<'a>`** — AST node enum (Literal, Regex, Nonterminal, Concatenation, Alternation, Skip, Next, Many, etc.).
- **`Token<'a, T>`** — Value + Span + optional comments. `Token::inner()` returns `&T`.
- **`AST<'a>`** — `IndexMap<Expression, Expression>`. Rule LHS → RHS, insertion-ordered.
- **`ParsedGrammar<'a>`** — Imports + AST.
- **`ImportDirective<'a>`** — Path, span, optional selective items.
- **`CompileRequest`** / **`CompileOutput`** / **`CompileTarget`** — `pipeline` public API. Targets: `Rust`, `Vm`, `Ts`, `Wasm`.
- **`PreparedGrammar`** — Output of `backend::prepare_grammar`; bundles `GrammarIR` + `BackendAnalysis` + solved strategies for the emitters.
- **`Emitter` trait** — Target-agnostic emission surface implemented once per backend (Rust / TS / WASM).

## Modules

### types.rs — AST Types
All AST node types, `Token` struct (with `inner()` accessor), `Comment`, `Comments`, `PartialEq`/`Hash` impls.

### grammar/ — Bootstrap Parser
Single-call parse via the generated `BbnfBootstrap` enum parser (`grammar/generated.rs`, checked in). `host::extract_grammar` lifts the bootstrap tree into a `ParsedGrammar`. Schema codegen for CST helpers lives in `grammar/schema/`.

### graph/ — Dependency Graph
Dependency graph construction (`deps.rs`), Tarjan SCC + topological sort (`scc.rs`), AST-level alias detection (`metadata.rs`). The IR-level FIRST/FOLLOW, dispatch, and span passes all live under `bbnf-ir::passes/sets/`.

### imports/ — Module System
`ModuleRegistry` loads transitive imports via DFS with partial-init cycle handling. `loader::load_module_graph` is the public entry. `resolve::resolve_imports_for` handles per-file import expansion, `transitive_local_deps` pulls selective transitive deps, `resolve::resolve_import_path` resolves path literals. Non-transitive: A imports B, B imports C → A doesn't see C.

### lower/ — AST → IR
`lower_to_ir` drives `BbnfBootstrapEnum → GrammarIR`. Sub-modules: `string_interner.rs` (dedup), `fn_table.rs` (host functions), `expression.rs` (node lowering + `try_specialize_map_fn`), `metadata.rs` (rule directives), `value_expr.rs` (`MapExpr` lowering).

### pipeline.rs + pipeline/ — Compilation Orchestrator
`pipeline.rs` is a thin facade over `pipeline/compile.rs`. The `compile_ast_request` entry point runs AST validation → lowering → the 16-op IR pass sequence → durable-DAG build → facts / strategy passes → backend preparation. The DAG is built exactly once per compile in `compile.rs` (currently line 430) — every `NodeId`-keyed consumer (`TypeMap`, `NodeFacts`, alt strategies) depends on it.

### backend/ — Multi-Target Code Generation
Target-agnostic driver (`driver/`) + per-target emitters (`rust/`, `ts/`, `wasm/`) behind a single `Emitter` trait. The driver walks `GrammarIR`, makes structural decisions, and calls `emit_*` methods; each backend's emitter impl block lives in `backend/<target>/emitter/mod.rs` and delegates to kind-grouped `_impl` methods on siblings. `patterns/` pre-solves delim-scan and key-dispatch per compile; `strategy/` runs CSP-driven alt/ref/repeat/seq/wrap strategy solvers; `prettify/` owns `@pretty` analysis and planning.

### generate/ — Two-Track Codegen Surface
- **Track 1 — CST helpers**: `CstSchema::from_ir(ir)` → `grammar::schema::emit::rust::generate` emits `children`, `span_text`, `identifier_text`, `walk_children`, and the visitor trait.
- **Track 2 — Backend parser code**: `BackendPreparation::from_ir(ir)` → `backend::rust::emitter::generate` emits parser functions, type definitions, dispatch tables, etc.

The two tracks are independent; the final `TokenStream` is their concatenation.

`generate/regex/` is the shared regex-to-TokenStream emitter used by all codegen paths. The tiered emission pipeline in `generate/regex/emit/` walks a pluggable pattern registry: SIMD-accelerated fast paths, negated char-class memchr, generalized scanners, HIR-based emission, and DFA-based emission.

`generate/serialize/` emits grammar-guided serializer impls.

## Conventions

- **`@token` directive**: `@token ruleName ;` marks a rule as a lexical token. `RuleMeta::is_token` carries the flag through lowering. Implies span eligibility. Uses fusion-style inlining (body inlined at call sites, enum variant preserved for `@pretty` compatibility).
- **`@debug` directive**: `@debug ruleName ;` / `@debug * ;` instruments rules for trace output. `backend/rust/trace.rs` emits `#[cfg(feature = "parser-trace")]` instrumentation for the Rust backend. `RuleMeta::debug` and `GrammarIR::debug_all` carry the directive through lowering.
- **`parse_that::regex::classify`**: HIR-based regex classification used by `lower/expression.rs` (`try_specialize_map_fn`) and by `generate/regex/` for fast-path routing. Detects `Numeric` (sign, fraction, exponent), `HexDigits`, `Identifier`, `QuotedString` structurally.
- **Fast-path regex emission**: `generate/regex/emit/` owns negated char-class memchr, SIMD whitespace, inline CSS ident/string scanners, and comma-or-whitespace `,|\s+`. Inline scanners are selected by the pattern registry, not by ad-hoc pattern strings.
- **`->` lowering**: `try_specialize_map_fn` in `lower/expression.rs` detects `Regex(numeric) -> f64` and `Regex(hex) -> user_fn` patterns and upgrades generic `FnDescriptor::Custom` to `NumberConvert`, `HexConvert`, or `Constant`. Constant detection recognizes literal expressions, numeric suffixed values, and boolean keywords.
- **Emitter trait consistency**: every backend implements exactly one `impl Emitter for XxxEmitter` block (Rust requires it). That block lives in `backend/<target>/emitter/mod.rs` and delegates each method to a `xxx_impl` sibling method. New emit kinds must be added to every backend.
- **Tests live under `tests/`**: no inline `#[cfg(test)]` modules in `src/`.
