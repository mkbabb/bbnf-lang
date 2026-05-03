# BC.W3 — Crate Dependency DAG

Date: 2026-05-03
Status: settled. Closes Phase-4 spec-depth gap H. The DAG below is the BC.W3 crate-split contract; sub-waves W3a-W3e own its execution.

## §1 ASCII dependency diagram

The arrow X → Y reads "X depends on Y". No cycles. Crate names are the post-W3 canonical names per Lock 7 (`crates/path/`, not `crates/bbnf-path/`).

```
                                  ┌─────────────────┐
                                  │  bbnf (umbrella)│  (lib.rs only; re-export shell;
                                  │  crates/core/   │   sunsets at BC.W6)
                                  └────────┬────────┘
                                           │
                  ┌────────────────────────┼────────────────────────┐
                  ▼                        ▼                        ▼
         ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
         │  bbnf-codegen   │────▶│   bbnf-parse    │────▶│  bbnf-runtime   │
         │  crates/        │     │   crates/       │     │  crates/        │
         │  bbnf-codegen/  │     │   bbnf-parse/   │     │  bbnf-runtime/  │
         └────────┬────────┘     └────────┬────────┘     └────────┬────────┘
                  │                       │                       │
                  ▼                       ▼                       ▼
         ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
         │    bbnf-ir      │     │    bbnf-ir      │     │     path        │
         │   crates/ir/    │     │   crates/ir/    │     │  crates/path/   │
         └────────┬────────┘     └────────┬────────┘     └────────┬────────┘
                  │                       │                       │
                  ▼                       ▼                       ▼
         ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
         │     egraph      │     │   parse-that    │     │   path-core     │
         │  crates/egraph/ │     │ (workspace ext) │     │ crates/path-core│
         └────────┬────────┘     └────────┬────────┘     └─────────────────┘
                  │                       │
                  ▼                       ▼
         ┌─────────────────┐     ┌─────────────────┐
         │   csp-solver    │     │   bbnf-regex    │
         │ crates/csp-     │     │ (parse-that/    │
         │ solver/         │     │  rust/bbnf-     │
         └─────────────────┘     │  regex/)        │
                                 └─────────────────┘
```

Sister leaf crates (no further deps within bbnf scope):
- `simd-scan` (consumed by `bbnf-codegen` for the `SimdScan` IR variant emit)
- `egraph-derive` (proc-macro consumed by `egraph`)
- `analysis` (consumed by `lsp` only)
- `lsp` (consumes `bbnf-parse`, `bbnf-runtime`, `analysis`)
- `bootstrap` (consumes `bbnf-parse`)
- `path-ts` (consumes `path-core`; cdylib for TS bindings)

## §2 Per-crate API contract

### bbnf-runtime (crates/bbnf-runtime/)

| Surface | Public | Private |
|---|---|---|
| Trait | `Visitor<'i, T>`, `Visit<'i, T>`, `VisitTypes` | `VisitorWalk<T>` (internal walk helper) |
| Type | `<G>Document<'i>` (per generated grammar), `<G>Value<'i>`, `<G>Arena`, `LazyValue<'a>`, `PathQuery<'i>` | `RawHandle`, `InternalCursor` |
| Function | `pointer!(Json, [...])`, `<JsonValue>::pointer(...)` (re-exported from path) | `cursor::decide_internal`, `frame::push_internal` |
| Path | `crates/bbnf-runtime/src/{lib,visitor,handle,arena}.rs`, `crates/bbnf-runtime/src/runtime/{<g>}/...` | `crates/bbnf-runtime/src/internal/...` |
| Dependency justification | bbnf-runtime is the pure-data substrate; no parsing, no codegen, no IR | depends on `path` (crates/path/, the path proc-macro consumer surface), no other bbnf-* deps |

### bbnf-parse (crates/bbnf-parse/)

| Surface | Public | Private |
|---|---|---|
| Trait | `Compile`, `Lower`, `LayoutSink` | `LoweringContextInternal`, `RecogniserDispatchTable` |
| Type | `GrammarIR`, `Layout`, `RuleId`, `RecogniserConfig`, `<G>Parser` | `LoweringFrame`, `ScannerScratch` |
| Function | `compile_grammar(source: &str) -> Result<GrammarIR, ...>`, `parse_<g>_source(...)`, `<G>Parser::parse(...)`, `<G>Parser::parse_in(input, &bump)`, `<G>Parser::parse_owned(...)` | `internal::resolve_first_set`, `internal::synth_dfa` |
| Path | `crates/bbnf-parse/src/{lib,source,parse,lower,host,pipeline}.rs`, `crates/bbnf-parse/src/parse/generated/<g>.rs` | `crates/bbnf-parse/src/internal/...` |
| Dependency justification | depends on `bbnf-runtime` because lowering reads `runtime::<G>::<G>View` for self-host BBNF reflection; depends on `bbnf-ir` for typed-IR construction; depends on `parse-that` for the BBNF self-host parser combinator surface | does NOT depend on `bbnf-codegen` (verified via `cargo tree -p bbnf-parse \| grep -c bbnf-codegen` returns 0) |

### bbnf-codegen (crates/bbnf-codegen/)

| Surface | Public | Private |
|---|---|---|
| Trait | `Emitter`, `Lowerer<B: Backend>` | `EmitterInternalContext` |
| Type | `RustLowerer`, `TsEmitter`, `WasmEmitter`, `TypedIRNode` (re-exported from `bbnf-ir::typed_ir`), `EmitOutput` | `RustEmitContext`, `TsEmitContext`, `WasmEmitContext` |
| Function | `RustLowerer::lower(typed_ir: &TypedGrammarIR) -> TokenStream`, `TsEmitter::emit(...)`, `WasmEmitter::emit(...)` | `internal::struct_direct_emit`, `internal::pratt_emit` |
| Path | `crates/bbnf-codegen/src/{lib,emitter,driver}.rs`, `crates/bbnf-codegen/src/{rust,ts,wasm}/...`, `crates/bbnf-codegen/src/optimiser/...` | `crates/bbnf-codegen/src/internal/...` |
| Dependency justification | depends on `bbnf-parse` because codegen consumes the GrammarIR that bbnf-parse produces; depends on `bbnf-ir` for typed-IR; depends on `egraph` and `csp-solver` for the optimiser substrate; depends on `simd-scan` for the SimdScan variant emission | does NOT depend on `bbnf-runtime` directly (codegen-emitted code references `bbnf-runtime` types through the `bbnf-parse` re-export channel) |

### bbnf-ir (crates/ir/) — disposition

**Stays as `crates/ir/`. Workspace-internal. Not promoted to `bbnf-ir` standalone publication.** Justification: `bbnf-ir`'s public surface (`TypedIRNode`, `Layout`, `RuleId`, `LayoutSink`) is consumed only by `bbnf-parse` and `bbnf-codegen`; no third-party consumer exists or is anticipated; the IR alphabet is still under amendment as new grammar features land. Promotion is a candidate for post-BD when the IR has stabilised across grammar additions.

| Surface | Public (within workspace) | Private |
|---|---|---|
| Trait | `Lower`, `IrPass` | `IrInternalAlloc` |
| Type | `TypedIRNode`, `IrNode`, `Layout`, `LayoutSink`, `RuleId`, `CharClassId`, `KeywordId`, `RegexId`, `HostFnRef` | `InternalNodeId` |
| Function | `passes::layout::resolve(&mut grammar_ir) -> Result<...>`, `typed_ir::lower(&grammar_ir, &layout) -> TypedGrammarIR` | `internal::node_intern_table` |
| Path | `crates/ir/src/{lib,types,typed_ir,passes,registry}.rs` | `crates/ir/src/internal/...` |
| Dependency justification | depends on `egraph` for the e-graph substrate at the optimiser's input; depends on `path-core` for path/projection types | no other workspace deps |

### path (crates/path/) — proc-macro surface

| Surface | Public | Private |
|---|---|---|
| Macro | `pointer!`, `path!` | (none — proc-macros expose no Rust API) |
| Function | (none — proc-macro crate) | `internal::macro_expand_pointer` |
| Path | `crates/path/src/{lib,macros,expand}.rs` | `crates/path/src/internal/...` |
| Dependency justification | proc-macro crate; depends on `path-core` for the path AST shared between proc-macro expansion and runtime path execution; no bbnf-runtime dep (proc-macros run at compile time) | (none) |

### path-core (crates/path-core/)

| Surface | Public | Private |
|---|---|---|
| Type | `PathSegment`, `PathQuery`, `TypedPath<G>`, `TerminalKind` | `PathInternalRepr` |
| Function | `PathQuery::execute(&document) -> Option<...>` | `internal::registry_lookup` |
| Path | `crates/path-core/src/{lib,segment,query,registry}.rs` | `crates/path-core/src/internal/...` |
| Dependency justification | leaf crate; consumed by `path` proc-macro at expansion time and by `bbnf-runtime` at execution time | no workspace deps |

### path-ts (crates/path-ts/)

| Surface | Public | Private |
|---|---|---|
| Type | (cdylib; exports C ABI) | (n/a) |
| Function | `path_ts_init(...)`, `path_ts_query(...)` (C ABI for TS bindings) | (n/a) |
| Path | `crates/path-ts/src/lib.rs` | (n/a) |
| Dependency justification | cdylib for TS / WASM consumers; depends on `path-core`; no bbnf deps | (none) |

### egraph (crates/egraph/)

| Surface | Public | Private |
|---|---|---|
| Trait | `Language`, `RewriteRule` | `EGraphInternalNodes` |
| Type | `EGraph<L: Language>`, `Class<L>`, `Id`, `Rewriter<L>` | `EClassData<L>` |
| Function | `EGraph::new()`, `eg.add(...)`, `eg.rewrite(...)`, `eg.extract(cost_fn)` | `internal::saturate_internal` |
| Path | `crates/egraph/src/{lib,graph,rewrite,language,extract}.rs` | `crates/egraph/src/internal/...` |
| Dependency justification | leaf optimisation substrate; depends on `egraph-derive` for the `Language` derive macro; consumed by `bbnf-codegen::optimiser` and by `bbnf-ir::passes` | (none) |

### csp-solver (crates/csp-solver/)

| Surface | Public | Private |
|---|---|---|
| Trait | `Constraint`, `Variable`, `Domain` | `SolverState` |
| Type | `CspProblem`, `Solution`, `Domain<T>`, `Constraint<V>` | `BacktrackingFrame` |
| Function | `CspProblem::new()`, `cp.solve()`, `cp.optimise(cost_fn)` | `internal::propagate_internal` |
| Path | `crates/csp-solver/src/{lib,problem,solver,propagate}.rs` | `crates/csp-solver/src/internal/...` |
| Dependency justification | leaf CSP substrate; consumed by `bbnf-codegen::optimiser` for cost-model decisions and by `egraph::extract` for cost-driven extraction; csc411 sibling is algorithm-evolution authoritative per `feedback_csp_always_optimize` | (none) |

### bbnf-regex (parse-that/rust/bbnf-regex/, post BC.W5b rename)

| Surface | Public | Private |
|---|---|---|
| Trait | `Pattern`, `Match` | `DfaInternalState` |
| Type | `Dfa`, `Hir`, `MatchResult` | `DfaTransitionTable` |
| Function | `Dfa::compile(hir: &Hir) -> Result<Dfa, ...>`, `Dfa::run(input, pos) -> Option<MatchResult>` | `internal::nfa_to_dfa` |
| Path | `parse-that/rust/bbnf-regex/src/{lib,hir,nfa,dfa,compile}.rs` | `parse-that/rust/bbnf-regex/src/internal/...` |
| Dependency justification | regex substrate; consumed by `bbnf-codegen::optimiser::regex_synthesise` for the RegexDfa IR variant; freezes at BC.W5 with publication candidacy | (none) |

### parse-that (workspace external; private path-dep per gap I)

| Surface | Public (within bbnf workspace) | Private |
|---|---|---|
| Trait | `Parser` (combinator), `Stream`, `Span` | `ParserInternal` |
| Type | `Combinator<O>`, `ParseError`, `BBNFCombinator` | various internal combinator structs |
| Function | combinator builders: `seq`, `alt`, `repeat`, `optional`, `pratt_with`, etc. | (none publicly exposed) |
| Path | `parse-that/rust/parse-that/src/...` | (most of the crate is internal combinator scaffolding) |
| Dependency justification | the BBNF self-host substrate; consumed by `bbnf-parse` for grammar source compilation; **PERMANENT private path-dep per gap I option (i)** | (none) |

### simd-scan (crates/simd-scan/)

| Surface | Public | Private |
|---|---|---|
| Trait | `SimdScan` | `SimdInternal` |
| Type | `Alphabet`, `ScanKind` | `NeonRegister`, `Sse4Register` |
| Function | `simd_scan_class(input, alphabet, pos)` (architecture-neutral with NEON / SSE4.2 specialisations) | `internal::neon_scan`, `internal::sse4_scan` |
| Path | `crates/simd-scan/src/{lib,neon,sse,scalar}.rs` | `crates/simd-scan/src/internal/...` |
| Dependency justification | architecture-neutral SIMD substrate; consumed by `bbnf-codegen` for the SimdScan IR variant emit; freezes at BC.W5 (workspace-internal; not yet a publication candidate) | (none) |

### bootstrap (crates/bootstrap/)

| Surface | Public | Private |
|---|---|---|
| Function | `bbnf_bootstrap_re_export()` | (none) |
| Path | `crates/bootstrap/src/lib.rs` | (none) |
| Dependency justification | re-export crate for the BBNF self-host bootstrap; consumed by xtask regen; depends on `bbnf-parse` only | (none) |

### analysis (crates/analysis/)

| Surface | Public | Private |
|---|---|---|
| Trait | `AnalysisPass` | `InternalAnalysisCache` |
| Function | `analyse_grammar(&grammar) -> AnalysisResult` | `internal::cache_lookup` |
| Path | `crates/analysis/src/lib.rs` | (currently flat; per `feedback_analysis_consolidation` analysis moves into IR passes by post-BC; analysis crate may sunset) |
| Dependency justification | LSP-shared library; consumed by `lsp` only; depends on `bbnf-parse`, `bbnf-ir` | per `project_analysis_consolidation`, eliminate analysis/ as standalone crate post-BC |

### lsp (crates/lsp/)

| Surface | Public | Private |
|---|---|---|
| Function | `lsp_main()`, `dap_main()` | (LSP server internals) |
| Path | `crates/lsp/src/{lib,bin}.rs`, `crates/lsp/src/handlers/...` | `crates/lsp/src/internal/...` |
| Dependency justification | LSP + DAP server; consumes `bbnf-parse`, `bbnf-runtime`, `analysis`; binary surface | (none) |

## §3 Re-export sunset rules

| Re-export | Where | Permanence | Sunset trigger |
|---|---|---|---|
| `bbnf::*` (umbrella `core` crate) | `crates/core/src/lib.rs` | Temporary | BC.W6: when downstream consumers migrate to direct sub-crate imports per `docs/migration/bc-core-split.md`; after BC.W6 the umbrella retains only `pub use bbnf_runtime::Visitor` (the canonical visitor entry) and `pub use bbnf_runtime::pointer` (re-exported from path); all other re-exports retire |
| `bbnf-parse::generated::<g>::*` (per-grammar) | `crates/bbnf-parse/src/parse/generated/<g>.rs` | Permanent | each generated grammar exports its `<G>Document`, `<G>Value`, `<G>Parser`; namespaced; consumers always use `bbnf_parse::generated::<g>::<Type>` |
| `bbnf-runtime::path::*` (path re-export) | `crates/bbnf-runtime/src/lib.rs` | Permanent | runtime needs to expose `pointer!`, `LazyValue`, etc.; the re-export channel is permanent |
| `bbnf::backend::*` (legacy) | `crates/core/src/lib.rs` | Sunsetted at W3d | replaced by `bbnf::codegen::*` (which re-exports from `bbnf-codegen`); the `backend` namespace retires; migration cookbook records the rename |
| `pub use bbnf::*` (BBNF aggregator privilege) | (current state) | Sunsetted at BA.W2 per BA agent surgery G05-5 | BBNF aggregator deletion lands at BA; BC inherits no aggregator privilege |

## §4 Migration notes (consumer-facing)

The migration cookbook at `/Users/mkbabb/Programming/bbnf-lang/docs/migration/bc-core-split.md` is the canonical user-facing reference. Summary of import path changes:

| Pre-W3 import | Post-W3 import | Reason |
|---|---|---|
| `use bbnf::backend::rust::Emitter;` | `use bbnf::codegen::Emitter;` (or direct `use bbnf_codegen::Emitter;`) | `backend/` namespace renames to `codegen/` and migrates to the new sub-crate |
| `use bbnf::runtime::json::JsonDocument;` | `use bbnf::generated::json::JsonDocument;` (or direct `use bbnf_parse::generated::json::JsonDocument;`) | per-grammar runtime types move to generated modules per `feedback_doc_alongside_code` and `audit/RESTART-SKETCH-2026-05-03.md:444-458` |
| `use bbnf::lower::*;` | `use bbnf::parse::lower::*;` (or direct `use bbnf_parse::lower::*;`) | `lower/` migrates into `bbnf-parse` |
| `use bbnf::path::PathQuery;` | `use bbnf::path::PathQuery;` (unchanged through umbrella; direct: `use path::PathQuery;` from `crates/path/`) | path crate name unchanged; package-name policy decision: package name remains `path`, NOT `bbnf-path` (cf. BA tranche surgery 6) |
| `use bbnf::ir::*;` | `use bbnf::ir::*;` (unchanged through umbrella; direct: `use bbnf_ir::*;` from `crates/ir/`) | bbnf-ir stays workspace-internal; no rename |

## §5 Dependency rule discipline

| Rule | Enforcement |
|---|---|
| `bbnf-parse` MUST NOT depend on `bbnf-codegen` | `cargo tree -p bbnf-parse \| grep -c bbnf-codegen` returns 0 (W3-G3 closer) |
| `bbnf-runtime` MUST NOT depend on `bbnf-parse` or `bbnf-codegen` | `cargo tree -p bbnf-runtime \| grep -E 'bbnf-parse\|bbnf-codegen' \| wc -l` returns 0 |
| `bbnf-ir` MUST NOT depend on `bbnf-parse` or `bbnf-codegen` | `cargo tree -p bbnf-ir \| grep -E 'bbnf-parse\|bbnf-codegen' \| wc -l` returns 0 |
| Sister crates (egraph, csp-solver, bbnf-regex, simd-scan) MUST NOT depend on any bbnf-* crate | `cargo tree -p egraph -p csp-solver -p bbnf-regex -p simd-scan \| grep -E 'bbnf-parse\|bbnf-codegen\|bbnf-runtime\|bbnf-ir' \| wc -l` returns 0 |
| `path-core` MUST NOT depend on any bbnf-* crate | `cargo tree -p path-core \| grep bbnf-` returns nothing |
| `parse-that` is a permanent private path-dep per gap I | `cargo metadata` shows path-dep entry; no crates.io publication for `parse-that` itself |

## §6 Workspace member count

Pre-W3 workspace: 12 members (`crates/{core,ir,analysis,lsp,bootstrap,simd-scan,path,path-core,path-ts,egraph,egraph-derive,csp-solver}`).

Post-W3 workspace: **15 members** (added: `bbnf-parse`, `bbnf-codegen`, `bbnf-runtime`; `core` slims to umbrella). Plus per-W3 sub-wave breakdown:

| Sub-wave | Action | Workspace member delta |
|---|---|---:|
| W3a | extract `bbnf-runtime` | +1 (12 → 13) |
| W3b | extract `bbnf-parse` | +1 (13 → 14) |
| W3c | extract `bbnf-codegen` | +1 (14 → 15) |
| W3d | umbrella `core` slim-down | 0 (15 → 15; core retained as shell) |
| W3e | xtask regen path update + migration cookbook | 0 (15 → 15; no new members) |

## §7 Validation checklist for BC.W3 close

- [ ] `cargo metadata --format-version 1 \| jq '.workspace_members \| length'` returns 15
- [ ] `cargo tree -p bbnf-parse \| grep -c bbnf-codegen` returns 0
- [ ] `cargo tree -p bbnf-runtime \| grep -E 'bbnf-parse\|bbnf-codegen' \| wc -l` returns 0
- [ ] `cargo check -p bbnf-parse -p bbnf-codegen -p bbnf-runtime -p bbnf-ir -p path -p path-core -p path-ts -p egraph -p egraph-derive -p csp-solver -p bbnf-regex -p simd-scan -p analysis -p lsp -p bootstrap` green
- [ ] `cargo nextest run -p bbnf-parse -p bbnf-codegen -p bbnf-runtime -p bbnf-ir -p bbnf` 100% pass
- [ ] `cargo xtask regen --check` produces byte-identical output (modulo path) to BB close artefact at `crates/bbnf-parse/src/parse/generated/`
- [ ] `docs/migration/bc-core-split.md` exists and migration tables resolve all import paths from `audit/CENSUS-2026-05-03.md` consumer-facing imports
