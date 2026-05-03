# Pass B — Agent B.1 — Inventory

Date: 2026-05-03
Lens: Exhaustive catalogue of every file in Pass B scope.
Scope source: `docs/restart/PASS-B-CODEGEN-MID.md` §Pass B Scope.

The inventory carries one row per source file; no classification
verdict is rendered (that is the synthesizer's task). `Author intent`
is the top-of-file docstring or the doc comment closest to the public
surface; stale narrative is flagged.

The directive names `crates/core/src/codegen/` as the codegen substrate;
on disk that subtree does not exist. The codegen substrate lives at
`crates/core/src/backend/` (asymmetric with `bbnf-ir`'s `passes/` —
Phase-4 surgery 22 names this). Every "codegen" reference below
resolves to `backend/`.

---

## §B.1 — Codegen substrate (`crates/core/src/backend/`)

### B.1.a — Backend root + emitter trait

| File | LOC | Public-API surface | Dependents | Invariants | Author intent |
|---|---:|---|---|---|---|
| `crates/core/src/backend/mod.rs` | 32 | re-exports `Emitter`, `types::*`, `unescape_literal`; `pub mod {driver, emitter, kernels, prettify, rust, strategy, ts, types, util, wasm}` | every backend consumer + xtask::regen | one Emitter trait + driver entry point | "Backend infrastructure: compilation driver, shared types, and emission trait" — Tranche-Y meta-language about deleted `patterns/` directory |
| `crates/core/src/backend/emitter.rs` | 566 | `pub trait Emitter` (~30 methods); per-shape default impls returning `Self::Output::default()` | rust/ts/wasm Emitter impls | every emit_* method has a default; backend overrides selectively | "The `Emitter` trait — backend-specific code emission" — fault per CENSUS §3.1: `_fallback: ...` underscore params unused at lines 96, 125, 332, 469 |
| `crates/core/src/backend/util.rs` | (small) | `unescape_literal` | hregex emitter, value-emitter | one-shot helper | unspecified |

### B.1.b — Driver subtree (`backend/driver/`)

| File | LOC | Public-API surface | Dependents | Invariants | Author intent |
|---|---:|---|---|---|---|
| `backend/driver/mod.rs` | 347 | `compile_*` driver dispatch over `IrNode` | per-shape emitters, regen | one walker, calls Emitter trait | shared driver per Lock 5 |
| `backend/driver/alt.rs` | 272 | `compile_alt`, branch projection, fallback indices | rust/ts/wasm alt emitters | byte-disjoint Alt → ByteDispatch; non-disjoint → linear | "Under tape-first emission" — narrative stale per CENSUS §1.2 |
| `backend/driver/seq.rs` | 246 | `compile_seq`, child grouping | per-backend Seq emit | one Seq decision; SeqChildGroup result | "Under tape-first emission" — stale |
| `backend/driver/repeat.rs` | 157 | `compile_repeat` | per-backend Repeat emit | repeat-shape selection | unspecified |
| `backend/driver/wrap.rs` | 143 | `compile_wrap`; transparent forwarding | per-backend Wrap emit | wrap eliding behaviour | unspecified |
| `backend/driver/reference.rs` | 171 | `compile_reference` | per-backend Ref emit | direct call to per-shape parse_<rule> | settled |
| `backend/driver/map.rs` | 72 | `compile_map`; MapExpr lowering | per-backend Map emit | host-fn invocation | settled |
| `backend/driver/node.rs` | 215 | `compile_node` (entry); IrNode dispatch | driver/* | one entry per IrNode kind | settled |
| `backend/driver/analysis.rs` | 226 | `BackendAnalysis`, `BackendPreparation`, `EffectiveBackendConfig`, `PreparedGrammar`, `TypeAnalysis`, `prepare_grammar` | every emitter | drive-side type / strategy resolution | "Tape<R>::with_capacity divisor" — stale per CENSUS |
| `backend/driver/prettify.rs` | 247 | prettify driver glue | rust prettify emitter | prettify is a separate channel | settled |

### B.1.c — Kernels (`backend/kernels/`) — generic emit primitives

| File | LOC | Public-API surface | Notes |
|---|---:|---|---|
| `backend/kernels/mod.rs` | 34 | re-exports per-kernel | settled |
| `backend/kernels/charclass.rs` | 260 | character-class emission | "legacy `emit_call` wrapper that fell back to `scan_ident`" — stale per CENSUS |
| `backend/kernels/charset_shapes.rs` | 45 | charset shape lookup | settled |
| `backend/kernels/identifier.rs` | 25 | identifier emission | settled |
| `backend/kernels/number.rs` | 33 | number emission | settled |
| `backend/kernels/balanced_wrap.rs` | 98 | balanced-wrap detection | settled |
| `backend/kernels/prefix_class.rs` | 115 | prefix-class emission | "legacy `emit_call`... `fall through to Unrecognized`" — stale per CENSUS |
| `backend/kernels/punct_ws_region.rs` | 155 | punct/ws region | settled |

### B.1.d — Strategy (`backend/strategy/`) — per-shape strategy decisions

| File | LOC | Public-API surface | Notes |
|---|---:|---|---|
| `backend/strategy/mod.rs` | 41 | re-exports | settled |
| `backend/strategy/alt_strategy.rs` | 196 | `AltStrategy::for_alt(&IrNode, &GrammarIR) -> AltStrategy` | byte-disjoint vs linear-try |
| `backend/strategy/seq_strategy.rs` | 60 | seq-shape strategy | settled |
| `backend/strategy/ref_strategy.rs` | 45 | ref-shape strategy | settled |
| `backend/strategy/repeat_strategy.rs` | 31 | repeat-shape strategy | settled |

### B.1.e — Types (`backend/types/`) — shared types

| File | LOC | Public-API surface | Notes |
|---|---:|---|---|
| `backend/types/mod.rs` | 128 | re-exports `decisions::*`; `SeqChildGroup`, `FlattenStrategy`, `ValuePlacement`, `AltBranchInfo`, `AltDispatch` | "buried under `backend/patterns/` as the lone non-shim survivor" — stale meta-language |
| `backend/types/decisions.rs` | 148 | `KeyClass`, `KeyDispatchConfig`, `BranchTaggedFlag`, etc. | "AM.3 per-branch tape surgery" — stale per CENSUS |

### B.1.f — Prettify driver shell (`backend/prettify/`)

| File | LOC | Public-API surface | Notes |
|---|---:|---|---|
| `backend/prettify/mod.rs` | (≤30) | `pub use types::*`; module aggregator | settled |
| `backend/prettify/types.rs` | 41 | `PrettyPolicy`, `PrettyRulePlan` | settled |
| `backend/prettify/plan.rs` | 194 | per-rule pretty plan | settled |
| `backend/prettify/sep_rewrite.rs` | 145 | sep rewrite | settled |
| `backend/prettify/analysis.rs` | 72 | prettify-time analysis | settled |

### B.1.g — Rust backend (`backend/rust/`)

#### B.1.g.i — Rust emitter root

| File | LOC | Public-API surface | Author intent |
|---|---:|---|---|
| `backend/rust/emitter/mod.rs` | 379 | `RustEmitter` impl block; orchestration | "Emitter trait implementation for the Rust backend" |
| `backend/rust/emitter/grammar.rs` | 468 | `emit_grammar`; per-grammar parse-entry emission | "AT.1.1: Resolve the projected payload type" — surviving doc; stale `tape` mentions per CENSUS |
| `backend/rust/emitter_types.rs` | 294 | `RustEmitCtx`, `RustEmitter` | settled |
| `backend/rust/ir_types.rs` | 372 | `BackendType` projection from `TypeDesc` | "prettify emitter; tape-first rule emission... Tranche AC.2: under tape-first" — stale |
| `backend/rust/ir_enums.rs` | (≤200) | typed-enum emission | "tape mention" per CENSUS — stale |

#### B.1.g.ii — Rust emitter shape modules

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `backend/rust/emitter/shapes/mod.rs` | 306 | shape dispatcher | settled |
| `backend/rust/emitter/shapes/scalar.rs` | 160 | scalar leaf emit | settled |
| `backend/rust/emitter/shapes/string.rs` | 217 | string-shape emit | settled |
| `backend/rust/emitter/shapes/number.rs` | 200 | number-shape emit | "TapeKind::Span leaf carries... TapeRec::PAYLOAD_F64_DIRECT_BIT" — stale per CENSUS |
| `backend/rust/emitter/shapes/object.rs` | 435 | object-shape emit | "tape mentions" |
| `backend/rust/emitter/shapes/arglist.rs` | 389 | arglist-shape emit | "tape mentions" |
| `backend/rust/emitter/shapes/unordered.rs` | 411 | unordered-shape emit | "Defensive fallback" at :288 — fault per CENSUS §10.4 |
| `backend/rust/emitter/shapes/hregex.rs` | 484 | hregex-shape emit; consumes parse-that bespoke HIR | settled |
| `backend/rust/emitter/shapes/cursor_param.rs` | 75 | cursor-param threading | settled |
| `backend/rust/emitter/shapes/substrate.rs` | 119 | substrate selection (struct-direct vs combinator) | "Lock 13 (No silent fallback)" — KEEP narrative |
| `backend/rust/emitter/shapes/alt_dispatch/mod.rs` | 173 | Alt-dispatch root | "AM.3 per-branch tape surgery" — stale |
| `backend/rust/emitter/shapes/alt_dispatch/branches.rs` | 422 | per-branch alt emit | settled |
| `backend/rust/emitter/shapes/array/mod.rs` | 514 | array-shape emit | "legacy record stream fallback" at :35 — fault per CENSUS |
| `backend/rust/emitter/shapes/array/element.rs` | 77 | per-element loop emit | settled |
| `backend/rust/emitter/shapes/dispatcher/mod.rs` | 76 | shape-dispatcher root | settled |
| `backend/rust/emitter/shapes/dispatcher/cross_shape.rs` | 338 | cross-shape dispatch | "legacy Alt-dispatch body (pre-W4)" at :118 — fault per CENSUS |
| `backend/rust/emitter/shapes/dispatcher/ref_call.rs` | 230 | ref-call dispatch | settled |
| `backend/rust/emitter/shapes/dispatcher/support.rs` | 902 | shape-dispatcher helpers | god module per CENSUS §5; "scalar fallback" narrative |
| `backend/rust/emitter/shapes/dispatcher/symbol_composition.rs` | 32 | symbol composition | settled |
| `backend/rust/emitter/shapes/flat/mod.rs` | 138 | flat shape root | "tape mention" at :24 — stale per CENSUS |
| `backend/rust/emitter/shapes/flat/struct_direct.rs` | 1033 | flat-shape struct-direct emit | god module per CENSUS §5 |
| `backend/rust/emitter/shapes/inline/structural_branch.rs` | 318 | inline structural-branch emit | settled |
| `backend/rust/emitter/shapes/keyword/mod.rs` | 59 | keyword shape root | settled |
| `backend/rust/emitter/shapes/keyword/struct_direct.rs` | 534 | keyword struct-direct emit | "Ref for now" :281; "legacy `push_leaf_with_unit()`" :85 — faults per CENSUS |
| `backend/rust/emitter/shapes/keyword/payload.rs` | 179 | keyword payload emit | settled |
| `backend/rust/emitter/shapes/pratt/mod.rs` | 47 | pratt shape root | settled |
| `backend/rust/emitter/shapes/pratt/struct_direct.rs` | 364 | pratt struct-direct emit | settled |
| `backend/rust/emitter/shapes/wrap/mod.rs` | 61 | wrap shape root | settled |
| `backend/rust/emitter/shapes/wrap/struct_direct.rs` | 622 | wrap struct-direct emit | "linear-try fallback" :198 — fault per CENSUS |

#### B.1.g.iii — Rust emitter helpers

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `backend/rust/emitter/keyword_dispatch.rs` | 212 | keyword-set dispatch helpers | settled |
| `backend/rust/emitter/path_plan.rs` | 356 | path-plan emission | settled |
| `backend/rust/emitter/precedence.rs` | 274 | precedence emit (Pratt) | settled |
| `backend/rust/emitter/profile.rs` | (≤200) | per-grammar profile emit | "tape mention" per CENSUS — stale |
| `backend/rust/emitter/regex_scan_adapter.rs` | 786 | HIR-to-DFA + transition tables | god module per CENSUS §5 |
| `backend/rust/emitter/registry_emit.rs` | 207 | StructRegistry emission | settled |
| `backend/rust/emitter/prettify/*.rs` | (≤300) | prettify emit channel | settled |
| `backend/rust/analysis/inline/{mod,visit,constraints}.rs` | 482 total | inline analysis | inline test at `mod.rs:37` — fault per CENSUS §7 |
| `backend/rust/view/named_types.rs` | 248 | view emission | settled |

### B.1.h — TypeScript backend (`backend/ts/`)

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `backend/ts/mod.rs` | 9 | aggregator | settled |
| `backend/ts/code.rs` | 78 | TS code surface | settled |
| `backend/ts/alt.rs` | 154 | Alt emit | settled |
| `backend/ts/dispatch.rs` | 105 | dispatch emit | settled |
| `backend/ts/projection.rs` | 196 | projection emit | "`declare function …` shim" :113 — fault per CENSUS |
| `backend/ts/repeat.rs` | 109 | repeat emit | settled |
| `backend/ts/ws.rs` | 34 | ws emit | settled |
| `backend/ts/emitter/mod.rs` | 338 | TsEmitter impl | settled |
| `backend/ts/emitter/grammar.rs` | 282 | per-grammar TS emit | settled |
| `backend/ts/emitter/value.rs` | 148 | value emit | settled |
| `backend/ts/emitter/leaves.rs` | 132 | leaf emit | settled |
| `backend/ts/emitter/binary.rs` | 134 | binary op emit | settled |

### B.1.i — WASM backend (`backend/wasm/`)

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `backend/wasm/mod.rs` | 11 | aggregator | settled |
| `backend/wasm/code.rs` | 118 | WASM code surface | settled |
| `backend/wasm/alt.rs` | 176 | Alt emit | settled |
| `backend/wasm/dispatch.rs` | 143 | dispatch emit | settled |
| `backend/wasm/escape.rs` | 11 | escape helper | settled |
| `backend/wasm/repeat.rs` | 120 | repeat emit | settled |
| `backend/wasm/ws.rs` | 94 | ws emit | settled |
| `backend/wasm/emitter/mod.rs` | 300 | WasmEmitter impl | settled |
| `backend/wasm/emitter/grammar.rs` | 62 | per-grammar wasm emit | settled |
| `backend/wasm/emitter/value.rs` | 120 | value emit | settled |
| `backend/wasm/emitter/leaves.rs` | 120 | leaf emit | settled |
| `backend/wasm/emitter/binary.rs` | 133 | binary op emit | settled |

**Backend totals** — 119 files, ~22,161 LOC. The Rust subtree is
~70% of that surface; TS is ~7%; WASM is ~6%; driver/strategy/types/
kernels/prettify/emitter share the remainder.

---

## §B.2 — Runtime substrate (`crates/core/src/runtime/`)

### B.2.a — Generic mechanism files

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `runtime/mod.rs` | 78 | aggregator + per-grammar `pub use <g>::*` | settled |
| `runtime/builder.rs` | 141 | `pub trait StructBuilder` | "selection between tape and struct" — narrative stale per CENSUS |
| `runtime/builder_template.rs` | 286 | `SimpleStructBuilder<V, A, C>`, `SimpleArenaSurface`, `SimpleCompound` | shared template for trivial cohort |
| `runtime/arena_template.rs` | 134 | shared arena surface | settled |
| `runtime/handle.rs` | 139 | `CompoundHandle`, `StringHandle` | settled |
| `runtime/view.rs` | 76 | `RuntimeView` trait | settled |
| `runtime/error.rs` | (≤80) | `DtaError`, `ParseErr` | settled |
| `runtime/path.rs` | 163 | `PathSegment<'a>`, `Path<'a>`, `IntoPathSegment` | duplicate of `crates/core/src/path/ir.rs::PathSegment` — fault per CENSUS §4.1 |

### B.2.b — Per-grammar runtime dirs (9 grammars)

| Grammar | Files | LOC summary |
|---|---|---|
| **bbnf/** | `arena.rs` 341, `builder.rs` 243, `document.rs` 453, `mod.rs` 51, `parse_with.rs` 120, `serialize.rs` 442, `value.rs` 96, `view.rs` 280 | 8 files, ~2026 LOC |
| **bnf/** | `arena.rs` 54, `builder.rs` 54, `document.rs` 171, `kind.rs` 55, `mod.rs` 18, `value.rs` 23, `view.rs` 64 | 7 files, ~439 LOC (trivial cohort) |
| **csv/** | `arena.rs` 55, `builder.rs` 54, `document.rs` 237, `kind.rs` 66, `mod.rs` 49, `value.rs` 57, `view.rs` 80 | 7 files, ~598 LOC (trivial cohort) |
| **css_l4/** | `arena.rs` 390, `builder.rs` 1014, `document.rs` 541, `mod.rs` 79, `parse_with.rs` 113, `value.rs` 852, `view.rs` 137 | 7 files, ~3126 LOC; god modules per CENSUS §5 |
| **css_pretty/** | 7 files | ~455 LOC (trivial cohort) |
| **ebnf/** | 7 files | ~445 LOC (trivial cohort) |
| **google_sheets/** | `arena.rs` 332, `builder.rs` 357, `document/canonical.rs` 411, `document/mod.rs` 150, `document/path_query.rs` 114, `document/view.rs` 135, `mod.rs` 56, `parse_with.rs` 114, `value.rs` 189, `view.rs` 95 | 10 files, ~1953 LOC |
| **json/** | `arena.rs` 186, `builder.rs` 382, `document.rs` 456, `mod.rs` 53, `parse_with.rs` 133, `value.rs` 121, `view.rs` 96 | 7 files, ~1427 LOC |
| **math/** | 7 files | ~467 LOC (trivial cohort) |

**Runtime totals** — 75 files, ~12,007 LOC. The trivial cohort
(BNF, CSV, EBNF, CSS Pretty, Math) totals ~2400 LOC of mechanical
duplication; the four specialised cohorts (BBNF, CSS L4, Sheets,
JSON) total ~8530 LOC of hand-written specialised logic. The
generic mechanism files total ~1077 LOC.

The `runtime/` directory has 11 immediate children mixing per-grammar
subdirs (9) with generic mechanism files (handle.rs, view.rs, path.rs,
builder.rs, builder_template.rs, arena_template.rs, error.rs, mod.rs)
— this is the archetype god directory per Lock 13 / `feedback_no_god_modules`.

---

## §B.3 — Pipeline + grammar generated output

### B.3.a — Pipeline (`crates/core/src/pipeline/` + `pipeline.rs`)

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `pipeline.rs` (file-form) | 103 | facade — `CompileTarget`, `CompileRequest`, `CompileOutput`, `CompileError`, `PipelineOptions` | "thin facade" per CENSUS §4.3 — collides with `pipeline/` directory |
| `pipeline/directives.rs` | 205 | directive parsing | settled |
| `pipeline/validate.rs` | 91 | validate stage | settled |
| `pipeline/compile/mod.rs` | 125 | compile stage entry | settled |
| `pipeline/compile/audit.rs` | 124 | audit stage | settled |
| `pipeline/compile/closure_partition.rs` | 212 | closure partition | settled |
| `pipeline/compile/pipeline.rs` | 481 | the actual pipeline | "Tape-direct ingress" :163 — stale per CENSUS |
| `pipeline/compile/target.rs` | 125 | target resolution | settled |
| `pipeline/compile/timer.rs` | 60 | per-stage timing | settled |

The `pipeline.rs` file-form module + `pipeline/` directory is a
violation of `feedback_directory_modules` per CENSUS §4.3.

### B.3.b — Generated grammar tree (`crates/core/src/grammar/generated/`)

| File | LOC | Notes |
|---|---:|---|
| `generated/mod.rs` | 35 | aggregator with `pub use bbnf::*` asymmetric per CENSUS §3.1 |
| `generated/bbnf.rs` | 21,503 | xtask emission |
| `generated/bnf.rs` | 3,290 | xtask emission |
| `generated/csv.rs` | 1,693 | xtask emission |
| `generated/css_l4.rs` | 107,138 | xtask emission |
| `generated/css_pretty.rs` | 9,021 | xtask emission |
| `generated/ebnf.rs` | 7,646 | xtask emission |
| `generated/google_sheets.rs` | 14,088 | xtask emission |
| `generated/json.rs` | 3,500 | xtask emission |
| `generated/math.rs` | 871 | xtask emission |

**Generated total** — 168,785 LOC across 9 grammar files + mod.rs (35).

Per-grammar `<g>.registry.json` sidecar files sit alongside (49,354
bbnf; 193,040 css_l4; etc.) — these are the StructRegistry cache the
emitter references at codegen.

---

## §B.4 — Optimiser sister crates

### B.4.a — `crates/egraph/`

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `egraph/Cargo.toml` | (small) | dep on `csp-solver`, `smallvec`, `rustc-hash` | settled |
| `egraph/src/lib.rs` | 152 | `Analysis`, `NoAnalysis`, `CostConfig`, `CostWeights`, `CALIBRATED_WEIGHTS`, `CspScheduler`, `DirtyDomain`, `ParentDirtyProp`, `EClass`, etc. | "deliberately domain-agnostic" |
| `egraph/src/analysis.rs` | (≤120) | `Analysis<N>` trait | settled |
| `egraph/src/cost_config.rs` | (≤80) | `CostConfig` | settled |
| `egraph/src/cost_weights.rs` | 191 | `CostWeights`, `CALIBRATED_WEIGHTS` | settled |
| `egraph/src/csp_scheduler.rs` | 368 | `CspScheduler`, `DirtyDomain`, `ParentDirtyProp` | settled |
| `egraph/src/eclass.rs` | (≤100) | `EClass` | settled |
| `egraph/src/egraph.rs` | 297 | `EGraph<N, A>` core | settled |
| `egraph/src/extract.rs` | 215 | extract pass | settled |
| `egraph/src/id.rs` | (≤50) | NodeId, ClassId | settled |
| `egraph/src/language.rs` | (≤100) | `Language` trait | settled |
| `egraph/src/rewrite.rs` | (≤150) | `Rewrite<N, A>` | settled |
| `egraph/src/scheduler.rs` | (≤200) | scheduler | settled |
| `egraph/src/unionfind.rs` | (≤100) | union-find | settled |
| `egraph/tests/saturation.rs` | 271 | saturation test | settled |
| `egraph/tests/csp_scheduler.rs` | 257 | csp-scheduler test | settled |
| `egraph/tests/egraph_basic.rs` | 171 | basic test | settled |

### B.4.b — `crates/egraph-derive/`

| File | LOC | Notes |
|---|---:|---|
| `egraph-derive/src/lib.rs` | 343 | `#[derive(Language)]` proc-macro per `feedback_derive_language` |

### B.4.c — `crates/csp-solver/`

42 source files; ~6,500 LOC across `src/`. Public surface:
- `lib.rs` 532 LOC — `Pruning`, `PropagationStrategy`, `OptimizationMode`, `SolveConfig`, builder/solver entry
- `builder/{assignment,mod}.rs` — assignment construction
- `constraint/{all_different,all_different_except,cardinality,dispatch,implication,lambda,not_equal,soft,traits}.rs` — constraint kit
- `domain/{bitset,cost_finite,finite,lattice,traits}.rs` — domain types
- `solver/{ac3,backjump,backtrack,gac_alldiff,gac_alldiff_except,local_search,monotonic,nogoods,optimize,propagate}.rs` — solving algorithms
- `puzzles/{futoshiki,sudoku}/*` — proof corpus per `feedback_readme_style`
- `py.rs` 405 LOC — PyO3 bindings (feature-gated)

Tests: `solver.rs` 1667, `lattice.rs` 985, `optimize.rs` 570, `sudoku.rs` 268.

### B.4.d — `crates/simd-scan/`

| File | LOC | Notes |
|---|---:|---|
| `simd-scan/src/lib.rs` | (≤120) | `scan_structural` entrypoint; per-arch dispatch via `is_aarch64_feature_detected!` etc. |
| `simd-scan/src/alphabet.rs` | 229 | `StructuralAlphabet`, `KernelShape` |
| `simd-scan/src/avx2.rs` | 370 | x86_64 AVX2 |
| `simd-scan/src/avx512.rs` | (≤200) | x86_64 AVX-512 VBMI2 (opt-in) |
| `simd-scan/src/compaction.rs` | (≤200) | bitmap → indices |
| `simd-scan/src/index.rs` | (≤80) | `StructuralIndex` |
| `simd-scan/src/neon.rs` | 719 | aarch64 NEON |
| `simd-scan/src/parity.rs` | 249 | quote-state |
| `simd-scan/src/scalar.rs` | (≤200) | portable fallback |
| `simd-scan/src/wasm.rs` | 433 | wasm32 SIMD |
| `simd-scan/tests/quote_parity.rs` | 189 | parity test |
| `simd-scan/tests/correctness.rs` | 181 | correctness oracle |

**Optimiser-crate totals** — 4 crates, ~16,180 LOC (including tests).
egraph: ~2400 LOC; egraph-derive: 343 LOC; csp-solver: ~6500 LOC src
+ ~3500 LOC tests; simd-scan: ~3400 LOC src + ~370 LOC tests.

---

## §B.5 — xtask

| File | LOC | Public-API | Notes |
|---|---:|---|---|
| `xtask/Cargo.toml` | 60 | bin + lib layout per AZ-IV.W0.5 | path-deps on bbnf-ir + bbnf |
| `xtask/src/main.rs` | 67 | clap CLI; one subcommand `Regen { grammar, check, staged, output }` | settled |
| `xtask/src/lib.rs` | 11 | re-export `regen` | settled |
| `xtask/src/regen.rs` | 849 | `regen::run` — manifest read, IR pipeline, emit, prettyplease format, write to disk | god module per CENSUS §5 (boundary case; single-purpose) |
| `xtask/tests/metadata_fail_closed.rs` | 120 | manifest validation test | settled |

xtask exposes ONE subcommand (Regen). No bench / check / test
subcommands — those live in workspace-level cargo.

---

## §B.6 — Generated-output budget evidence

```
$ find crates/core/src/grammar/generated -name '*.rs' | xargs wc -l
   21503 crates/core/src/grammar/generated/bbnf.rs
    3290 crates/core/src/grammar/generated/bnf.rs
  107138 crates/core/src/grammar/generated/css_l4.rs
    9021 crates/core/src/grammar/generated/css_pretty.rs
    1693 crates/core/src/grammar/generated/csv.rs
    7646 crates/core/src/grammar/generated/ebnf.rs
   14088 crates/core/src/grammar/generated/google_sheets.rs
    3500 crates/core/src/grammar/generated/json.rs
     871 crates/core/src/grammar/generated/math.rs
      35 crates/core/src/grammar/generated/mod.rs
  168785 total
```

Per-grammar registry sidecar JSON sizes (parallel evidence):

```
$ ls -la crates/core/src/grammar/generated/*.json
   49354 bbnf.registry.json
    3808 bnf.registry.json
  193040 css_l4.registry.json
   11944 css_pretty.registry.json
    2084 csv.registry.json
   28458 ebnf.registry.json
   27211 google_sheets.registry.json
    4415 json.registry.json
     359 math.registry.json
```

The 168,785 LOC distribution is heavily skewed: CSS L4 alone is ~63%
of the total, BBNF is ~13%, Sheets is ~8%. The five trivial cohort
grammars (BNF, CSV, EBNF, CSS Pretty, Math) total 22,521 LOC ~13% —
each grammar's relative output is roughly proportional to its
grammar source's rule count and shape complexity.

---

## §B.7 — Pass-B aggregate inventory

| Subtree | Files | LOC |
|---|---:|---:|
| `crates/core/src/backend/` | 119 | 22,161 |
| `crates/core/src/runtime/` | 75 | 12,007 |
| `crates/core/src/pipeline/` + `pipeline.rs` | 9 | 1,526 |
| `crates/core/src/grammar/generated/` | 10 | 168,785 |
| `crates/egraph/`, `crates/egraph-derive/` | ~17 | ~2,743 |
| `crates/csp-solver/` | 42 | ~10,000 (src+tests) |
| `crates/simd-scan/` | ~12 | ~3,770 |
| `xtask/` | 5 | 1,047 |

**Pass B grand total** — ~289 source files; ~221,000 LOC. Generated
output is 76% of that mass; runtime + codegen substrate ~16%; the
remainder is sister-crate optimiser substrate + xtask.

---

## Notes on shape-modules public API uniformity

Per Lock 13's sibling-API uniformity test: the shape modules under
`backend/rust/emitter/shapes/` carry mixed sub-API. Three patterns
co-exist:

1. Single-file shape (e.g. `scalar.rs`, `string.rs`, `number.rs`,
   `arglist.rs`, `unordered.rs`, `hregex.rs`, `cursor_param.rs`,
   `substrate.rs`, `object.rs`) — flat siblings of `mod.rs`
2. Directory-form shape (e.g. `array/`, `dispatcher/`, `flat/`,
   `inline/`, `keyword/`, `pratt/`, `wrap/`, `alt_dispatch/`) — but
   each carries different file taxonomy: `array/{mod, element}`,
   `dispatcher/{mod, cross_shape, ref_call, support, symbol_composition}`,
   `flat/{mod, struct_direct}`, `inline/{structural_branch}`,
   `keyword/{mod, payload, struct_direct}`, `pratt/{mod, struct_direct}`,
   `wrap/{mod, struct_direct}`, `alt_dispatch/{mod, branches}`.
3. The `struct_direct.rs` sub-module is a *shape-decision* boundary
   (struct-direct vs combinator), but only some shapes carry it —
   keyword, flat, wrap, pratt do; array, dispatcher, inline do not.
   The asymmetry implies the dispatch is per-shape ad-hoc rather
   than data-driven.

This is fault per Lock 13 (sibling-API divergence) and per
`feedback_no-orthogonal-codepaths` (struct_direct as a separate
codepath rather than the unified codegen).
