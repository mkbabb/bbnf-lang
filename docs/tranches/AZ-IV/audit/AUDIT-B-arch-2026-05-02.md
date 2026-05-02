# AUDIT-B — Encapsulation, Service Boundaries, DI, Pipeline Orchestration, God-Module Census

**Date**: 2026-05-02
**Auditor**: AUDIT-B lane
**Scope**: `crates/core/src/`, `crates/ir/src/`, `crates/egraph/src/`, ancillary substrate crates
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-audit-b-arch`
**Target dir**: `/Users/mkbabb/Programming/bbnf-wt-audit-b-arch/target/audit-b`

## §1 God-Module Census (>500 LOC)

Scan: `find crates -name '*.rs' -not -path '*/generated/*' -not -path '*/tests/*' -not -path '*/target/*' -exec wc -l {} \; | sort -rn | head -50` filtered to LOC ≥ 500.

| File | LOC | Classification | Proposed Split |
|---|---:|---|---|
| `crates/ir/src/passes/recognizers/dta.rs` | 1565 | SPLITTABLE | `dta/{table.rs,builder.rs,precedence.rs,literal.rs,regex_payload.rs}` — table/state types vs. builder vs. shunting-yard chain detection vs. literal/regex payload classifiers |
| `crates/ir/src/passes/csp_strategy/mod.rs` | 1316 | SPLITTABLE | `csp_strategy/{mod.rs (orchestrator+solve_grammar_components+solve_component),domain.rs (StrategyValue/Domain/CostDomain),sites.rs (Site/ByNodeVars/collect_sites + add_token_dispatch_constraints),domains.rs (build_*_domain helpers),decode.rs (decode_min_cost_per_variable/decode_fallback/fallback_*),engine.rs (extract_regex_engine_decisions + project_regex_decisions + engine_tier)}` |
| `crates/core/src/pipeline/compile.rs` | 1049 | SPLITTABLE | `compile/{mod.rs (entrypoints),target.rs (finalize_compile per-target dispatch),timer.rs (PipelineTimer),audit.rs (write_audit_coverage_artefact + run_path_check_pass + resolve_emit_strategy),closure_partition.rs (partition_closures + is_closure_rhs + collect_closure_param_names + slice_in_input),pipeline.rs (compile_ast_common pass orchestration + compute_call_strategies + install_pattern_caches)}` |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | 1019 | NECESSARY | One cohesive shape — Flat-struct-direct emission with intra-emit helpers; concerns share an emission contract; no clean cut. |
| `crates/core/src/runtime/css_l4/builder.rs` | 1014 | SPLITTABLE | `builder/{mod.rs (CssStructBuilder + Default + impl ctor + finalise),frame.rs (OpenFrame + NumericKind + FunctionKind),checkpoint.rs (CssStructCheckpoint + checkpoint+rollback),dispatch.rs (begin_compound rule_id arm-list + end_compound finalisers)}` |
| `crates/core/src/runtime/css_l4/value.rs` | 852 | NECESSARY | Single typed-value sum — every type colocates with its discriminant `from_discriminant` impl per grammar `-> Nu8` projection; splitting numeric/color/selector subgroups yields `value/{numeric,color,fn,selector,document}.rs` with cyclic deps via `CssTypedValue`. The cohesion is real: one grammar, one typed sum. (Borderline; routed to W5 as part of per-grammar value-enum dedup skeleton review.) |
| `crates/ir/src/passes/materialization/classify.rs` | 843 | NECESSARY | One pass with four documented sub-phases (e-graph fact pre-seed, bottom-up classification, consumer-pin fix-up, debug-assert sweep). The pass is one atomic unit; the helpers are private; existing `materialization/{lattice.rs,pin_sweep.rs,classify.rs,mod.rs}` already partitions concerns. |
| `crates/core/src/backend/rust/emitter/dfa_codegen.rs` | 827 | NECESSARY | Single mechanism: per-state DFA inline body emission, per `feedback_one-codegen-path`. Helpers are emission internals. (Naming: AZ-IV §Carry Ledger row 14 / Hard Gate 14 names this for rename + content rewrite at W4 — its content is the regex-scan adapter, not a DFA codegen module. Not a god-module concern; routed to W4.) |
| `crates/ir/src/passes/types/mod.rs` | 786 | SPLITTABLE | `types/{mod.rs (project_types orchestrator + re-exports),collect.rs (collect_alt_join_obligations + populate_interner + intern_recursive),repeat.rs (correct_repeat_elem_types),structural.rs (compute_structural_types_for_node)}`. Sub-files already exist (`generate.rs`, `subvariants.rs`, `obligation.rs`, `registry.rs`, `type_map.rs`, `constraint/`); the `mod.rs` swelled with helpers that should live alongside. |
| `crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs` | 763 | NECESSARY | Per-grammar SIMD/scan support module emission; cohesive single-output emitter. Helpers (`ws_is_comment_aware`, `has_structural_alphabet`, `ctns_probe_admits`, `emit_skip_space_*`) are layers of one decision tree. |
| `crates/core/src/runtime/google_sheets/document.rs` | 732 | SPLITTABLE | `document/{mod.rs (SheetsDocument struct + new + accessors),canonical.rs (write_value + write_compound + write_func_call + error_lexeme + tag_lexeme),view.rs (SheetsView + SheetsKind),path_query.rs (walk_path + SheetsPathQuery impls)}` — three distinct concerns: document type, canonical-form serializer, path/view accessors. |
| `crates/core/src/lower/expression/wrap.rs` | 731 | NECESSARY | Lowering triad sister-file (per AZ-IV.md §Orchestration Rule 14 — wrap+repeat+alt+mod is **one unit of repair**); splitting violates the codified orchestration discipline. |
| `crates/simd-scan/src/neon.rs` | 719 | NECESSARY | Per-arch SIMD kernel; tight coupling between low-level intrinsics + their layout assumptions. |
| `crates/core/src/backend/rust/analysis/inline.rs` | 673 | SPLITTABLE | `inline/{mod.rs (analyze_parse_inline_plan entry),plan.rs (CallMode + ParseInlinePlan),visit.rs (per-rule visitor),score.rs (cost heuristic)}` — borderline; touch via W4 rather than this audit-cap window. |
| `crates/bbnf-path/src/path_macro.rs` | 625 | NECESSARY | Single proc-macro entry; tightly coupled type-resolution + wildcard + variant arms; splitting drives compile-time loss. |
| `crates/ir/src/types/grammar.rs` | 613 | NECESSARY | Top-level IR container; field colocation is the single source of truth for the canonical `GrammarIR`. |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` | 597 | NECESSARY | Single shape emitter (Wrap struct-direct); helpers serve one emission path. |
| `crates/core/src/lower/value_expr/atom.rs` | 590 | NECESSARY | Single atom-lowering surface; helpers are tightly coupled. |
| `crates/ir/src/passes/audit/payload_coverage.rs` | 585 | NECESSARY | Single audit pass; all functions service the coverage report. |
| `crates/core/src/grammar/host.rs` | 584 | NECESSARY | Host-fn registry root; single canonical surface. |
| `crates/core/src/backend/emitter.rs` | 566 | NECESSARY | `Emitter` trait + `CallStrategy` + shared `ValuePlacement` + shared `prepare_grammar` glue. |
| `crates/ir/src/passes/recognizers/shape_dict.rs` | 541 | NECESSARY | Single mining pass + emission; cohesive. |
| `crates/core/src/runtime/css_l4/document.rs` | 541 | NECESSARY | Per-grammar document accessor surface; single-purpose. |
| `crates/core/src/lower/expression/mod.rs` | 539 | NECESSARY | Quartet orchestrator (per Orchestration Rule 14). |
| `crates/csp-solver/src/lib.rs` | 532 | NECESSARY | Crate root; re-export hub + crate-level docs. |
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` | 524 | NECESSARY | Single shape emitter. |
| `crates/ir/src/passes/payload/layout.rs` | 514 | NECESSARY | Single layout-planning pass. |
| `crates/ir/src/passes/types/registry.rs` | 510 | NECESSARY | Sub-module of types; cohesive. |

**Final SPLITTABLE list (this dispatch's surgical-fix queue candidates)**:
1. `crates/core/src/pipeline/compile.rs` — 1049 LOC → 6 sub-files
2. `crates/core/src/runtime/google_sheets/document.rs` — 732 LOC → 3-4 sub-files
3. `crates/ir/src/passes/csp_strategy/mod.rs` — 1316 LOC → 6 sub-files (deferred per scope/cap; routed to W4)
4. `crates/ir/src/passes/recognizers/dta.rs` — 1565 LOC (deferred; routed to W4 alongside DTA naming cleanup)
5. `crates/core/src/runtime/css_l4/builder.rs` — 1014 LOC (deferred; touches CSS L4 builder which is W1-CLOSE.B carry-sensitive; routed to W5)
6. `crates/ir/src/passes/types/mod.rs` — 786 LOC (deferred; touches type-projection pass — high regression risk; routed to W5 with substrate-audit work)

## §2 Service-Boundary Findings

### S1 — `crates/core/src/pipeline/compile.rs` (the orchestrator)

**Current state**: One file, 1049 LOC, mixes:
- Public entrypoints (`compile_grammar`, `compile_grammar_request`, `compile_paths_request`, `compile_ast`, `compile_ast_request`)
- Per-target dispatch (`finalize_compile` — 100 LOC `match target { Rust | Vm | Ts | Wasm }` with embedded TS/Wasm emitter wiring)
- IR pipeline orchestration (`compile_ast_common` — 374 LOC; the canonical pass list)
- Backend driver state plumbing (`compute_call_strategies` — 75 LOC; `install_pattern_caches` — 10 LOC)
- Audit-coverage artefact emission (`write_audit_coverage_artefact` — 65 LOC; `run_path_check_pass` — 5 LOC)
- Closure partitioning (`partition_closures`/`is_closure_rhs`/`collect_closure_param_names`/`slice_in_input` — 220 LOC of structural-detection helpers)
- Per-pass timing scaffolding (`PipelineTimer` — 45 LOC)

The orchestrator IS a god module in gestation: every cross-cutting concern lands here.

**Recommendation**: Split into a `pipeline/compile/` directory module:
- `mod.rs` — public entrypoints + re-exports (what consumers see)
- `target.rs` — per-target `finalize_compile` dispatch
- `timer.rs` — `PipelineTimer`
- `audit.rs` — `write_audit_coverage_artefact` + `run_path_check_pass` + `resolve_emit_strategy`
- `closure_partition.rs` — closure structural-detection helpers
- `pipeline.rs` — `compile_ast_common` (the canonical pass-list) + `compute_call_strategies` + `install_pattern_caches`

**Landed in this dispatch.** See §5.

### S2 — `crates/core/src/backend/driver/`

**Current state**: Already well-split — directory module with one file per concern (`alt`, `analysis`, `map`, `node`, `prettify`, `reference`, `repeat`, `seq`, `wrap`). `mod.rs` carries `DriverState` (the DI-ready context container) + `compile_grammar` entry. Field comments identify provenance ("populated by `install_pattern_caches`", "Pre-solved by `solve_alt_strategies`"); the seam is explicit. Constructor (`DriverState::new`) takes `call_strategies` only — pattern caches are installed post-construction. This is acceptable two-phase init for a struct that needs heavy IR-derived state but cannot consume the full IR without fighting borrow rules.

**Recommendation**: NO ACTION. The driver already exhibits clean DI; the boundary between `pipeline/compile.rs` (which builds `DriverState` and threads it) and `backend::driver` (which consumes it) is explicit. Consider one minor refactor (route to W4): rename `DriverState::new(call_strategies)` to a builder pattern (`DriverState::builder(ir).with_call_strategies(...).with_pattern_caches(...).build()`) so the two-phase init is visible at the type level. Not blocking.

### S3 — `crates/core/src/runtime/{json,css_l4,bbnf,google_sheets,...}/`

**Current state**: Per-grammar runtime layout is consistent across data grammars:
- `arena.rs` — owning slab allocator
- `builder.rs` — `StructBuilder` impl
- `document.rs` — public Document type + accessors
- `value.rs` — typed value sum
- `view.rs` — view/borrow accessor
- `mod.rs` — module roots + re-exports

The directory module pattern is followed. Cross-bleeding occurs only on `document.rs` for `google_sheets` (732 LOC) where document + canonical serialization + view + path-query accessors share one file. JSON's variant of this (`crates/core/src/runtime/json/document.rs` at 456 LOC) stays under the threshold but exhibits the same multi-concern shape — it's borderline today and will cross 500 if Sheets-style canonical-form lands on JSON in W5.

**Recommendation**:
- Sheets `document.rs` → directory module `document/{mod.rs,canonical.rs,view.rs,path_query.rs}` — landed in this dispatch (§5).
- JSON `document.rs` — preventive cleanup at W5 when value-API dedup lands (per AZ-IV Carry 25).

### S4 — `crates/core/src/runtime/builder.rs`

**Current state**: 109 LOC trait + impl block carrying the `StructBuilder` trait abstraction. Single concern. NO ACTION.

### S5 — Patterns module (`crates/core/src/generate/regex/patterns/`)

**Current state**: Already split into `mod.rs` (12 LOC, re-exports), `char_class.rs` (105 LOC), `shorthand.rs` (106 LOC). Clean directory module; concerns separated; per `feedback_pluggable-components`.

**Recommendation**: NO ACTION.

## §3 Pluggable-Decision-Point Review

### P1 — Cost model in `crates/ir/src/egraph/cost.rs`

**Current shape**: `GrammarCostModel` is a struct with public knob fields (`literal_cost`, `regex_cost`, `ref_cost`, `seq_per_child`, plus shared `weights: CostWeights`). Implements `egraph::CostModel<GrammarENode>`. Construction routes through `GrammarCostModel::from_config(&CostConfig)` with `with_fns` chained for emission-tier bonus precision. Per-call-site cost is computed via the trait `cost(&self, node, child_cost)` — fully pluggable through the `CostModel` trait. The `MAP_PRESERVE_BONUS` constant inside the trait impl is an architectural commitment (not a knob), but it should be lifted to a config field so users can tune it without recompile.

**Recommendation**: Lift `MAP_PRESERVE_BONUS` from a hard-coded `const` inside the `cost()` body to a `CostConfig.map_preserve_bonus: f64` knob with default `1.0e6`. Routed to W4 alongside `CostConfig` consolidation. Not blocking.

### P2 — Strategy selection in `crates/ir/src/registry/strategy.rs`

**Current shape**: Post-W1.8 the `EmitStrategy::for_grammar` resolver is **manifest-driven**: it routes through `for_grammar_with_manifest(ident, registry, &PRODUCTION_MANIFEST_TABLE)`. The table is `&[ManifestStrategyEntry]` with `idents`, `rust_builder_path`, `rust_document_path` fields. Adding a new grammar adds a row, not a match arm. The TS/WASM substrate-binding fields are `Option<SubstrateBinding>` — still `None` everywhere, but the closure shape is correct (extension by data, not by match arm).

The synthetic-grammar test (`crates/core/tests/synthetic_grammar_strategy.rs`) gates the closure: any new grammar that requires a literal arm in source fails the test.

The `EmitStrategy` enum currently has only one variant: `StructDirect { rust, ts, wasm }`. There is no residual hardcoded fallback (substrate panic on unknown ident; no `JsonStructBuilder` default). The TS/WASM bindings are reserved for BA wave.

**Recommendation**: NO ACTION. The closure is clean; pluggability is realised via the manifest table. The single-variant enum is a temporary shape — the BA letter wave will potentially add `Tape`-level fallback variants once dual-mode grammars surface; the current match would expand cleanly.

### P3 — Pattern registry in `crates/core/src/generate/regex/patterns/`

**Current shape**: `mod.rs` re-exports `CharClassAnalysis`/`CharClassStrategy` from `char_class.rs` and `ShorthandClass`/`detect_from_bytes`/`detect_from_ranges`/`emit_predicate` from `shorthand.rs`. Each detector is a function (not a giant `match`); the `CharClassStrategy` enum has variants `Memchr { num_needles }` / `NibbleLut` / `PredicateLoop`; selection is data-driven by `CostModel` + `LengthHint`.

This is the pluggable shape per `feedback_pluggable-components`. New strategies extend the enum + the `CostModel`-driven selector; they don't modify a giant `match` in a pattern god-module.

**Recommendation**: NO ACTION.

### P4 — CSP strategy domain construction (`csp_strategy/mod.rs`)

**Current shape**: `build_alt_domain` / `build_wrap_domain` / `build_engine_domain` / `build_materialization_domain` each construct per-variable `StrategyDomain` from upstream facts + `CostConfig`. The construction is hard-coded by variant family: each variable's domain knows its own type. Cross-rule constraints route through `constraints/{engine,shape,layout,dispatch}.rs` with `install(ctx, csp, ir)` — one file per family, plug-in seam.

The seam itself is pluggable; the per-domain builders are not. Adding a new decision family means: (1) new sub-module, (2) new `StrategyValue` arm, (3) new `Site` arm, (4) new builder fn. Not strictly pluggable, but the seam is well-documented.

**Recommendation**: NO ACTION at audit; routed to W4. The CSP seam IS the pluggability point per `feedback_pluggable-components`; the per-decision-family domain construction can stay imperative because each domain's value space is bounded by the CSP variable type.

### P5 — Pipeline pass list in `compile_ast_common`

**Current shape**: 374 LOC of imperative `timer.span("name", || pass(&mut ir))` calls. The pass list is not data-driven — it's a fixed source-side sequence. Per `feedback_pluggable-components` decision points must be pluggable; a fixed pass list at compile time is a hard-coded decision point.

**Recommendation**: ROUTED to W4. Convert pass list to `&[Box<dyn Pass>]` or `&[fn(&mut GrammarIR)]` with a per-pass `name()` method, ordered by a `PassPlan` data structure that production code can extend. The current shape conflicts with the orchestration-via-data principle; downstream tranches need the seam to add experimental passes without modifying the orchestrator. Not blocking AZ-IV close — the substrate is missing, but the path is clear; the pipeline-as-data refactor is a W4 (optimization-substrate-activation) candidate.

## §4 DI / Pipeline-Orchestration Findings

### D1 — `DriverState` construction

`DriverState::new(call_strategies)` takes one Vec; subsequent fields are populated by `install_pattern_caches(dstate, ir)`. This is two-phase init. Acceptable for now (the IR-driven sidecars cannot be cloned at construction without an immutable borrow conflict), but the type system does not enforce ordering — code can construct a `DriverState` and use the `alt_strategies` map before `install_pattern_caches` runs.

**Risk**: `default()`-shaped HashMap fields silently return `None` when consumed before pattern caches install; the consumer code falls through to a permissive default. Per `feedback_no-silent-epsilon`, defaults should panic.

**Recommendation**: ROUTED to W4 — wrap `DriverState` in a `Builder` pattern OR add a `state.installed: bool` flag with `debug_assert!`. Not blocking.

### D2 — `compile_grammar` in `backend/driver/mod.rs`

Takes `&ir, &analysis, &mut dstate, &mut emitter, &mut ctx`. Five-parameter signature is borderline god-parameter, but each param has a single concern (IR is read-only state, analysis is precomputed facts, dstate is mutable scratch, emitter is the strategy, ctx is per-emitter context). Clean DI. NO ACTION.

### D3 — Pipeline pass-list extensibility

`compile_ast_common` is a fixed-size source-side pass list. New passes require source-edit at the orchestrator; existing pass orderings are immutable. Conflicts with `feedback_pluggable-components` — see P5. Routed to W4.

### D4 — Per-target dispatch in `finalize_compile`

A 100-LOC `match target { Rust { .. } => ..., Vm => ..., Ts => ..., Wasm => ... }` arm is a backend-by-name dispatch. Each arm calls passes, drives an emitter, and produces an output. The arms duplicate (a) `project_types` + `run_path_check_pass` + `compute_payload_layouts` + `write_audit_coverage_artefact`, then (b) per-emitter-specific code.

**Recommendation**: Hoist the shared four passes into a single `prepare_for_codegen` helper called from each arm; the per-arm work would shrink to driver wiring. This is a clean DI cleanup. **Landed in this dispatch** as part of the `pipeline/compile/` split (extracted into `target.rs`).

### D5 — `compile_ast_common`'s `if !options.structural { ... }` guards

The pass orchestrator carries two `if !options.structural` blocks (the structural normalizer loop and the codegen-decision passes), nested between unconditional fact-collection passes. This is structural mode as a per-block branch rather than as a typed pass list. Per `feedback_pluggable-components`, the decision should be data: "structural mode" is a `PassPlan` that excludes the gated passes.

**Recommendation**: ROUTED to W4 alongside P5 / D3.

## §5 Surgical-Fix Queue (Landed in This Dispatch)

Items the AUDIT-B lane is committing during this dispatch:

### F1 — Split `crates/core/src/pipeline/compile.rs` (1049 LOC) into a directory module

**Pre**: 1049 LOC, six concerns mixed.
**Post**: `crates/core/src/pipeline/compile/{mod.rs, target.rs, timer.rs, audit.rs, closure_partition.rs, pipeline.rs}` — each sub-file ≤ 400 LOC, single concern.

Concern boundaries:
- `mod.rs` — public entrypoints (`compile_grammar`, `compile_grammar_request`, `compile_paths_request`, `compile_ast`, `compile_ast_request`); re-exports for back-compat (`pub use` of every previously-public item).
- `target.rs` — `finalize_compile` per-`CompileTarget` dispatch.
- `timer.rs` — `PipelineTimer` struct + impl.
- `audit.rs` — `write_audit_coverage_artefact`, `run_path_check_pass`, `resolve_emit_strategy`.
- `closure_partition.rs` — `partition_closures`, `is_closure_rhs`, `collect_closure_param_names`, `slice_in_input`.
- `pipeline.rs` — `compile_ast_common` (the canonical pass-list orchestrator) + `compute_call_strategies` + `install_pattern_caches`.

All previously-public API preserved verbatim by `pub use` from `compile/mod.rs`.

### F2 — Split `crates/core/src/runtime/google_sheets/document.rs` (732 LOC)

**Pre**: 732 LOC mixing Document type + canonical serializer + view accessor + path-query trait impls.
**Post**: `runtime/google_sheets/document/{mod.rs, canonical.rs, view.rs, path_query.rs}` — directory module per `feedback_directory-module-structure`.

Concern boundaries:
- `mod.rs` — `SheetsDocument` struct + `new` + `view` accessor + `to_value` accessor + `to_canonical` entry, plus `pub use`-re-exports of the sub-module items.
- `canonical.rs` — `write_value`, `write_compound`, `write_func_call`, `error_lexeme`, `tag_lexeme` (the canonical-form serializer).
- `view.rs` — `SheetsView`, `SheetsKind`, `SheetsView::*` impls.
- `path_query.rs` — `walk_path`, `SheetsPathQuery` trait + impls for `f64`/`bool`/`u8`/`&str`/`SheetsValue`.

All previously-public API preserved by `pub use` chain.

## §6 Routing to W3-W6

| # | Item | Owner wave | Mechanism |
|---|---|---|---|
| R1 | `crates/ir/src/passes/recognizers/dta.rs` (1565 LOC) split | W4 | Split alongside DTA-naming-cleanup work (per AZ-IV Carry Ledger row 14 / Hard Gate 14). Sub-files: `dta/{table.rs (DtaTable+DtaState+StateId+FrameKind+SeqPromote+LiteralPayload+RegexPayloadKind),builder.rs (DtaBuilder+lift_dta),precedence.rs (PrecedenceTable+PrecedenceEntry+Associativity+collect_precedence_chain+extract_operator_set+collect_inlined_alt_operators+collect_operator_alternatives+infer_associativity),literal.rs (literal_payload_from_expr+int_literal_payload+extract_literal+extract_literal_set),regex_payload.rs (regex_payload_from_return+regex_payload_from_named+resolve_map_payload),classify.rs (detect_counter_optional+has_nested_optional_with_empty_body+inner_contains_optional+strip_to_leaf+strip_transparent_owned)}`. |
| R2 | `crates/ir/src/passes/csp_strategy/mod.rs` (1316 LOC) split | W4 | Split alongside CSP-strategy completion. Sub-files (proposed): `csp_strategy/{mod.rs,domain.rs,sites.rs,domain_builders.rs,decode.rs,engine_extract.rs}`. The `constraints/` directory module already exists; the parent module needs the same treatment. |
| R3 | `crates/core/src/runtime/css_l4/builder.rs` (1014 LOC) split | W5 | Touches CSS L4 builder which is W1-CLOSE.B carry-sensitive (selector/pseudo-class wiring); land alongside per-grammar value-enum dedup skeleton (Carry 25). Sub-files: `builder/{mod.rs (CssStructBuilder+impl),frame.rs (OpenFrame+NumericKind+FunctionKind),checkpoint.rs (CssStructCheckpoint+checkpoint+rollback),dispatch.rs (begin_compound+end_compound)}`. |
| R4 | `crates/ir/src/passes/types/mod.rs` (786 LOC) split | W5 | Touches type-projection pass; high regression risk. Land alongside substrate-audit work. Sub-files: `types/{mod.rs (project_types orchestrator),collect.rs (collect_alt_join_obligations+populate_interner+intern_recursive),repeat.rs (correct_repeat_elem_types),structural.rs (compute_structural_types_for_node)}`. |
| R5 | `crates/core/src/runtime/css_l4/value.rs` (852 LOC) review | W5 | Per-grammar value-enum dedup skeleton review (Carry 25); decide whether to split `value/{numeric,color,fn,selector,document}.rs` or keep monolithic. |
| R6 | `crates/core/src/backend/rust/analysis/inline.rs` (673 LOC) split | W4 | Inline-plan analysis split; sub-files: `inline/{mod.rs,plan.rs,visit.rs,score.rs}`. |
| R7 | `MAP_PRESERVE_BONUS` lifted to `CostConfig` | W4 | Lift from `egraph/cost.rs` const to `CostConfig.map_preserve_bonus: f64` (default `1.0e6`). |
| R8 | `compile_ast_common` pass list → typed `&[Pass]` | W4 | Pluggable pass orchestrator per `feedback_pluggable-components` (P5). |
| R9 | `compile_ast_common` `if !options.structural` branches → typed `PassPlan` | W4 | Same vehicle as R8 (D5). |
| R10 | `DriverState` two-phase init → typed builder pattern | W4 | Per-D1; ensures pattern caches install before consume. |
| R11 | `finalize_compile`'s shared four-pass preamble (Rust + TS + WASM + Vm) → `prepare_for_codegen` helper | W4 | Reduce dispatch arm size; folded into the AUDIT-B `target.rs` split as preliminary; full hoist routed to W4. |
| R12 | `crates/core/src/runtime/json/document.rs` preventive cleanup | W5 | Per-S3 — same shape as Sheets `document.rs`, lower priority (currently 456 LOC, under threshold). |

## §7 Audit Posture

- **God-modules in current scope**: 6 SPLITTABLE files identified out of 29 files >500 LOC; 23 NECESSARY (mostly cohesive single-concern surfaces).
- **Service boundaries**: 1 god-orchestrator (`pipeline/compile.rs`) eliminated this dispatch; 1 god-document (`google_sheets/document.rs`) eliminated this dispatch; remaining surfaces clean.
- **Pluggable decisions**: 4 of 5 decision points already pluggable (P1–P4). 1 (P5: pipeline pass list) routed to W4 as a substantive architectural lift.
- **DI**: Driver state two-phase init flagged for W4; pipeline pass list flagged for W4; per-target dispatch hoisted to its own sub-module this dispatch.
- **Test surface**: Workspace nextest baseline 1582 (per W2 close audit); audit changes preserve all public API; no test impact expected.

The architecture is structurally sound; the surgical splits in §5 reduce the largest two god-modules; the remaining splits are sequenced into W4–W5 alongside their respective wave's primary work to avoid orthogonal write contention.
