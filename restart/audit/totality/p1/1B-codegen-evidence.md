---
agent: T-P1-1B-codegen-evidence
pass: T-P1
cycle: V5-SKV18-totality
cycle_self_label: SK-V18-TOTALITY-EXCAVATION
generated_at: 2026-06-01T00:00:00Z
spec_surfaces_audited:
  - restart/ARCHITECTURE.md §7.2-§7.4 (BackendShape canon, derive_backend_shape, impl status)
  - restart/ARCHITECTURE.md §10 + §10.1 (codegen + lowerers + rewrite-budget)
  - restart/locks/LOCKS.md Lock 5, Lock 10, Lock 14 (+ SK-V15/SK-V17 addenda)
  - restart/skinny/tranches/sk-v18/SPEC.md §6 (G3 un-fork emitter)
  - restart/skinny/tranches/sk-v18/research/p2/SYNTHESIS-RESEARCH.md + rA-emitter-unify.md
  - restart/skinny/tranches/sk-v18/audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md
files_audited_count: 16
divergence_count:
  implemented: 6
  unimplemented: 7
  impl_exceeds_spec: 2
  unknown: 3
  enumeration_note: "Per-row bucket map across the Spec-Claim table (16 verdict rows), the Divergences section (D1-D6), and Open Questions (U1-U3) per CH1-V4-F8, matching the 1A enumeration model. implemented(6) = the 6 clean IMPLEMENTED table rows: 5-shape canon, derive_backend_shape, cost pipeline, select_lowering, LoweredRust fail-closed, SinkOnly lowers. (The 7th nominally-IMPLEMENTED row 'Generated source committed' folds into D1's verbatim-courier divergence — its CSS arm is the `CSS_GENERATED_RS:701` const, so it is NOT counted clean-implemented.) unimplemented(7) = the 4 marker-string lowerer rows (EagerTape, OffsetTape, EventTape, CollapsedStage = D2) + the 3 DIVERGE rows (SinkOnly fixed-literal body = D3, Lock-5 RuntimeEmitterKind fork = D1, backend_shape config-carried strategy = D5). impl_exceeds_spec(2) = 'five lowerers as scaffolds' (matches the §7.4 scaffold concession) + '§10.1 rewrite-budget one REWRITE_SET wired' (D6). unknown(3) = U1 (cost model ever selects non-SinkOnly), U2 (decision_csp non-tautological), U3 (marker-string rule_plans feed any committed generated.rs). D4 (stale spec line-references) is a spec-defect documentation reconcile counted within the unimplemented/diverge surface, not a separate bucket. Sum = 6+7+2+3 = 18."
locks_amendment_candidates: 0
---

# 1B — Codegen-Layer Evidence (SK-V18 T-P1 Totality Excavation)

## Executive Summary

The 5-shape `BackendShape` canon `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
is WHOLE as a discriminator: the enum (`ir/src/lib.rs:340-346`), the `derive_backend_shape`
selector (`passes/src/lib.rs:392`), and the per-shape `select_lowering` dispatch
(`codegen/src/lower/mod.rs:18-26`) all enumerate exactly five shapes, matching Lock 10 and
ARCHITECTURE.md §7.3. The cost-model pipeline (egraph candidate gen → CSP feasibility →
extraction) is substantive, not tautological on its face. BUT the lowerer BODIES are the
divergence: four of five shapes (Eager/Offset/Event/Collapsed) emit MARKER-STRING plans via one
shared `tape_plan::render_rule` (`lower/tape_plan.rs:58`), not real Rust recursive-descent bodies
— exactly the "label-string lowerer scaffolds" §7.4 concedes. Only `SinkOnly` has a real
AST-walk (`lower/sink_only.rs:122 lower_program`), and even its rendered JSON body is
FIXED-LITERAL (`json_sink_direct.rs` render fns take `&mut String`, push hardcoded `{[",-tfn`
dispatch). The largest spec-vs-impl divergence: `RuntimeEmitterKind{CompiledLowering,RequestFacts}`
(`grammar_provider.rs:40-42`) is a JSON-vs-CSS GRAMMAR-FAMILY fork that ARCHITECTURE.md never
mentions (grep == 0) — Lock 5 forbids per-grammar emit forks, yet it is LIVE. SK-V18 G3/R-A
target its DELETE in favor of `BackendShape` dispatch; the spec already canonicalizes that axis,
so the fork is a pure impl-side divergence the totality spec absorbs by deletion.

## Spec-Claim ↔ Implementation Table

| Spec claim (path:line) | Impl (path:line) | Verdict | Note |
|---|---|---|---|
| 5-shape canon `{Eager,Offset,Event,SinkOnly,Collapsed}` (ARCH:1091-1115; LOCKS:107-108) | `ir/src/lib.rs:340-346` enum | IMPLEMENTED | Exactly five variants; matches Lock 10 search domain verbatim |
| `derive_backend_shape(grammar_ir, rule_id) -> BackendShape` at `passes::recognizers` (ARCH:1135) | `passes/src/lib.rs:329 mod recognizers` + `:392 fn derive_backend_shape` | IMPLEMENTED | Location matches; ARCH:1409 cites IR enum at `lib.rs:401-408` but enum is at `:340-346` (stale line ref) |
| Cost pipeline: egraph candidate-gen → CSP feasibility → cost extraction (ARCH:1118-1163) | `passes/src/backend_egraph.rs:36 select` (real EGraph add/union/extract) + `decision_csp.rs:16 finalize_rule` | IMPLEMENTED | Substantive surface: EGraph, CSP SAT/budget; tautology open (U2) |
| `select_lowering` dispatch on `cost.chosen: BackendShape` (ARCH:1414 cites `lower/mod.rs:17-24`) | `codegen/src/lower/mod.rs:18-26` | IMPLEMENTED | 5-arm `match cost.chosen`; zero grammar names — Lock-14-clean discriminator |
| `LoweredRust` fail-closed against cost/CSP/policy/union facts (ARCH §7.3 cost-derivation) | `lower/rust.rs:32-92 lower_to_rust` + `:112 validate_policy_facts` | IMPLEMENTED | W7 fail-closed: shape↔cost agreement, active-cost, CSP sat, per-grammar policy, substrate union all gated |
| `SinkOnly` lowers BackendIr→`SinkOnlyProgram` with field/source roster (ARCH:1447-1448) | `lower/sink_only.rs:122 lower_program` (real AST `SinkOnlyExpr` walk) | IMPLEMENTED | Substantive AST + DirectBuild roster + `policy_summary.backend_shape` |
| `EagerTape` lowering = rust_recursive_descent_body, eager source[pos] reads (ARCH:1182) | `lower/eager_tape.rs:16 → tape_plan::render_rule(Eager)` | UNIMPLEMENTED | Emits MARKER STRING `runtime_plan::EagerTapeRule … ops=N`, NOT Rust; ARCH:1202 self-concedes NOT-ADMITTED marker-string lowerer |
| `OffsetTape` lowering = EventCursor over retained offsets (ARCH:1183) | `lower/offset_tape.rs:16 → tape_plan::render_rule(Offset)` | UNIMPLEMENTED | Marker string; ARCH:1203 NOT-ADMITTED. REDRESS fence: the admissible EventCursor lowering consumes the SINGLE substrate's event stream IN-LOOP; a *retained parser-local* EventCursor (SK-V5 items 51/53, REJECT — item 51 at `skinny/REDRESS.md:742-768`, item 53 at `:784-813`; `:769-783` is item 52, a non-rejected profiling reassay carved out per CH3-V3-005, matching the EventTape sibling at `:57`) is pre-blocked. Cross-cite the 1A fence `1A-substrate-evidence.md:84` (1A-SUB-012). |
| `EventTape` lowering = EventCursor over compact event cells (ARCH:1184) | `lower/event_tape.rs:16 → tape_plan::render_rule(Event)` | UNIMPLEMENTED | Marker string; ARCH:1204 NOT-ADMITTED. REDRESS fence: same as OffsetTape — the EventTape lowering consumes the single substrate's event stream in-loop; the SK-V5-rejected *retained parser-local* EventCursor (items 51/53, `skinny/REDRESS.md:742-813`, REJECT — item 51 at `:742-768`, item 53 at `:784-813`; span widened to cover BOTH per CH3-V2-004, matching the OffsetTape sibling) must NOT be revived. Cross-cite `1A-substrate-evidence.md:84` (1A-SUB-012). |
| `CollapsedStage` = rust_caller_shim + hand NASM kernel x86-only (ARCH:1186,1206) | `lower/collapsed_stage.rs:16 → tape_plan::render_rule(Collapsed)` | UNIMPLEMENTED | Marker string; ARCH:1206 NOT-ADMITTED, x86-only, aarch64 mechanically refused (UNKNOWN-2D-05) |
| `SinkOnly` rendered body = direct typed-field writes DERIVED from grammar (ARCH:1185; G1 target) | `json_sink_direct.rs:124,251,326,497` render fns take `&mut String` only | DIVERGE | FIXED-LITERAL: hardcoded `{[",-tfn` dispatch (`:138-163`) NOT derived from `value=object\|array\|…`; rA §0 R2/G1 finding |
| Lock 5: no source-emit-per-backend, no emitter walking grammar by family (LOCKS:181; ARCH:2095) | `grammar_provider.rs:40-42 RuntimeEmitterKind{CompiledLowering,RequestFacts}` + `runtime_generator.rs:16-25` match fork | DIVERGE | GRAMMAR-FAMILY fork (JSON→emit_from_source / CSS→emit_request_facts); undocumented in ARCH (grep==0); SK-V18 G3 DELETE target |
| Five lowerers as 17-LOC scaffolds need real per-shape bodies (ARCH:1281; SK-V18 D07:1280-1282) | `lower/{eager,offset,event}_tape.rs` + `lower/collapsed_stage.rs` each 17-18 LOC | IMPL_EXCEEDS_SPEC | Spec NAMES them as scaffolds — impl matches the scaffold concession exactly, not the §7.3 prose target. (Brace-shorthand `collapsed}_tape.rs` corrected per CH1-V2-F2 — the fourth file is `lower/collapsed_stage.rs`, NOT `collapsed_tape.rs` which does not exist; `lower/mod.rs:1` declares `pub mod collapsed_stage;`.) |
| Generated source committed, xtask not proc-macro (Lock 6; ARCH:2110) | `runtime_generator.rs` const couriers + xtask regen path | IMPLEMENTED | Committed-source model holds; but CSS is verbatim `CSS_GENERATED_RS:701` const (addendum-1 verbatim-blob) |
| §10.1 rewrite-budget = 3 pools (legality/normalization/cost-driven) (ARCH:2127-2131) | `skinny/crates/passes/src/backend_egraph.rs:9` one `REWRITE_SET = sk-v15-w7-direct-sink-normalization-v1` (root-relative path per CH1-V3-F9, matching the correctly-pathed `:49`) | IMPL_EXCEEDS_SPEC | One named rewrite set wired; spec's 3-pool budget separation not realized as 3 distinct pools |
| `backend_shape` is side-table, no config-carried strategy (Lock 10:269; LOCKS side-table) | `grammar_provider.rs:32-37` carries `emitter: RuntimeEmitterKind` field | DIVERGE | `emitter` is config-carried per-profile strategy, not a cost-model output (rA §0 relocated-seam already present) |

## Divergences Catalogued

**D1 (HIGH, loc_delta ≈ −910 courier + fork-arm delete) — `RuntimeEmitterKind` grammar-family fork is undocumented + Lock-5-divergent.**
`grammar_provider.rs:40-42` defines `pub enum RuntimeEmitterKind { CompiledLowering, RequestFacts }`,
carried as `RuntimeProfileContract.emitter` (`:33`), dispatched at `runtime_generator.rs:16-25`
(`match request.profile_contract.emitter`) and again at `grammar_provider.rs:110` (CSS-exempts the
`first_unsupported()` fail-closed check). JSON routes `CompiledLowering → emit_from_source`; CSS
routes `RequestFacts → emit_request_facts` (verbatim `CSS_GENERATED_RS` const). ARCHITECTURE.md
mentions `RuntimeEmitterKind` ZERO times — it is an undocumented impl-only fork. Lock 5
(LOCKS:181; ARCH:2095) forbids "a trait-based emitter walking grammar directly" / source-emit-
per-backend duplication; this two-arm fork is the spec-vs-impl divergence the SK-V18 totality
spec must absorb. SK-V18 G3/R-A (SPEC:1070-1188; rA-emitter-unify.md §3) recommends DELETE +
dispatch on the lowered `BackendShape` — and the spec ALREADY canonicalizes that axis via
`select_lowering` (`lower/mod.rs:18`). So the fix is a PATH change, not a new spec primitive.
ANSWER to the SK-V18 lens: the spec already canonicalizes `BackendShape` dispatch; the
`RuntimeEmitterKind` fork is a pure impl-side divergence, NOT a missing spec primitive.

**D2 (HIGH, loc_delta sourced two ways — if the fold WIRES the existing engine (the intended posture) the un-fork band is `≤450 hand source/test/gate LOC` per SPEC G3 `restart/skinny/tranches/sk-v18/SPEC.md:440`; if the 4 skinny lowerers (17-LOC scaffolds) instead require REAL per-shape lowering bodies it is the intrinsic-blocked `600-1400 LOC joint decision-engine wiring envelope` per `restart/ARCHITECTURE.md:1280-1282`. The prior uncited `+400..+1200 … SK-V18 SPEC §6 budget` figure traced to NO source — grep of that SPEC for `+400..+1200`/`four real`/`per-shape bod` = 0; CH4-V2-008 corrected.) — Four of five lowerers are marker-string scaffolds, not Rust bodies.**
`lower/{eager,offset,event}_tape.rs` + `lower/collapsed_stage.rs` (the fourth file is `collapsed_stage.rs`, NOT `collapsed_tape.rs`; CH1-V2-F2/CH4-V2-008) each delegate to one shared
`tape_plan::render_rule(rule, flavor)` (`lower/tape_plan.rs:58`). The output is a structured
MARKER STRING — `runtime_plan::EagerTapeRule generated_runtime=ParserState+TapeBuilder rule=X
ops=N` with per-op lines like `eager_match_literal_hex(...) -> ParserState::match_literal`
(`tape_plan.rs:110-158`). No `source[pos]` read, no EventCursor, no Rust emitted. ARCH:1202-1206
self-concedes all four NOT-ADMITTED marker-string lowerers; ARCH:1281 names them "17-LOC
scaffolds." The §7.3 per-shape lowering-output table (ARCH:1180-1186) is the unrealized spec
target. The 5-shape CANON is whole in `lower/mod.rs`; the 5-shape BODIES are not.

**D3 (HIGH, loc_delta net ≈0..+150 — replace ~4 fixed-literal `render_*(&mut String)` push sites with a `SinkOnlyExpr` walk; SK-V18 G1) — `SinkOnly` rendered JSON body is fixed-literal, not grammar-derived.**
`json_sink_direct.rs:4 render(program: &SinkOnlyProgram)` validates the program but then calls
`render_value_dispatch(&mut out)`, `render_container_rules(&mut out)`, `render_string_rule`,
`render_utility_rules` — all taking ONLY `&mut String` (`:124,251,326,497`). The hardcoded
dispatch bytes `{[",-tfn` (`:138-163`) are NOT derived from the `SinkOnlyExpr` structural IR
(`sink_only.rs:69-96`) nor from `value = object|array|string|number|bool|null`. Only
`render_header` (`:68`) and `render_number_emitter` (`:457`) read program data. So the ONE
substantive lowerer still couriers fixed literals — the addendum-1 verbatim-blob class, just
fragmented across push_str sites (SYNTHESIS-AUDIT-OVERFIT R2/G1). SK-V18 G1 fixes this BEFORE G3.

**D4 (MEDIUM, loc_delta ≈0 — doc/spec line-reference reconcile, no source LOC) — Stale spec line-references for the five-shape enum + passes path.**
ARCH:1409 cites `skinny/crates/ir/src/lib.rs:401-408` as the enum owner; the enum is actually at
`ir/src/lib.rs:340-346`. ARCH:1410-1414 cites `passes/src/lib.rs:28-60/:387-438/:446-506`; live
`derive_backend_shape` is at `passes/src/lib.rs:392` and the choose path at `:473`. ARCH:1135
cites `passes::recognizers::derive_backend_shape` — `mod recognizers` is INLINE in `lib.rs:329`,
not a separate file. Non-semantic drift but path:line citations must reconcile in the totality fold.

**D5 (MEDIUM, loc_delta +1 — the SK-V18 R16 `+1-line PartialEq` derive at `skinny/xtask/src/regen.rs:5`, plus the PLANNED `runtime_target_rows_collapsed` co-gate, defined in SK-V18 SPEC at `restart/skinny/tranches/sk-v18/SPEC.md:247` — `rg runtime_target_rows_collapsed skinny/crates skinny/xtask` returns NO live definition; it is a spec-planned structural gate, not a current code symbol) — `RuntimeProfileContract.emitter` is config-carried strategy (relocated-seam present).**
The 7 CSS `RuntimeTarget` rows are byte-identical except decorative columns
(`profile`/`output_dir`/`output_labels`); the `emitter` field is a per-profile strategy, not a
cost-model output. rA §0 names this the "relocated-seam already in the metadata" — even after a
naive un-fork, a strategy moved into the neutral row is the textbook seam. `RuntimeTarget` today
derives only `Clone, Copy, Debug` (no `PartialEq`) at `skinny/xtask/src/regen.rs:5`
(over `pub(crate) struct RuntimeTarget` at `:6`), so the row-collapse co-gate is not yet
enforceable (SK-V18 R16 recipe pins the `+1-line PartialEq` derive at
`skinny/xtask/src/regen.rs:5`). The `runtime_target_rows_collapsed` co-gate named here is the
PLANNED SK-V18 gate at `restart/skinny/tranches/sk-v18/SPEC.md:247` (MUST be true at G3/P3, full-row
PartialEq over BOTH nested structs); it has NO live definition in `skinny/crates`/`skinny/xtask`
yet, so the name is a spec-planned symbol, not a current-code citation (CH1-V3-F12).

**D6 (LOW, loc_delta +60..+200 — split the single `REWRITE_SET` into three named legality/normalization/cost-driven pools) — §10.1 three-pool rewrite-budget not realized as three pools.**
ARCH:2127-2131 specifies `legality-rewrites` / `normalization-rewrites` / `cost-driven-rewrites`
as three budget pools with distinct legality-vs-cost discipline. `skinny/crates/passes/src/backend_egraph.rs:9` wires ONE
named `REWRITE_SET = sk-v15-w7-direct-sink-normalization-v1` (root-relative per CH1-V3-F9). The per-category pool separation is
spec-only; impl has a single normalization rewrite set inside `select`.

## Gaps / Missing Primitives

- **G-GAP-1: real per-shape lowering bodies (4 shapes).** `EagerTape`/`OffsetTape`/`EventTape`/
  `CollapsedStage` have NO Rust-emitting lowerer; `tape_plan.rs` is a single marker-string
  renderer. The §7.3 artefact-triple table (ARCH:1180-1186) has no realized counterpart. SK-V18
  D07 prices this as intrinsic-blocked if the 17-LOC scaffolds need real bodies (vs wiring).
- **G-GAP-2: grammar-derived `SinkOnly` projection.** `json_sink_direct.rs` must walk
  `SinkOnlyExpr` (Seq/Alt/RepeatLoop/DirectBuild/RegexProgram/CallRule/TapeEmit/ValueProject) to
  derive dispatch from grammar, not push fixed literals. SK-V18 G1.
- **G-GAP-3: unified grammar-agnostic emitter.** No single `render(program)` serving JSON+CSS;
  the two arms emit DIFFERENT rosters (`COMPILED_RUNTIME_FILES` 8 files vs
  `REQUEST_FACTS_RUNTIME_FILES` 5 files) hard-validated per arm (rA §0). SK-V18 G3.
- **G-GAP-4: CSS reaches `lower_to_rust`.** CSS never lowers — `emit_request_facts` couriers the
  `CSS_GENERATED_RS:701` const; the `.bbnf` is parsed only for request-identity facts
  (`grammar_provider.rs:108`), never to shape the body. SK-V18 G2 (G3's hard predecessor).
- **G-GAP-5: aarch64 `CollapsedStage` strategy.** Spec mechanically refuses CollapsedStage on
  aarch64 (ARCH:1206; `admits_collapsed_stage` x86-bound); the M5 Max target has NO collapsed-
  stage path — UNKNOWN-2D-05, requires a 2E source-backed strategy before any admission.

## Open Questions (UNKNOWN → verify_action)

- **U1: Does the cost model EVER select a non-`SinkOnly` shape for the live JSON/CSS grammars?**
  If JSON lowers to `SinkOnly` and CSS never lowers at all, the 4 marker-string lowerers are
  never exercised on a real grammar — making the "5-shape whole" claim true for the discriminator
  but vacuous for the bodies. VERIFY: instrument `derive_backend_shape` over `json.bbnf` +
  `stylesheet.bbnf` and record the chosen-shape histogram (do NOT run cargo here — T-P3 dispatch).
- **U2: Is `decision_csp::finalize_rule` non-tautological (does fact removal flip SAT/selection)?**
  Lock 10 v+1 (LOCKS:599) requires "a non-tautological CSP whose fact removal or alteration
  changes SAT/UNSAT or selection." `skinny/crates/passes/src/decision_csp.rs:151 selected_rule_count = u32::from(csp_status
  == "sat")` and `:265 assert_eq csp_status == "sat"` (root-relative per CH1-V3-F9) suggest a near-always-sat path. VERIFY at
  T-P3: mutate one grammar fact and confirm SAT/selection delta.
- **U3: Does `LoweredRust.rule_plans` (marker strings) feed ANY committed `generated.rs`, or is it
  a dead diagnostic surface?** `lower_to_rust` returns marker-string `RuleLoweringPlan.body`, but
  `emit_from_request` (JSON arm) routes through `emit_from_source`→`SinkOnlyProgram`→
  `json_sink_direct`, bypassing the tape-plan markers. VERIFY: trace whether any
  `runtime_generator` path consumes `rule_plans` into shipped output, or whether it is
  test/diagnostic-only (which would make D2 a fully-dead-code divergence).

## (1E only) Amendment-Candidate Count: 0

T-P1 1B holds no LOCKS-amendment authority and surfaces ZERO 1E amendment candidates. All
divergences resolve WITHIN the existing 16 locks + the standing SK-V15/SK-V17 addenda: the
5-shape canon, the FactStream-not-6th-shape clause, and the tape-category clause already govern
every codegen-layer finding. The `RuntimeEmitterKind` fork (D1) is a Lock-5 / Lock-14 VIOLATION
to be pruned by SK-V18 G3, not a lock to amend. The aarch64 `CollapsedStage` gap (G-GAP-5) is the
named UNKNOWN-2D-05, already lock-governed (G-Omega-gated, no 6th shape). Disposition is T-P3;
ratification is Pass Omega.
