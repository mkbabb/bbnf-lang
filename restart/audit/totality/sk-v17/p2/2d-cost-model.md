---
agent: 2D
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T23:55:00Z
t_p1_inventories_consumed: [1a, 1b, 1c, 1d, 1e, 1f]
primary_sources_cited: 11
techniques_grounded: 7
techniques_refuted: 4
prior_cycle_dispositions_folded:
  accepted:
    - CH3-2D-V2      # ACCEPT 21/21 — REVISE-2D-01 folded verbatim at 2d:74,:242-243; admits_collapsed_stage refuses aarch64; UNKNOWN-2D-05 not a defer-loop
    - CH6-V2         # 100% ACCEPT (41/41) — all 2D rows re-grounded at HEAD 91b6893b0
  rejected: []
  revised:
    - CH1-2F-01-RESIDUAL  # V3 fold — FOLD-2D-05 (2d:186) shared the alphabet.rs:118 anchor for the StructuralAlphabet MANIFEST grounding; struct + rich-alphabet fields resolve at :19-37 (verified live: struct at :19, fields :19-37; :118 is KernelShape::select ONLY). Re-anchored :19-37 for the alphabet-as-data manifest; :118 retained for KernelShape::select. Anchor-precision only; claim true and grounded elsewhere; zero orphan.
  v2_revised_carried_forward:
    - CH1-2D-01      # V2 — 2d:64,:66,:288 — simdjson "builds ONE tape" re-anchored to arXiv:1902.08318; :1203 kept for sonic lineage note only
    - REVISE-2D-01   # V2 — CH4 2d:69 — SPEC §9 host-block (:851-852) made PRIMARY frame before Lemire-2023 ICPP cite
    - CH3-2D         # V2 — 2d:69 framing-precedence — asmjson refuted-route host-block precedence
    - CH5-V1-003     # V2 — 2d:206-207 (shared, owned by 2f-F6) — named live coupling-site arena.rs:47 StructRegistry::compound_kind_for_layout
  first_cycle_additions:
    - T2D17-SUBSTRATE-MANIFEST-NOT-6TH-SHAPE
    - T2D17-COST-SELECTS-INTO-UNIFIED-TAPE
    - T2D17-LAZY-VALUEREF-IS-PROJECTION-NOT-SHAPE
    - T2D17-DERIVE-BACKEND-SHAPE-DEFENSIBILITY
    - T2D17-NEON-CLASSIFIER-IS-LOCK16-PRIMITIVE-NOT-SHAPE
    - T2D17-FIELDSOURCE-FENCE-COST-INVARIANT
    - T2D17-AARCH64-COLLAPSEDSTAGE-UNKNOWN-2D-05
locks_amendment_candidates: 3
master_head: 91b6893b0
sk_cycle: SK-V17-totality-T-P2
fold_target: crates/core (SK-V18 fold), skinny = proven engine
host_close_route: Apple-M5-Max-aarch64
prior_2d_extended: restart/audit/totality/p2/2D-cost-model.md (SK-V15 V2, literature base reused)
first_hygiene_action_folded: CH1-V5-001 VERIFIED-RESOLVED-ON-DISK at master 91b6893b0 (1b:12+1b:97 carry enumerated `collapsed_stage}.rs`; grep ',collapsed}' in {1a,1b,1e}=0; collapsed_stage.rs exists, collapsed_tape.rs does not — residual T-P1 REVISE discharged, no 2D fold required)
---

## Executive Summary

T-P2 2D designs the **greater-spec fold of the SKINNY-proven tape/NEON/projection
model**, grounded on the T-P1 six-divergence excavation (master `91b6893b0`). The
cost-model row carries the single load-bearing 2D design question of divergence **D**:
the proven flat unified tape (skinny SoA `Tape<'input>`,
`skinny/crates/runtime/src/tape/mod.rs:94`; core AoS `TapeRec`,
`crates/core/src/runtime/tape/record.rs:103`) must be absorbed into the 5-shape
`BackendShape` canon — and the verdict, by the **LAC-1E-14 precedent already in the
V1 spec** (`restart/locks/LOCKS.md:100-109`), is that the unified tape is the
**substrate the five shapes project into, NOT a 6th `BackendShape`**. The five shapes
(`EagerTape`/`OffsetTape`/`EventTape` retain a queryable document; `SinkOnly` does not;
`CollapsedStage` fuses mask-state for AVX-512-class x86) are the per-rule *projection
modes*; the tape is the substrate-manifest carrier under `substrate_target ∈
{local_temp_only, existing_tape, direct_sink, admitted_fact_output}`
(`LOCKS.md:121-122`). The lazy `ValueRef<G>` (skinny `tape/mod.rs:175`,
`value_from_ref` `grammars/json/value.rs:143`) is the *materialization plane over the
substrate*, equally not a shape. The cost model — `derive_backend_shape`
(`skinny/crates/passes/src/lib.rs:392`) routing `backend_egraph::select` (`:498`) →
`decision_csp::finalize_rule` (`:499`) — is defensible as the published
equality-saturation → CSP-feasibility → cost-extraction pipeline class, and selects a
*projection mode into the one tape*, never a second substrate. The AoS↔SoA convergence
is a Lock-1 single-encoding fold question (not a shape question); the NEON
`select_classifier(alphabet)` is a Lock-16 grammar-neutral primitive feeding the cost
model a scan-cost fact, not a shape. No 6th shape; no new substrate; no parallel
directive (G-Omega gated). First hygiene action CH1-V5-001 VERIFIED RESOLVED-ON-DISK at
master `91b6893b0` (`1b:12`+`1b:97` carry enumerated `collapsed_stage}.rs`; `grep ',collapsed}'`
in `{1a,1b,1e}`=0); residual T-P1 REVISE discharged, no 2D fold required.

## Technique Grounding Table

| spec-claim / T-P1-divergence-id | primary source cited | grounded / refuted / partial | bbnf-specific note |
|---|---|---|---|
| `T2D17-DERIVE-BACKEND-SHAPE-DEFENSIBILITY`: per-rule shape selection is candidate-gen → non-destructive rewrite → CSP feasibility → cost extraction, not a fixed P1-P8 cascade. | Tate, Stepp, Tatlock, Lerner, "Equality Saturation: a New Approach to Optimization", POPL 2009 (<https://www.cs.cornell.edu/~lerner/papers/popl09.html>); Willsey et al., "egg: Fast and Extensible Equality Saturation", POPL 2021; Fraser/Hanson/Proebsting, "Engineering a Simple, Efficient Code Generator Generator" (BURG), LOPLAS 1992, DOI 10.1145/151640.151642; Google OR-Tools CP-SAT (<https://developers.google.com/optimization/cp/cp_example>). | grounded as technique class; the live skinny pipeline matches the shape (`skinny/crates/passes/src/lib.rs:415` `choose_backend_shape`, `:498` egraph select, `:499` CSP finalize); the P1-P8 cascade survives as diagnostic vocabulary (`ARCHITECTURE.md:1165-1176`). | The selector lives ONLY in skinny (`grep -rn derive_backend_shape crates/` = 0; 1b BSHAPE17-002). The SK-V18 fold WIRES it into core atop the single `EmitStrategy::StructDirect` (`crates/ir/src/registry/strategy.rs:104`); dependent crates `egraph`+`csp-solver` already present (root `Cargo.toml:2`). |
| `T2D17-COST-SELECTS-INTO-UNIFIED-TAPE`: the cost model selects a per-rule *projection mode into the one tape*, never a second substrate. | Langdale & Lemire, "Parsing Gigabytes of JSON per Second", VLDB Journal 2019 (arXiv:1902.08318, <https://arxiv.org/abs/1902.08318>) — simdjson stage-2 builds ONE tape (the external "builds ONE tape" claim is grounded *here*, the paper, not at any local ARCH row); the shapes are reading modes over it. | grounded | Skinny's `LayoutFacts.backend_shape ∈ {5 shapes}` (`ARCHITECTURE.md:1075`,`1088`) selects how the rule projects; the proven SoA `Tape` is the single carrier (`tape/mod.rs:94`). Maps to `substrate_target=existing_tape` (`LOCKS.md:121`); a shape selecting a NEW substrate is the Lock-1 violation the cost model must reject in CSP (`ARCHITECTURE.md:1131-1133`). |
| `T2D17-SUBSTRATE-MANIFEST-NOT-6TH-SHAPE`: the unified tape (AoS or SoA) is a substrate-manifest category, NOT a 6th `BackendShape`. | V1-spec precedent: LAC-1E-14 FactStream 5th substrate category (`restart/locks/LOCKS.md:100-109`); ARCHITECTURE FactStream-not-6th-shape (`:1796`,`:1803`,`:2141`). | grounded (by spec precedent) | The exact LAC-1E-14 reasoning transfers: FactStream is a *substrate-manifest classification*, not a shape variant; the tape is the *substrate the shapes project into* (`ARCHITECTURE.md:1088` "the five ways the substrate may project"). The proven flat tape therefore folds as the **carrier under the 5 shapes**, NOT as `BackendShape::FlatTape`. A 6th shape remains G-Omega gated (`LOCKS.md:107-109`). |
| `T2D17-LAZY-VALUEREF-IS-PROJECTION-NOT-SHAPE`: the lazy `ValueRef<G>` value-API is the unified materialization plane *over* the substrate, not a shape. | Li et al., "Mison: A Fast JSON Parser for Data Analytics", VLDB 2017 (<https://www.microsoft.com/en-us/research/publication/mison-fast-json-parser-data-analytics/>) — consumer-known lazy projection; lazy materialization is a read strategy, not a parse substrate. The sonic-rs lazy-value *lineage note only* is local-anchored at `ARCHITECTURE.md:1203` (the `OffsetTape` "event-cursor over retained offsets — sonic-rs lazy-value lineage" row); the external lazy-projection technique itself is grounded in the Mison/sonic-rs published record, NOT at :1203. | grounded | Proven: `ValueRef<'doc,'input,K,G>` (`tape/mod.rs:175`), `value_from_ref` zero per-node heap alloc (`grammars/json/value.rs:143`). The retain-vs-not axis is captured by the *shapes* (`EagerTape`/`OffsetTape`/`EventTape` retain queryable doc; `SinkOnly` does not — `ARCHITECTURE.md:1097-1108`); `ValueRef<G>` is the LAZY read over the retaining shapes. The fold lifts per-grammar EAGER value enums (`css_l4/value.rs:414`) to the one grammar-parametric projection (1d SK17L-002). |
| `T2D17-NEON-CLASSIFIER-IS-LOCK16-PRIMITIVE-NOT-SHAPE`: the shared NEON `select_classifier(alphabet)` is a Lock-16 grammar-neutral primitive feeding the cost model a scan-cost fact, not a shape and not a substrate. | Langdale & Lemire VLDB-J 2019 (stage-1 structural classification is a primitive feeding stage-2); Lock 16 NEON allowlist + manifest (`LOCKS.md:478`,`:607`). | grounded | `select_classifier(alphabet:&[u8;64])` (`skinny/crates/bbnf-simd/src/dispatch.rs:42`) / `scan_structural(input,&StructuralAlphabet)` (`crates/simd-scan/src/lib.rs:80`) is already grammar-general (1b BSHAPE17-009; alphabet-as-data — the `StructuralAlphabet` manifest at `crates/simd-scan/src/alphabet.rs:19-37`, the only grammar datum — 8 grammars wired). It produces a transient `Vec<u32>` index — `substrate_target=local_temp_only` or `existing_tape` (index IS the tape, `ARCHITECTURE.md:1088`). It contributes a `scan_cost` cost-fact to the model; it is NEVER a `BackendShape`. |
| `T2D17-FIELDSOURCE-FENCE-COST-INVARIANT`: the `StructLayout`/`FieldSource` projection walk is a compile-time cost-emission, NOT a per-leaf runtime registry lookup. | Measured refutation: 28-65×/983×/10583× (`SPEC.md:793-795`); simdjson's flat-tape access pattern avoids per-node pointer chasing (Langdale & Lemire VLDB-J 2019). | grounded (fence) | `begin_compound(&StructLayout)` takes a pre-resolved layout reading only `layout.rule_id & 0x1F` (`crates/core/src/runtime/tape/mod.rs:185-186`). A naive per-leaf `StructRegistry::layout(rule)` (`struct.rs:331`) re-opens the worst regression. The cost model must price the projection walk at compile time; runtime carries the resolved layout by reference. **0-LOC fence; CRITICAL if violated.** |
| `T2D17-AARCH64-COLLAPSEDSTAGE-UNKNOWN-2D-05`: aarch64-NEON absorbs under the four LLVM shapes' scan-leaf FFI; aarch64 `CollapsedStage` is the spec-named open unknown. | **PRIMARY FRAME**: asmjson is host-blocked per the SPEC §9 barred-candidate list — "asmjson collapsed-stage FSM (x86, host-blocked)" (`restart/skinny/tranches/sk-v17/SPEC.md:851-852`, in §9 `:782` the route ledger): a barred, refuted x86-only route, never a latent aarch64 candidate. The literature cite is diagnostic x86 architecture-pressure ONLY: asmjson AVX-512 (Lemire 2023 ICPP), Sneller "Branchless Code With AVX-512" (<https://sneller.ai/blog/branchless-code-avx-512/>). | refuted on aarch64 (host-blocked SPEC §9); diagnostic-only on x86; aarch64 analogue UNKNOWN | `CollapsedStage` is `target.arch==x86`+`avx512bw`+`Entry(_)` (LAC-2D-06; `ARCHITECTURE.md:1206`); aarch64 mechanically refused. SK-V17 NEON sits under the four LLVM shapes' scan-leaf FFI (`ARCHITECTURE.md:1284`), NOT in CollapsedStage. The aarch64 candidate is UNKNOWN-2D-05 (`ARCHITECTURE.md:1206`,`:1279`), needing a 2E source-backed strategy before admission. No aarch64 re-derivation in this pass. |

## §2 Candidate / Fold Enumeration (load-bearing)

Each fold candidate: **shape** (what the design is) · **T-P1-divergence antecedent**
(which excavation row demands it) · **grammar-neutral verdict** (Lock 14) · **lock
surface** (which locks bind it). The candidate set is the fold of the SKINNY-proven
cost/tape/projection model into the V1 spec; the discriminating design choice is D.

### FOLD-2D-01 — Tape-as-substrate is a substrate-manifest category, NOT a 6th BackendShape (the load-bearing D verdict)

- **Shape.** The proven flat unified tape (AoS `TapeRec` 16-byte / SoA `Tape` SoA) is
  folded as the **substrate-manifest carrier** the five `BackendShape` variants project
  into, classified by `substrate_target ∈ {local_temp_only, existing_tape, direct_sink,
  admitted_fact_output}` (`LOCKS.md:121-122`). It is **NOT** a new `BackendShape::FlatTape`
  / `BackendShape::UnifiedTape` variant. The 5-shape search domain
  `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` is preserved verbatim
  (skinny enum `ir/lib.rs:340`; `all_backend_shapes()` `cost.rs:333`).
- **T-P1-divergence antecedent.** Divergence **D** (1b BSHAPE17-001/003/005; 1e
  D-1E-SKV17-04; 1d SK17L-006): "BackendShape 5-shape canon must absorb tape-as-substrate
  … propose, do NOT silently add a 6th." Divergence **A** (1a tape AoS↔SoA; 1e
  D-1E-SKV17-01): the AoS↔SoA convergence is the substrate question the manifest resolves.
- **Grammar-neutral verdict.** GROUNDED grammar-neutral. The substrate carries no grammar
  policy (`Tape`/`ValueRef`/`TapeBuilder` are grammar-agnostic, 1d split-table); the
  shapes are per-rule projection modes selected by the cost model from grammar-derived
  facts (`ARCHITECTURE.md:1138-1140`). The classification transfers to CSS/Sheets/BBNF by
  construction (alphabet-as-data; FactStream precedent already covers CSS,
  `LOCKS.md:110-112`).
- **Lock surface.** Lock 1 substrate-union (`LOCKS.md:75`) — exactly-one-encoding
  post-fold, the tape IS the union; Lock 1 v+1 LAC-1E-14 (`:100-109`) — the directly
  governing precedent (5th category = substrate-manifest, NOT 6th shape); Lock 10 5-shape
  canon (`:107-109`,`:599`) — the 5-variant search domain held verbatim; a 6th shape is
  G-Omega gated. **This is the FOLD-2D row that discharges the D divergence without a 6th
  shape.**

### FOLD-2D-02 — The cost model selects a per-rule projection mode INTO the one tape

- **Shape.** `derive_backend_shape(grammar_ir, rule_id) -> BackendShape`
  (`skinny/crates/passes/src/lib.rs:392`) is wired into core as the per-rule selector,
  routing `backend_egraph::select` (`:498`) → `decision_csp::finalize_rule` (`:499`) →
  cost extraction. Its output is the *projection mode* (which of the 5 shapes), and every
  `BackendExpr`/rewrite/extraction declares `substrate_target` (`ARCHITECTURE.md:1129-1133`);
  the e-graph rejects any plan whose `substrate_target` is not one of the four admitted
  values (LAC-2D-06). The selector replaces the single-variant `EmitStrategy::StructDirect`
  (`crates/ir/src/registry/strategy.rs:104`) whole-grammar binding with the per-rule
  cost decision.
- **T-P1-divergence antecedent.** Divergence **D** (1b BSHAPE17-002/003): the cost
  selector lives only in skinny; core carries one `EmitStrategy::StructDirect` variant. The
  fold WIRES the selector (egraph+csp-solver present in core `Cargo.toml:2`), 600-1400 LOC
  joint envelope (BSHAPE17-002 ⊕ 003, non-additive).
- **Grammar-neutral verdict.** GROUNDED grammar-neutral, with a binding gate: the CSP/cost
  facts MUST carry zero grammar names (`json_*`/`css_*` forbidden — refuted in the SK-V15
  2D as the `csp_named_grammars` tautology, prior 2D §Refuted; `decision_csp.rs:116-124`).
  The selector consumes generated FIRST/follow + layout + host + recovery + output mode +
  cost facts (`ARCHITECTURE.md:1271`), all grammar-as-data.
- **Lock surface.** Lock 10 Decision-Engine clause (`LOCKS.md:599`) — needs ≥1 asserted
  rewrite with nonzero work, a non-tautological CSP, grammar-neutral facts, gate-consumed
  selection; Lock 1 v+1 substrate_target manifest (`:117-127`) — extraction rejects
  non-admitted `substrate_target`; Lock 14 (`:603`) — generic crates carry no grammar
  branches.

### FOLD-2D-03 — Lazy ValueRef<G> is the unified materialization plane over the substrate (not a shape)

- **Shape.** The one grammar-parametric `ValueRef<'doc,'input,K,G>` projection
  (`skinny/crates/runtime/src/tape/mod.rs:175`), read by `value_from_ref`
  (`grammars/json/value.rs:143`, zero per-node heap alloc), is the unified materialization
  plane. The fold retargets the existing `@generated by xtask regen-*` value generator
  (1d SK17L-002; `crates/core/src/runtime/{json,css_l4}/value.rs:1`) to emit `ValueRef<G>`
  instead of the per-grammar EAGER typed enums (`CssTypedValue` `css_l4/value.rs:414`). The
  retain-vs-not axis is already carried by the *shapes* (`EagerTape`/`OffsetTape`/`EventTape`
  retain a queryable document, `SinkOnly` does not — `ARCHITECTURE.md:1097-1108`); the lazy
  read is the cost-cheapest materialization over the retaining shapes.
- **T-P1-divergence antecedent.** Divergence **C** (1b BSHAPE17-007; 1d SK17L-002/003; 1e
  D-1E-SKV17-02): per-grammar eager value enums → grammar-parametric `ValueRef<G>`; the
  eager `OpenFrame` builders (`css_l4/builder.rs:16` 817 LOC; `json/builder.rs:9` 231 LOC)
  are the AZ-IV-pre-blocked DELETION target. preserve-rich-ast (B): the lazy projection
  must reach byte-equal parity, never flatten the typed AST for speed.
- **Grammar-neutral verdict.** GROUNDED grammar-neutral by construction; JSON-WITNESSED +
  CSS first-mover (1d JSON-empirical/grammar-neutral split). A CSS-only generator that
  cannot re-emit JSON FAILS CH2 (`ARCHITECTURE.md`; SPEC `:62`,`:557`). The `FieldSource`
  walk IS the BackendRule recipe but MUST be compile-time (FOLD-2D-06 fence).
- **Lock surface.** Lock 1 (`LOCKS.md:75`) — one materialisation surface, one Visitor;
  AZ-IV eager value tree pre-block (118×, SPEC `:791`) — the eager builders are
  fold-deletion, never carry-forward; Lock 14 (`:603`) — value API stays a regen output
  (Lock-14 HONOURED; hand-written per-grammar would be the violation).

### FOLD-2D-04 — AoS↔SoA convergence is a Lock-1 single-encoding fold, decided by cost not by shape

- **Shape.** Core's 16-byte AoS `TapeRec` (`record.rs:103`, const-asserted 16-byte/align-4
  `:120-121`) and skinny's SoA `Tape` (`tape/mod.rs:94`, six members) are both Lock-1-admitted
  offset tapes. The fold names the SINGLE post-fold encoding; the choice is a measured
  layout-cost decision (cache-line packing, scan-emit write width, `ValueRef` cursor stride),
  NOT a `BackendShape` choice. A dual AoS/SoA end-state is a Lock-1 violation
  (`LOCKS.md:75` "parallel substrates are dead"), not a tree-local option.
- **T-P1-divergence antecedent.** Divergence **A** (1a tape AoS↔SoA; 1d SK17L-001/U-SK17L-001;
  1e D-1E-SKV17-01): proven SoA vs fold-target AoS; both admitted; the convergence is the
  SK-V18 open question. Core mod-doc admits "AoS first … later SoA split"
  (`crates/core/src/runtime/tape/mod.rs:6-9`).
- **Grammar-neutral verdict.** GROUNDED grammar-neutral; the tape encoding carries no
  grammar policy in either shape (`push_plain_offset` is one branchless u32 write,
  `assembler.rs`). The encoding decision is a host-arch layout cost (2E-adjacent: NEON store
  width, cache-line residency), grammar-invariant.
- **Lock surface.** Lock 1 (`LOCKS.md:75`,`:118-127`) — exactly-one-encoding closure
  (U-SK17L-002 / CH5-S8 obligation); Lock 15 i-cache/cache residency budget (the AoS 16-byte
  record vs SoA column packing is a residency-cost decision). 200-600 LOC SK-V18 (1d
  SK17L-001 / 1e D-1E-SKV17-01). **This is NOT a shape; the cost model picks the projection
  mode, this fold picks the carrier encoding.**

### FOLD-2D-05 — The shared NEON classifier is a Lock-16 primitive feeding a scan-cost fact

- **Shape.** `select_classifier(alphabet:&[u8;64])` / `scan_structural(input,&StructuralAlphabet)`
  (`skinny/.../dispatch.rs:42`; `crates/simd-scan/src/lib.rs:80`) is a Lock-16 grammar-neutral
  primitive. The grammar datum is the **`StructuralAlphabet` manifest** — the rich-alphabet
  struct (`singletons`/`digraph_mask`/`digraph_pairs`/`quote_classes`) defined at
  `crates/simd-scan/src/alphabet.rs:19-37` (the alphabet-as-data carrier; CH1-2F-01-RESIDUAL
  re-anchor — :118 is **only** `KernelShape::select`, the data-driven lowering picker, verified
  live at HEAD `91b6893b0`). In the cost model it contributes
  a `scan_cost` fact (classification throughput per the supplied alphabet); it never *is* a
  shape and never *retains* state across calls. Its transient `Vec<u32>` index is the tape's
  `offsets` (index IS the tape, `ARCHITECTURE.md:1088`) → `substrate_target=existing_tape`,
  or `local_temp_only` when not retained.
- **T-P1-divergence antecedent.** Divergence **E** (1b BSHAPE17-009; 1d SK17L-008; 1e
  D-1E-SKV17-06): NEON is already grammar-general across 8 grammars (impl-exceeds-spec); the
  spec's JSON-scanner narrative must absorb the alphabet-parametrised shared form (0-LOC
  narrative fold). Scope-reconcile multi-arch `crates/simd-scan` against the proven
  aarch64-only set (architecture pressure, not a defect).
- **Grammar-neutral verdict.** GROUNDED grammar-neutral; alphabet-as-data is the Lock-14
  vehicle (the only grammar datum). The scan-cost fact the model consumes carries the
  alphabet cardinality / digraph count, not a grammar name.
- **Lock surface.** Lock 16 primitive-manifest (`LOCKS.md:607`) — scalar-reference +
  checkasm parity, aarch64 hardware gate, same-wave consumer (the cost model row);
  Lock 1 v+1 ELEVATION (`:137-149`) — NO cross-call classifier-state retention
  (`retention_lifetime=retained-across-call-boundary` is the REJECT class); aarch64-only
  (no x86/AVX/SVE, SPEC `:806`).

### FOLD-2D-06 — The FieldSource/StructLayout projection walk is a compile-time cost-emission (the regression fence)

- **Shape.** The cost model prices the `FieldSource{TypedLeaf,BranchTag,SeqPosition,
  RepeatElement,RuleReference}` walk (`crates/ir/src/registry/struct.rs:84`) at **compile
  time**, baking the resolved projection into the generated parser body. `begin_compound`
  takes a pre-resolved `&StructLayout` reading only `layout.rule_id & 0x1F`
  (`crates/core/src/runtime/tape/mod.rs:185-186` — grep-zero `StructRegistry` in that file,
  verified live: the tape path is fence-clean today); a naive per-leaf
  `StructRegistry::layout(rule)` (`struct.rs:331`) re-opens 28-65×/983×/10583×. **The LIVE
  coupling-site the fence severs is present-tense at `crates/core/src/runtime/bbnf/arena.rs:47`**
  — `StructRegistry::compound_kind_for_layout(layout)` in the eager arena path (verified at
  master `91b6893b0`). FOLD-2D-03's deletion of the eager `OpenFrame` builders severs this
  live wire; the fence is not abstract prose, it names the wire it cuts. (Shared CH5-V1-003
  manifest edit owned by 2f-F6; this row cross-references the same live coupling-site.)
- **T-P1-divergence antecedent.** Divergence **F** (1b BSHAPE17-006/007; 1d SK17L-004;
  1e D-1E-SKV17-03): the StructRegistry/FieldSource fence keeping the AZ-IV indirection
  pre-blocked. Plus the Lock-2 `StructLayout`→canonical-name 960-site drift (1e
  D-1E-SKV17-05; `grep -rn StructLayout crates/`=960).
- **Grammar-neutral verdict.** GROUNDED grammar-neutral; the projection walk is the
  BackendRule recipe, grammar-as-data, resolved once. The fence is a cost INVARIANT, not a
  per-grammar branch.
- **Lock surface.** Lock 1 (`LOCKS.md:75`) — no per-leaf indirection in the hot path
  (pre-block SPEC `:793-795`); Lock 10 cost model — the projection cost is a compile-time
  extraction, never a runtime registry deref; Lock 2 — `StructLayout`→`Layout`/`LayoutFacts`
  canonical-name reconciliation (`LOCKS.md:160`). **0-LOC fence; CRITICAL/regression if
  violated.**

### FOLD-2D-07 — aarch64 CollapsedStage stays UNKNOWN-2D-05; NEON absorbs under the four LLVM shapes

- **Shape.** `CollapsedStage` remains `target.arch==x86`+`avx512bw`+`Entry(_)`
  (LAC-2D-06; `ARCHITECTURE.md:1206`); the aarch64 candidate is the spec-named UNKNOWN-2D-05,
  needing a 2E source-backed strategy before any admission. SK-V17 aarch64-NEON sits under the
  four LLVM shapes' scan-leaf FFI (`ARCHITECTURE.md:1284`) — the cost model never selects
  `CollapsedStage` on aarch64. The `admits_collapsed_stage` predicate mechanically refuses
  aarch64 (LAC-2D-06).
- **T-P1-divergence antecedent.** Divergence **D** (1b BSHAPE17-005; 1d SK17L-006; 1e
  D-1E-SKV17-04): CollapsedStage x86-pinned; aarch64 = UNKNOWN-2D-05; absorb NEON without a
  6th shape, no aarch64 CollapsedStage re-derivation. The asmjson FSM is host-blocked per the
  SPEC §9 barred-candidate list FIRST — "asmjson collapsed-stage FSM (x86, host-blocked)"
  (`SPEC.md:851-852`, the §9 route ledger `:782`) — so no T-P3 reader treats it as a latent
  aarch64 route; the AVX-512 literature is diagnostic x86 pressure only.
- **Grammar-neutral verdict.** GROUNDED grammar-neutral (the canon holds for all grammars);
  CollapsedStage is the only shape that bifurcates to hand-written NASM
  (`ARCHITECTURE.md:1186`,`:1284`), and that path is x86-only diagnostic.
- **Lock surface.** Lock 10 5-shape canon (`LOCKS.md:599`) — all-five gate over exactly the
  five shapes, no 6th; Lock 16 v+1 close-state (`:506-513`,`:607`) — AVX-512 literature is
  x86 architecture-pressure ONLY, cannot close aarch64; ARCH-3A-D11 architecture-pressure
  boundary (`ARCHITECTURE.md:1275-1282`).

## Architectural Assertions Defended

| assertion | defence | SK-V18 fold rule |
|---|---|---|
| The unified tape is the substrate the 5 shapes project into, not a 6th shape. | LAC-1E-14 FactStream precedent: a new admitted-product category is a *substrate-manifest classification*, NOT a `BackendShape` variant (`LOCKS.md:100-109`); ARCHITECTURE `:1088` "the five ways the substrate may project". | Fold the proven flat tape as the carrier under the 5 shapes; classify by `substrate_target`; never add `BackendShape::FlatTape`. A 6th shape is G-Omega gated. |
| The cost model selects per-rule INTO the one tape; a shape selecting a new substrate is rejected in CSP. | Every `BackendExpr` declares `substrate_target ∈ {local_temp_only,existing_tape,direct_sink,admitted_fact_output}`; e-graph extraction rejects non-admitted values (LAC-2D-06; `ARCHITECTURE.md:1129-1133`; `LOCKS.md:117-127`). | Wire `derive_backend_shape` (skinny `passes/lib.rs:392`) into core; replace whole-grammar `EmitStrategy::StructDirect` with the per-rule cost decision; egraph+csp-solver present (`Cargo.toml:2`). |
| Lazy `ValueRef<G>` is the materialization plane over the retaining shapes, not a shape. | The retain-vs-not axis IS the shape axis (`EagerTape`/`OffsetTape`/`EventTape` retain; `SinkOnly` does not, `ARCHITECTURE.md:1097-1108`); `ValueRef<G>` (`tape/mod.rs:175`) is the lazy read over the retaining shapes (Mison/sonic-rs lineage). | Retarget the existing regen generator to emit `ValueRef<G>`; delete the eager `OpenFrame` builders; preserve-rich-ast byte-equal parity. |
| The NEON classifier is a Lock-16 primitive feeding a scan-cost fact. | `select_classifier(alphabet)` is grammar-general (alphabet-as-data, 8 grammars wired, 1b BSHAPE17-009); transient index = `local_temp_only`/`existing_tape`; no cross-call state (Lock 1 v+1, `LOCKS.md:137-149`). | The cost model consumes a `scan_cost` fact (alphabet cardinality), not a grammar name; the classifier is admitted under Lock 16 with scalar-ref + checkasm; never a shape, never a substrate. |
| The FieldSource/StructLayout walk is a compile-time cost-emission. | `begin_compound(&StructLayout)` reads `layout.rule_id & 0x1F` only (`tape/mod.rs:185-186`); a per-leaf `StructRegistry::layout` re-opens 28-65×/983×/10583× (SPEC `:793-795`). | Price the projection at compile time; bake into the generated body; runtime carries the resolved layout by reference. 0-LOC fence; CRITICAL if violated. |

## Architectural Assertions Refuted

| refuted assertion | why refuted (literature + measurement) | consequence for the fold |
|---|---|---|
| The proven flat tape should fold as a 6th `BackendShape` (e.g. `FlatTape`/`UnifiedTape`). | The tape is the *substrate the shapes project into*, not a projection mode (`ARCHITECTURE.md:1088`). The LAC-1E-14 precedent already resolved the identical class for FactStream: a new admitted-product category is a substrate-manifest classification, NOT a 6th shape (`LOCKS.md:100-109`). A 6th shape is G-Omega gated (`LOCKS.md:107-109`,`:599`; SPEC `:806` second-substrate block names "sixth `BackendShape`"). | The D divergence is discharged by FOLD-2D-01 (substrate-manifest category), never by a shape addition. Any T-P3 proposal that adds a 6th shape silently is a CH5/CH3 REJECT. |
| The lazy `ValueRef<G>` projection is a new `BackendShape`. | Materialization-laziness is a *read strategy over a retaining shape* (Mison consumer-known projection; sonic-rs lazy-value), orthogonal to the retain-vs-not shape axis already encoded in the 5 shapes. | `ValueRef<G>` folds as the value-API plane (FOLD-2D-03), not as a shape; the shape stays one of the five. |
| A zero-rule e-graph / tautological CSP / `perf_cost:0` proves the cost model selects the shape. | Prior 2D §Refuted (SK-V15): `backend_egraph.rs:65-67` runs 0 rewrite rules; `decision_csp.rs:53-83` self-selects then accepts all; `lib.rs:571-587` assigns every candidate `perf_cost:0` (carried at master `91b6893b0` per 1b BSHAPE17-002 surface). A candidate set is not a derivation proof (Lock 10 clause, `LOCKS.md:599`). | The fold WIRES the selector but the selector is inert without ≥1 asserted rewrite + non-tautological CSP + measurement-bearing extraction; the fold cost is the 600-1400 LOC activation, not the enum. |
| AVX-512 CollapsedStage (asmjson/Sneller) can close the cost model on aarch64 / M5 Max. | asmjson requires AVX-512BW; Sneller's technique is AVX-512 mask-register branchlessness — neither is aarch64 (ARCH-3A-D11, `ARCHITECTURE.md:1275-1282`; Lock 16 v+1 close-state `LOCKS.md:506-513`). | aarch64 CollapsedStage stays UNKNOWN-2D-05 (FOLD-2D-07); the cost model never selects it on aarch64; x86 CollapsedStage rows are diagnostic pressure only. |

## Open Research Questions

| UNKNOWN | verify_action |
|---|---|
| UNKNOWN-2D-05 (carried): What is the aarch64 CollapsedStage analogue, if any? | The aarch64 source set is CONCRETE and already grounded — Arm A64 ISA manual FSM-dispatch lineage + Lemire 2026 `svmatch_u8` post + Validark 2024 (the set 2b's primitive-vocabulary dossier grounds; per 2E U-2E-04 the bounded refutation is: NEON has no AVX-512-mask branchless-FSM analogue, so the x86 CollapsedStage technique does not port). This is NOT a defer-loop (CH6-V1-R01 reconciled): the verify_action names the concrete refuting sources, it does not punt to "a future cycle". CollapsedStage stays a 5-shape candidate with x86 diagnostic evidence only; aarch64 admission mechanically refused (`ARCHITECTURE.md:1206`,`:1279`; LAC-2D-06). |
| UNKNOWN-2D-S17-01: Does the post-fold single tape encoding resolve to SoA (proven) or AoS (core)? | T-P2/T-P3 name the convergence target by a measured layout-cost decision (cache-line residency, scan-emit write width, `ValueRef` cursor stride) under Lock 15; a dual end-state is a Lock-1 violation, not an option (U-SK17L-002 / CH5-S8). |
| UNKNOWN-2D-S17-02: Which cost facts must the model carry to select the projection mode without overfit? | Populate `perf_cost`, scan_cost (alphabet cardinality), materialization_bytes, capacity, static_size, generated_LOC, parity availability from row-local facts; fail closed on grammar-named (`json_*`/`css_*`) or broadcast/stale evidence (prior 2D LAC-2D-02/03; `LOCKS.md:599`). |
| UNKNOWN-2D-S17-03: Does the `EmitStrategy::StructDirect` whole-grammar binding fold cleanly into the per-rule cost selector? | T-P3 maps StructDirect as the SinkOnly/struct-builder arm of the 5-shape canon (1b BSHAPE17-003); the 9-row `PRODUCTION_MANIFEST_TABLE` re-keys, regen-gated; 600-1400 LOC joint envelope (BSHAPE17-002 ⊕ 003, non-additive). |

## LOCKS-AMENDMENTS-CANDIDATE

| candidate | target | proposed amendment | supporting evidence |
|---|---|---|---|
| LAC-2D-S17-01 | Lock 1 / Lock 10 substrate-manifest | The proven flat unified tape (AoS or SoA) is folded as a **substrate-manifest carrier** under `substrate_target=existing_tape`, NOT a 6th `BackendShape`. The 5 shapes are projection modes into the one tape; the tape is the substrate they project into. Mirrors LAC-1E-14 (FactStream 5th category, not 6th shape). | `LOCKS.md:75`,`:100-109`,`:121-122`; `ARCHITECTURE.md:1088`; skinny `Tape` `tape/mod.rs:94`; core `TapeRec` `record.rs:103`; 1b BSHAPE17-001/005, 1e D-1E-SKV17-01/04, 1d SK17L-001/006. |
| LAC-2D-S17-02 | Lock 10 / cost model substrate-target gate | `derive_backend_shape` selects a per-rule projection mode INTO the existing tape; every candidate/rewrite/extraction declares `substrate_target` and the e-graph rejects any plan whose target is not `{local_temp_only,existing_tape,direct_sink,admitted_fact_output}`. A shape selecting a NEW substrate is a CSP-INFEASIBLE plan, not a tree-local choice. | `LOCKS.md:117-127`,`:599`; `ARCHITECTURE.md:1129-1133`; skinny `passes/lib.rs:392`,`:498`,`:499`; LAC-2D-06. |
| LAC-2D-S17-03 | Lock 16 / cost model scan-fact | The shared NEON `select_classifier(alphabet)` contributes a grammar-neutral `scan_cost` fact (alphabet cardinality / digraph count) to the cost model; it is a Lock-16 primitive, never a `BackendShape`, never a retained substrate; transient index = `local_temp_only`/`existing_tape`; no cross-call state. | `LOCKS.md:137-149`,`:607`; `skinny/.../dispatch.rs:42`; `crates/simd-scan/src/lib.rs:80`, `alphabet.rs:19-37` (StructuralAlphabet manifest — the alphabet-as-data carrier; `:118` is `KernelShape::select` only); 1b BSHAPE17-009, 1d SK17L-008. |

No further candidates. Scanned axes: BackendShape enum cardinality (5, held); cost-model
pipeline class (egraph→CSP→extraction, grounded); substrate-target manifest (4 values, no
5th tape-target needed — tape IS `existing_tape`); lazy-ValueRef-as-plane (not a shape);
NEON-classifier-as-primitive (not a shape); FieldSource compile-time fence (cost invariant);
aarch64 CollapsedStage (UNKNOWN-2D-05, carried).

## Source Index

Primary external sources (all real, verifiable): Tate/Stepp/Tatlock/Lerner POPL 2009
equality saturation; Willsey et al. POPL 2021 egg; Fraser/Hanson/Proebsting LOPLAS 1992
BURG (DOI 10.1145/151640.151642); Google OR-Tools CP-SAT documentation; Li et al. VLDB 2017
Mison; Langdale & Lemire VLDB Journal 2019 simdjson (arXiv:1902.08318) — the "builds ONE
tape" external claim is grounded in this paper, not via any local ARCH anchor; the sonic-rs
lazy-value note is local-anchored separately at `ARCHITECTURE.md:1203` (lineage note only).
asmjson AVX-512 (Lemire 2023 ICPP); Sneller "Branchless Code With AVX-512". Literature base extended from the
prior-totality 2D dossier (`restart/audit/totality/p2/2D-cost-model.md`, SK-V15 V2).

Primary local sources (re-anchored live at master `91b6893b0`): the six T-P1 inventories
`restart/audit/totality/sk-v17/p1/{1a..1f}-*.md` + `hardening/HARDENING-T-P1-SKV17-CONVERGED.md`;
`restart/skinny/tranches/sk-v17/SPEC.md`; `restart/ARCHITECTURE.md` (§7.3 BackendShape
`:1088-1206`, FactStream-not-6th-shape `:1796`,`:1803`,`:2141`, bifurcation `:1284`,
architecture-pressure boundary `:1275-1282`); `restart/locks/LOCKS.md` (Lock 1 `:75`,
LAC-1E-14 `:100-116`, substrate-manifest `:117-127`, ELEVATION `:137-149`, Lock 10 `:599`,
Lock 14 `:603`, Lock 16 `:607`); skinny `ir/lib.rs:340`, `cost.rs:333`, `passes/lib.rs:392`,
`runtime/src/tape/mod.rs:94`,`:175`, `grammars/json/value.rs:143`, `bbnf-simd/src/dispatch.rs:42`;
core `crates/core/src/runtime/tape/{mod.rs:185,record.rs:103}`, `crates/ir/src/registry/{strategy.rs:104,struct.rs:84,202,313,331}`,
`crates/simd-scan/src/lib.rs:80`,`alphabet.rs:19-37` (StructuralAlphabet manifest) `:118` (`KernelShape::select`), root `Cargo.toml:2`.
