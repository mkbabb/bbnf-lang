---
agent: 2A
pass: T-P2-research
cycle: V3
generated_at: 2026-05-29T20:00:00Z
subject: greater-spec-fold (SKINNY-proven unified-tape/lazy-view/StructLayout-projection + aarch64-NEON → V1 totality spec)
t_p1_inventories_consumed: [1a, 1b, 1c, 1d, 1e, 1f]
t_p1_converged: restart/audit/totality/sk-v17/p1/hardening/HARDENING-T-P1-SKV17-CONVERGED.md
t_p1_locked_sha: 445925167154de73540e3ea3283d0170371de790
master_head: 91b6893b0b61d1c3213d02afe4ec62f22c16ae38
live_reverify_at_head: true   # every load-bearing anchor re-greped live at 91b6893b0 this cycle: StructLayout=960, LayoutFacts crates/=0, TapeStructBuilder = 3 files {tape/mod.rs, tape/record.rs, tests/tape_substrate.rs} (ONLY the test wires it outside runtime/tape/ — production grep-zero confirmed dead), value_from_ref=0 in crates/core json/value.rs + present skinny json/value.rs:143 (16-byte AoS record.rs / proven SoA tape/mod.rs:94-100 + ValueRef :175), OpenFrame json/builder.rs:9 + css_l4/builder.rs:16, begin_compound reads layout.rule_id & 0x1F mod.rs:185-186, arena.rs:47 StructRegistry::compound_kind_for_layout LIVE, FieldSource struct.rs:84 / layout() :331, select_classifier dispatch.rs:42, scan_structural simd-scan/lib.rs:80, StructuralAlphabet alphabet.rs:19-37 (struct+rich-alphabet at :19), eq-set fan 87 LOC / 8 distinct NEON intrinsics / 27 calls, table_64 line-3 scalar delegate, admits_collapsed_stage ARCH:1206
primary_sources_cited: 9
techniques_grounded: 7
techniques_refuted: 2
prior_cycle_dispositions_folded:
  accepted:   # V2 returned 8/8 ACCEPT for 2A across CH1/CH4/CH5/CH6 (and CH2/CH3/CH7 carried no 2A REVISE) — all six folds + two refutations + three LACs converged
    - FOLD-2A-A-flat-tape-adoption
    - FOLD-2A-B-eager-openframe-retirement
    - FOLD-2A-C-lazy-valueref-plane
    - FOLD-2A-D-tape-as-substrate-manifest-not-6th-shape
    - FOLD-2A-E-neon-classifier-lock16-entry      # was V1 CH4-2a-001 REJECT + CH5-V1-004 REVISE; V2 ACCEPT — carried unchanged
    - FOLD-2A-F-structregistry-fieldsource-fence  # was V1 CH5-V1-003 REVISE; V2 ACCEPT — carried unchanged
    - refuted-row-1-collapsedstage-aarch64
    - refuted-row-2-json-scanner-framing
  rejected: []   # 0 fold deleted across V1→V2→V3
  revised: []    # ZERO open REVISE carried into V3 — V2 closed all six V1 non-ACCEPTs (CH4-2a-001, CH5-V1-003, CH5-V1-004, CH2-V1-R3, CH6-V1-V03, CH7-003) with live-verified folds; V2 CHALLENGE returned no new 2A REVISE
  first_cycle_additions: []
  v3_precision_sharpenings:   # CH1-lane precision nits surfaced against 2A anchors — honest sharpenings re-executed at HEAD, NO verdict change
    - TAPESTRUCTBUILDER-GREP-PRECISION   # prior "grep-zero outside runtime/tape/" → precise: 1 site, the TEST crates/core/tests/tape_substrate.rs; production runtime grep-zero (dead) confirmed, the verdict (UNWIRED) is unchanged and now exactly stated
    - EQSET-INTRINSIC-COUNT-PRECISION    # prior "12 NEON intrinsics" → precise live count: 8 DISTINCT NEON intrinsic names {vld1q_u8,vceqq_u8,vandq_u8,vorrq_u8,vdupq_n_u8,vget_low_u8,vget_high_u8,vaddv_u8} across 27 total intrinsic CALLS in the 87-LOC body; still the only real NEON Layer-1 body (verdict unchanged)
  cross_cutting_anchor_confirmations:   # items the V2 frontmatter flagged as touching 2A's anchors — re-verified clean at HEAD, no 2A edit required
    - CH1-2F-01-RESIDUAL-CONFIRMED       # LAC-2F-FOLD-03 re-anchor target is alphabet.rs:19-37 (the anchor LAC-2A-SKV17-02 already uses); 2A's LAC-2A-SKV17-02 cites crates/simd-scan/src/alphabet.rs:19-37 — already correct, no 2A change; the residual was a 2F-owned re-anchor
    - CH6-V1-V03-CONFIRMED               # the independent admits_collapsed_stage ARCH:1206 mechanical anchor is carried at FOLD-2A-D :205-213 (defence-in-depth beyond the LAC-1E-14 precedent) — re-verified live at ARCH:1206
locks_amendment_candidates: 3
---

## Executive Summary

2A designs the GREATER-SPEC fold: how the SKINNY-PROVEN
flat-tape + lazy `ValueRef<G>` projection + `StructLayout`-routed materialization +
shared aarch64-NEON `select_classifier(alphabet)` model generalizes into the V1
totality spec — NOT a JSON-engine grounding (that was the skinny S-P2), but the
totality architecture fold the SK-V18 implementation must be directed by. The
landscape rests on six T-P1-LOCKED divergences (CONVERGED 97.4%, SHA `445925167`,
re-anchored live at HEAD `91b6893b0`). The fold is monotonic: skinny-proven →
`crates/core` (the fold target), never the reverse. The central design move:
`crates/core` adopts the proven SoA `Tape` + lazy `ValueRef<G>` and RETIRES the
eager `OpenFrame` builders (`json/builder.rs:9` 231 LOC, `css_l4/builder.rs:16` 817
LOC) — the SK-V17-EXCAVATED MATERIALIZATION divergence (divergence B, eager
`OpenFrame`, SK-V17 SPEC `:791`). The dead `TapeStructBuilder` is wired ONLY by a
test (`crates/core/tests/tape_substrate.rs`) — production runtime grep-zero,
confirming it sits dead while the eager builders carry the live substrate. Three
distinct facts are kept un-conflated (CH7-003): (a) the JSON *recognizer* is
measured >SOTA-vs-sonic (RESULTS 51/51, the only >SOTA-witnessed plane); (b) the
materialization gap is a CODE-SHAPE divergence (eager `OpenFrame`, NOT a benchmarked
CSS deficit — lightningcss is UNMEASURED-PENDING, SPEC `:207`); (c) full typed-AST
parity with lightningcss is an SK-V18 strict-equality GATE
(`assert_lightningcss_strict_equality` SPEC `:98`), not a property held at this pass.
The load-bearing architectural verdict, grounded against the LAC-1E-14 `FactStream`
precedent: the flat tape is NOT a 6th `BackendShape`; it is the SUBSTRATE itself —
the thing the five shapes project FROM — and the AoS↔SoA convergence is a
substrate-encoding question under Lock 1's exactly-one-encoding closure, not a shape
addition. The shared NEON classifier vocabulary folds as a Lock-16 primitive-manifest
entry (grammar-neutral, scalar-ref + checkasm). The `StructRegistry`/`FieldSource`
fence keeps the AZ-IV indirection (28-65×/983×/10583×) pre-blocked: the `FieldSource`
walk is compile-time projection-emission, never per-leaf. Two spec assertions are
REFUTED by the proven path: §7.3's aarch64-`CollapsedStage` framing (mechanically
refused) and the §7.3 narrative's JSON-scanner framing (the classifier is already
alphabet-parametric).

## Technique Grounding Table

| Spec-claim / T-P1-divergence-id | Source cited (proven path / spec / literature) | Grounded / Refuted / Partial | bbnf-specific note |
|---|---|---|---|
| **A** — flat tape is the adoptable substrate (T-P1 §A AoS↔SoA) | SK-V17 SPEC §0.1.2 `:46-53` "the EXISTING `Tape`/`ValueRef`/`TapeBuilder` is the only substrate (Lock 1)"; proven SoA `Tape<'input>` `skinny/crates/runtime/src/tape/mod.rs:94-100` (six members: `source`, `offsets:Vec<u32>`, sparse `flag_cursors:Vec<u32>`/`flag_values:Vec<u8>`, `payloads:PayloadArena`, `id`); sonic-rs lazy-value lineage (ARCH `:1203`) | **grounded** | The proven tape (ONE `offsets: Vec<u32>`, sparse position-keyed flags, no class-column) is JSON-witnessed >SOTA (RESULTS 51/51 strict A/GO > sonic). `crates/core` carries the SHAPE of the substrate already (16-byte AoS `TapeRec` `record.rs`, `TapeStructBuilder` `tape/mod.rs`) but UNWIRED — production grep-zero, exercised ONLY by `tests/tape_substrate.rs`. The fold adopts the proven SoA encoding or proves AoS parity, exactly one survives. |
| **B** — eager `OpenFrame` is the retirement target (T-P1 §B) | live eager `OpenFrame` enum `crates/core/src/runtime/json/builder.rs:9` (231 LOC `JsonStructBuilder`); `CssStructBuilder` `OpenFrame` god-module `css_l4/builder.rs:16` (817 LOC, six `pending_*` Vecs `:74-79` + `pending_value:Option` `:71`); AZ-IV 118× pre-block SK-V17 SPEC `:791` | **grounded** | The eager builders ARE the live `crates/core` substrate while `TapeStructBuilder` sits dead (test-only). They are the AZ-IV-pre-blocked fold-DELETION target, not a carry-forward. SK-V18 wires the tape consumer (the SK-V17 SPEC directs skinny to prove it first). |
| **C** — lazy `ValueRef<G>` is the unified materialization plane (T-P1 §C) | proven `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `skinny/crates/runtime/src/tape/mod.rs:175`; `value_from_ref<'doc,'input:'doc>` `grammars/json/value.rs:143` (zero per-node heap alloc); ONE `BackendRule`-walking generator SK-V17 SPEC §0.1.3 `:54-72` | **grounded** | `crates/core` has NO `ValueRef`/`value_from_ref` (grep=0 in `json/value.rs`); CSS value is eager `CssTypedValue<'p>` `css_l4/value.rs:414`. The divergence is the EAGER materialization shape, not the `@generated` provenance (Lock 14 honoured — `value.rs:1` carries `@generated by xtask regen-*`). The fold retargets the existing generator to emit lazy `ValueRef<G>` (preserve-rich-ast). |
| **D** — tape is the substrate, NOT a 6th BackendShape (T-P1 §D) | LAC-1E-14 `FactStream` precedent `restart/locks/LOCKS.md:100-116` (5th substrate-manifest CATEGORY, explicitly "NOT a 6th BackendShape variant"); 5-shape canon held `:107-108` (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`); ARCH §7.3 substrate-union `:1088` "if structural offsets are retained, the structural projection IS the tape" | **grounded** | The flat tape is the SUBSTRATE the five shapes project FROM — `OffsetTape`/`EventTape`/`EagerTape` retain a queryable tape, `SinkOnly` does not. Proposing a 6th shape is FORBIDDEN (G-Omega gated, Lock 10 v+1). The tape folds as a substrate-manifest fact, not a shape. **PROPOSE, do not silently add.** |
| **E** — shared NEON classifier is a Lock-16 primitive-manifest entry (T-P1 §E) | proven `select_classifier(alphabet:&'static [u8;64])` `skinny/crates/bbnf-simd/src/dispatch.rs:42` → `SelectedClassifier` (`Vec<u32>` structural index); core `scan_structural(input,&StructuralAlphabet)` `crates/simd-scan/src/lib.rs:80` wired (scan-leaf) across 8 grammars; Lock 16 manifest `restart/locks/LOCKS.md:480-489`; NEON byte-classify allowlist `:454-461`; the ONE real NEON Layer-1 body `byte_class_from_eq_set_64_neon` (87 LOC, 8 distinct NEON intrinsics / 27 calls, `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs`); per-primitive admission table is 2b FOLD-L1 + the S-P2-LOCKED L1–L9 manifest | **grounded (per-primitive admission deferred to 2b)** | The classifier is grammar-general by CONFIG-BREADTH (alphabet-as-data is the Lock-14 vehicle, SK-V17 SPEC §2.1 `:315`), value-plane-exercised on JSON+CSS only — NOT proof-breadth across the 8 wired grammars (CH2-V1-R3). The fold folds the NARRATIVE; the Lock-16 per-primitive admission cost (scalar-ref + checkasm + scalar-delegate-vs-NEON-body split) is enumerated in 2b, NOT blanket-admitted here (CH4-2a-001): only the eq-set fan is a proven NEON body; `byte_class_from_table_64_neon` / `bitmap_prefix_xor_64_neon` are live line-3 scalar delegates to `crate::scalar::*` (verified at HEAD). aarch64-only; x86 kernels in `crates/simd-scan` are architecture-pressure, not a close path. |
| **F** — `StructRegistry`/`FieldSource` per-leaf fence (T-P1 §F) | `begin_compound(&StructLayout)` reads only `layout.rule_id & 0x1F` `crates/core/src/runtime/tape/mod.rs:185-186` (pre-resolved, NO per-leaf lookup); `StructRegistry::layout(rule_id)` `crates/ir/src/registry/struct.rs:331`; `FieldSource` enum `:84`; live coupling-site `StructRegistry::compound_kind_for_layout(layout)` `crates/core/src/runtime/bbnf/arena.rs:47`; 28-65×/983×/10583× regression SK-V17 SPEC `:793-795` | **grounded** | The `FieldSource{TypedLeaf,BranchTag,SeqPosition,RepeatElement,RuleReference}` walk MUST be compile-time projection-emission, resolved once at codegen. `begin_compound` already takes a pre-resolved `&StructLayout` — the fold inherits the correct shape ONLY if the caller resolves the layout once. A naive per-leaf `StructRegistry::layout` re-opens the worst measured regression. 0-LOC fence; CRITICAL if violated. |
| §7.3 `CollapsedStage` "fuses mask-state and emission for AVX-512-class hardware" (`ARCHITECTURE.md:1088,:1109-1114`) is the aarch64 NEON union route | `admits_collapsed_stage` co-requires `target.arch==x86`+`avx512bw`+`Entry(_)` `ARCHITECTURE.md:1206` (verbatim "aarch64 mechanically refused"), LAC-2D-06 `LOCKS.md:520-533`; SK-V17 aarch64-only bars x86/AVX/SVE SPEC `:258,:806`; aarch64 candidate is UNKNOWN-2D-05 `ARCHITECTURE.md:1206` | **refuted** | The SK-V17-proven aarch64 model has NO `CollapsedStage` — NEON sits under the four LLVM shapes' scan-leaf FFI (`select_classifier`→`Vec<u32>`→tape consumes it). The fold must NOT route aarch64-NEON through `CollapsedStage`; the aarch64-CollapsedStage question is the SPEC-NAMED open unknown, not a fresh gap and not the SK-V18 fold target. |
| §7.3 narrative frames the scanner as a JSON structural scanner (the `OnceCell<StructuralIndex>` per-parser, the JSON `scan_structurals` isomorphism anchor) | proven scan is alphabet-parametric `select_classifier(alphabet:&[u8;64])` `dispatch.rs:42` / `scan_structural(&StructuralAlphabet)` wired across ALL 8 generated grammars (`grep -c scan_structural` = 1 each) `crates/simd-scan/src/lib.rs:80` | **refuted (impl-exceeds-spec)** | The classifier is grammar-general by config (alphabet-as-data), not JSON-pinned. The spec's JSON-scanner framing UNDERSTATES the proven shape; the fold is a 0-LOC narrative correction to the alphabet-parametric shared form + a scope reconcile of the multi-arch `crates/simd-scan` against the proven aarch64-only set. |

## The Six Folds — Candidate Enumeration (LOAD-BEARING)

Each fold carries: **shape** (what changes in `crates/core` / the V1 spec); the
**T-P1-divergence antecedent**; the **grammar-neutral verdict** (Lock 14); and the
**lock surface** (which locks the fold engages, substrate-union-preserving per
Lock 1, 5-shape-canon-coherent per Lock 10). The monotonic direction is
skinny-proven → `crates/core`; aarch64 only; preserve-rich-ast throughout.

### FOLD-2A-A — `crates/core` adopts the flat tape, AoS↔SoA converges to ONE

**Shape.** `crates/core`'s 16-byte AoS `TapeRec` (`record.rs:103`, const-asserted
`size_of==16` + `align==4` at `:120-121`) and the proven SoA `Tape<'input>`
(`offsets:Vec<u32>` + sparse `flag_cursors:Vec<u32>`/`flag_values:Vec<u8>` +
`PayloadArena`, `skinny/crates/runtime/src/tape/mod.rs:94-100`) are BOTH Lock-1-admitted
offset-tape shapes. The fold names the single post-fold encoding. The proven SoA
shape is JSON-witnessed >SOTA; the core mod-doc itself declares AoS provisional
("kept AoS first … the same `TapeCursor` API rides a later SoA split",
`crates/core/src/runtime/tape/mod.rs:6-9`). The fold direction the spec should
DIRECT: SK-V18 adopts the proven SoA encoding (the witnessed shape) OR proves AoS
parity on the benched plane — exactly one survives, the dual encoding is a transient
fold-state only.

**T-P1-divergence antecedent.** §A (CONVERGED §A "SoA-proven vs AoS-in-core,
UNWIRED"); SUB17-002 (1a); SK17L-001 (1d); D-1E-SKV17-01 (1e); U-SUB17-001 /
U-SK17L-001 / 1E-SKV17-U1 (the convergence-target open question). The UNWIRED state
is now precise: the AoS `TapeStructBuilder` is exercised ONLY by
`crates/core/tests/tape_substrate.rs` (V3 grep-precision); production runtime
grep-zero — dead, not live, confirming the divergence claim is exact.

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL. The tape carries ZERO grammar-specific
code: `push_plain_offset` is one branchless u32 write with no grammar policy
(`skinny` assembler.rs:42,71); `TapeStructBuilder` "dispatches on the `StructLayout`
… never on per-grammar route strings" (`crates/core/src/runtime/tape/mod.rs:54-56`).
The proven tape carries NO class-column (sparse position-keyed flags, one offsets
vector) — the SoA shape is grammar-as-data, Lock 14 honoured.

**Lock surface.** Lock 1 (substrate-union — exactly-ONE-encoding closure post-fold;
`LOCKS.md:75` "parallel substrates are dead"; the AoS/SoA dual is admissible ONLY as
a transient fold-state, a post-fold dual end-state is a Lock-1 VIOLATION).
Substrate-union-PRESERVING: ONE tape per tree today; the fold collapses the cross-tree
drift to one encoding, it does not add a substrate. 5-shape-canon-coherent: untouched —
the tape is the substrate the shapes project from, FOLD-2A-D governs the shape
relationship. Risk: medium; 200-600 LOC SK-V18 fold; eager-builder retirement (FOLD-2A-B)
touches 22+ files; re-emit generator-side / regen-gated.

### FOLD-2A-B — retire the eager `OpenFrame` builders (the SK-V18 demolition)

**Shape.** The live `crates/core` substrate is the eager `OpenFrame` machine:
`JsonStructBuilder` (`json/builder.rs:9` `OpenFrame<'p>` enum, 231 LOC) and
`CssStructBuilder` `OpenFrame` (`css_l4/builder.rs:16`, 817 LOC god-module, six
`pending_*` Vecs + `pending_value:Option`). `TapeStructBuilder` is grep-zero in
production (only `tests/tape_substrate.rs` wires it — dead in the live runtime). The
fold RETIRES the eager builders and wires the tape builder as the live substrate —
the SK-V17-EXCAVATED materialization divergence (eager `OpenFrame`, SK-V17 SPEC
`:791`), a CODE-SHAPE divergence; the JSON recognizer is the only >SOTA-witnessed
plane, the materialization gap is not a benchmarked CSS deficit (CH7-003). This IS the
spec-directed SK-V18 implementation: the spec must direct the demolition, not carry
the eager builders forward. The structural SIMD scan is ALREADY wired across all 8
grammars feeding these eager builders (`css_l4.rs:15982`, `json.rs:732`); the missing
primitive is the TAPE CONSUMER, not the scan.

**T-P1-divergence antecedent.** §B "Eager OpenFrame builder (the load-bearing
divergence)" (CONVERGED); SUB17-003 + Gaps "no live tape consumer" (1a); SK17L-003
(1d); the AZ-IV K-block SK-V17 SPEC `:791`,`:139`.

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL (deletion of per-grammar runtime is
Lock-14-ALLOWED-but-EAGER). The eager `OpenFrame`/`CssStructBuilder` are per-grammar
RUNTIME surfaces (Lock 14 ALLOWED), but their EAGER shape is the AZ-IV pre-block; the
fold replaces them with the grammar-neutral tape + lazy `ValueRef<G>` projection. No
grammar policy enters a generic crate — the tape is grammar-agnostic, the value
surface is generator-emitted per grammar.

**Lock surface.** Lock 1 (no `Vec<OpenFrame>::clone` parallel-substrate pathology —
the 86.07% samply pathology Lock 1 names; the eager `pending_*` Vecs are the shape
this lock forbids retaining). Lock 14 (eager per-grammar runtime → generator-emitted
lazy projection). Substrate-union-PRESERVING: retires the dead-vs-live asymmetry,
leaves ONE live tape. 5-shape-canon-coherent: the retired builders are not shapes;
their replacement projects via `SinkOnly`/`OffsetTape`. Risk: HIGH; demolition of
1048 LOC across the two builders (231 + 817) + 22+ touched files; SK-V18 wires the
consumer.

### FOLD-2A-C — lazy `ValueRef<G>` as the unified materialization plane

**Shape.** `crates/core` carries NO `ValueRef`/`value_from_ref` (grep=0 in
`json/value.rs`); the value layer is per-grammar eager typed enums (`CssTypedValue<'p>`
`css_l4/value.rs:414`) `@generated by xtask regen-{json,css}` (header at `value.rs:1`).
The proven plane is the grammar-parametric `ValueRef<'doc,'input,K,G:EventGrammar>`
(`skinny/crates/runtime/src/tape/mod.rs:175`) read by `value_from_ref` per grammar
(JSON witness `json/value.rs:143`, zero per-node heap alloc). The fold makes the ONE
`BackendRule`-walking accessor generator (SK-V17 SPEC §0.1.3 `:54-72`, W2 §5) emit
`document/value/view/visitor` over `BackendRule` as the lazy `ValueRef<G>` projection
for ALL 8 per-grammar value surfaces. This is the UNIFIED materialization plane: one
generator, one lazy projection, every `->` in the grammar reaches the tape emitter
(typed-materialization-invariant) and is reconstructed on demand via `ValueRef`
(preserve-rich-ast — no per-leaf `Box::new`, no f64-alloc-per-number, no per-color
`Box<CssColor>`).

**T-P1-divergence antecedent.** §C "Value-API (per-grammar eager vs lazy-ValueRef<G>)"
(CONVERGED); SUB17-004 (1a); SK17L-002 (1d); D-1E-SKV17-02 (1e). The §C verdict: the
divergence is the EAGER materialization shape, not the `@generated` provenance.

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL. The `@generated` value APIs are already
a regen output (Lock 14 HONOURED — a generic generator, not hand-written per-grammar
runtime). The fold lifts the generator to emit lazy `ValueRef<G>`; the JSON rider's
hand-written `value_from_ref` must re-emit BYTE-EQUAL through the new generator (R-CH2-1,
SK-V17 SPEC `:552-557`) — a CSS-only generator that never re-emits JSON is the
generic-named-CSS-generator failure mode (CH2) and FAILS. Generality is exercised on
JSON + CSS only; Sheets/BBNF-self is the SK-V18 proof (`projection_generality_exercise
∈ {json, css_l4}`, SK-V17 SPEC `:327`).

**Lock surface.** Lock 1 (lazy view over sealed tape, no eager value tree). Lock 14
(one grammar-agnostic generator template, no hand-written per-grammar runtime). Lock 2
(the `BackendRule`/`FieldSource` walk IS the layout-projection recipe). Coupled to
FOLD-2A-F (the walk is compile-time, not per-leaf). Substrate-union-PRESERVING: the
`ValueRef` borrows `&'doc Tape<'input>`, the `&'i Tape<'i>` + cursor shape Lock 1 names.
Risk: HIGH; 300-700 LOC GENERATOR-SIDE (the template body in the accessor generator),
distinct from the per-grammar regen × 8 OUTPUT LOC (a separately-budgeted propagation
surface under generated-size-budget — the regen fan-out is generated, not authored,
and is priced as the generated-size-budget delta, not as generator-side LOC).

### FOLD-2A-D — the tape is a SUBSTRATE-MANIFEST category, NOT a 6th BackendShape

**Shape.** The §7.3 5-shape `BackendShape` canon is `{EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage}` (`LOCKS.md:107-108`, `ARCHITECTURE.md:1091-1115`). The flat
tape the fold adopts (FOLD-2A-A) is the SUBSTRATE — the thing the five shapes project
FROM, not a sixth way to project. `OffsetTape`/`EventTape`/`EagerTape` RETAIN a
queryable tape; `SinkOnly` does not; `CollapsedStage` fuses mask-state and emission.
The flat tape is what `OffsetTape`/`EventTape` retain. The fold PROPOSES (does NOT
silently add): the unified flat tape folds as a SUBSTRATE-MANIFEST fact under the
Lock 1 substrate manifest (`substrate_target ∈ {local_temp_only, existing_tape,
direct_sink, admitted_fact_output}`, `LOCKS.md:118-127`), exactly as LAC-1E-14 made
`FactStream` the 5th substrate-manifest CATEGORY without a 6th shape. The proven
aarch64 union sits under the four LLVM shapes' scan-leaf FFI.

**Independent mechanical anchor (beyond the LAC-1E-14 precedent, CH6-V1-V03).** The
no-6th-shape verdict does NOT rest on the precedent-echo alone: `admits_collapsed_stage`
mechanically co-requires `target.arch==x86`+`avx512bw`+`Entry(_)` (`ARCHITECTURE.md:1206`,
verbatim "aarch64 mechanically refused", LAC-2D-06 `LOCKS.md:520-533`). On the aarch64
target the predicate is FALSE by construction — there is no shape-level route the tape
could occupy that the five shapes do not already cover, so the tape MUST be a substrate
not a shape. This is an independent mechanical fact (a live predicate at a cited line),
not a re-reading of the same precedent — defence-in-depth so the 5-fold cohort agreement
is 5 independent groundings, not 1×5 (CH6-V1-V03, VERIFIED at HEAD this cycle).

**T-P1-divergence antecedent.** §D "BackendShape 5-shape canon (Lock 10) … must absorb
tape-as-substrate" (CONVERGED §D + the fold mandate); SUB17-005 (1a); SK17L-006 (1d);
D-1E-SKV17-04 (1e); the LAC-1E-14 precedent `LOCKS.md:100-116`. The aarch64
`CollapsedStage` is UNKNOWN-2D-05, the SPEC-named open unknown.

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL. The 5-shape canon + the substrate manifest
are grammar-agnostic; the per-rule shape is cost-model-derived from Grammar IR facts
(`derive_backend_shape`, `skinny/crates/passes/src/lib.rs:392`), no BBNF directive
carries the choice (Lock 10). The substrate-manifest classification is config/data, not
code.

**Lock surface.** Lock 10 (5-shape canon HELD — NO 6th shape; a sixth shape / new
directive / new BIR variant remains G-Omega gated, `LOCKS.md:109,:599`). Lock 1
(the substrate manifest is where the tape folds; the tape is `existing_tape`, not a
new substrate). Substrate-union-PRESERVING + 5-shape-canon-COHERENT by construction —
this fold IS the coherence guarantee. **PROPOSAL only**; Pass Omega applies; do NOT
silently add a 6th shape. Risk: 0 LOC (canon holds); medium (the aarch64-NEON absorption
narrative + the UNKNOWN-2D-05 record). LAC candidate (see below).

### FOLD-2A-E — shared NEON classifier vocabulary as a Lock-16 primitive-manifest entry

**Shape.** The proven aarch64 NEON leaf is `select_classifier(alphabet:&'static [u8;64])
-> SelectedClassifier` (`skinny/crates/bbnf-simd/src/dispatch.rs:42`) producing only a
`Vec<u32>` structural index. `crates/core`'s wired form is
`scan_structural(input,&StructuralAlphabet)` (`crates/simd-scan/src/lib.rs:80`) with a
richer alphabet config (`StructuralAlphabet` struct at `alphabet.rs:19`: `singletons`,
`digraph_mask:[u64;4]`, `digraph_pairs`, `quote_classes`) wired (scan-leaf) across all 8
generated grammars. The fold records the shared classifier as a Lock-16 primitive-manifest
ENTRY — but the per-primitive admission cost is NOT blanket-asserted over a vocabulary
(CH4-2a-001); each primitive carries its OWN {scalar-ref · checkasm · close-state ·
consumer} row, enumerated authoritatively in 2b FOLD-L1 + the S-P2-LOCKED L1–L9 manifest
and cross-referenced here:

| classifier primitive | close-state (verified live at HEAD) | NEON body? | Lock-16 admission |
|---|---|---|---|
| `byte_class_from_eq_set_64_neon` | 87 LOC, **8 distinct NEON intrinsics across 27 calls** (`aarch64/byte_class_from_eq_set_64.rs`); distinct set `{vld1q_u8, vceqq_u8, vandq_u8, vorrq_u8, vdupq_n_u8, vget_low_u8, vget_high_u8, vaddv_u8}` | **YES — the one real NEON Layer-1 body** | scalar-ref `scalar/byte_class_from_eq_set_64.rs` + strict checkasm + same-wave consumer (tape structural decode) — admitted as a NEON-body row |
| `byte_class_from_table_64_neon` | line-3 delegate to `crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar` (`aarch64/byte_class_from_table_64.rs:3`) | NO | `scalar-delegate-non-ASM` (2b A4 / LAC-2b-SKV17-03) — NOT priced as a NEON-body row, no NEON consumer to claim |
| `bitmap_prefix_xor_64_neon` | line-3 scalar delegate to `crate::scalar::*` | NO | `scalar-delegate-non-ASM` — same close-state |
| `bitmap_next_set_bit` / `bulk_emit_positions_64` / `eob_pad_clamp` | scalar-ref + checkasm present (2b A3) | per 2b manifest | admitted per-row in 2b, consumer-bound there |

The Lock-16 manifest ENTRY this fold raises is therefore: abstract-primitive name (NEON
byte-classify, the `vqtbl4q_u8`/`vqtbl1q_u8` table-lookup + movemask family in the
allowlist `LOCKS.md:454-461`) + published citation (Lemire 2019; Validark 2024) +
hardware gate (aarch64 NEON) bound to the eq-set fan as the ONLY proven NEON body, with
the two delegates filed `scalar-delegate-non-ASM` per 2b's close-state taxonomy. The
eq-set fan's actual intrinsic palette (8 distinct of the `vceqq`/`vandq`/`vorrq` equality
+ horizontal-reduce family, not the table-lookup `vqtbl` family) is recorded precisely so
the manifest entry is checkasm-anchored to the real body, not a generic NEON gesture. The
`1:1 ARCH-signature mapping` is naming, not exercise-proof (CH4-2a-001 / CH2-V1-R3).

**Substrate_target is a PRE-condition, not settled (CH5-V1-004).** The `Vec<u32>` index
becomes the tape's `offsets` (`substrate_target=existing_tape`, REDRESS-53 fence) ONLY
once the tape is wired and the all-8-carrier `OnceCell<StructuralIndex>` classification
fires. TODAY the index is held in a retained `OnceCell<StructuralIndex>`
(`crates/core/src/grammar/generated/json.rs:701`, `::core::cell::OnceCell<` confirmed)
SEPARATE from any wired tape — its present-tense classification is `local_temp_only`
(pending) or, if mis-wired beside a tape, a REDRESS-53 sidecar. This fold's
`substrate_target` resolves to `existing_tape` only AFTER the F7 / U-2E-02
classify-before-wiring step (`2f:336-361` / `2e:336`); until then it is the retained
`OnceCell` pending classification. `retention_lifetime=transient-single-call` post-wiring
(Lock 1 v+1 — no cross-call classifier state). Alphabet-as-data is the only grammar datum
(Lock-14 vehicle), config-breadth, value-plane-exercised JSON+CSS only.

**T-P1-divergence antecedent.** §E "NEON shared classifier (JSON-only narrative vs
shared-classifier reality)" (CONVERGED); SUB17-007/008 (1a, impl-exceeds-spec);
SK17L-006/008 (1d); D-1E-SKV17-06 (1e). The proven classifier is wired (scan-leaf)
across 8 grammars; its grammar-generality is config-breadth (alphabet-as-data),
value-plane-exercised JSON+CSS only — not a fleet-wide value-fold proof (CH2-V1-R3).

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL. The classifier's ONLY grammar datum is the
`alphabet:&[u8;64]` passed to `select_classifier` (Lock 14 vehicle, SK-V17 SPEC §2.1
`:315`); the CSS `;{` pair uses the eq-set fan, NOT the lo6 `& 0x3f` slot-59 collision
table (SPEC `:316`). The richer core `StructuralAlphabet` (digraphs, quote-classes) is
config-breadth, JSON/CSS-exercised-only, not breadth-of-proof; generality is
alphabet-as-data, Lock 14 honoured.

**Lock surface.** Lock 16 (primitive manifest ENTRY — citation + abstract-primitive name
+ hardware gate + scalar-ref + checkasm; the v+1 manifest `LOCKS.md:480-489`; aarch64
allowlist `:454-461`). Lock 1 v+1 (no cross-call retained classifier state,
`:137-149`; the index IS the tape, not a parallel vector). Lock 14 (alphabet-as-data).
aarch64-only — x86/avx2/avx512/wasm kernels in `crates/simd-scan` are architecture-pressure,
a fold-SCOPE reconcile (narrow-to-aarch64 vs retain), NOT an x86 close path. Risk: low/
0-LOC narrative fold + 100-400 LOC scope reconcile. LAC candidate (see below).

### FOLD-2A-F — the `StructRegistry`/`FieldSource` per-leaf fence (regression firewall)

**Shape.** `begin_compound(&StructLayout)` (`crates/core/src/runtime/tape/mod.rs:185`)
takes a PRE-RESOLVED layout by reference and reads only `layout.rule_id & 0x1F`
(`:186`) — NO per-leaf registry lookup inside it. The `StructLayout` originates from
`StructRegistry::layout(rule_id)` (`crates/ir/src/registry/struct.rs:331`), a
`BTreeMap<RuleId, StructLayout>` (`:314`); `FieldSource{TypedLeaf, BranchTag,
SeqPosition, RepeatElement, RuleReference}` lives in the same registry (`:84`). The
fold keeps the `FieldSource`/`StructLayout` walk COMPILE-TIME projection-emission
(codegen emits the field routing once, the `BackendRule`-walk recipe of FOLD-2A-C),
NEVER a per-leaf runtime `StructRegistry` indirection. A naive per-leaf
`StructRegistry::layout(rule)` on the hot path re-opens the worst measured regression:
28-65× (bbnf/sheets), 983× (css bootstrap, 606.4ms), 10583× (WATCHDOG tailwind, 77.6s).
This is the AZ-IV/StructRegistry pre-block kept inviolate.

**The fence names the LIVE coupling-site it severs, not a hypothetical (CH5-V1-003,
shared with 2c-F/2d-06/2e-F/2f-F6; the shared edit is owned by 2f-F6, named here for
self-containment).** A runtime `StructRegistry` call exists TODAY at
`crates/core/src/runtime/bbnf/arena.rs:47` — `StructRegistry::compound_kind_for_layout(layout)`
in the EAGER arena path (verified live at HEAD: `BbnfCompoundKind::from_layout` calls
`StructRegistry::compound_kind_for_layout(layout)` and matches on the returned route
string). This is the present-tense coupling the fence forbids on the hot path;
FOLD-2A-B's deletion of the eager arena builders is the act that SEVERS it. The tape
path is verified fence-clean by contrast: `begin_compound`
(`crates/core/src/runtime/tape/mod.rs:185-186`) reads `layout.rule_id & 0x1F` only, with
grep-zero `StructRegistry`. The fence is thus falsifiable (a present wire FOLD-B
removes), not a prose hypothetical ("a future lookup would be bad").

**T-P1-divergence antecedent.** §F "StructRegistry / FieldSource per-leaf hot-path fence
(the regression firewall)" (CONVERGED); SUB17-009 (1a) + the DO-NOT-REDRIVE FENCE;
SK17L-004 + L-SK17-02/02b (1d); D-1E-SKV17-03 (1e). The `begin_compound` already takes
a resolved `&StructLayout` — the fold inherits the correct shape ONLY if the caller
resolves the layout once.

**Grammar-neutral verdict.** GRAMMAR-NEUTRAL. `begin_compound` dispatches on the
`StructLayout`, never on per-grammar route strings (`mod.rs:54-56`); the `FieldSource`
walk is the grammar-derived `BackendRule` recipe resolved at codegen, no grammar policy
in the hot path.

**Lock surface.** Lock 1 (the substrate-union firewall — no per-leaf registry lookup;
the structural-projection-IS-the-tape clause). Lock 14 (`FieldSource` walk is the
`BackendRule` projection, not a per-rule branch catalogue). Substrate-union-PRESERVING
(0 LOC fence; CRITICAL/regression class if violated). Plus the Lock-2 `StructLayout`→
`Layout` 960-site rename surface (deferred; `grep -rc StructLayout crates/`=960 live at
HEAD; `LayoutFacts` grep=0 in `crates/` — the side-table is skinny/prior-totality-only).
LAC reconcile is L02 (owned by 1E LAC-1E-SKV17-04, not re-raised here). Risk: 0-LOC
fence; CRITICAL if violated.

## Architectural Assertions Defended (the proven path supports these spec surfaces)

1. **The Lock 1 substrate-union "structural projection IS the tape" (`ARCHITECTURE.md:1088`)
   is the correct fold frame.** The proven SoA `Tape` retains `offsets:Vec<u32>` as the
   structural projection; the wired `OnceCell<StructuralIndex>` in the 8 core parsers
   becomes the tape's `offsets` (`existing_tape`) or `local_temp_only` — never a retained
   index parallel to a wired tape (REDRESS-53). Defended.

2. **The 5-shape canon (`LOCKS.md:107-108`) absorbs aarch64-NEON without a 6th shape.**
   NEON sits under the four LLVM shapes' scan-leaf FFI; the substrate manifest carries the
   tape, the shapes project from it. The LAC-1E-14 `FactStream` precedent is the binding
   model: substrate-manifest category, not shape addition; the `admits_collapsed_stage`
   x86-co-requirement (`ARCH:1206`) is the independent mechanical corroborator. Defended
   (FOLD-2A-D).

3. **The cost-model-derived per-rule shape selection (Lock 10, `derive_backend_shape`
   `skinny/crates/passes/src/lib.rs:392`) is grammar-neutral.** No BBNF directive carries
   the materialization plan; the selector mines Grammar IR facts. Defended.

4. **preserve-rich-ast via lazy `ValueRef` projection (SK-V17 SPEC §0.1.5 `:78-83`).** The
   typed CSSOM is reconstructed on demand, value-plane population parity holds, never
   flattened, never eager. The fold's preserve-rich-ast OBLIGATION is full typed-AST
   parity with lightningcss as the SK-V18 strict-equality GATE
   (`assert_lightningcss_strict_equality` SPEC `:98`, `css_typed_summary_equal` SPEC
   `:129`) — a non-negotiable TARGET on the UNMEASURED-PENDING CSS surface (SPEC `:207`),
   NOT a property held at this pass (CH7-003). Defended as the obligation/target form
   (FOLD-2A-C).

## Architectural Assertions Refuted (the load-bearing rows — these constrain T-P3)

1. **§7.3 routes the aarch64-NEON union through `CollapsedStage` ("fuses mask-state and
   emission", `ARCHITECTURE.md:1088,:1109-1114`).** REFUTED by the proven path: the
   SK-V17 aarch64 model has NO `CollapsedStage`. `CollapsedStage` is mechanically refused
   on aarch64 (`admits_collapsed_stage` co-requires `target.arch==x86`, `ARCHITECTURE.md:1206`
   verbatim "aarch64 mechanically refused", LAC-2D-06); the proven NEON leaf produces a
   `Vec<u32>` index consumed by the tape under the four LLVM shapes. The fold MUST NOT
   direct aarch64-NEON through `CollapsedStage`; the aarch64-CollapsedStage path is the
   SPEC-NAMED UNKNOWN-2D-05, not the SK-V18 fold target. **Constraint on T-P3: the spec's
   CollapsedStage-as-NEON-route framing is wrong for aarch64; the fold absorbs NEON under
   the four LLVM shapes' scan-leaf FFI.**

2. **§7.3's JSON-scanner framing understates the proven classifier as JSON-pinned.**
   REFUTED: the classifier is alphabet-parametric (`select_classifier(alphabet:&[u8;64])`,
   `scan_structural(&StructuralAlphabet)`) and wired (scan-leaf) across 8 grammars. The
   classifier's grammar-generality is CONFIG-BREADTH (alphabet-as-data, the Lock-14
   vehicle), value-plane-exercised on JSON+CSS only — the 8-grammar scan wiring
   (bnf/csv/ebnf/css_pretty/google_sheets/bbnf included) is NOT a fleet-wide value-fold
   proof (CH2-V1-R3, `LOCKS.md:423-425`; the value-plane fold under test, FOLD-2A-C lazy
   `ValueRef<G>`, is JSON+CSS only). The spec narrative must fold to the alphabet-parametric
   shared form (a 0-LOC narrative correction). **Constraint on T-P3: the scanner is
   impl-exceeds-spec by config-breadth; the spec must absorb the shared (alphabet-as-data)
   classifier, not describe a JSON scanner — without overclaiming value-plane proof beyond
   JSON+CSS.** (2E `:238-239` carries this distinction; this assertion site matches it.)

## Open Research Questions (UNKNOWN → verify_action)

| UNKNOWN | Blocking question | verify_action |
|---|---|---|
| U-2A-01 | Does SK-V18 adopt the proven SoA `Tape` encoding into `crates/core` (witnessed shape) or keep AoS and prove parity? Exactly one survives (Lock 1 closure). | T-P3/Pass-Omega names the convergence-target tape shape against the core mod-doc "AoS first … later SoA split" (`crates/core/src/runtime/tape/mod.rs:6-9`) vs proven `skinny/.../tape/mod.rs:94`. Carry as a CATALOGUED divergence (the dual end-state is a Lock-1 violation), not merely an open question (LAC-1E-SKV17-01). The AoS builder is exercised ONLY by `tests/tape_substrate.rs` — measure the SoA-vs-AoS parity on the benched JSON plane before deletion. |
| U-2A-02 | Does the multi-arch `crates/simd-scan` (neon/avx2/avx512/wasm/scalar) narrow to the proven aarch64 set post-fold, or retain x86 kernels as architecture-pressure? | T-P2/T-P3 reconcile `crates/simd-scan/src/lib.rs:53-65` against the aarch64-only mandate (SK-V17 SPEC `:258`) WITHOUT admitting x86 as a close path. Scope decision, not a defect (LAC-1E-SKV17-06; 100-400 LOC). |
| U-2A-03 | Is the aarch64-NEON-under-four-LLVM-shapes absorption (no 6th shape) sufficient, or does a source-backed aarch64 collapsed-stage candidate (UNKNOWN-2D-05) ever close? | T-P2 2E/2D research: the aarch64 candidate requires a 2E source-backed strategy before any admission (`ARCHITECTURE.md:1206,:1279-1282`); until then aarch64 CollapsedStage admission is mechanically refused. The SK-V18 fold does NOT depend on it — NEON absorbs under the four LLVM shapes. |

## LOCKS-AMENDMENTS-CANDIDATE

Candidates only; disposition is T-P3 3C, G3-gated; merge is Pass Omega, post-G-Omega.
Three candidates this cycle, all refinements (no new lock, no lock retirement, no 6th
shape, no new directive/BIR variant/substrate). Scanned axes: Lock 1 (substrate
manifest — tape-as-substrate classification), Lock 10 (5-shape canon coherence /
aarch64 absorption), Lock 16 (shared NEON classifier manifest entry). The AoS/SoA
one-encoding closure (LAC-1E-SKV17-01), the no-per-leaf-registry fence
(LAC-1E-SKV17-02), the StructLayout reprice (LAC-1E-SKV17-04), the UNKNOWN-2D-05 record
(LAC-1E-SKV17-05), and the simd-scan multi-arch scope (LAC-1E-SKV17-06) are OWNED by 1E
and carried by reference, NOT re-raised here (triumvirate discipline — 1E owns the locks
lane). 2A raises only the fold-DESIGN candidates the substrate-manifest/classifier-entry
design surfaces.

| candidate | type | target locks | proposed candidate text | supporting path:line evidence | loc/risk/wave_hint |
|---|---|---|---|---|---|
| LAC-2A-SKV17-01 | refinement | L01, L10 | Record the unified flat tape as a SUBSTRATE under the Lock 1 substrate manifest, NOT a 6th `BackendShape`: per the LAC-1E-14 `FactStream` precedent (`LOCKS.md:100-116`), the tape is the substrate the five shapes (`OffsetTape`/`EventTape`/`EagerTape` retain it, `SinkOnly` does not, `CollapsedStage` fuses) project FROM; it carries `substrate_target=existing_tape`; adding a 6th shape remains G-Omega gated. The no-6th-shape verdict is independently corroborated by `admits_collapsed_stage` co-requiring `target.arch==x86` (aarch64 mechanically refused, `ARCH:1206`). | `restart/locks/LOCKS.md:100-116` (FactStream precedent), `:107-108` (5-shape canon held), `:118-127` (substrate manifest); `restart/ARCHITECTURE.md:1088` (structural projection IS the tape), `:1206` (admits_collapsed_stage x86-co-requirement); `crates/core/src/runtime/tape/mod.rs:54-58`. | 0 LOC (classification) / medium / SK-V18 fold + T-P3 3C |
| LAC-2A-SKV17-02 | refinement | L16 | Add the shared aarch64-NEON `select_classifier(alphabet)` classifier as a Lock-16 primitive-manifest ENTRY (grammar-neutral), with PER-PRIMITIVE admission (not blanket-vocabulary): only `byte_class_from_eq_set_64_neon` (87 LOC, 8 distinct NEON intrinsics across 27 calls) is admitted as a NEON-body row (scalar-ref + strict checkasm + same-wave consumer); `byte_class_from_table_64_neon` / `bitmap_prefix_xor_64_neon` are filed `scalar-delegate-non-ASM` per 2b's close-state taxonomy, NOT priced as NEON-body rows. abstract-primitive name (NEON byte-classify, allowlist `:454-461`), citation (Lemire 2019 / Validark 2024), hardware gate (aarch64 NEON), `substrate_target` RESOLVES to `existing_tape` only AFTER the F7/U-2E-02 OnceCell classify-before-wiring step (today the index is retained `OnceCell<StructuralIndex>`, pending), `retention_lifetime=transient-single-call` post-wiring, alphabet-as-data as the only grammar datum, config-breadth value-plane-exercised JSON+CSS only. | `skinny/crates/bbnf-simd/src/dispatch.rs:42`; `skinny/crates/bbnf-simd/src/aarch64/{byte_class_from_eq_set_64,byte_class_from_table_64}.rs` (table_64 line-3 scalar delegate); `crates/simd-scan/src/lib.rs:80`; `crates/simd-scan/src/alphabet.rs:19-37` (StructuralAlphabet struct + rich-alphabet at :19); `crates/core/src/grammar/generated/json.rs:701`; `restart/locks/LOCKS.md:454-461,:480-489,:137-149,:423-425`. | 0 LOC (manifest row) / low / SK-V18 fold + T-P3 3C |
| LAC-2A-SKV17-03 | refinement | L10 | Fold the §7.3 narrative's JSON-scanner + CollapsedStage-as-NEON-route framing to the proven aarch64 model: the classifier is alphabet-parametric and grammar-general (impl-exceeds-spec); aarch64-NEON sits under the four LLVM shapes' scan-leaf FFI, NOT `CollapsedStage`; the aarch64-CollapsedStage path is the SPEC-named UNKNOWN-2D-05, not the SK-V18 fold target. | `restart/ARCHITECTURE.md:1088,:1109-1114,:1206`; `restart/locks/LOCKS.md:520-533`; `skinny/crates/bbnf-simd/src/dispatch.rs:42`; `restart/skinny/tranches/sk-v17/SPEC.md:258,:806,:852-854`. | 0 LOC (narrative fold) / medium / T-P2 research + T-P3 3C |

## V3 Fold Provenance (V2 → V3)

V2 CHALLENGE returned **8/8 ACCEPT** for 2A across every lens that scanned it (CH1
"2A — 6 folds + 8 grounding rows — ALL ACCEPT"; CH4 converted the V1 `CH4-2a-001`
REJECT and confirmed all six folds ACCEPT; CH5 confirmed `CH5-V1-003`/`CH5-V1-004`
FOLDED + ACCEPT; CH6 8/8 ACCEPT). No open 2A REVISE, no orphan, no REJECT carried
into V3. The V3 cycle therefore carries **zero verdict-changing folds** for 2A and
makes only two honest CH1-lane precision sharpenings, both re-executed live at HEAD
`91b6893b0`:

1. **`TapeStructBuilder` grep precision** — the prior "grep-zero outside `runtime/tape/`"
   is sharpened to its exact ground-truth: ONE site outside `runtime/tape/`, the test
   `crates/core/tests/tape_substrate.rs`; the PRODUCTION runtime grep-zero (dead) is
   confirmed. The UNWIRED verdict is unchanged and now exactly stated — a test wiring is
   not a live consumer, which strengthens (not weakens) the FOLD-2A-A/B "dead builder"
   claim.

2. **eq-set fan intrinsic count precision** — the prior "12 NEON intrinsics" is sharpened
   to the exact live count: **8 distinct NEON intrinsic names** (`vld1q_u8`, `vceqq_u8`,
   `vandq_u8`, `vorrq_u8`, `vdupq_n_u8`, `vget_low_u8`, `vget_high_u8`, `vaddv_u8`) across
   **27 total intrinsic calls** in the 87-LOC body. The "only real NEON Layer-1 body"
   verdict is unchanged; the palette is the equality + horizontal-reduce family, not the
   `vqtbl` table-lookup family, so the manifest entry is now checkasm-anchored to the
   actual body.

Two cross-cutting items the V2 frontmatter flagged as touching 2A's anchors are
re-confirmed clean, requiring no 2A edit: **CH1-2F-01-RESIDUAL** (LAC-2F-FOLD-03's
re-anchor target is `alphabet.rs:19-37`, the anchor `LAC-2A-SKV17-02` already cites —
2A was already correct; the residual is 2F-owned) and **CH6-V1-V03** (the independent
`admits_collapsed_stage` `ARCH:1206` mechanical anchor is carried at FOLD-2A-D and
re-verified live this cycle).
