---
agent: 3C
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
t_p1_locked_sha: 91b6893b0
t_p1_excavation_sha: 445925167154de73540e3ea3283d0170371de790
t_p2_locked_sha: 2a76916ac
t_p1_inventories_consumed: [1A, 1B, 1C, 1D, 1E, 1F]
t_p2_dossiers_consumed: [2A, 2B, 2C, 2D, 2E, 2F]
v1_surface_targeted: LOCKS.md
proposed_deltas_count: 5
delta_summary:
  carried_from_prior_cycle:
    - D-SKV17-L01-tape-substrate-union
    - D-SKV17-L02-structlayout-reconcile
    - D-SKV17-L10-tape-category-not-sixth-shape
    - D-SKV17-L14-valueref-classifier-generalisation
    - D-SKV17-L16-neon-classifier-manifest
  removed: []
  answered:
    - LAC-2F-FOLD-01
    - LAC-2F-FOLD-02
    - LAC-2F-FOLD-03
    - LAC-2F-FOLD-04
    - LAC-2F-FOLD-05
    - LAC-1E-SKV17-01
    - LAC-1E-SKV17-02
    - LAC-1E-SKV17-03
    - LAC-1E-SKV17-04
    - LAC-1E-SKV17-05
    - LAC-1E-SKV17-06
    - 2F-FOLD-U1
    - 2F-FOLD-U2
    - 2F-FOLD-U3
  newly_added: []
prior_cycle_dispositions_folded:
  accepted:
    - T-P1-V5-clean-final-G1-auto-pinned
    - T-P2-V3-normal-3Z-lock
  rejected: []
  revised:
    - CH6-V1-07-u3-receiver-reanchor
    - CH6-V1-09-locked-input-provenance
    - CH1-V1-hunk-header-arithmetic
    - CH5-V2-R01-distribution-invariant-gate-object
---

# 3C LOCKS Crystallisation — SK-V17 Tape-Fold (cycle V3)

## Executive Summary

This packet disposes every LOCKED T-P2 `LOCKS-AMENDMENTS-CANDIDATE` and its T-P1
antecedent into ONE v+1 `LOCKS.md` delta. **14 candidates total** — the 5 LOCKED
fold LACs (`LAC-2F-FOLD-01..05`), the 6 T-P1 excavation LACs they extend
(`LAC-1E-SKV17-01..06`), and the 3 Open Research Questions (`2F-FOLD-U1/U2/U3`) —
collapse into **5 lock-addendum clauses** on Locks 1, 2, 10, 14, 16. The
dispositions: **9 ACCEPT, 3 ACCEPT (ORQ-crystallised), 2 MODIFY, 0 REJECT, 0 DEFER**
(14 total). No candidate is silently dropped (a CH1 + CH6 REJECT class), and the
three ORQs are each ACCEPTed as crystallised disposition text — not deferred — per
the LOCKED T-P2 §3Z recommendations.

The proposed `LOCKS.md` delta is an **SK-V17 T-P3 addendum** appended before the
existing `## v+1 Governance Boundary` (`restart/locks/LOCKS.md:610`), immediately
after the inherited `## SK-V15 T-P3 v+1 Crystallisation Addendum`
(`restart/locks/LOCKS.md:581`-`608`). It is a proposed-only document; Pass Omega
CRUD applies it post-G-Omega. The addendum **preserves the 16 numbered locks
verbatim** (`restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`),
**preserves the five `BackendShape` variants** `{EagerTape, OffsetTape, EventTape,
SinkOnly, CollapsedStage}` (`restart/locks/LOCKS.md:107`-`108`), and **adds no new
directive, BIR variant, substrate, public substrate API, retained sidecar, lock,
lock retirement, or sixth shape**. The load-bearing fold — tape-as-substrate is a
substrate-manifest CATEGORY, not a 6th `BackendShape` — is crystallised in the
negative, on the LAC-1E-14 FactStream precedent plus the independent
`admits_collapsed_stage` aarch64-refusal ground.

T-P1 is carried as **clean-final / G1-auto-pinned** input
(`restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:80`-`82`); T-P2 is carried
as the **normal §3Z-locked** research input
(`restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:15`-`19`,
V2=98.6% + V3=100.0%, zero orphan REVISE). The line-level proposed diff is emitted
separately as `3c-locks-v+1-diff.md` (the G-Omega gate object).

## V3 Delta Summary

This is cycle V3. The five lock-addendum clauses (folding all 14 candidates) carry
forward unchanged in count and body; the cycle folds the prior-cycle CHALLENGE
REVISEs only. The §4 mandate to regenerate the V{N} Delta Summary block each cycle
is discharged here.

| class | disposition | evidence |
|---|---|---|
| Carried from prior cycle | All five clauses `D-SKV17-L01`/`L02`/`L10`/`L14`/`L16` carry from V2 with body and citation base intact (14 candidates → 5 clauses on Lock 1 / 2 / 10 / 14 / 16). | `3c-locks-v+1-diff.md:58`-`66`. |
| Removed | None. No live LAC is silently dropped (a CH1 + CH6 REJECT class). T-P3 is proposal-only (`restart/prompts/totality/PASS-3-SYNTHESIS.md:228`). | n/a |
| Answered | None new at V3. All 14 candidates were dispositioned at V1 and remain in the matrix below. | n/a |
| Newly added | None at V3. | n/a |
| Revised (folded prior dispositions) | **CH1-V1-hunk-header-arithmetic** (`V1/CH1.md`): the v+1-diff hunk header read `@@ -606,6 +606,52 @@` causing `git apply` corrupt-patch / EXIT 128; FOLDED to `@@ -606,7 +606,22 @@` (7 old-side context, 22 new-side context+insert); `git apply --check` EXIT 0 re-verified at master HEAD 2a76916ac. **CH6-V1-07-u3-receiver-reanchor** (`V1/CH6.md`): 2F-FOLD-U3 receiver re-anchored to the EXISTING 5-shape `BackendShape` gate (`restart/locks/LOCKS.md:107`-`109`) + the G-Omega 6th-shape amendment path, with the 2E-source aarch64-strategy wave named as the blocker precondition for a future ADD, not a phantom future receiver (`3c-locks-crystallisation.md:138`). **CH6-V1-09-locked-input-provenance** (`V1/CH6.md`): the 0-REJECT/0-DEFER tally defended on LOCKED-input provenance with the five refutation rows preserved as REJECT-class clause text (`:159`-`181`). **CH5-V2-R01-distribution-invariant-gate-object** (`V2/CH5.md:123`-`155`): the R03 distribution invariant — present at `:197`-`200` here — was absent from the `3c-locks-v+1-diff.md` Invariant Check (the G3 gate object Pass Omega applies); FOLDED by copying the distribution-invariant bullet into that doc's Invariant Check after `:88`. | `restart/audit/totality/sk-v17/p3/hardening/V1/CH1.md`; `.../V1/CH6.md`; `.../V2/CH5.md:43`,`:99`,`:123`-`155`. |

## Proposed Delta Table

| proposed delta | source finding ids | affected LOCKS section | rationale |
|---|---|---|---|
| **D-SKV17-L01-tape-substrate-union** | LAC-2F-FOLD-01, LAC-2F-FOLD-04, LAC-1E-SKV17-01, LAC-1E-SKV17-02 (carrier subset), LAC-1E-SKV17-03, 2F-FOLD-U1, 2F-FOLD-U2 | Lock 1 | The SK-V18 fold retires the live eager `OpenFrame` builders, converges AoS `TapeRec` onto the proven SoA `Tape` as the single post-fold encoding, and declares `substrate_target` on all 8 `OnceCell<StructuralIndex>` carriers before wiring. Exactly ONE encoding survives (`restart/locks/LOCKS.md:75`); a dual AoS/SoA end-state is REJECT. The `FieldSource`/`StructLayout` projection walk is compile-time emission resolved once; ANY per-leaf runtime `StructRegistry::layout(rule)` indirection re-opens the 28-65×/983×/10583× regression (`restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`) and is REJECT. The live coupling site `crates/core/src/runtime/bbnf/arena.rs:47` is severed by the eager-builder retirement. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:116`,`:126`,`:128`,`:140`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:580`,`:583`. |
| **D-SKV17-L02-structlayout-reconcile** | LAC-2F-FOLD-05, LAC-1E-SKV17-04 | Lock 2 | The Lock-2 `StructLayout`→`Layout` reconcile is priced by TWO disjoint paths: (a) full rename across 960 generator-side sites + regen of 8 parsers + ~16 tests; (b) re-scope toward a `LayoutFacts.backend_shape` side-table — but `LayoutFacts`/`backend_shape` are grep-zero in `crates/` (skinny/prior-totality-only), so path-(b)'s `crates/core` realisation is NON-ZERO, sized as the 0→N introduce-site delta. The v+1 note (`restart/locks/LOCKS.md:162`-`166`) bars Lock-2 closure by `LayoutFacts` alone while public `Layout`/`LayoutSink` remain absent; path-(b) is a re-scope, not a closure. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:117`,`:130`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:584`. |
| **D-SKV17-L10-tape-category-not-sixth-shape** | LAC-2F-FOLD-02, LAC-1E-SKV17-05, 2F-FOLD-U3 | Lock 10 | The tape folds into the spec as the **substrate the 5 `BackendShape` shapes project from**, recorded at the Lock 1 substrate manifest (`substrate_target = existing_tape`) per the LAC-1E-14 FactStream precedent (`restart/locks/LOCKS.md:100`-`116`) — NOT a 6th `BackendShape` variant. The 5-shape domain holds verbatim (`restart/locks/LOCKS.md:107`-`108`); a 6th variant remains G-Omega gated (`restart/locks/LOCKS.md:109`) and SK-V17 §9-barred (`restart/skinny/tranches/sk-v17/SPEC.md:808`). The verdict stands on TWO independent grounds: the categorical precedent, and the `admits_collapsed_stage` x86-binding (`restart/ARCHITECTURE.md:1151`,`:1282`) that mechanically refuses on aarch64. The aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05, not a fresh gap; no x86 close path, no D6 second substrate. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:118`,`:129`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:581`. |
| **D-SKV17-L14-valueref-classifier-generalisation** | LAC-2F-FOLD-03 (value/scan generality half), LAC-1E-SKV17-03 (scan), LAC-1E-SKV17-04 (regen), the LAC-2F-FOLD-01 ValueRef generator | Lock 14 | The lazy grammar-parametric `ValueRef<'doc,'input,K,G>` projection is the one materialization plane, re-emitted by a single grammar-agnostic generator (`restart/locks/LOCKS.md:349`) that resolves the layout once at codegen — replacing the per-grammar EAGER value enums. The fold is **scope-honest**: the `ValueRef<G>` value-plane fold is exercised JSON+CSS only; Sheets/BBNF-self are by-construction under SK-V18, not by-exercise. The shared classifier's grammar-generality is config-breadth (alphabet-as-data) across 8 grammars — a SEPARATE axis from the value-fold, never asserted fleet-wide. preserve-rich-ast holds; no grammar branch enters any generic crate. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:119`,`:127`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:582`,`:530`-`534`. |
| **D-SKV17-L16-neon-classifier-manifest** | LAC-2F-FOLD-03 (manifest half), LAC-1E-SKV17-06 | Lock 16 | Register the shared NEON `select_classifier(alphabet)` / `scan_structural(input, &StructuralAlphabet)` classifier as a Lock-16 primitive-manifest ROW: abstract primitive = alphabet-parametrised byte classification; scalar reference `scalar/byte_class_from_eq_set_64.rs`; checkasm parity; `substrate_target = existing_tape`; `retention_lifetime = transient-single-call`; same-wave consumer = the tape. The eq-set fan is the one proven NEON Layer-1 body; `byte_class_from_table_64`/`bitmap_prefix_xor_64` are honestly-declared scalar passthroughs. The multi-arch `crates/simd-scan` scope-reconcile binds WITHOUT admitting x86 as a close path (no-SVE, aarch64-only). Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:120`,`:131`; `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:582`. |

## Per-Clause Cost Matrix

This matrix costs the proposed `LOCKS.md` addendum text and its governance
propagation only. It authorizes no implementation work. Any SK-V18 wave that
cannot consume a clause inside its cap records intrinsic-block, REDRESS, or
G-Omega wave-graph amendment — never hidden overflow.

| proposed delta | doc LOC | risk | affected SK-V18 waves (hint) | consumer/gate | propagation count | hard-cap fit | fail action |
|---|---:|---|---|---|---:|---|---|
| D-SKV17-L01-tape-substrate-union | 5-8 | high | W1 (eager-builder retirement) / W2 (SoA convergence + OnceCell classification) / W-fold-gate | Lock 1 substrate manifest + StructRegistry no-per-leaf-lookup fence gate | 3 (Lock 1, Lock 10 cross-ref, MASTER-PLAN §H) | Yes; Pass Omega doc-only, consumed by W1/W2 substrate-union + fence gates. | REDRESS/revert any dual AoS/SoA end-state or per-leaf `StructRegistry::layout(rule)` runtime walk; G-Omega wave-graph amendment for any new substrate; no challenge-time implementation. |
| D-SKV17-L02-structlayout-reconcile | 3-5 | medium | W1/W2 (generator-side rename) / W-fold-gate | Lock 2 closure review over `Layout`/`LayoutSink` evidence + 960-site rename gate | 2 (Lock 2, MASTER-PLAN §H rename row) | Yes; Pass Omega doc-only, consumed by W1/W2 rename-vs-side-table review. | REDRESS/revert any Lock 2 close claimed by `LayoutFacts` alone while public `Layout`/`LayoutSink` absent; intrinsic-block if path-(b) introduce-site count is not established before sizing. |
| D-SKV17-L10-tape-category-not-sixth-shape | 4-6 | medium | n/a (canon + precedent; 0 LOC impl) / W-fold-gate | Lock 10 5-shape gate + Lock 1 substrate-manifest category placement | 3 (Lock 1, Lock 10, ARCHITECTURE §7.3 + MASTER-PLAN §13) | Yes; Pass Omega doc-only, consumed by the all-five `BackendShape` gate. | G-Omega wave-graph amendment for any sixth-shape, directive, or BIR variant; no aarch64 CollapsedStage admission without a 2E source-backed strategy; no x86 close path. |
| D-SKV17-L14-valueref-classifier-generalisation | 5-8 | high | W2 (ValueRef generator) / W-CSS / W-fold-gate | Lock 14 grammar-name + grammar-shape leak census + single-generator gate | 3 (Lock 14, Lock 16 cross-ref, ARCHITECTURE/MASTER NEON+value narrative) | Yes; Pass Omega doc-only, consumed by W2 generator + CSS provider gates. | intrinsic-block for any generic-crate grammar branch, any hand-written per-grammar runtime file, or any fleet-wide value-fold claim without Sheets/BBNF-self witness; no challenge-time implementation. |
| D-SKV17-L16-neon-classifier-manifest | 4-6 | medium | W-scan / W-fold-gate | Lock 16 manifest row, strict checkasm (`BBNF_SIMD_STRICT=1`), aarch64 hardware gate, same-wave consumer | 3 (Lock 16, Lock 14 cross-ref, MASTER-PLAN §4 SIMD allowlist) | Yes; Pass Omega doc-only, consumed by the Lock-16 primitive gate. | intrinsic-block for primitive admission without manifest + scalar-ref + checkasm + aarch64 + same-wave consumer; no x86 close-path admission; scalar passthroughs declared `scalar-delegate-non-ASM`. |

## Disposition Matrix

Every candidate receives exactly one of **ACCEPT / REJECT / MODIFY / DEFER**. A
silent drop is forbidden (CH1 + CH6 REJECT class). The three Open Research
Questions are dispositioned as ACCEPT (crystallised into the relevant clause)
per the LOCKED T-P2 §3Z recommendations — they are NOT engineered-defers; each
names a concrete SK-V18 verify-action with a receiving gate.

| candidate | source | target locks | disposition | folds into | supporting path:line evidence | rationale |
|---|---|---|---|---|---|---|
| **LAC-2F-FOLD-01** | 2F | L01, L10 | **ACCEPT** | D-SKV17-L01 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:580`; `restart/locks/LOCKS.md:75`; `crates/core/src/runtime/css_l4/builder.rs:16`; `skinny/crates/runtime/src/tape/mod.rs:94`; `restart/skinny/tranches/sk-v17/SPEC.md:110`-`114` | The eager-OpenFrame retirement + AoS→SoA single-encoding closure + all-8 OnceCell substrate_target pre-gate is the LOCKED §3Z fold #1; carried clean V2→V3. Accept verbatim into the Lock 1 substrate-union clause. |
| **LAC-2F-FOLD-02** | 2F | L01, L10 | **ACCEPT** | D-SKV17-L10 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:581`; `restart/locks/LOCKS.md:100`-`116` (LAC-1E-14 precedent), `:107`-`109`; `restart/ARCHITECTURE.md:1088`; `restart/skinny/tranches/sk-v17/SPEC.md:808` | The tape-as-substrate-manifest-CATEGORY (not a 6th `BackendShape`) is the LOCKED §3Z fold #4 — the dispatch's "propose, do NOT silently add a 6th" discharged in the negative on two independent grounds. Accept; the 5-shape canon is preserved verbatim. |
| **LAC-2F-FOLD-03** | 2F | L14, L16 | **ACCEPT** | D-SKV17-L14 (generality half), D-SKV17-L16 (manifest half) | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:582`; `restart/locks/LOCKS.md:453`-`489`, `:137`-`149`; `restart/skinny/tranches/sk-v17/SPEC.md:314`-`317`; `crates/simd-scan/src/lib.rs:80`; `skinny/crates/bbnf-simd/src/dispatch.rs:42` | The shared NEON classifier Lock-16 manifest row + JSON-first→alphabet-as-data narrative fold is the LOCKED §3Z fold #3 (carried the lone V2 REVISE CH1-2F-01-RESIDUAL, now folded). The candidate spans two locks; split across the Lock 14 generality clause and the Lock 16 manifest clause. |
| **LAC-2F-FOLD-04** | 2F | L01 | **ACCEPT** | D-SKV17-L01 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:583`; `restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`; `crates/ir/src/registry/struct.rs:84,202,313,331`; `crates/core/src/runtime/tape/mod.rs:185`-`186` | The no-per-leaf-registry-lookup fence is the LOCKED §3Z fold #5 (the regression firewall, AZ-IV pre-blocked). Accept into the Lock 1 substrate-union clause as a co-load-bearing fence beside the eager-builder retirement. |
| **LAC-2F-FOLD-05** | 2F | L02 | **MODIFY** | D-SKV17-L02 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:584`; `restart/locks/LOCKS.md:160`,`:162`-`166`; `grep StructLayout crates/`=960, `grep 'backend_shape\|LayoutFacts' crates/`=0; `skinny/crates/passes/src/lib.rs:90,:96,:385` | The Lock-2 reconcile is a sub-surface refinement (the §3Z sixth LAC, not one of the five core fold designs). MODIFY: the locks crystallisation does NOT choose path-(a) full-rename vs path-(b) side-table; it records both priced paths and bars Lock-2 closure by `LayoutFacts` alone — the route choice is an SK-V18 wave decision, not a lock edit. |
| **LAC-1E-SKV17-01** | 1E | L01, L10 | **ACCEPT** | D-SKV17-L01 | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:178`; `restart/locks/LOCKS.md:75`; `crates/core/src/runtime/tape/record.rs:103`; `skinny/crates/runtime/src/tape/mod.rs:94` | The T-P1 one-substrate closure obligation (exactly ONE encoding, dual end-state is NOT permissible) is the antecedent LAC-2F-FOLD-01 extends into fold-direction form. Accept; subsumed by the fold LAC. |
| **LAC-1E-SKV17-02** | 1E | L01, L10 | **ACCEPT** | D-SKV17-L01 | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:179`; `restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`; `crates/ir/src/registry/struct.rs:84,202,313`; `crates/core/src/runtime/tape/mod.rs:185`-`186` | The T-P1 no-per-leaf-registry-lookup fence is the antecedent LAC-2F-FOLD-04 extends. Accept; subsumed by the fold LAC's fence text. |
| **LAC-1E-SKV17-03** | 1E | L01, L14 | **ACCEPT** | D-SKV17-L01 (classification), D-SKV17-L14 (8-grammar scan generality) | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:180`; `crates/core/src/grammar/generated/json.rs:732`, `css_l4.rs:15982`, `google_sheets.rs:3559`, `bbnf.rs:4843`; `restart/locks/LOCKS.md:118`-`127` | The all-8-grammar `OnceCell<StructuralIndex>` substrate_target classification (the COH-014 false-negative corrected to the all-8 census). Accept; carried in the Lock 1 pre-gate. |
| **LAC-1E-SKV17-04** | 1E | L02 | **MODIFY** | D-SKV17-L02 | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:181`; `restart/locks/LOCKS.md:160,:162`-`166`; `crates/ir/src/registry/struct.rs:202`; `grep LayoutFacts crates/`=0 | The T-P1 Lock-2 rename re-pricing (the V5-re-priced path-(b): `LayoutFacts` skinny/prior-totality-only, path-(b) core realisation non-zero). MODIFY for the same reason as LAC-2F-FOLD-05: record both priced paths, do not choose the route in the lock. |
| **LAC-1E-SKV17-05** | 1E | L10 | **ACCEPT** | D-SKV17-L10 | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:182`; `restart/ARCHITECTURE.md:1206,:1279`-`1280`; `restart/locks/LOCKS.md:107`-`108,:520`-`533`; `restart/skinny/tranches/sk-v17/SPEC.md:258` | The aarch64 CollapsedStage as the spec-named UNKNOWN-2D-05 (5-shape canon holds, absorbs aarch64-NEON without a 6th shape, 0 LOCKS edit). Accept; carried in the Lock 10 clause. |
| **LAC-1E-SKV17-06** | 1E | L16 | **ACCEPT** | D-SKV17-L16 | `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:183`; `crates/simd-scan/src/lib.rs:80`; `skinny/crates/bbnf-simd/src/dispatch.rs:42`; `restart/skinny/tranches/sk-v17/SPEC.md:258` | The multi-arch `crates/simd-scan` scope reconcile (narrow-to-aarch64 vs retain x86/avx2/wasm kernels WITHOUT admitting x86 as a close path). Accept; carried in the Lock 16 clause as the scope-reconcile rider on the manifest row. |
| **2F-FOLD-U1** | 2F ORQ | L01 | **ACCEPT** (crystallised) | D-SKV17-L01 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:563`; `crates/core/src/runtime/tape/mod.rs:6`-`9`; `restart/skinny/tranches/sk-v17/SPEC.md:110`-`114`; `restart/locks/LOCKS.md:75` | SoA `Tape` as the declared SK-V18 convergence-target encoding vs AoS-keep-and-prove-parity. ACCEPT the §3Z recommendation (SoA, the proven-and-benched encoding) as crystallised disposition: the lock requires exactly-one-encoding; the SoA-vs-AoS adopt choice lands at the W2 SoA-convergence gate. Receiver: SK-V18 W2. Blocker: AoS→SoA fold-state must collapse to ONE encoding before close. Gate: W2 substrate-union gate. Not a defer — the lock text names the closure obligation; the encoding choice is an implementation route the same clause governs. |
| **2F-FOLD-U2** | 2F ORQ | L01, L14 | **ACCEPT** (crystallised) | D-SKV17-L01 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:564`; `restart/skinny/tranches/sk-v17/SPEC.md:577,:825,:839`; `restart/locks/LOCKS.md:118`-`127` | Each of the 8 `OnceCell<StructuralIndex>` carriers classified `existing_tape` vs `local_temp_only` BEFORE wiring, else REDRESS-53 re-entry. ACCEPT as crystallised pre-gate disposition: the Lock 1 clause requires the all-8 substrate_target declaration before any tape wiring. Receiver: SK-V18 W2 OnceCell-classification pre-gate. Blocker: a retained parallel index re-opens REDRESS-53. Gate: W2 §9-condition-1 (index == tape-offsets identity). |
| **2F-FOLD-U3** | 2F ORQ | L10 | **ACCEPT** (crystallised) | D-SKV17-L10 | `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:565`; `restart/ARCHITECTURE.md:1282,:1151`; `restart/skinny/tranches/sk-v17/SPEC.md:854` | Whether aarch64 CollapsedStage ever admits, or NEON permanently sits under the 4 LLVM shapes' scan-leaf FFI. ACCEPT the §3Z assertion (NO admission without a 2E source-backed aarch64 strategy; no x86 close path, no D6 second substrate) as crystallised disposition. Receiver: the EXISTING 5-shape `BackendShape` gate (`restart/locks/LOCKS.md:107`-`109`) plus the G-Omega 6th-shape amendment path (`restart/locks/LOCKS.md:109`) — no phantom future wave is named as receiver. Blocker (precondition for ANY future ADD): `admits_collapsed_stage` x86-binding mechanically refuses aarch64 (`restart/ARCHITECTURE.md:1151`,`:1282`); UNKNOWN-2D-05 requires a 2E source-backed aarch64-strategy wave before any admission can even be proposed, and any sixth shape stays G-Omega-gated. Gate: the 5-shape `BackendShape` gate. The 2E-source wave is the blocker precondition for a future ADD, NOT a named existing receiver. |

### Disposition tally

| disposition | count | candidates |
|---|---:|---|
| ACCEPT | 9 | LAC-2F-FOLD-01, LAC-2F-FOLD-02, LAC-2F-FOLD-03, LAC-2F-FOLD-04, LAC-1E-SKV17-01, LAC-1E-SKV17-02, LAC-1E-SKV17-03, LAC-1E-SKV17-05, LAC-1E-SKV17-06 |
| ACCEPT (ORQ crystallised) | 3 | 2F-FOLD-U1, 2F-FOLD-U2, 2F-FOLD-U3 |
| MODIFY | 2 | LAC-2F-FOLD-05, LAC-1E-SKV17-04 |
| REJECT | 0 | — |
| DEFER | 0 | — |

The two MODIFYs are the Lock-2 `StructLayout` reconcile pair: the locks
crystallisation records BOTH priced paths (path-(a) 960-site rename; path-(b)
side-table) and bars Lock-2 closure by `LayoutFacts` alone, but does NOT pick the
SK-V18 implementation route inside the lock text — route selection is a wave
decision the clause governs. This is the standard "do not choose the
implementation route in locks" MODIFY, identical in kind to the prior-totality 3C
treatment of `LAC-1E-V1-04`
(`restart/audit/totality/p3/3C-locks-crystallisation.md:85`).

**Why a 0-REJECT / 0-DEFER tally is not paper-close.** T-P3 crystallises rather
than re-adjudicates because its inputs are §3Z-LOCKED, not open: T-P1 is
clean-final/G1-auto-pinned and T-P2 converged at V2=98.6% + V3=100.0% ACCEPT with
zero orphan REVISE (`restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:15`-`19`).
A LOCKED fold design carries no live REJECT to surface — the adjudication already
happened in T-P2. The REJECT-equivalent content survives in the five refutation
rows below (6th-shape, per-leaf `StructRegistry::layout`, AoS/SoA dual end-state,
fleet-wide value-plane, x86/SVE close route), each crystallised as REJECT-class
clause text inside the addendum rather than as a disposition-row REJECT. The
0-REJECT tally is therefore the LOCKED-input provenance, not an absence of
hostile content; a hostile re-read of the gate object finds the refutations
intact in §"Refutation rows preserved".

## Refutation rows preserved (the most load-bearing)

The crystallisation preserves every LOCKED T-P2 §3Z refutation as a REJECT-class
clause inside the addendum text — refutations, not deferrals:

1. **A 6th `BackendShape` variant — REFUTED** (folds into D-SKV17-L10). The tape is the substrate the 5 shapes project from (LAC-1E-14 precedent + `admits_collapsed_stage` x86-binding); a 6th variant is G-Omega-gated (`restart/locks/LOCKS.md:109`) and SK-V17 §9-barred (`restart/skinny/tranches/sk-v17/SPEC.md:808`).
2. **Per-leaf runtime `StructRegistry::layout(rule)` projection walk — REFUTED** (folds into D-SKV17-L01). Re-opens the 28-65×/983×/10583× regression (`restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`); the walk MUST be compile-time resolved once. AZ-IV indirection pre-block held.
3. **AoS/SoA dual end-state — REFUTED as a Lock-1 closure** (folds into D-SKV17-L01). Exactly ONE encoding survives (`restart/locks/LOCKS.md:75`); coexistence is admissible ONLY as a transient fold-state.
4. **Fleet-wide value-plane proof — REFUTED** (folds into D-SKV17-L14). The `ValueRef<G>` value-plane fold is JSON+CSS-exercised only; Sheets/BBNF-self are by-construction under SK-V18, not by-exercise. Lock 14 grammar-neutrality is preserved without a fleet-wide over-claim.
5. **Any x86/AVX-512/SVE close route — REFUTED** (folds into D-SKV17-L16 + D-SKV17-L10). aarch64 is primary; no SVE; asmjson/Sneller framed host-blocked, diagnostic-only.

## Consequences

**Positive.** The diff turns the five LOCKED fold designs + their T-P1
antecedents + three ORQs into one coherent SK-V17 lock addendum, keeps the 16-lock
count and 5-shape canon visible and verbatim, and crystallises the load-bearing
tape-as-substrate verdict in the negative (no silent 6th shape) on two independent
grounds. It resolves the substrate-classification, ValueRef-plane, and
NEON-manifest gaps as governance text without authorizing any SK-V18 wave.

**Cost.** This is a documentation-only proposal with per-clause propagation costs
stated above (2-8 doc LOC per clause; high risk on the substrate-union and
value-generalisation clauses because they fence the worst measured regressions).
Pass Omega CRUD decides whether to keep the addendum as one section or distribute
each clause into the affected lock body — a CH1 placement question, not a
disposition question. **Distribution invariant**: under ANY distribution, the
Lock-10 tape-category clause MUST retain an inline cross-reference to the Lock-1
substrate manifest (`substrate_target = existing_tape`, `restart/locks/LOCKS.md:118`-`127`).
Severing that cross-ref re-opens a silent-6th-shape reading — the tape's
substrate-CATEGORY placement is load-bearing precisely because it is anchored to
the Lock-1 manifest, not free-standing in Lock 10. No implementation is authorized;
no clause may launder wave overflow into challenge time.

**Propagation.** This touches `LOCKS.md` only. 3A (`ARCHITECTURE.md` §7.3 / 1088
substrate-projection + value-plane + NEON narrative), 3B (`MASTER-PLAN.md` §H fold
waves + §13 SIMD), and 3E (per-grammar `BackendShape` matrix + Lock 14
generalisation) cross-reference the same 5 delta ids. The 5-shape canon must stay
coherent across 3A/3B/3E per PASS-3 §8.2; D-SKV17-L10 is the binding coherence
clause.

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 | Should Pass Omega distribute the SK-V17 addendum under each lock body (Lock 1 / 2 / 10 / 14 / 16) instead of appending one SK-V17 addendum section after the SK-V15 addendum? | Pass Omega CRUD owner for `restart/locks/LOCKS.md` | Governance-style placement only; no LAC is deferred by this decision. | CH1 path-resolution + clean `git apply --check` of `3c-locks-v+1-diff.md`. |
| CH4 | Are the 5 clauses (folding 14 candidates) too dense for downstream SK-V18 wave owners to consume per-clause? | Pass Omega CRUD owner + 3A/3B/3E propagation owners | Downstream split may be needed if one addendum section hides a wave/gate responsibility for the eager-builder-retirement, ValueRef-generator, or NEON-manifest moves. | CH4 per-clause cost matrix + same-wave consumer/gate review. |
| CH6 | Can any candidate be treated as already satisfied by current `LOCKS.md` text (e.g. LAC-2F-FOLD-02 by the in-force LAC-1E-14 FactStream category at `restart/locks/LOCKS.md:100`-`116`)? | Pass Omega CH1/CH6 reviewers | Current text must already contain resolving evidence with valid path:line citations and no paper close; the SK-V17 clause makes the tape-as-substrate placement explicit where LAC-1E-14 carved only the FactStream category. | CH1 citation-resolution matrix + CH6 anti-paper-close review. |
