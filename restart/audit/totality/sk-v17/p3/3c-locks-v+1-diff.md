---
agent: 3C
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-29T00:00:00Z
master_head: 2a76916ac1959ef027df4d28e09be2b0b0bbec7f
v1_surface_targeted: LOCKS.md
gate_object: G-Omega
proposed_deltas_count: 5
delta_summary:
  carried_from_prior_cycle: [D-SKV17-L01-tape-substrate-union, D-SKV17-L02-structlayout-reconcile, D-SKV17-L10-tape-category-not-sixth-shape, D-SKV17-L14-valueref-classifier-generalisation, D-SKV17-L16-neon-classifier-manifest]
  removed: []
  answered: [LAC-2F-FOLD-01, LAC-2F-FOLD-02, LAC-2F-FOLD-03, LAC-2F-FOLD-04, LAC-2F-FOLD-05, LAC-1E-SKV17-01, LAC-1E-SKV17-02, LAC-1E-SKV17-03, LAC-1E-SKV17-04, LAC-1E-SKV17-05, LAC-1E-SKV17-06, 2F-FOLD-U1, 2F-FOLD-U2, 2F-FOLD-U3]
  newly_added: []
prior_cycle_dispositions_folded:
  accepted: [T-P1-V5-clean-final-G1-auto-pinned, T-P2-V3-normal-3Z-lock]
  rejected: []
  revised: [CH6-V1-07-u3-receiver-reanchor, CH6-V1-09-locked-input-provenance, CH1-V1-hunk-header-arithmetic, CH5-V2-R01-distribution-invariant-gate-object]
---

# 3C LOCKS v+1 Proposed Diff — SK-V17 Tape-Fold (cycle V3)

## Executive Summary

This is a **proposed-only** line-level diff for `restart/locks/LOCKS.md` and the
**G-Omega gate object** for T-P3 SK-V17. It adds one **SK-V17 T-P3 Crystallisation
Addendum** immediately after the in-force `## SK-V15 T-P3 v+1 Crystallisation
Addendum` (`restart/locks/LOCKS.md:581`-`608`) and before the existing `## v+1
Governance Boundary` (`restart/locks/LOCKS.md:610`). The hunk inserts at the blank
lines 608-609. It crystallises the five LOCKED T-P2 fold designs
(`LAC-2F-FOLD-01..05`), their six T-P1 antecedents (`LAC-1E-SKV17-01..06`), and the
three Open Research Questions (`2F-FOLD-U1/U2/U3`) into five lock-addendum clauses
on Locks 1, 2, 10, 14, and 16.

The hunk **preserves the 16 numbered locks**, **preserves the five `BackendShape`
variants** `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`, and
**adds no new directive, BIR variant, substrate, public substrate API, retained
sidecar, lock, lock retirement, or sixth shape**. Candidate coverage, per-row
dispositions (9 ACCEPT, 3 ORQ-ACCEPT, 2 MODIFY, 0 REJECT, 0 DEFER), and per-clause
costs are in `3c-locks-crystallisation.md`. T-P3 proposes; Pass Omega CRUD applies
this post-G-Omega.

## V3 Delta Summary

This is cycle V3. The five lock-addendum clauses carry forward unchanged in count
and body; the cycle folds the V2 CHALLENGE REVISEs only. No clause is added,
removed, or answered anew.

| class | disposition | evidence |
|---|---|---|
| Carried from prior cycle | All five addendum clauses (Lock 1 / 2 / 10 / 14 / 16) carry from V2 with body and citation base intact. | `3c-locks-v+1-diff.md:58`-`66`. |
| Removed | None. T-P3 is proposal-only and may not silently drop a candidate (`restart/prompts/totality/PASS-3-SYNTHESIS.md:228`). | n/a |
| Answered | None. The V3 fold is REVISE-class only. | n/a |
| Newly added | None at V3. | n/a |
| Revised (folded prior dispositions) | **CH5-V2-R01** (`V2/CH5.md:123`-`155`, REVISE): the R03 distribution invariant landed in `3c-locks-crystallisation.md:197`-`200` but was ABSENT from this gate object's Invariant Check (`grep -c "distribution invariant" 3c-locks-v+1-diff.md` = 0). The gate object travels to Pass Omega CRUD; the apply-time constraint must ride it. FOLDED: the distribution-invariant bullet is now in the Invariant Check after the "No new directive …" bullet (`:88`), copied verbatim from the crystallisation doc per the V2 concrete fix. **CH6-V1-07-u3-receiver-reanchor / CH6-V1-09-locked-input-provenance / CH1-V1-hunk-header-arithmetic** carried in V2 body (U3 receiver re-anchored to the existing 5-shape gate + G-Omega path; 0-REJECT defended on LOCKED-input provenance with five preserved refutation rows; hunk header `@@ -606,7 +606,22 @@` arithmetic-correct, `git apply --check` EXIT 0). | `restart/audit/totality/sk-v17/p3/hardening/V2/CH5.md:43`,`:99`,`:123`-`155`; `.../V1/CH1.md`; `.../V1/CH6.md`. |

## Proposed Unified Diff

```diff
diff --git a/restart/locks/LOCKS.md b/restart/locks/LOCKS.md
--- a/restart/locks/LOCKS.md
+++ b/restart/locks/LOCKS.md
@@ -606,7 +606,22 @@
 
 - Lock 16 primitive-manifest clause: every primitive or parse-that-family route records owner (`parse-that-regex`, `bbnf-simd`, or generated provider), scalar oracle, strict parity/checkasm command, Apple M5 Max/aarch64 hardware gate or explicit fallback, same-wave consumer, row movement target, lock16 status, fallback state, LOC/risk/wave owner/hard-cap fit, rollback/abrogate rule, and final disposition. Legacy `skinny/crates/bbnf-regex` is a temporary path awaiting Lock 11 rename cleanup, not an admissible future owner or peer owner. `scalar-delegated` is an admissible fallback only when no SIMD row movement is claimed; source inventory and `bbnf.asm` macro names are not admission. Non-strict parity is exploratory. PMU counters support close only with row-local command/input/equality/timing and no broadcast group. SVE/SVE2 primitives must not be filed as NEON/AdvSIMD; future `svmatch_u8` requires an SVE2 host and scalable-vector dispatch. PMULL/CSSC, CollapsedStage, DotProd/I8MM, ternary bitwise, and CSS semantic reuse require the same manifest and consumer proof before admission. Runtime regex/DFA manifest and consumer proof are necessary but never sufficient; any runtime regex/DFA substrate requires prior G-Omega amendment to Lock 1 before Lock 16 admission can proceed. Evidence: `restart/audit/totality/p1/1E-locks-evidence.md:143`, `restart/audit/totality/p2/2A-sota-landscape.md:111`, `restart/audit/totality/p2/2B-primitive-vocabulary.md:201`-`204`, `restart/audit/totality/p2/2E-host-arch-esoterica.md:139`-`141`, `restart/audit/totality/p2/2F-parse-that-gaps.md:119`-`122`.
 
+
+## SK-V17 T-P3 Crystallisation Addendum
+
+This addendum is the SK-V17 T-P3 3C tape-fold crystallisation. It preserves the 16 numbered locks and the exact five BackendShape variants `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`; it adds no directive, BIR variant, substrate, public substrate API, retained sidecar, lock, lock retirement, or sixth shape. T-P1 is a clean-final/G1-auto-pinned input; T-P2 is the normal Section 3Z locked research input (V2=98.6% + V3=100.0% ACCEPT, zero orphan REVISE). It crystallises the five LOCKED T-P2 fold designs (LAC-2F-FOLD-01..05), their six T-P1 antecedents (LAC-1E-SKV17-01..06), and the three Open Research Questions (2F-FOLD-U1/U2/U3). The monotonic direction is skinny→totality: the SKINNY-proven engine wins V1-authoritative; the totality spec never dictates back to a live skinny iteration. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:80`-`82`, `restart/audit/totality/sk-v17/p2/hardening/HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:15`-`19`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:616`-`623`.
+
+- Lock 1 tape-substrate-union clause: the SK-V18 fold retires the live eager `OpenFrame` builders (`crates/core/src/runtime/css_l4/builder.rs:16` 817 LOC, `crates/core/src/runtime/json/builder.rs:9`) into the flat-tape commit-by-construction, converges the AoS `TapeRec` (`crates/core/src/runtime/tape/record.rs:103`, 16-byte/align-4 const-asserted) onto the PROVEN-AND-BENCHED SoA `Tape<'input>` (`skinny/crates/runtime/src/tape/mod.rs:94`) as the SINGLE post-fold encoding, and declares `substrate_target` on ALL 8 `OnceCell<StructuralIndex>` carriers (json/ebnf/bnf/csv/css_l4/css_pretty/google_sheets/bbnf) before any tape wiring. Exactly ONE encoding survives (`restart/locks/LOCKS.md:75`); a dual AoS/SoA end-state is REJECT as a Lock-1 closure, admissible ONLY as a transient fold-state. The SoA `Tape` is the recommended convergence-target encoding (the proven-and-benched form); the AoS→SoA adopt-vs-parity choice lands at the SK-V18 substrate-union gate, governed by the exactly-one-encoding obligation. The `FieldSource` projection walk inside the live `StructRegistry` (`crates/ir/src/registry/struct.rs:84,313`) is compile-time emission resolved once at codegen; ANY per-leaf runtime `StructRegistry::layout(rule)` indirection in the tape/projection hot path re-opens the measured 28-65×/983×/10583× regression (`restart/skinny/tranches/sk-v17/SPEC.md:793`-`795`) and is REJECT. `begin_compound` reads `layout.rule_id & 0x1F` only (`crates/core/src/runtime/tape/mod.rs:185`-`186`, grep-zero `StructRegistry`); the live coupling site `crates/core/src/runtime/bbnf/arena.rs:47` (`match StructRegistry::compound_kind_for_layout(layout)`) is severed by the eager-builder retirement. Each `OnceCell<StructuralIndex>` carrier must resolve to `existing_tape` (index IS the tape) or `local_temp_only` before wiring, else a retained parallel index re-opens REDRESS-53 (`restart/skinny/tranches/sk-v17/SPEC.md:577`,`:825`,`:839`). Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:116`,`:126`,`:128`,`:140`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:580`,`:583`.
+
+- Lock 2 StructLayout-reconcile clause: `StructLayout` is Lock-2-retired (canonical name `Layout`/`LayoutFacts`, `restart/locks/LOCKS.md:160`) yet live across 960 sites in `crates/`. The reconcile is priced by TWO disjoint paths, neither chosen inside this lock: (a) full rename `StructLayout`→`Layout` across 960 generator-side sites, regenerating 8 parsers + ~16 tests; (b) re-scope toward a `LayoutFacts.backend_shape` side-table — but `LayoutFacts`/`backend_shape` are grep-zero in `crates/` (skinny/prior-totality-only, `skinny/crates/passes/src/lib.rs:90,:96`), so path-(b)'s `crates/core` realisation is NON-ZERO, sized as the 0→N introduce-site delta (`grep StructLayout crates/`=960 rename surface vs `grep 'backend_shape\|LayoutFacts' crates/`=0 side-table surface). The v+1 note (`restart/locks/LOCKS.md:162`-`166`) bars Lock-2 closure by `LayoutFacts` alone while public `Layout`/`LayoutSink` remain absent; path-(b) is a re-scope, not a closure. Route selection is an SK-V18 wave decision the clause governs, not a lock edit. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:117`,`:130`,`:181`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:584`.
+
+- Lock 10 tape-category clause: the tape folds into the spec as the SUBSTRATE the five `BackendShape` shapes project from, recorded at the Lock 1 substrate manifest (`substrate_target = existing_tape`, `restart/locks/LOCKS.md:119`-`127`) per the LAC-1E-14 FactStream precedent (`restart/locks/LOCKS.md:100`-`116`) — NOT a 6th `BackendShape` variant. The five-shape Lock-10 search domain holds verbatim: `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (`restart/locks/LOCKS.md:107`-`108`). ARCH §7.3 already frames the five shapes as the substrate's projections (`restart/ARCHITECTURE.md:1088`); the fold makes the substrate-category placement explicit. A 6th variant remains G-Omega gated (`restart/locks/LOCKS.md:109`) and SK-V17 §9-barred (`restart/skinny/tranches/sk-v17/SPEC.md:808`). The verdict stands on TWO independent grounds: the categorical precedent, and the `admits_collapsed_stage` x86-binding (`restart/ARCHITECTURE.md:1151`,`:1282`) that mechanically refuses on aarch64. The aarch64 CollapsedStage is the spec-named UNKNOWN-2D-05 (`restart/ARCHITECTURE.md:1206`,`:1279`-`1280`), not a fresh gap; NEON sits under the four LLVM shapes' scan-leaf FFI. No aarch64 CollapsedStage admission lands without a 2E source-backed strategy; no x86 close path; no D6 second substrate (`restart/skinny/tranches/sk-v17/SPEC.md:854`). Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:118`,`:129`,`:182`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:581`.
+
+- Lock 14 ValueRef/classifier-generalisation clause: the lazy grammar-parametric `ValueRef<'doc,'input,K,G:EventGrammar>` projection (`skinny/crates/runtime/src/tape/mod.rs:175`) is the ONE materialization plane, re-emitted by a single grammar-agnostic generator (`restart/locks/LOCKS.md:349`) that resolves the layout once at codegen — replacing the per-grammar EAGER value enums (`crates/core/src/runtime/css_l4/value.rs:414`). The `G:EventGrammar` type parameter is the generality vehicle; the `@generated` per-grammar allowance keeps it grammar-neutral by construction. The fold is SCOPE-HONEST: the `ValueRef<G>` value-plane fold is exercised JSON+CSS ONLY; Sheets/BBNF-self are by-construction under SK-V18, NOT by-exercise (`sheets_witness` 24-LOC stub), and may not be claimed fleet-wide. The shared classifier's grammar-generality is config-breadth (alphabet-as-data) across 8 of 9 generated grammars — a SEPARATE axis from the value-fold, never the same as fleet-wide value-plane proof. preserve-rich-ast holds (`restart/skinny/tranches/sk-v17/SPEC.md:252`); the lazy view is the materialization plane, never a flattening of the typed AST. No grammar branch, no hand-written per-grammar runtime file, enters any generic crate. Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:119`,`:127`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:582`,`:530`-`534`.
+
+- Lock 16 NEON-classifier-manifest clause: the shared NEON `select_classifier(alphabet)` / `scan_structural(input, &StructuralAlphabet)` classifier is registered as a Lock-16 primitive-manifest ROW: abstract primitive = alphabet-parametrised byte classification; scalar reference `scalar/byte_class_from_eq_set_64.rs`; checkasm parity under `BBNF_SIMD_STRICT=1`; `substrate_target = existing_tape`; `retention_lifetime = transient-single-call` (no cross-call retained classifier state per the Lock 1 v+1 ELEVATION, `restart/locks/LOCKS.md:137`-`149`); same-wave consumer = the tape. The eq-set fan is the one proven NEON Layer-1 body (87 LOC, 8 distinct NEON intrinsics); `byte_class_from_table_64` and `bitmap_prefix_xor_64` are honestly-declared `scalar-delegate-non-ASM` passthroughs, not SIMD row-movers. The JSON-first NEON narrative folds to the alphabet-as-data form (`restart/skinny/tranches/sk-v17/SPEC.md:314`-`317`, the alphabet is the only grammar datum). The multi-arch `crates/simd-scan` scope-reconcile (narrow-to-aarch64 vs retain x86/avx2/wasm/scalar kernels) binds WITHOUT admitting x86 as a close path; aarch64 is primary, no SVE (`restart/skinny/tranches/sk-v17/SPEC.md:806`). Evidence: `restart/audit/totality/sk-v17/p1/1E-locks-evidence.md:120`,`:131`,`:183`, `restart/audit/totality/sk-v17/p2/2f-fold-gaps.md:582`.
+
 
 ## v+1 Governance Boundary
 
 The v+1 text above is active only because Pass Omega CHALLENGE converged and
```

## Apply-Check

The hunk inserts at `restart/locks/LOCKS.md:608`-`609` (the two blank lines after
the SK-V15 addendum's Lock 16 clause at `:607`, before the `## v+1 Governance
Boundary` heading at `:610`). The context lines are:

- pre-context: the SK-V15 addendum's Lock-16 clause (`restart/locks/LOCKS.md:607`) + one blank line (`:608`).
- insertion: the SK-V17 addendum (heading + intro + five clauses).
- post-context: the blank line + `## v+1 Governance Boundary` (`restart/locks/LOCKS.md:609`-`610`).

## Invariant Check

- **Numbered locks unchanged**: the current `restart/locks/LOCKS.md` carries 16 numbered lock headings (`restart/locks/LOCKS.md:75,160,170,179,181,183,200,202,260,269,319,328,336,349,436,453`). The hunk adds no numbered lock, retires none, renumbers none.
- **BackendShape canon = five variants verbatim**: `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` (`restart/locks/LOCKS.md:107`-`108`); the addendum restates the five in the heading and the Lock 10 clause. No sixth variant; a sixth remains G-Omega gated (`restart/locks/LOCKS.md:109`).
- **No new directive / BIR variant / substrate / public substrate API / retained sidecar**: the tape is recorded as a substrate-manifest CATEGORY (`substrate_target = existing_tape`), not a new substrate or a sixth shape; the NEON classifier is `retention_lifetime = transient-single-call`, no cross-call retained state; the `OnceCell<StructuralIndex>` carriers must declare `existing_tape` or `local_temp_only`, never a retained parallel index.
- **Distribution invariant**: if Pass Omega distributes the addendum clauses into Lock bodies, the Lock-10 tape-category clause MUST retain an inline cross-reference to the Lock-1 substrate manifest (`substrate_target = existing_tape`, `restart/locks/LOCKS.md:118`-`127`); a Lock-10 clause severed from the Lock-1 manifest anchor re-opens the silent-6th-shape reading. The present-state guard — the inline cross-ref the Lock-10 clause already carries at `3c-locks-v+1-diff.md:62` — is intact; this is a forward-distribution apply-time fence the gate object carries with it, not a present defect.
- **Governance boundary in force**: the addendum sits above the `## v+1 Governance Boundary` (`restart/locks/LOCKS.md:610`-`619`); it is proposed text only, applied by Pass Omega CRUD post-G-Omega.
- **Monotonic skinny→totality**: the addendum adopts the SKINNY-proven SoA `Tape`/`ValueRef<G>`/NEON model INTO core; it never relocates core constructs into skinny or dictates back to a live skinny iteration.
