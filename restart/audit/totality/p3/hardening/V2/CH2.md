---
lens: CH2
name: GENERALITY / LOCK 14
pass: T-P3-synthesis
cycle: V2
generated_at: 2026-05-23T23:55:00-04:00
disposition: ACCEPT
scope: "CH2 generality and Lock 14; V1 REVISE-CH2-V1-01 discharge verification across 4 amended 3F sites"
artifacts_audited:
  - restart/prompts/totality/PASS-3-SYNTHESIS.md
  - restart/audit/totality/p3/3A-architecture-synthesis.md
  - restart/audit/totality/p3/3B-master-plan-reconciliation.md
  - restart/audit/totality/p3/3C-locks-crystallisation.md
  - restart/audit/totality/p3/3C-locks-v+1-diff.md
  - restart/audit/totality/p3/3D-skinny-fold.md
  - restart/audit/totality/p3/3E-grammar-generalisation.md
  - restart/audit/totality/p3/3F-migration-handoff.md
  - restart/audit/totality/p3/hardening/V1/CH2.md
  - restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md
---

# T-P3 V2 CH2 Generality / Lock 14

## Lens Contract

PASS-3 §3 CH2 GENERALITY (`restart/prompts/totality/PASS-3-SYNTHESIS.md:108`-`111`)
binds Lock 14 to hold across 3A surface deltas, 3B wave reconciliation, and 3E
grammar-generalisation; 3E concrete for CSS L4 / Sheets / BBNF-self; 3C accepts
no JSON-narrowing amendment; the future-grammar onboarding test survives. PASS-3
§8.1 + §8.2 bind the 16-lock count and the 5-shape `BackendShape` canon as
invariants every artefact must preserve
(`restart/prompts/totality/PASS-3-SYNTHESIS.md:210`-`211`).

The V2 dispatch context narrows the CH2 lens to a single REVISE-discharge
target carried over from V1
(`restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md:23`,
`restart/audit/totality/p3/hardening/V1/CH2.md:78`-`131`):

1. **F-V2-CH2+CH6+CH7-3F-A — 3F-MIG-004 LAC-1E-14 misclassification.** V1
   REVISE-CH2-V1-01 found 3F-MIG-004 mis-classified `FactStream` as a "5th
   BackendShape variant (gates Lock 1 + Lock 10 v+1)" at three artefact-internal
   sites (`:104`, `:125`, `:311`). V2 must mirror 3C V4-3 hunk wording verbatim
   at all amended sites: `FactStream` = **5th SUBSTRATE category at Lock 1
   manifest, NOT 6th BackendShape variant**; the 5-shape `BackendShape` search
   domain at Lock 10 HOLDS; Lock 1 substrate manifest and Lock 10 BackendShape
   search domain are ORTHOGONAL axes.
2. **Cohort Lock 14 unchanged.** V1 ACCEPT-discharged F1, F3-F8 (7/8). V2 must
   verify no regression on the 5-shape canon, the 7-step onboarding test, the
   12 L14-HC clauses, the zero-JSON-narrowing discipline, and the LAC-2F-V5-02
   ELEVATED substrate-union strengthening.

## Verdict

**ACCEPT.**

V2 mechanically discharges the V1 REVISE-CH2-V1-01 defect at **four** 3F sites
(table row `:104`; proposed-text §4 `:125`; CH2 open question `:311`; discipline
citation `:327`). All four sites now mirror 3C V4-3 hunk wording verbatim:
`FactStream` lands as the 5th admitted-product category at the Lock 1
**SUBSTRATE manifest**, alongside `OffsetTape`/`EventTape`/`SinkOnly`/`CollapsedStage`;
it is **NOT a 6th `BackendShape` variant**; the 5-shape `BackendShape` search
domain at Lock 10 — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`
— HOLDS; the two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search
domain) are ORTHOGONAL; any 6th `BackendShape` variant remains G-Omega gated per
Lock 10 v+1 + PASS-3 §8.1. Lock 14 holds across the seven amended artefacts;
the V1 cohort ACCEPT rows (F1, F3-F8) carry forward unchanged.

The V2 dispatch context notes "all 3 sites" but the V2 amendment in fact
discharged a **fourth** site at `:327` (discipline citation §8.2 binding) which
is a CH2-positive over-discharge: the discipline-block now carries an explicit
mirror-clause naming the 3C V4 hunk V4-3 source, hardening the carrier-coherence
gate against future drift.

## Evidence

| check | disposition | evidence |
|---|---|---|
| **F1: 3F-MIG-004 table row (`:104`) mirrors 3C V4-3 verbatim** | ACCEPT | The table row now reads: "LAC-1E-14 lands `FactStream` as the 5th admitted-product category at the Lock 1 SUBSTRATE manifest (alongside OffsetTape/EventTape/SinkOnly/CollapsedStage), NOT a 6th `BackendShape` variant — the 5-shape `BackendShape` search domain at Lock 10 (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`) holds. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL; LAC-1E-14 touches the manifest axis only" (`restart/audit/totality/p3/3F-migration-handoff.md:104`). All four canonical wording elements present: (a) 5th SUBSTRATE category; (b) NOT 6th BackendShape variant; (c) 5-shape Lock 10 search domain HOLDS; (d) two axes ORTHOGONAL. Cites `3C-L01-factstream-fifth-category` (`restart/audit/totality/p3/3C-locks-crystallisation.md:32`) + V4-3 hunk verbatim (`restart/audit/totality/p3/3C-locks-v+1-diff.md:118`-`140`). |
| **F2: 3F-MIG-004 proposed-text §4 (`:125`) mirrors 3C V4-3 verbatim** | ACCEPT | The proposed MIGRATION.md delta paragraph now reads: "LAC-1E-14 lands `FactStream` as the **5th admitted-product category at the Lock 1 SUBSTRATE manifest**... The 5th category is a substrate-manifest classification only; it is **NOT a 6th `BackendShape` variant**. The 5-shape `BackendShape` search domain at Lock 10 — `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}` — HOLDS. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL; LAC-1E-14 touches the manifest axis only. Any 6th `BackendShape` variant remains G-Omega gated per Lock 10 v+1 + PASS-3 §8.1 (not in scope at this MIGRATION row)" (`restart/audit/totality/p3/3F-migration-handoff.md:125`). All four canonical elements present and emphatically bolded. Doc-only delta with zero impl-tail explicitly declared; W8 budget pinned at 3C-L01-factstream-fifth-category (60-150 docs). |
| **F3: 3F CH2 open question (`:311`) reframed as RESOLVED** | ACCEPT | The CH2 row in the V1 Gated Open Questions table now opens with "RESOLVED: LAC-1E-14 lands `FactStream` as 5th admitted-product category at the **Lock 1 SUBSTRATE manifest** (NOT a 6th `BackendShape` variant); the 5-shape `BackendShape` search domain at Lock 10 (`{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`) HOLDS. The two axes (Lock 1 substrate manifest vs Lock 10 BackendShape search domain) are ORTHOGONAL" (`restart/audit/totality/p3/3F-migration-handoff.md:311`). The forward question is reframed from the V1-faulty "5th variant vs `admitted_fact_output` substrate_target without canon expansion" into the V2-correct "does Pass Omega Ω-C ARCH-CRUD acceptance of the substrate-category-not-shape carrier wording surface any downstream coherence drift in 3A/3B/3D/3E?" — which is a CH6-positive: it converts an open dispositional question into a downstream coherence-gate check with explicit receiver (Ω-C ARCH-CRUD intake), explicit blocker (already dispositioned at 3C V1 ACCEPT + V4-3 hunk verbatim), and explicit gate (Ω-C confirms carrier wording across 3A executive summary + 3B coherence matrix + 3D fold row + 3E L14-HC-07 hardening clause). |
| **F4: 3F discipline citation (`:327`) carries §8.2 mirror clause (V2 over-discharge)** | ACCEPT | The Discipline Citations section now carries a NEW line: "5-shape BackendShape canon coherent across 3A + 3B + 3E per `restart/prompts/totality/PASS-3-SYNTHESIS.md §8.2:211` (3F MIG-004 fact-stream row mirrors 3C V4 hunk V4-3 verbatim: `FactStream` lands as 5th admitted-product category at the Lock 1 SUBSTRATE manifest, NOT a 6th `BackendShape` variant; 5-shape Lock 10 search domain HOLDS; the two axes are orthogonal)" (`restart/audit/totality/p3/3F-migration-handoff.md:327`). This site was not in the V1 REVISE-CH2-V1-01 enumeration; the V2 amendment over-discharges by inscribing the §8.2 binding into the citation block itself, creating a structural mirror-gate against future drift. All four canonical elements present. |
| **F5: 5-shape canon preserved across 3A/3B/3D/3E (V1 F1 carry-forward)** | ACCEPT | 3A executive summary preserves "the 5-shape canon, the substrate-union fence, and the no-new-directive/no-new-BIR/no-new-substrate gate" (`restart/audit/totality/p3/3A-architecture-synthesis.md:23`); ARCH-3A-D07 wording "NOT a sixth BackendShape" (`:39`); ARCH-3A-D07 cost row "Block if CSS fact stream becomes retained substrate or a sixth BackendShape" (`:75`). 3B coherence matrix row 1: "5-shape canon unchanged; FactStream is substrate-target classification, not 6th BackendShape" (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:182`); MP-3B-V1-D06: "The 5-shape BackendShape canon stays unchanged; the FactStream category is a substrate-target classification, not a 6th BackendShape" (`:127`). 3D row: "CSS fact streams = output planes, not 6th shape" (`restart/audit/totality/p3/3D-skinny-fold.md:183`); 3D FOLD-3D-013 cost row references 6-class anti-paper-close taxonomy. 3E: "keep the five `BackendShape` variants" (`restart/audit/totality/p3/3E-grammar-generalisation.md:32`); L14-HC-07: "fact streams are output planes... do not create a sixth `BackendShape`" (`:210`); 3E-D05 "Classify CSS fact streams as admitted output planes, not retained sidecars and not a sixth `BackendShape`" (`:225`). Zero cohort drift. |
| **F6: 3C accepts no JSON-narrowing amendment (V1 F3 carry-forward)** | ACCEPT | 3C V4 disposition matrix routes 51 candidates with 38 ACCEPT + 13 MODIFY + 0 REJECT + 0 DEFER (`restart/audit/totality/p3/3C-locks-crystallisation.md:54`-`59`); silent-drop census ZERO; V1 amended census-drop verified. The Lock 14 v+1 hunk permits generated files under `runtime/src/grammars/<name>/` only when produced by rostered generator (`restart/audit/totality/p3/3C-locks-v+1-diff.md:272`-`290`); no JSON-narrowing language anywhere in the V2-amended diff. Hunk V4-3 explicit: "5th admitted-product category at the Lock 1 substrate manifest... NOT a 6th `BackendShape` variant. The 5-shape `BackendShape` search domain at Lock 10 holds: `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`" (`:124`-`131`). |
| **F7: 3E concrete for CSS L4 (15 sub-grammars) / Sheets / BBNF-self (V1 F4 carry-forward)** | ACCEPT | 3E V4 5 shapes × 15 CSS L4 sub-grammar matrix (`restart/audit/totality/p3/3E-grammar-generalisation.md:93`-`128`); Other-Grammars matrix covers Sheets formulas/functions/arrays/infix + BBNF-self grammar/expression/directive (`:137`-`:149`); primitive vocabulary transfer table maps every primitive family across CSS L4/Sheets/BBNF-self (`:153`-`:163`). No V2 narrowing. |
| **F8: 7-step onboarding test survives intact (V1 F5 carry-forward)** | ACCEPT | 3E §"Future-Grammar Onboarding Test" reproduces all 7 steps verbatim (`restart/audit/totality/p3/3E-grammar-generalisation.md:165`-`190`). Fail-closed rule preserved: "if onboarding requires a new directive, BIR variant, `BackendShape`, public substrate API, retained sidecar, or hand-coded generic behavior" (`:189`). |
| **F9: Lock 14 v+1 holds across 3A + 3B + 3E surface deltas (V1 F6 carry-forward)** | ACCEPT | 3A ARCH-3A-D08 Pattern H = 67 hand-written runtime files across 9 grammars, 0/9 carry `@generated` markers (`restart/audit/totality/p3/3A-architecture-synthesis.md:40`); ARCH-3A-D09 binds Lock 14 zero-new-`.rs`-files invariant (`:41`). 3B MP-3B-V1-D03 Pattern H per-tranche census; MP-3B-V1-D09 Lock 14 v+1 generic-crate forward invariant (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`, `:130`). 3E L14-HC-01..L14-HC-12 twelve Lock 14 hardening clauses (`restart/audit/totality/p3/3E-grammar-generalisation.md:200`-`213`). 3C V2-amended Pattern H census command corrected to drop `-maxdepth 2` (per CH7 V2 discharge), now returns 67 across 9 dirs; the 3F-MIG-003 census command at `:123` continues to use `-mindepth 2 -maxdepth 2` (correct for the per-grammar runtime root scope vs cohort-wide). |
| **F10: LAC-2F-V5-02 ELEVATED preserved (V1 F7 carry-forward)** | ACCEPT | 3C V4 elevation of LAC-2F-V5-02 to STRONGEST AMENDMENT SURFACE preserved verbatim at V2; "no cross-call retained classifier state, period" continues to generalise REDRESS 96/97/98 to ALL transient classifier-state primitives (`restart/audit/totality/p3/3C-locks-crystallisation.md:31`, `:124`; v+1 diff hunk V4-2). CH2-positive: the elevation hardens Lock 1 substrate-union v+1 by forbidding cross-call carry that would otherwise create a hidden retained sidecar — which would in turn imply a 6th substrate plane. The V2 amendments do not weaken or rephrase this elevation. |
| **F11: V1 CH2 carry-forward constraints (1-7) hold at V2 HEAD (V1 F8 carry-forward)** | ACCEPT | V1 carry-forward constraint #4 ("Resolve CSS fact-stream placement as an output-plane taxonomy or `SinkOnly` product only if the five-shape canon and no-retained-sidecar rule are preserved; LAC-1E-14 V4 disposition confirms 5th *substrate* category, not 6th BackendShape — Pass Omega CRUD must consume 3C V4 hunk V4-3 wording verbatim and reject any 3F-MIG-004 wording until V2 repair lands") is now FULLY DISCHARGED by V2: 3F-MIG-004 at all 4 sites mirrors 3C V4 hunk V4-3 verbatim. V1 carry-forward constraints #1-3, #5-7 (generated-output fence, negative-control, provider-manifest, primitive policy, L14-HC-09 enum-drift, L14-HC-10 pass-layer leak) remain in force. |

## Cycle Disposition

**CH2 disposition for T-P3 V2: ACCEPT.**

All 11 evidence checks ACCEPT (11/11 = 100%). The V1 REVISE-CH2-V1-01 defect is
mechanically discharged at 4 amended 3F sites (one over-discharge at `:327`
discipline citation, hardening the §8.2 mirror gate). The remaining cohort
ACCEPT rows from V1 (F1, F3-F8) carry forward unchanged with zero regression on
the 5-shape canon, Lock 14 v+1, 7-step onboarding, zero-JSON-narrowing, and
LAC-2F-V5-02 substrate-union strengthening.

CH2 V1 → V2 trajectory: 7/8 ACCEPT (87.5%) → 11/11 ACCEPT (100%). The V2 cycle
satisfies the §3Z LOCK-eligibility threshold for CH2 (≥95%). With CH2 V2 at
100% and per §3Z requiring two consecutive ≥95% cycles, V3 confirming pass
must hold CH2 ACCEPT at ≥95% to close the cohort §3Z LOCK at V2+V3.

## Carry-Forward Constraints (Pass Omega / S-P3)

V1 CH2 carry-forward constraints (1-7) remain in force with one V2 sharpening:

1. **(V1-carried)** Preserve the exact Lock 14 fence from 3C: generated grammar
   names are allowed only as rostered generated output, never as hand-coded
   generic provider or role-policy branches.
2. **(V1-carried)** Do not reduce the negative-control rule below the T-P2/3E
   standard. A fleet-wide generality claim needs CSS L4 plus Sheets or BBNF-self
   witness/negative-control; the single CSS L4 declaration-values row remains
   admitted evidence only.
3. **(V1-carried)** Resolve the provider-manifest layout in the Lock 14 registry
   wave by proving JSON, CSS, and a Sheets or BBNF-self provider without editing
   generic code.
4. **(V1-DISCHARGED, V2-LOCKED)** Resolve CSS fact-stream placement as an
   output-plane taxonomy preserving five shapes and no-retained-sidecar rule.
   **LAC-1E-14 V4 disposition is now mirrored verbatim across 3C V4-3 hunk + 3F
   at 4 sites (`:104`, `:125`, `:311`, `:327`); Pass Omega Ω-C ARCH-CRUD intake
   may consume any of the four sites as canonical.** The 3F open-question
   reframe (`:311`) explicitly routes downstream coherence-drift checks to Ω-C
   intake gating CRUD-1 §9.2 fold.
5. **(V1-carried)** Keep shared primitive policy caller/generated-owned. JSON
   punctuation, string, number, quote, escape, and no-string/no-number policy
   must not become shared crate constants.
6. **(V1-carried)** 3E L14-HC-09 RuntimeProvider 2→8 enum-drift fault baseline:
   future grammar additions MUST land via generated manifest + workspace
   metadata, never by editing `skinny/crates/codegen/src/grammar_profile.rs`.
7. **(V1-carried)** 3E L14-HC-10 pass-layer JSON-byte/literal leak repair:
   Sheets/BBNF-self onboarding requires BOTH 1B-D8 recognizer-byte plane AND
   1B-D10 materialization-role plane sourced from generated grammar metadata.
8. **(V2-NEW)** 3F discipline-citation `:327` §8.2 mirror clause is now
   structural: any future amendment to 3F MIG-004 wording must re-mirror 3C V4
   hunk V4-3 at all four sites simultaneously. The `:327` citation acts as a
   self-referential mirror-gate; modifying any one of `:104`/`:125`/`:311`
   without re-mirroring `:327` (or vice versa) is a CH2 coherence-break that
   re-opens REVISE-CH2-V1-01.

## Findings

### F-CH2-V2-01 — V1 REVISE-CH2-V1-01 fully discharged across 4 sites

The V1 blocking defect (3F-MIG-004 misclassifying LAC-1E-14 as a "5th
BackendShape variant gating Lock 1 + Lock 10 v+1") is mechanically discharged
at all 4 sites in `restart/audit/totality/p3/3F-migration-handoff.md`:

- `:104` table row — mirrors 3C V4-3 hunk verbatim, all 4 canonical elements
  present.
- `:125` proposed-text §4 — mirrors 3C V4-3 hunk verbatim, all 4 canonical
  elements present and emphatically bolded; doc-only delta with zero impl-tail
  explicitly declared.
- `:311` CH2 open question — reframed as RESOLVED with explicit Pass Omega Ω-C
  ARCH-CRUD downstream coherence-gate check.
- `:327` discipline citation — V2 over-discharge: §8.2 mirror clause structurally
  inscribed into the citation block, creating a self-referential mirror-gate
  against future drift.

### F-CH2-V2-02 — Cohort 3A/3B/3D/3E carrier wording remains coherent

The V1 ACCEPT-discharged carrier wording in 3A executive summary (`:23`),
ARCH-3A-D07 (`:39`), 3B coherence matrix row 1 (`:182`), MP-3B-V1-D06 (`:127`),
3D fold row (`:183`), 3E L14-HC-07 (`:210`), and 3E-D05 (`:225`) all remain
verbatim at V2 HEAD. No V2 amendment to any cohort artefact introduces drift.

### F-CH2-V2-03 — Lock 14 12-clause hardening matrix holds at V2 HEAD

3E L14-HC-01..L14-HC-12 twelve Lock 14 hardening clauses
(`restart/audit/totality/p3/3E-grammar-generalisation.md:200`-`213`) carry
forward unchanged at V2; the V1 carry-forward additions (L14-HC-09
RuntimeProvider enum-drift, L14-HC-10 pass-layer JSON-byte/literal leaks) bind
the next-cycle implementation gates.

### F-CH2-V2-04 — V2 §3Z LOCK-eligibility satisfied for CH2

CH2 V2 = 11/11 ACCEPT (100%); satisfies the §3Z ≥95% threshold. Per
§3Z requiring two consecutive ≥95% cycles, V3 confirming pass must hold CH2
ACCEPT at ≥95% to close cohort §3Z LOCK at V2+V3. The V2 amendments are
mechanical and bounded; no V3 regression risk anticipated absent
artefact-internal coherence breaks in 3A/3B/3D/3E.
