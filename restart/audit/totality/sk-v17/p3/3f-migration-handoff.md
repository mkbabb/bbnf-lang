---
agent: 3F
pass: T-P3-synthesis
cycle: V3
generated_at: 2026-05-29T00:00:00Z
t_p1_inventories_consumed: [1a-substrate-evidence, 1b-codegen-evidence, 1c-runtime-evidence, 1d-skinny-lessons, 1e-locks-evidence, 1f-coherence-scan, 1f-anti-pattern, 1f-past-corpora]
t_p2_dossiers_consumed: [2a-sota-landscape, 2b-primitive-vocabulary, 2c-grammar-neutrality, 2d-cost-model, 2e-host-arch, 2f-fold-gaps]
v1_surface_targeted: "MIGRATION.md + HANDOFF.md"
proposed_deltas_count: 8
delta_summary:
  carried_from_prior_cycle: [3F17-MH-01, 3F17-MH-02, 3F17-MH-03, 3F17-MH-04, 3F17-MH-05, 3F17-MH-06, 3F17-MH-07, 3F17-MH-08]
  removed: []
  answered: []
  newly_added: []
prior_cycle_dispositions_folded:
  accepted: [3F-D01-D09-migration-receiver-eager-retirement-registry-fence, CH3-3D-skinny-fold-monotonic-named-gate, CH6-3F-next-cycle-directive-concrete-measurable, CH1-SKV17-01-3c-locks-v+1-diff-hunk-header-arithmetic, CH7-S17-V2-R1-OQ-css-sota-stamp]
  rejected: []
  revised: [CH7-S17-V2-R1-css-sota-overclaim-exec-summary-and-handoff-carrier]
---

# 3F — MIGRATION + HANDOFF + Next-Cycle Dispatch (SK-V17 totality fold)

## Executive Summary

This artefact PROPOSES only; it does not amend `restart/MIGRATION.md` or
`restart/HANDOFF.md` (PASS-3 §2 / §6 — Pass Omega CRUD-4 applies, post-G-Omega).
The top-level surfaces still present **SK-V15 / Pass Omega V9** as current
implementation authority (`restart/HANDOFF.md:5-13`, `restart/MIGRATION.md:30-36`),
while the live track has advanced: SK-V16 closed at `1c5bd7a25` with the shared
flat-tape SUBSTRATE landed but UNWIRED for CSS, and SK-V17 is the converged skinny
**contract for** CSS-on-tape / lazy-`ValueRef` / shared-NEON — CSS the SK-V17
first-mover, the **CSS >SOTA bar UNMEASURED-PENDING and held as the SK-V18 proof
obligation** (`restart/skinny/tranches/sk-v17/HANDOFF.md:44-45`, `SPEC.md:207`); the
JSON model is >SOTA-proven (`skinny/RESULTS.md`)
(`restart/skinny/tranches/sk-v17/HANDOFF.md:28-104`, S-P3 CONVERGED `:189-191`).
T-P2 LOCKED five fold designs (LAC-2F-FOLD-01..05) at `91b6893b0`
(`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:77-221`). This 3F crystallises those into
eight proposed MIGRATION/HANDOFF deltas: an **SK-V18 fold receiver** carrying the
eager-`OpenFrame` retirement, the AoS→SoA single-encoding closure, the `ValueRef<G>`
projection-generator fold, the `StructLayout`→`Layout` 960-site rename, the
shared-NEON manifest reconcile, and the two regression FENCES (no per-leaf
`StructRegistry`, no second substrate). The next directive: T-P3 locks → G3
auto-passes under the active pin → Pass Omega CRUD-4 applies or records a
blocked/extension remainder → G-Omega authorises V1 patches (LOCKS merge
G-Omega-gated) → only then **SK-V18 W0** (the totality `crates/core` fold) dispatches.

## V3 Delta Summary

| bucket | delta ids | note |
|---|---|---|
| Carried from prior cycle | 3F17-MH-01..08 | All eight deltas carried unchanged into V3. No delta TABLE row was REVISEd or REJECTed; the V2 CH7 REVISE landed on EXECUTIVE-SUMMARY + HANDOFF-CARRIER prose (the CSS >SOTA framing), not on a delta. The prior SK-V14 `restart/audit/totality/p3/3F-migration-handoff.md` remains a STRUCTURE precedent only — its 3F-MH-001..007 IDs belong to the SK-V15 dispatch, not carried (different subject: SK-V15 PRUNE-then-REBUILD vs SK-V17 tape-fold). |
| Removed | none | — |
| Answered | CH7-S17-V2-R1-OQ | The V2 CH7 open question (`V2/CH7.md:210-212`) — should the cold-start HANDOFF carrier carry a one-line "CSS >SOTA = SK-V18 obligation, NOT met" stamp adjacent to the SK-V18 dispatch line — is ANSWERED: the stamp is added to the HANDOFF carrier next-directive block. The four standing Open Questions remain routed to their CH lenses + Pass Omega CRUD-4 / SK-V18 wave receivers. |
| Newly added | none | The eight deltas survive CHALLENGE unmodified; no new delta warranted. |
| Folded (REVISE) | CH7-S17-V2-R1 | Over-stated CSS speed framing folded out (two prose edits, both proposal-only). The exec summary (`:31`) and HANDOFF carrier (`:84`) asserted SK-V17 "**proves CSS-on-tape … >SOTA**"; ground truth contradicts — SK-V17 HANDOFF:44-45 "the **>SOTA bar is NOT met and nothing on the CSS path moved**"; SPEC:207 "ALL per-corpus lightningcss endpoints are **UNMEASURED-PENDING**"; 3E P5b "CSS tape consumer … **NOT yet measured**". Reworded to: converged CONTRACT for CSS-on-tape (CSS first-mover; CSS >SOTA bar UNMEASURED-PENDING, SK-V18 proof obligation, NOT met); JSON model >SOTA-proven (`skinny/RESULTS.md`). Restores coherence with 3D's SCOPE-HONESTY BANNER, 3E P5b, and 3F's own next-cycle CH3 text (`:202`). No candidate dropped, no lock touched. |
| Folded (cross-artefact, prior) | CH1-SKV17-01 | Load-bearing G3-gate-object fold carried from V2: the `3c-locks-v+1-diff.md:49` hunk header reads `@@ -606,7 +606,22 @@`; `git apply --check` returns EXIT 0 clean against `LOCKS.md` at master HEAD `2a76916ac`. 3F's next-cycle directive (steps 1, 5) names `3c-locks-v+1-diff.md` as the G3/G-Omega gate object; the corrected header is the precondition for that gate to apply. |

## Proposed Delta Table

Every delta cites a T-P1 finding-id, a T-P2 LAC/dossier, or a V1 spec surface at
path:line. Receiver/blocker/gate triple on every carry. The implementation receiver
is **SK-V18** (the `crates/core` fold); SK-V17 is the SKINNY proof, already
converged in plan.

| delta id | proposed delta | source T-P1/T-P2 finding-id cited | affected V1 surface section | receiver / blocker / gate | rationale |
|---|---|---|---|---|---|
| **3F17-MH-01** | Insert a current **§0.0 SK-V18 Tape-Fold Migration Receiver** before the historical `## 0.1 Current SK-V15 V9 Migration Receiver` (`restart/MIGRATION.md:30`). It carries SK-V18 as the implementation tranche that adopts the SKINNY-proven unified-tape / lazy-view / NEON model into `crates/core/`, with the five LAC rows as receiver entries. Keep §0.1 + §0.2 Pass-Omega-V2..V9 sections as HISTORICAL SK-V15 records, not current dispatch authority. | T-P1 COH17-003 (core tape UNWIRED, SK-V18 subject; `1f-coherence-scan.md:80,104`); 1D SK17L-001 (proven SoA tape unwired in totality; `1d-skinny-lessons.md:60-72`); T-P2 LOCKED fold §3Z (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:77-84`). | `restart/MIGRATION.md` before `## 0.1` (`:30`); historical receivers occupy `:30`-`:160`. | Receiver: Pass Omega CRUD-4 (doc); SK-V18 W0+ (implementation, post-G-Omega). Blocker: any current-authority text routing the next implementation through SK-V15 W0-W11 after SK-V17 close. Gate: G-Omega + CRUD-LOG. | Avoids the Pass-Omega-V9 / SK-V15 receiver presenting as current when SK-V16 closed and SK-V17 converged; gives the SK-V18 fold a single current entry point. Avoids name collision with the historical §0.1 receiver (CH1/CH6 hygiene). |
| **3F17-MH-02** | Add to §0.0 a **LAC receiver table** mapping each LOCKED fold design to its `crates/core` migration rule + blocker + gate: (1) tape-as-unified-substrate — retire eager `OpenFrame` builders + converge AoS `TapeRec` onto proven SoA `Tape` as the single surviving encoding; (2) lazy `ValueRef<G>` value-API via one projection generator; (3) shared NEON `select_classifier(alphabet)` as a Lock-16 manifest row; (4) BackendShape disposition — substrate-manifest CATEGORY, no 6th shape; (5) StructRegistry/FieldSource fence — compile-time-resolved-once. | T-P2 LAC-2F-FOLD-01..05 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:85-221`); T-P1 COH17-001/002/004/006 + Cross-Tree Substrate Map (`1f-coherence-scan.md:78-96`); 1D SK17L-001..010 (`1d-skinny-lessons.md:35-45`). | `restart/MIGRATION.md` §0.0 (new) cross-referencing §19.4 Runtime Substrate (`:902-917`) and §20 carry ledger (`:954-967`). | Receiver: SK-V18 waves (per 3B). Blocker: a LAC row landing without its same-wave consumer. Gate: per-LAC SK-V18 exit gate; LOCKS rows G-Omega-gated via 3C. | Makes the migration route from the LOCKED fold designs, not from prose; every fold obligation gets a receiver/blocker/gate row consumable by SK-V18. |
| **3F17-MH-03** | Add a **single-encoding closure gate** to §19.4 Runtime Substrate (`restart/MIGRATION.md:902-917`): after the SK-V18 fold, EXACTLY ONE tape encoding survives in `crates/core` (`grep` proves AoS `TapeRec` retired OR SoA `Tape` retired, never both live); the dual AoS/SoA state is admissible ONLY as a transient fold-state, never a Lock-1 closure. Extend the `rg "OpenFrame\|...` check to assert `JsonStructBuilder`/`CssStructBuilder` eager `OpenFrame` retirement. | T-P1 COH17-001 + U-COH17-002 (exactly-one-encoding closure obligation, `LOCKS.md:75`; `1f-coherence-scan.md:78,102,125`); 1A SUB17-001 (AoS↔SoA); T-P2 fold #1 + REJECT-3 (AoS/SoA dual refuted as Lock-1 closure; `HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:85-108,238-240`). | `restart/MIGRATION.md` §19.4 (`:902-917`); cross-ref §19.5 Generated Equality (`:919-927`) for the regen-gated re-emit. | Receiver: SK-V18 encoding-convergence wave. Blocker: a committed dual-encoding end-state. Gate: §19.4 substrate gate + Lock 1 (`LOCKS.md:75`). | Encodes the Lock-1 "parallel substrates are dead" closure as a mechanical migration gate, so the fold cannot ship a permanent AoS/SoA dual. |
| **3F17-MH-04** | Add a **`StructLayout`→`Layout` rename migration row** to §0.0 + the §20 punch list (`restart/MIGRATION.md:954-967`): the Lock-2-retired term is live at 960 sites in `crates/`; the rename is GENERATOR-SIDE (regenerating all 8 parsers + ~16 tests), regen-gated, NOT a hand-patch. Price it as the 960-site surface, not 40-120 LOC. | T-P1 COH17-006 (StructLayout Lock-2-retired but 960-site-live; `1f-coherence-scan.md:83,107`); 1D SK17L-007 (`1d-skinny-lessons.md:42`); T-P2 LAC-2F-FOLD-05 (Lock-2 sub-surface reconcile, two disjoint paths; `HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:216-221`). | `restart/MIGRATION.md` §0.0 + §20 punch list (`:954-967`); cross-ref §19.5 (`:919-927`). | Receiver: SK-V18 codegen-rename wave. Blocker: any hand-patch of `StructLayout` outside the generator (violates clean-regen discipline). Gate: §19.5 generated-equality (`git diff --exit-code`) + Lock 2 (`LOCKS.md:160`). | Carries the 960-site mispricing correction so SK-V18 budgets the rename as a generator+regen surface, not a small hand edit; routes through clean-regen. |
| **3F17-MH-05** | Add a **regression-fence clause** to §0.0 + §19.4: ANY per-leaf runtime `StructRegistry::layout(rule)` / `compound_kind_for_layout` indirection in the tape/projection hot path is REJECT (re-opens the measured 28-65× / 983× / 10583× regression). The `FieldSource` walk is compile-time projection-emission resolved ONCE at codegen. Name the live coupling site `crates/core/.../bbnf/arena.rs:47` that the eager-`OpenFrame` retirement (3F17-MH-03) severs. | T-P1 COH17-002 fold-note + Cross-Tree Substrate Map (FieldSource walk compile-time-once; `1f-coherence-scan.md:93-94`); 1D SK17L-004 + ledger L-SK17-02 (`1d-skinny-lessons.md:39,27`); T-P2 fold #5 + REJECT-2 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:193-214,234-237`). | `restart/MIGRATION.md` §0.0 + §19.4 (`:902-917`). | Receiver: every SK-V18 projection-emission row. Blocker: a per-leaf registry lookup re-entering the hot path. Gate: §19.4 substrate gate + Lock 1; the AZ-IV pre-block (SPEC `:791-794`). | Holds the AZ-IV indirection pre-block as a binding migration fence, so the fold cannot resurrect the registry-in-hot-path regression. |
| **3F17-MH-06** | Add a **no-second-substrate fence** to §0.0: the projection generator emits accessors over the EXISTING `Tape`/`ValueRef`; an introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` ALONGSIDE the proven `Tape`/`ValueRef` is a Lock-1 type-ambivalence violation (REJECT). The shared NEON classifier carries `substrate_target = existing_tape` / `retention_lifetime = transient-single-call`; no sidecar mask producer, no parallel source pass, no sixth BackendShape. | T-P1 COH17-008 (alphabet reconcile, one substrate; `1f-coherence-scan.md:85`); SK-V17 SPEC pre-block "No second substrate" (`restart/skinny/tranches/sk-v17/HANDOFF.md:171-174`); T-P2 fold #3 + #4 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:132-191`). | `restart/MIGRATION.md` §0.0; cross-ref §9.3 `simd-scan` → `bbnf-simd` (`:642`) for the scan-crate scope reconcile. | Receiver: SK-V18 tape-wiring + NEON-manifest waves. Blocker: a parallel substrate / sidecar producer / 6th shape introduced by the fold. Gate: Lock 1 substrate-union (`LOCKS.md:48,75`) + Lock 10 5-shape canon. | Encodes the CH5 hidden-coupling firewall: the fold wires the proven tape, never a second one; the substrate union holds. |
| **3F17-MH-07** | **Replace the top-level HANDOFF "Current Totality Override"** (`restart/HANDOFF.md:3-118`) with SK-V17-state: SK-V16 closed at `1c5bd7a25`; SK-V17 S-P3 CONVERGED (the tape-fold contract); T-P1 CONVERGED `91b6893b0`, T-P2 CONVERGED V3 (LOCKED LACs), T-P3 active; Pass Omega is the next totality receiver; **SK-V18 is the next implementation tranche** adopting the fold into `crates/core`. Route SK-V17 wave authority through the extant `restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md`; cite no missing companion prompt as current authority. | T-P1 1F COH17-003 (stale top-level authority vs live tape-fold posture; `1f-coherence-scan.md:80`); SK-V17 HANDOFF live split-truth + next-move (`restart/skinny/tranches/sk-v17/HANDOFF.md:28-104,189-191`); T-P2 ready-for-T-P3 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:284-323`). | `restart/HANDOFF.md` Current Totality Override + authority list + read-order (`:3-118`); historical SK-V6 body (`:120-479`) stays provenance-only. | Receiver: Pass Omega CRUD-4. Blocker: stale SK-V15/Omega-V9 dispatch authority remaining current; a missing-prompt citation. Gate: G-Omega + CRUD-4; SK-V18 W0 pre-dispatch verification consumes `DISPATCH-PROMPT.md`. | Cold-start agents must land on SK-V17 T-P3 / Pass Omega / SK-V18 fold, not SK-V15 W0-W11 or the historical SK-V6 body. |
| **3F17-MH-08** | **Replace the HANDOFF "Pass Omega V9 SK-V15 dispatch directive"** (`restart/HANDOFF.md:86-118`) with a **Pass-Omega → G-Omega → SK-V18 W0** directive (see Next-Cycle Dispatch Directive below). Carry the gate posture: G-Omega is the only mandatory user gate under the active pin; G3 auto-passes on T-P3 lock; the LOCKS merge is itself G-Omega-gated (PASS-OMEGA §6). CRUD-4 either completes current-state HANDOFF/MIGRATION cleanup or records a blocked/extension remainder naming the exact remainder/receiver/blocker/gate. | T-P3 §6 G3 gate (`PASS-3-SYNTHESIS.md:179-198`); PASS-OMEGA §4 CRUD-4 owns HANDOFF+MIGRATION (`PASS-OMEGA.md:68`) + §6 G-Omega (`PASS-OMEGA.md:96-110`); PASS-3 §7 + ORCHESTRATOR hard-cap (overruns surface as extension, no silent defer; `PASS-3-SYNTHESIS.md:200-206`). | `restart/HANDOFF.md` dispatch directive + checklist (`:86-118`). | Receiver: Pass Omega CRUD-4 → G-Omega → SK-V18 W0. Blocker: no CRUD-4, no G-Omega, an unresolved invariant, or incomplete current-state cleanup without a blocked/extension record. Gate: G-Omega; then SK-V18 W0 entry gate. | Gives the next worker a concrete, measurable dispatch path; prevents direct implementation dispatch from T-P3 prose; closes the engineered-defer aperture (CH6). |

## Proposal-Only Text Carriers

These carriers are NOT applied here. They are suggested content shapes for Pass
Omega CRUD-4, post-G-Omega.

### MIGRATION Carrier (§0.0)

```md
## 0.0 Current SK-V18 Tape-Fold Migration Receiver

Status: proposal-only until Pass Omega converges, CRUD-4 applies, and G-Omega
closes. SK-V16 closed at `1c5bd7a25`; SK-V17 (the SKINNY tape-fold **contract** for
CSS-on-tape / lazy-`ValueRef` / shared-NEON; **JSON >SOTA-proven, CSS >SOTA the
SK-V18 proof obligation, bar not yet met**) S-P3 CONVERGED. SK-V18 is the
implementation tranche that adopts the SKINNY-proven unified-tape / lazy-view /
NEON model into `crates/core/`. The five LOCKED fold designs (T-P2 LAC-2F-FOLD-01..05)
are the receiver set. No SK-V18 implementation wave dispatches until CRUD-4 applies
this receiver and G-Omega authorises the required V1 patches.

| Receiver (LAC) | Migration rule (crates/core) | Blocker | Gate |
|---|---|---|---|
| LAC-2F-FOLD-01/02 tape-as-unified-substrate | Retire eager `OpenFrame` builders (`json/builder.rs`, `css_l4/builder.rs:16`); converge AoS `TapeRec` (`tape/record.rs:103`) onto proven SoA `Tape` as the SINGLE surviving encoding; all-8 `OnceCell<StructuralIndex>` declare `substrate_target` before wiring. | a committed AoS/SoA dual end-state; a sidecar index. | §19.4 substrate gate + Lock 1 (`LOCKS.md:75`). |
| LAC-2F-FOLD-03 lazy `ValueRef<G>` value-API | One `BackendRule`/`FieldSource`-walking projection generator emits `document/value/view/visitor` over the EXISTING `Tape`/`ValueRef`; `@generated`-allowed; resolved once at codegen. JSON+CSS exercised; Sheets/BBNF-self by-construction (SK-V18). | per-grammar eager value enums kept as the live plane; a per-leaf registry walk. | §19.5 generated-equality + Lock 14. |
| LAC-2F-FOLD-03 shared NEON classifier | Register `select_classifier(alphabet)` as a Lock-16 primitive-manifest ROW (alphabet-as-data); `substrate_target=existing_tape`, transient-single-call; scalar-ref + checkasm parity; aarch64-only NEON. | x86/AVX-512/SVE close path; a cross-call classifier-state carry. | Lock 16 manifest + §9.3 simd-scan scope reconcile. |
| LAC-2F-FOLD-02 BackendShape disposition | The tape is the substrate the 5 shapes project from — a substrate-manifest CATEGORY at the Lock-1 manifest, NOT a 6th BackendShape variant; the 5-shape canon `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` holds verbatim. | a silent 6th shape; an aarch64 CollapsedStage admission without G-Omega. | Lock 1 manifest + Lock 10 5-shape domain. |
| LAC-2F-FOLD-04 StructRegistry/FieldSource fence | ANY per-leaf runtime `StructRegistry::layout(rule)` indirection in the tape/projection hot path is REJECT; the FieldSource walk is compile-time-resolved-once. Live coupling at `bbnf/arena.rs:47` is severed by eager-builder retirement. | a per-leaf registry lookup re-entering the hot path. | §19.4 substrate gate + AZ-IV pre-block (SPEC `:791-794`). |
| LAC-2F-FOLD-05 (Lock-2 sub-surface) StructLayout→Layout | The Lock-2-retired `StructLayout` (960 live sites in `crates/`) renames to `Layout` GENERATOR-SIDE, regenerating all 8 parsers + ~16 tests; regen-gated, never hand-patched. | a hand-patch outside the generator. | §19.5 generated-equality + Lock 2 (`LOCKS.md:160`). |

Migration fences (binding on every SK-V18 row):
- EXACTLY ONE tape encoding survives post-fold; the AoS/SoA dual is transient-only.
- No second substrate: the projection generator emits over the EXISTING `Tape`/`ValueRef`;
  no introduced `StructLayout`/`TapeStructBuilder`/`TapeCursor` alongside it (Lock 1 type-ambivalence REJECT).
- No per-leaf `StructRegistry` indirection in the hot path (AZ-IV pre-block).
- No x86/AVX-512/SVE close path (aarch64 NEON + optional dotprod/i8mm only).
- No fact-stream String as a live CSS admission plane (diagnostic-only).
```

### HANDOFF Carrier (Current Totality Override)

```md
## Current Totality Override - 2026-05-30

Status: SK-V17 T-P3 synthesis is active. SK-V16 closed at `1c5bd7a25` (shared
flat-tape SUBSTRATE landed, UNWIRED for CSS). SK-V17 (the SKINNY tape-fold proof)
S-P3 CONVERGED. T-P1 CONVERGED (`91b6893b0`), T-P2 CONVERGED V3 (five LOCKED
LACs), T-P3 active. After T-P3 cohort lock, G3 auto-passes under the active
G-Omega-only pin and the packet flows into Pass Omega, whose CRUD wave applies the
proposed MIGRATION/LOCKS/ARCHITECTURE/MASTER-PLAN deltas (LOCKS merge G-Omega-gated).

The next IMPLEMENTATION tranche is **SK-V18**: it adopts the SKINNY-proven
unified-tape / lazy-`ValueRef` / shared-NEON model into the totality `crates/core/`
tree, per the five LOCKED fold designs. No SK-V18 wave dispatches until Pass Omega
CRUD-4 has updated HANDOFF/MIGRATION and G-Omega has authorised the required V1
patches. Current SK-V17 skinny wave authority routes through
`restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md`; cite no missing companion
prompt as current authority.

CRUD-4 cap handling: before G-Omega, CRUD-4 either completes current-state
HANDOFF/MIGRATION cleanup or records a blocked/extension decision naming the exact
remainder, receiver, blocker, and gate. Any remainder touching current dispatch
truth blocks SK-V18 W0 until resolved.

Next directive after Pass Omega / G-Omega: dispatch SK-V18 W0 (the crates/core
tape-fold) through the SKINNY triumvirate; follow the SK-V18 wave order (per the
MASTER-PLAN reconciliation, 3B), preserving the LAC dependency rows and the
migration fences, stopping on any unresolved invariant.

> CSS >SOTA = SK-V18 obligation, NOT met: SK-V17 proved the JSON model >SOTA
> (`skinny/RESULTS.md`) and converged the CSS-on-tape CONTRACT; ALL per-corpus
> lightningcss endpoints are UNMEASURED-PENDING (`SPEC.md:207`) and the CSS >SOTA
> bar is explicitly NOT met (`restart/skinny/tranches/sk-v17/HANDOFF.md:44-45`).
> SK-V18 W0 carries the CSS >SOTA bar as a PROOF OBLIGATION, not an achieved win.
```

## Consequences

| delta id | positive consequence | cost / LOC budget / risk class | propagation (surfaces touched) | wave alignment | fail action |
|---|---|---|---|---|---|
| 3F17-MH-01 | Current migration authority starts at SK-V18 fold, not stale SK-V15/Omega-V9. | 30-50 doc LOC; LOW risk if §0.1 not renumbered. | 1: MIGRATION. | Pass Omega CRUD-4 before G-Omega. | If §0.0 cannot fit, record blocked/extension remainder; block SK-V18 W0. |
| 3F17-MH-02 | Every LOCKED fold design gets a receiver/blocker/gate row. | 60-110 doc LOC; MEDIUM (table density). | 2: MIGRATION + §19.4/§20 cross-link. | Pass Omega CRUD-4; SK-V18 waves consume post-G-Omega. | If a LAC row lacks a same-wave consumer, route 3B wave-graph amendment. |
| 3F17-MH-03 | Lock-1 single-encoding closure becomes a mechanical migration gate. | 15-30 doc LOC; HIGH risk class (regression-adjacent encoding fold). | 2: MIGRATION §19.4 + §19.5. | SK-V18 encoding-convergence wave. | If a dual end-state ships, fail §19.4 gate; revert per REDRESS. |
| 3F17-MH-04 | 960-site rename budgeted correctly as generator+regen, not a small hand edit. | 20-35 doc LOC (the ROW); the rename itself is 960-site generator-side. MEDIUM. | 2: MIGRATION §0.0 + §20. | SK-V18 codegen-rename wave. | If hand-patched, fail §19.5 `git diff --exit-code`; route clean-regen. |
| 3F17-MH-05 | AZ-IV indirection pre-block held as a binding fence. | 15-30 doc LOC; HIGH (28-65×/983×/10583× regression class). | 2: MIGRATION §0.0 + §19.4. | every SK-V18 projection-emission row. | If per-leaf registry re-enters, REJECT the hunk; block the wave. |
| 3F17-MH-06 | CH5 hidden-coupling firewall: one substrate, no sidecar, no 6th shape. | 20-35 doc LOC; MEDIUM. | 2: MIGRATION §0.0 + §9.3. | SK-V18 tape-wiring + NEON-manifest waves. | If a parallel substrate is introduced, REJECT (Lock 1 type-ambivalence). |
| 3F17-MH-07 | Cold-start handoff routes to SK-V17 T-P3 / Pass Omega / SK-V18, not SK-V15/SK-V6. | 90-160 doc LOC (replaces a long current-state block). MEDIUM. | 2: HANDOFF + Pass Omega CRUD-4 log. | Pass Omega CRUD-4 before G-Omega; SK-V18 W0 verifies. | If stale authority or a missing-prompt citation remains, record blocked/extension; block W0. |
| 3F17-MH-08 | Concrete measurable dispatch path; no implementation from T-P3 prose; no engineered-defer. | 45-90 doc LOC; LOW. | 3: HANDOFF + MIGRATION + Pass Omega CRUD-4. | T-P3 lock → G3 → Pass Omega → G-Omega → SK-V18 W0. | If CRUD-4 or G-Omega incomplete, record blocked/extension remainder; keep W0 blocked. |

## Next-Cycle Dispatch Directive

1. **T-P3 cohort completes** 3A-3F (this artefact is 3F), commits before CHALLENGE,
   then CH1-CH6 (+CH7) hardening iterates until convergence (≥95% ACCEPT ×2,
   zero orphan REVISE, V≤5) or the V5 ceiling (`PASS-3-SYNTHESIS.md:137-156`). 3C's
   `3c-locks-v+1-diff.md` is the G3 gate object; its unified-diff hunk header reads
   `@@ -606,7 +606,22 @@` and `git apply --check` returns EXIT 0 clean against
   `restart/locks/LOCKS.md` at master HEAD `2a76916ac` (the V1 mis-counted
   `@@ -606,6 +606,52 @@` is folded out per CH1-SKV17-01). A non-applying gate object
   blocks G3; this gate object applies.
2. **G3 (mandatory gate)** is presented per `PASS-3-SYNTHESIS.md:179-198`. Under the
   active user pin (G-Omega is the only mandatory gate; `restart/skinny/tranches/sk-v17/HANDOFF.md:124-129`),
   G3 auto-passes on cohort lock; the synthesis flows into Pass Omega.
3. **Pass Omega dispatches** per `restart/prompts/pass-contracts/PASS-OMEGA.md`. It
   consumes the converged T-P3 artefacts + SK-V17 REDRESS/RESULTS into a CHALLENGE
   wave (§3), then a six-agent CRUD wave (§4). CRUD-4 owns HANDOFF + MIGRATION
   (`PASS-OMEGA.md:68`); CRUD-3 owns LOCKS, its merge G-Omega-gated until sign-off
   (`PASS-OMEGA.md:67,98`). No CRUD agent edits beyond what CHALLENGE CONSOLIDATED
   authorises (`PASS-OMEGA.md:74`).
4. **CRUD-4 resolves current-state HANDOFF/MIGRATION cleanup** (deltas 3F17-MH-01..08)
   before G-Omega. If the hard cap blocks the cleanup, CRUD-4 records a
   blocked/extension decision naming the exact remainder, receiver, blocker, and gate
   (`PASS-3-SYNTHESIS.md:200-206`); any remainder touching current dispatch truth
   blocks SK-V18 W0 until complete. No silent deferral.
5. **G-Omega (mandatory)** authorises the required V1 patches; the LOCKS v+1 merge
   applies the 3C disposition matrix (`PASS-OMEGA.md:96-110`). On G-Omega close the
   V1 spec is v+1; the next cycle dispatches per the Omega-F directive.
6. **Only after** Pass Omega CRUD-4 current-state truth is complete, G-Omega has
   authorised the HANDOFF/MIGRATION/LOCKS patches, and SK-V17 authority routes
   through the extant `restart/skinny/tranches/sk-v17/DISPATCH-PROMPT.md`, may the
   orchestrator update HANDOFF to `ready-for-SK-V18-W0` and dispatch **SK-V18 W0**
   (the `crates/core` tape-fold) through the SKINNY triumvirate
   (`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`).
7. **SK-V18 waves** then adopt the five LOCKED fold designs in the dependency order
   set by the MASTER-PLAN reconciliation (3B), preserving the LAC receiver rows and
   the migration fences (single-encoding, no-second-substrate, no-per-leaf-registry,
   aarch64-only, no-fact-stream-admission). SK-V18 cannot close while any LAC row
   lacks proof, REDRESS route, revert evidence, or intrinsic-block proof; and never
   with a dual encoding, a 6th BackendShape, or a per-leaf registry walk.
8. **Three Open Research Questions** carried from T-P2 (`HARDENING-T-P2-SKV17-V3-CONSOLIDATED.md:303-312`)
   are dispositioned by T-P3 (2F-FOLD-U1 SoA-adopt vs AoS-parity → 3A/3C; 2F-FOLD-U2
   the 8 `OnceCell` carrier classification → 3A; 2F-FOLD-U3 aarch64 CollapsedStage
   admission → 3A/3E). 3F does not resolve them; it carries them as SK-V18 entry
   conditions feeding the migration fences above.

## Open Questions

| lens | question | receiver | blocker | gate |
|---|---|---|---|---|
| CH1 / CH6 | The HANDOFF currently carries a "Pass Omega V9" dispatch directive and §0.1 MIGRATION an "SK-V15 V9 Migration Receiver" (`restart/HANDOFF.md:86`, `restart/MIGRATION.md:30`). Should CRUD-4 mark these HISTORICAL (SK-V15 lineage) and label the current astral pass "Pass Omega V{N+1}" to avoid false-current citation, given SK-V16 closed and SK-V17 converged? | Pass Omega CRUD-4. | Name/version collision between the historical Omega-V9/SK-V15 receiver and the current SK-V17→SK-V18 receiver. | G-Omega sign-off text + CRUD-LOG. |
| CH3 | Does every proposed HANDOFF/MIGRATION delta PRESERVE the SK-V17 pre-block list — AZ-IV eager-value-tree, StructRegistry hot-path indirection, fact-stream-as-admission, x86/AVX/SVE, second substrate, 6th BackendShape, FNV-production (`restart/skinny/tranches/sk-v17/HANDOFF.md:148-185`)? 3F17-MH-03/05/06 assert they do; CH3 must confirm no fence is weakened. | SK-V18 W0+ wave plans (3B). | A reopened pre-block route or an omitted REDRESS family. | CH3/CH7 during SK-V18 wave plan + 3C disposition matrix. |
| CH4 | Is the 960-site `StructLayout`→`Layout` rename (3F17-MH-04) realistically a single SK-V18 codegen-rename wave, or does it need its own dependency-row split given it regenerates all 8 parsers + ~16 tests? | SK-V18 codegen-rename wave (3B alignment). | Mispricing the rename surface; a regen that fails `git diff --exit-code`. | §19.5 generated-equality + 3B wave allocation. |
| CH5 | Does the BackendShape-as-substrate-manifest-CATEGORY disposition (3F17-MH-06, via LAC-2F-FOLD-04) stay coherent with 3A's ARCHITECTURE §7.3 delta and 3E's per-grammar matrix — i.e. no surface presents the tape as a silent 6th shape? (5-shape canon coherence, PASS-3 §8.2.) | 3A / 3E authors; Pass Omega CRUD-1/CRUD-2. | A 3A/3B/3E surface that touches one without the others. | CH3 coherence + 3C disposition (`admits_collapsed_stage` x86-binding). |
| CH7 / CH6 | **ANSWERED (V3).** The V2 CH7 OQ asked whether the cold-start HANDOFF carrier should stamp "CSS >SOTA = SK-V18 obligation, NOT met" adjacent to the SK-V18 dispatch line. The stamp is now added to the carrier next-directive block (`:143-147`), mirroring 3D's SCOPE-HONESTY BANNER, so no cold-start agent reads CSS >SOTA as achieved. CH6 anti-paper-close should re-scan the V3 carrier to confirm the stamp is at the right altitude and no residual over-claim survives. | 3F (this artefact, V3) → Pass Omega CRUD-4 carries the stamp into the applied HANDOFF. | A cold-start agent reading CSS >SOTA as achieved from the carrier. | CH6 re-scan of the V3 HANDOFF carrier + Pass Omega CRUD-4 application. |
