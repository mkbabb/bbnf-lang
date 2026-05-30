# Pass Omega V5 — SK-V17 Tape-Fold CRUD Leg (CRUD-4 HANDOFF + MIGRATION)

Pass: Pass Omega.
Cycle: V5 (SK-V17 T-P3 tape-fold CRUD application).
Gate: G-Omega CLOSED by user this turn (2026-05-30).
Master HEAD at apply: `2a76916ac` (CRUD-3 LOCKS landed at `7157be073`,
recorded `c3d6e6fd9`).
Status: CRUD-4 HANDOFF + MIGRATION complete.

This leg is a DISTINCT Pass Omega V5 leg, recorded in its own file to avoid
racing on the shared `CRUD-LOG.md` while sibling CRUD legs (CRUD-1
ARCHITECTURE, CRUD-2 MASTER-PLAN, CRUD-5 SKINNY CORPUS) write concurrently. It
applies the 3F deltas (3F17-MH-01..08,
`restart/audit/totality/sk-v17/p3/3f-migration-handoff.md`) to
`restart/HANDOFF.md` and `restart/MIGRATION.md`, post-G-Omega per
`restart/prompts/pass-contracts/PASS-OMEGA.md` §4 (CRUD-4) + §6.

## Gate Record

G-Omega CLOSED by explicit user authorization this turn for the SK-V17
tape-fold CRUD application phase. The substantive Omega synthesis + CHALLENGE
were discharged by the SK-V17 T-P3 convergence (§3Z, commit chain to
`2a76916ac`); this leg is the post-G-Omega CRUD application.

## Source

LOCKED proposed deltas, T-P3 §3Z:
`restart/audit/totality/sk-v17/p3/{3a-architecture-synthesis,3b-master-plan-reconciliation,3c-locks-crystallisation,3d-skinny-fold,3e-grammar-generalisation,3f-migration-handoff}.md`
+ `3c-locks-v+1-diff.md` (G-Omega gate object; `git apply --check` EXIT 0 at
`2a76916ac`) + `HARDENING-T-P3-SKV17-V3-CONSOLIDATED.md`.

## Receiver Log

| CRUD | Receiver | Operation | Files | Status | Commit | Notes |
|---|---|---|---|---|---|---|
| CRUD-4 | HANDOFF + MIGRATION | Update (apply 3F deltas) | `restart/HANDOFF.md`, `restart/MIGRATION.md` | complete | this commit | 8 3F17-MH deltas; top-level state = SK-V17 tape-fold G-Omega CLOSED 2026-05-30; eager-OpenFrame→tape migration fate; SK-V17 W0-W5 dispatchable + SK-V18 crates/core adoption; post-CRUD-3 LOCKS cross-ref |

## Applied Deltas

| delta id | surface | applied |
|---|---|---|
| 3F17-MH-01/02 | `MIGRATION.md` §0.0 (new) | SK-V17 Tape-Fold Migration Receiver + LAC-2F-FOLD-01..05 receiver/blocker/gate table; SK-V15 V9 receiver demoted to §0.1 HISTORICAL. |
| 3F17-MH-03 | `MIGRATION.md` §19.4 | single-encoding closure gate (EXACTLY ONE tape encoding; eager-`OpenFrame` retirement; no per-leaf `StructRegistry`; no second substrate). |
| 3F17-MH-04 | `MIGRATION.md` §20 punch list | `StructLayout`→`Layout` 960-site generator-side rename row; regen-gated, NOT hand-patch. |
| 3F17-MH-05 | `MIGRATION.md` §0.0 + §19.4 fences | AZ-IV per-leaf-`StructRegistry` indirection REJECT; `bbnf/arena.rs:47` coupling severed by eager-builder retirement. |
| 3F17-MH-06 | `MIGRATION.md` §0.0 fences | no-second-substrate / no-sidecar / no-6th-shape; `substrate_target=existing_tape`, `retention_lifetime=transient-single-call`. |
| 3F17-MH-07 | `HANDOFF.md` Current Totality Override | top-level state = SK-V17 tape-fold G-Omega CLOSED 2026-05-30; SK-V16 closed `1c5bd7a25`; SK-V17 W0-W5 dispatchable; SK-V18 next implementation tranche; authority/read-order routed to SK-V17 surfaces. |
| 3F17-MH-08 | `HANDOFF.md` dispatch directive + checklist | Pass-Omega → G-Omega → SK-V17 W0-W5 / SK-V18 W0 directive; CSS >SOTA = SK-V18 obligation NOT-met stamp. |

## CRUD-4 Verification (post-apply)

- **16-lock count PRESERVED**: `grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md`
  = 16 (HANDOFF/MIGRATION are doc surfaces; no lock added/retired).
- **5-shape BackendShape canon verbatim, NO 6th**:
  `{EagerTape,OffsetTape,EventTape,SinkOnly,CollapsedStage}` restated in
  `MIGRATION.md` §0.0; no six-variant tuple in either surface
  (`grep -nE "...,CollapsedStage,[A-Za-z]"` empty).
- **Tape = substrate-manifest CATEGORY** (LAC-1E-14 precedent): the §0.0
  BackendShape-disposition row records the tape as the substrate the 5 shapes
  project from, NOT a 6th `BackendShape`; cross-references
  `restart/locks/LOCKS.md:107`-`109` and the CRUD-3 addendum at `:610`-`618`.
- **aarch64-only**: §0.0 fences + the §0.0 NEON row carry `aarch64 NEON +
  optional dotprod/i8mm only; no x86/AVX-512/SVE close path`.
- **preserve-rich-ast**: the lazy `ValueRef<G>` value-API row preserves the
  typed `document/value/view/visitor` projection over the existing tape; no
  flattening admitted.
- **No re-opened REDRESS**: AZ-IV eager (pre-blocked, §19.4 fence),
  StructRegistry indirection (REJECT fence), fact-stream (diagnostic-only
  fence) all held closed; no pre-block route reopened.
- **Post-CRUD-3 LOCKS cross-ref**: HANDOFF + MIGRATION §0.0 cite the SK-V17
  T-P3 Crystallisation Addendum at `restart/locks/LOCKS.md:610`-`618` and the
  CRUD-3 commit `7157be073`.
- **Clean-regen discipline / dirty-file preservation**: only
  `restart/HANDOFF.md` + `restart/MIGRATION.md` + this astral log staged for the
  doc leg; pre-existing dirty SK-V12/13 research JSON, skinny `css_l4_*`
  generated.rs, `docs/precepts`, and other modified files untouched.

## Residual Absorption (2 non-blocking REVISE)

- **CH4-V3-01** (D07 scaffold→body cost-cell band, ~4×270=800-1100 LOC): a
  cost-row residual. It is a MASTER-PLAN/cost-table surface concern, not a
  HANDOFF/MIGRATION surface; the §0.0 `StructLayout`→`Layout` 960-site row
  prices the rename surface as the generator surface, but the cost-cell band
  rides forward to the cost-table CRUD leg (CRUD-2 MASTER-PLAN).
- **CH6-V3-7** (3E defer-word re-order + 3C anti-silent-satisfy clause): the
  anti-silent-satisfy clause is honoured by the §0.0 fences carrying
  receiver/blocker/gate on every LAC row and the HANDOFF dispatch directive's
  no-silent-deferral CRUD-cap clause; the 3E defer-word re-order is a
  grammar-generalisation-surface concern (CRUD-5), not HANDOFF/MIGRATION.

## Omega-Equivalent Audit Note

This CRUD-4 leg performs no orthogonal synthesis; the Omega synthesis +
CHALLENGE were discharged at SK-V17 T-P3 convergence (`139ab1e4a`). The leg's
audit obligation is coherence: the top-level HANDOFF override and the
`MIGRATION.md` §0.0 receiver now present the same SK-V17 tape-fold state the
CRUD-3 LOCKS addendum encoded — five LOCKED fold designs, the eager-`OpenFrame`
→ unified-tape migration fate, the five-shape canon with the tape as a
substrate-manifest category, and the SK-V17 W0-W5 / SK-V18 W0 dispatch ladder.
No surface presents the tape as a silent 6th shape; no surface routes the next
implementation through the historical SK-V15 W0-W11 or SK-V6 body. The CSS
>SOTA obligation is stamped NOT-met adjacent to the SK-V18 dispatch line so no
cold-start agent reads it as achieved.

## Next Dispatch

The remaining Pass Omega V5 SK-V17 CRUD legs (CRUD-2 MASTER-PLAN, CRUD-5
SKINNY CORPUS, CRUD-6 AUDIT) apply the corresponding 3b/3d/3e deltas to the
named V1 surfaces. After all legs complete, the next sequenced implementation
step is SK-V17 skinny waves W0-W5 under the SKINNY triumvirate, then SK-V18 W0
(the `crates/core` tape-fold) post-G-Omega.
