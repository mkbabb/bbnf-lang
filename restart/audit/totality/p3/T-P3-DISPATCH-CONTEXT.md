# T-P3 Dispatch Context — SK-V15 Totality Synthesis Pass

Authored after SK-V15 T-P2 normal §3Z LOCK at commit `cafb95682`.
T-P3 dispatches per `restart/prompts/totality/PASS-3-SYNTHESIS.md` and
`restart/prompts/ORCHESTRATOR.md` §3W/§3Z. Six parallel synthesis workers
3A..3F fan out per the PASS-3 scope matrix, then seven CHALLENGE lenses harden
the packet. CH7 is binding for SK-V15 per PASS-IMPL V1 forward-lens addenda.

**G3 gate override:** PASS-3 §6 describes G3 as mandatory, but the active user
pin says do not relinquish control except at G-Omega. Therefore G3 auto-passes
on cohort §3Z LOCK. Only G-Omega triggers user relinquish.

Each T-P3 worker reads §0 through §4 plus its own per-worker row.

## §0 — Authority

1. `restart/prompts/totality/PASS-3-SYNTHESIS.md` — T-P3 contract, scope
   matrix, frontmatter schema, CHALLENGE lens overlay, hard caps, and
   proposal-only boundary.
2. `restart/prompts/ORCHESTRATOR.md` §3W/§3Z and non-negotiables.
3. `restart/audit/totality/p1/{1A,1B,1C,1D,1E,1F-coherence-scan,1F-anti-pattern,1F-past-corpora}.md`
   — SK-V15 T-P1 inventories.
4. `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`
   — T-P1 clean-final / G1-auto-pinned governance packet.
5. `restart/audit/totality/p2/{2A,2B,2C,2D,2E,2F}-*.md` — SK-V15 T-P2
   research dossiers.
6. `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`
   — T-P2 normal §3Z LOCK packet.
7. `restart/audit/skinny-impl-overfit/V1/CONSOLIDATED-AUDIT.md` — PASS-IMPL
   V1 CSS/PATTERN-H/Lock-14/Decision-Engine implementation floor.
8. `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
   `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, `restart/MIGRATION.md` —
   V1 spec surfaces. T-P3 reads and proposes diffs only; Pass Omega CRUD edits.
9. `restart/skinny/tranches/sk-v15/{SYNTHESIS,SPEC,HANDOFF,DISPATCH-PROMPT,ORCHESTRATOR-PROMPT}.md`
   — SK-V15 skinny pass output and wave plan.
10. `skinny/REDRESS.md` and `skinny/RESULTS.md` — empirical and refutation
    floor.

## §1 — Locked Ground Truth

**T-P1 governance:** T-P1 V5 closed every known hardening defect and auto-pinned
G1, but did not achieve a normal two-clean-cycle §3Z lock because V4 was REVISE
and V5 was the hard ceiling. T-P3 must preserve that note and must not rewrite
T-P1 as normal §3Z.

**T-P2 governance:** T-P2 V2 and V3 returned 7/7 `ACCEPT` with zero orphan
`REVISE`, zero `REJECT`, no target packet edits, and V≤5. T-P2 is a normal §3Z
LOCK at `cafb95682`.

**PASS-IMPL V1 floor:** JSON is honest; CSS L4 is contrived; Pattern H is not
collapsed; Lock 14 gate holes exist; Decision Engine is scaffold; no CSS Value
API exists in the skinny admission path. T-P3 must surface these as proposal
inputs, not close them by prose.

**SK-V15 wave direction:** PRUNE before REBUILD. T-P3 feeds Pass Omega V5; it
does not dispatch implementation waves directly and it does not edit V1 spec
surfaces.

## §2 — Discipline

- HARD CAP: 45 min per synthesis worker. At 0.9N save the artefact; at N halt
  with current evidence and unresolved items named.
- Workers write only their assigned artefact(s). Do not stage or commit.
- Do not edit `restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`,
  `restart/locks/LOCKS.md`, `restart/HANDOFF.md`, or `restart/MIGRATION.md`.
  T-P3 proposes diffs; Pass Omega CRUD applies them post-G-Omega.
- Every proposed delta cites a T-P1 inventory, T-P2 dossier, PASS-IMPL audit,
  SK-V15 skinny surface, REDRESS/RESULTS row, or V1 spec surface at path:line.
  Uncited delta = CH1 REJECT.
- 3C is the LOCKS singularity: every 1E and 2X LOCKS-AMENDMENTS-CANDIDATE
  receives ACCEPT/REJECT/MODIFY/DEFER. Silent drops are forbidden.
- Preserve the 16-lock count and the 5-shape `BackendShape` canon. A new lock,
  lock retirement, new directive, new BIR variant, public substrate API,
  retained sidecar, or sixth `BackendShape` remains G-Omega-gated.
- CHALLENGE runs CH1-CH7 for SK-V15. CH7 covers wave-graph cycle detection,
  broadcast-admission detection, gate-exclusion detection, CSS fake parity,
  wrong-host close evidence, FNV bench-contrivance leakage, and delete-before-
  provider sequencing.

## §3 — Output Structure

Each worker writes one artefact at the assigned path. 3C writes two artefacts.
All outputs use PASS-3 §2.1 frontmatter and the body sections required by
PASS-3 §2.

| Worker | Scope | Output |
|---|---|---|
| 3A | ARCHITECTURE.md surface synthesis | `restart/audit/totality/p3/3A-architecture-synthesis.md` |
| 3B | MASTER-PLAN.md wave reconciliation | `restart/audit/totality/p3/3B-master-plan-reconciliation.md` |
| 3C | LOCKS crystallisation and v+1 diff | `restart/audit/totality/p3/3C-locks-crystallisation.md` + `restart/audit/totality/p3/3C-locks-v+1-diff.md` |
| 3D | Skinny-to-totality fold synthesis | `restart/audit/totality/p3/3D-skinny-fold.md` |
| 3E | Grammar-generalisation synthesis | `restart/audit/totality/p3/3E-grammar-generalisation.md` |
| 3F | MIGRATION + HANDOFF + next-cycle dispatch | `restart/audit/totality/p3/3F-migration-handoff.md` |

## §4 — Per-Worker Entry Constraints

**3A:** Distil T-P1 substrate/codegen/runtime divergences, T-P2 grounded
techniques, PASS-IMPL implementation floor, and SK-V15 wave constraints into
ARCHITECTURE.md proposed deltas. Preserve proposal-only boundary and five-shape
canon.

**3B:** Reconcile MASTER-PLAN waves against the SK-V15 PRUNE-then-REBUILD wave
plan, T-P1/T-P2 evidence, PASS-IMPL floor, and REDRESS. Classify landed,
refuted, pending, and new waves. Do not propose implementation shortcuts.

**3C:** Consolidate every live 1E and 2X LAC into one LOCKS v+1 disposition
matrix and line-level diff. Include SK-V15 T-P2 LACs and carry T-P1 G1
governance honestly. Preserve 16 locks and 5-shape canon.

**3D:** Fold skinny lessons monotonically into totality. Skinny wins become
V1-authoritative proposal inputs; skinny rejections become locks-strengthening
evidence; totality never dictates back to live skinny.

**3E:** Build the non-JSON generality story from 2C plus PASS-IMPL and SK-V15
SPEC: CSS typed provider, Sheets/BBNF-self negative controls, future grammar
onboarding, primitive transfer, and Lock 14 hardening clauses.

**3F:** Produce proposal-only MIGRATION/HANDOFF deltas and the next-cycle
directive after Pass Omega V5. Carry the directive that implementation waves do
not begin until Pass Omega CRUD closes and G-Omega authorises required spec
patches.

## §5 — Post-Dispatch

After the 3A..3F artefacts land and commit, dispatch CH1-CH7 under
`restart/audit/totality/p3/hardening/V1/`. Iterate per §3Z until ≥95% ACCEPT
for two consecutive cycles with no orphan `REVISE`, or V≤5 hard ceiling. On
T-P3 lock, G3 auto-passes under the active user pin and the packet flows into
Pass Omega V5. G-Omega remains the only mandatory user gate.
