# G-Omega Packet - Pass Omega V5 W5R

Status: mandatory user gate packet.
Date: 2026-05-26.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V5 consumes:

- REDRESS-209 W5 rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W5R-corrective-packet.md`.
- W5 research, plan, CHALLENGE, and redress artifacts under
  `restart/skinny/tranches/sk-v14/research/`.
- Pass Omega V4 W4R close packet, because W5R builds on the amended W4/W5 split.
- Omega-A through Omega-F V5 artifacts in this directory.

Omega V5 disposition before CRUD: W5R is coherent, zero-lock-change, and
requires SPEC / SYNTHESIS / MASTER / HANDOFF / MIGRATION / limited skinny-corpus
surface updates before W5A can dispatch.

## Challenge Verdict

`restart/audit/totality/astral/V5/hardening/CONSOLIDATED.md`: 6/6 ACCEPT after
fold. Zero open defects and zero orphan REVISEs remain.

## Proposed Locks Diff

`restart/audit/totality/astral/V5/locks-diff.md`: zero delta. CRUD-3 is
read/no-op.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V5/master-plan-diff.md`:

- W5 splits into W5A generator capability and W5B provider/template deletion.
- W5A is grammar-neutral: no `grammar_id == css_l4` branch, all seven CSS
  companions through the source-consuming path, JSON unchanged-output proof, and
  Sheets/BBNF-self fail-closed or generated-role witnesses.
- W5A + W5B stay inside the original W5 <=1.4k C-1 part-A budget.
- W6 remains W6.0 CSS L4 root-runtime collapse plus W6.1-W6.8 remaining dirs,
  but is conditional on W5B close.
- W8/W9/W10 remain globally blocked until PRUNE-1 through PRUNE-5 close.
- Future T-P3 CH3/CH5 lenses gain delete-target / rebuild-capability checks.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. W5R is wave-graph/generator-gate sequencing only. |
| CRUD-2 | `restart/MASTER-PLAN.md` | Update §13.3 W5A/W5B/W6 rows and global PRUNE block wording. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; preserve 16 locks and five-shape BackendShape canon. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Record REDRESS-209, W5R block, W5A/W5B ownership, and next dispatch directive. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` plus tranche-local W5R pointers | Limited text alignment; BENCH/SUBSTRATE read/no-op unless local drift is found. |
| CRUD-6 | audit packet + REDRESS supersession note | Write post-authorization `CRUD-LOG.md` + `G-OMEGA-SIGNOFF.md` and add a narrow REDRESS-209 supersession note after CRUD applies. |
| SPEC patch | `restart/skinny/tranches/sk-v14/SPEC.md`, `SYNTHESIS.md` | Apply W5A/W5B split, caps, gates, and W6/W8/W9/W10 blockers under the same G-Omega authorization. |
| Tranche dispatch patch | `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`, `DISPATCH-PROMPT.md` | Update R3 PRUNE-3 split, W5R pre-dispatch guard, and NEW-CH3/NEW-CH5 addenda. |
| Tranche handoff patch | `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record G-Omega V5 closure and make W5A the next executable wave. |

## Gate Question

Choose one:

1. Authorise: close G-Omega V5 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V6 extra confirming wave: run another challenge/fold cycle before CRUD.
