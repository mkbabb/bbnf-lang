# G-Omega Packet - Pass Omega V6 W5BR

Status: mandatory user gate packet.
Date: 2026-05-26.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V6 consumes:

- REDRESS-210 W5B rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-plan.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5BR-corrective-packet.md`.
- W5B research artifacts A-D.
- W5A admit commit `286233fa2`.
- Pass Omega V5 close packet, because V6 corrects the W5A/W5B split.

Omega V6 disposition before CRUD: W5BR is coherent, zero-lock-change, and
requires SPEC / SYNTHESIS / MASTER / HANDOFF / MIGRATION / limited skinny-corpus
surface updates before W5B-GEN can dispatch.

## Challenge Verdict

`restart/audit/totality/astral/V6/hardening/CONSOLIDATED.md`: 6/6 ACCEPT. Zero
open defects and zero orphan REVISEs remain.

## Proposed Locks Diff

`restart/audit/totality/astral/V6/locks-diff.md`: zero delta. CRUD-3 is
read/no-op.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V6/master-plan-diff.md`:

- W5A remains admitted.
- W5B becomes W5B-GEN: provider-free runtime generator body construction.
- W5C-DELETE becomes provider/template deletion and post-W5 Lock 14 baseline
  close.
- W5B-GEN receives an explicit <=1.0k C-1 part-A source/test LOC cap; W5C-DELETE
  keeps <=400 deletion/baseline LOC; W6 remains unchanged.
- W6 becomes conditional on W5C-DELETE close.
- W7 and W8/W9/W10 remain blocked by the PRUNE chain.
- The W5B generic-crate grep is repaired to ripgrep-correct, production-scoped
  syntax.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. W5BR is wave-graph/generator-gate sequencing only. |
| CRUD-2 | `restart/MASTER-PLAN.md` | Update §13.3 W5B-GEN/W5C-DELETE/W6 rows and global PRUNE block wording. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; preserve 16 locks and five-shape BackendShape canon. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Record REDRESS-210, W5BR block, W5B-GEN/W5C ownership, and next dispatch directive. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` plus tranche-local W5BR pointers | Limited text alignment; BENCH/SUBSTRATE read/no-op unless local drift is found. |
| CRUD-6 | audit packet + REDRESS supersession note | Write post-authorization `CRUD-LOG.md` + `G-OMEGA-SIGNOFF.md` and add a narrow REDRESS-210 supersession note after CRUD applies. |
| SPEC patch | `restart/skinny/tranches/sk-v14/SPEC.md`, `SYNTHESIS.md` | Apply W5B-GEN/W5C split, caps, gates, W6/W8/W9/W10 blockers, and grep repair under the same G-Omega authorization. |
| Tranche dispatch patch | `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md`, `DISPATCH-PROMPT.md` | Update R3 PRUNE-3 split, W5BR pre-dispatch guard, and deletion-before-replacement challenge addendum. |
| Tranche handoff patch | `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record G-Omega V6 closure and make W5B-GEN the next executable wave. |

## Gate Question

Choose one:

1. Authorise: close G-Omega V6 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V7 extra confirming wave: run another challenge/fold cycle before CRUD.
