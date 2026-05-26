# G-Omega Packet - Pass Omega V4 W4R

Status: mandatory user gate packet.
Date: 2026-05-26.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V4 consumes:

- REDRESS-184 W4 rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W4R-corrective-packet.md`.
- Pass Omega V3 W2R close packet, because W4R builds on amended W2/W6 split.
- Omega-A through Omega-F V4 artifacts in this directory.

Omega V4 disposition before CRUD: W4R is coherent, zero-lock-change, and
requires SPEC / SYNTHESIS / MASTER / HANDOFF / MIGRATION / limited skinny-corpus
surface updates before W4 can rerun.

## Challenge Verdict

`restart/audit/totality/astral/V4/hardening/CONSOLIDATED.md`: 6/6 ACCEPT.
Zero open defects and zero orphan REVISEs remain.

## Proposed Locks Diff

`restart/audit/totality/astral/V4/locks-diff.md`: zero delta. CRUD-3 is read/no-op.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V4/master-plan-diff.md`:

- W4 becomes CSS L4 admit-ledger PRUNE only: rolling delta 0/24 plus 24
  REDRESS entries; no provider/template deletion.
- W5 absorbs CSS provider/template deletion into the existing
  grammar-agnostic provider replacement wave.
- W6 remains W6.0 CSS L4 root-runtime collapse plus W6.1-W6.8 remaining dirs.
- W8/W9/W10 remain globally blocked until PRUNE-1..PRUNE-5 close.
- Active dispatch-surface path corrections: `restart/skinny/ROLLING-SOTA-DELTA.md`
  is the live rolling delta file; `skinny/ROLLING-SOTA-DELTA.md` does not
  exist.
- REDRESS-183 receives a narrow supersession note; W2R and W3 are admitted, and
  REDRESS-184 is the current blocker.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. W4R is wave-graph-only. |
| CRUD-2 | `restart/MASTER-PLAN.md` | Update §13.3 W4/W5 rows and W4R receiver note. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; preserve 16 locks. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Record REDRESS-184, W4R block, amended W4/W5 ownership, and next dispatch directive. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING}.md` plus tranche-local W4R pointers | Limited text alignment; BENCH/COMPILER/SUBSTRATE read/no-op. |
| CRUD-6 | audit packet + REDRESS supersession note | Write post-authorization CRUD log/signoff and add a narrow REDRESS-183 supersession note; no legacy nuke needed for W4R. |
| SPEC patch | `restart/skinny/tranches/sk-v14/SPEC.md`, `SYNTHESIS.md` | Apply amended dispatch authority under the same G-Omega authorization. |
| Tranche dispatch patch | `restart/skinny/tranches/sk-v14/ORCHESTRATOR-PROMPT.md` | Update R3 PRUNE-2/PRUNE-3 wording to W4 ledger-only and W5 provider deletion/replacement. |
| Tranche handoff patch | `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record REDRESS-184 and W4R gate. |
| Tranche dispatch-prompt patch | `restart/skinny/tranches/sk-v14/DISPATCH-PROMPT.md` | Add pre-dispatch verification that G-Omega V4 + CRUD landed before W5/provider deletion. |
| Rolling-delta path correction | SK-V14 SPEC / DISPATCH prompt active references | Replace nonexistent `skinny/ROLLING-SOTA-DELTA.md` references with `restart/skinny/ROLLING-SOTA-DELTA.md`. |

## Gate Question

Choose one:

1. Authorise: close G-Omega V4 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V5 extra confirming wave: run another challenge/fold cycle before CRUD.
