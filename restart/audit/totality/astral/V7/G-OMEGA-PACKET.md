# G-Omega Packet - Pass Omega V7 W5B-GENR

Status: mandatory user gate packet.
Date: 2026-05-26.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V7 consumes:

- REDRESS-211 W5B-GEN rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-redress.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GEN-plan.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-GENR-corrective-packet.md`.
- W5B-GEN CHALLENGE V2/V3 convergence, with V3 §3Z LOCKED for the rejection
  route.
- Pass Omega V6 close packet, because V7 corrects the W5B-GEN/W5C-DELETE split.

Omega V7 disposition before CRUD: W5B-GENR is coherent, zero-lock-change, and
requires SPEC / MASTER / HANDOFF / MIGRATION / limited skinny-corpus surface
updates before W5B-FRONTEND can dispatch.

## Challenge Verdict

`restart/audit/totality/astral/V7/hardening/CONSOLIDATED.md`: 6/6 ACCEPT after
fold. Zero open defects and zero orphan REVISEs remain.

## Proposed Locks Diff

`restart/audit/totality/astral/V7/locks-diff.md`: zero delta. CRUD-3 is
read/no-op.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V7/master-plan-diff.md`:

- W5A remains admitted.
- W5B-GEN is rejected by REDRESS-211 and splits into W5B-FRONTEND and W5C-GEN.
- W5B-FRONTEND owns generic BBNF grammar-source frontend/import/IR closure; CSS
  L4 is the strict positive witness; compatibility syntax such as `@ws` lowers
  into canonical IR and is not a new public directive.
- W5C-GEN owns the provider-free runtime generator body consuming request plus
  frontend IR.
- W5D-DELETE owns provider/template deletion and Lock 14 baseline close.
- W6 becomes conditional on W5D-DELETE close.
- W7 and W8/W9/W10 remain blocked by the PRUNE chain.
- W5B-FRONTEND and W5C-GEN must add explicit Lock 14 owner-path and parent-diff
  routing in `skinny/crates/bbnf-bench/src/lock14_baseline.rs` before touching
  their source owner paths.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. V7 is wave-graph/generator-frontend sequencing only; standalone `@ws` remains retired. |
| CRUD-2 | `restart/MASTER-PLAN.md` + SK-V14 SPEC authority | Update W5B-FRONTEND/W5C-GEN/W5D-DELETE rows, caps, gates, W6 dependency, and Lock 14 owner-routing requirements. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; preserve 16 locks and five-shape BackendShape canon. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md`, `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record REDRESS-211, W5B-GENR block, W5B-FRONTEND next dispatch, and W5C-GEN/W5D-DELETE guards. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` | Limited text alignment; BENCH/SUBSTRATE read/no-op. |
| SPEC patch | `restart/skinny/tranches/sk-v14/{SPEC,SYNTHESIS,ORCHESTRATOR-PROMPT,DISPATCH-PROMPT}.md` | Apply the W5B-FRONTEND/W5C-GEN/W5D-DELETE split, caps, guards, challenge routing, and Lock 14 owner-path parent-diff requirements. |
| CRUD-6 | audit packet + cleanup | Write post-authorization `CRUD-LOG.md` and `G-OMEGA-SIGNOFF.md`; no source/generated/RESULTS movement. |

## Gate Question

Choose one:

1. Authorise: close G-Omega V7 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V8 extra confirming wave: run another challenge/fold cycle before CRUD.
