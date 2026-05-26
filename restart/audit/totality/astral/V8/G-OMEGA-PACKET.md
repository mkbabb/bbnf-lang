# G-Omega Packet - Pass Omega V8 W5B-FRONTENDR

Status: mandatory user gate packet.
Date: 2026-05-26.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V8 consumes:

- REDRESS-212 W5B-FRONTEND rejection.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTEND-redress.md`.
- `restart/skinny/tranches/sk-v14/research/skv14-W5B-FRONTENDR-corrective-packet.md`.
- W5B-FRONTEND CHALLENGE V2 disposition, including the cap-accounting rejection
  that routed the one-shot V7 W5B-FRONTEND shape into Omega.
- Pass Omega V7 close packet, because V8 corrects the V7 one-shot
  W5B-FRONTEND cap and close shape.
- Pass Omega V8 Omega A-F source packet plus V2 CHALLENGE convergence.

Omega V8 disposition before CRUD: W5B-FRONTENDR is coherent, zero-lock-change,
and requires SPEC / MASTER / HANDOFF / MIGRATION / limited skinny-corpus surface
updates before W5B.0 LOCK14-GATE can dispatch.

## Challenge Verdict

`restart/audit/totality/astral/V8/hardening/V2/CONSOLIDATED.md`: 6/6 ACCEPT.
Zero open defects and zero orphan REVISEs remain.

V1 carried one CH1 Correctness REVISE. The fold landed at commit `284e5683c`,
adding exact construct owner/type/test rows, exact W5B.0 Lock 14 tests,
per-test nonzero proof requirements, and W5B LOC-accounting constraints.

## Proposed Locks Diff

`restart/audit/totality/astral/V8/locks-diff.md`: zero delta. CRUD-3 is
read/no-op.

The 16-lock count and five-shape BackendShape canon remain preserved.

## Proposed Master Plan / SPEC Diff

`restart/audit/totality/astral/V8/master-plan-diff.md`:

- W5B-FRONTEND becomes an aggregate sub-wave sequence:

```text
W5A
  -> W5B.0 LOCK14-GATE
  -> W5B.1 IMPORT-CLOSURE
  -> W5B.2 LAYOUT-DISCARD
  -> W5B.3 PRETTY-SPAN-PROJECTION
  -> W5B.4 REQUEST-CONSUMER
  -> W5C-GEN
  -> W5D-DELETE
  -> W6
  -> W7
  -> W8/W9/W10
```

- Each W5B.N sub-wave carries HARD CAP 30 min, commit-safe evidence at 27 min,
  and halt at 30 min. Aggregate W5B-FRONTEND cap is <=150 min.
- W5B-FRONTEND closes only after W5B.0 through W5B.4 all admit.
- W5C-GEN remains blocked until aggregate W5B-FRONTEND close.
- W5D-DELETE remains blocked until W5C-GEN close.
- W6, W7, and W8/W9/W10 remain blocked by the PRUNE chain.
- W5B.0 is Lock14-only: owner-path roster, parent-diff routing, W5C/W5D subject
  rejection, modified-provider/template rejection, all-template guard,
  `grammar_provider.rs` exception, and generic owner-path leak census.
- W5B.1 through W5B.4 carry exact construct rows with owner file/type, target
  representation, exact positive test, and exact fail-closed test.
- Every exact W5B test writes a dedicated `/tmp/skv14-w5b-<test-name>.log` and
  carries a dedicated nonzero `rg` assertion. Wildcard aggregate log greps are
  rejected.
- Redress report edits and reject-only `skinny/REDRESS.md` edits count in W5B
  LOC accounting.
- W5B's non-admit maintain gate is exact no-diff unless a fresh
  SK-V14-open full-table maintain run is chosen inside W5B.4.
- No provider/template deletion, provider-free generator body replacement,
  public `@ws` revival, grammar-name branches in generic crates, or borrowing
  from W5C-GEN/W5D-DELETE/W6/new-admit waves is authorized in W5B.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-1 | `restart/ARCHITECTURE.md` | Read/no-op. V8 is wave-graph/cap-accounting only; no BIR/substrate/public-syntax amendment. |
| CRUD-2 | `restart/MASTER-PLAN.md` + SK-V14 SPEC authority | Update W5B-FRONTEND to W5B.0..W5B.4 aggregate form, caps, gates, exactness rows, no-diff maintain rule, and W5C/W5D/W6 dependencies. |
| CRUD-3 | `restart/locks/LOCKS.md` | Read/no-op; verify 16 locks and five-shape BackendShape canon. |
| CRUD-4 | `restart/HANDOFF.md`, `restart/MIGRATION.md`, `restart/skinny/tranches/sk-v14/HANDOFF.md` | Record REDRESS-212, W5B-FRONTENDR block, W5B.0 LOCK14-GATE next dispatch, and W5B.1..W5B.4 / W5C / W5D guards. |
| CRUD-5 | `restart/skinny/{INDEX,WORKSPACE,HARDENING,COMPILER}.md` | Limited text alignment for active W5B sub-wave authority; BENCH/SUBSTRATE read/no-op. |
| SPEC patch | `restart/skinny/tranches/sk-v14/{SPEC,SYNTHESIS,ORCHESTRATOR-PROMPT,DISPATCH-PROMPT}.md` | Apply W5B.0..W5B.4 aggregate split, sub-wave caps, close semantics, challenge routing, exact W5B.0 tests, construct exactness table, dedicated log proof rule, and LOC accounting. |
| CRUD-6 | audit packet + cleanup | Write post-authorization `CRUD-LOG.md` and `G-OMEGA-SIGNOFF.md`; no source/generated/RESULTS movement. |

## Gate Question

Choose one:

1. Authorise: close G-Omega V8 and apply the proposed CRUD / SPEC patches.
2. Hold for review: stop before applying any patch.
3. V9 extra confirming wave: run another challenge/fold cycle before CRUD.
