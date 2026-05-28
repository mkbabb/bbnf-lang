# G-Omega Packet - Pass Omega V9 SK-V15 Authority

Status: mandatory user gate packet.
Date: 2026-05-28.
Do not apply CRUD until the user authorizes G-Omega.

## Cohort Lock Declaration

Pass Omega V9 consumes the SK-V15 pass closure packet:

- SK-V15 skinny S-P3 V4 locked
  `restart/skinny/tranches/sk-v15/SPEC.md` and
  `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md` as the W0-W11
  implementation contract.
- T-P1 closed as clean-final / G1-auto-pinned at
  `restart/audit/totality/p1/hardening/HARDENING-T-P1-V5-CONSOLIDATED.md`;
  this is explicitly not rewritten as normal two-clean-cycle §3Z.
- T-P2 closed by normal §3Z at
  `restart/audit/totality/p2/hardening/HARDENING-T-P2-V3-CONSOLIDATED.md`.
- T-P3 closed by final V5 all-ACCEPT confirmation at
  `restart/audit/totality/p3/hardening/HARDENING-T-P3-V5-CONSOLIDATED.md`.
- Pass Omega V9 source packet:
  `restart/audit/totality/astral/V9/ΩA-coherence-audit.md` through
  `restart/audit/totality/astral/V9/ΩF-migration-handoff.md`,
  `locks-diff.md`, and `master-plan-diff.md`.
- Pass Omega V9 V2 CHALLENGE convergence:
  `restart/audit/totality/astral/V9/hardening/V2/CONSOLIDATED.md`.

The active implementation authority after authorized CRUD is SK-V15 W0-W11.
SK-V14 W5B / Pass Omega V8 remains historical/pre-block evidence only.

## Challenge Verdict

`restart/audit/totality/astral/V9/hardening/V2/CONSOLIDATED.md`: 6/6 ACCEPT.
Zero orphan `REVISE`, zero `REJECT`.

V1 carried two defects:

- CH1: malformed MASTER/SPEC pseudo-diff plus stale T-P2 authority tokens.
- CH4: ambiguous CRUD and SPEC scope plus missing consolidated
  no-source/no-generated boundary.

The V2 fold landed at `9d336c606`; V2 hardening convergence landed at
`44ef58c35`.

## Proposed Locks Diff

`restart/audit/totality/astral/V9/locks-diff.md`: one G-Omega-gated addendum
before `## v+1 Governance Boundary`.

The addendum:

- preserves the 16 numbered locks;
- preserves the exact five `BackendShape` variants
  `{EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`;
- keeps `FactStream` outside `BackendShape`;
- adds no directive, BIR variant, substrate, public substrate API, retained
  sidecar, lock, lock retirement, or sixth shape;
- preserves Apple M5 Max / aarch64-only admission evidence and keeps x86 /
  AVX-512 diagnostic only.

Verification recorded by V9:

```sh
grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md
# 16

find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
# 67

awk '/^diff --git/{flag=1} flag && $0 != "```"{print}' restart/audit/totality/astral/V9/locks-diff.md | git apply --check -
# exits clean
```

## Proposed Master-Plan Operations

`restart/audit/totality/astral/V9/master-plan-diff.md` is an anchored operation
list, not a unified diff.

CRUD-2 applies only `restart/MASTER-PLAN.md` operations:

- mark §13.3 SK-V14 W0..W11 as historical/pre-block and superseded for active
  dispatch by new §13.5;
- mark §13.4 T-P3 V4 MP-NW rows as historical/pre-block and superseded for
  active dispatch by new §13.5;
- insert new §13.5 SK-V15 PRUNE-then-REBUILD receiver block, importing the
  locked SK-V15 W0-W11 implementation graph;
- update §25 implementation order from stale SK-V13/SK-V14 routing to SK-V15
  W0 first after G-Omega V9 and authorized CRUD.

SK-V15 `SPEC.md` and `DISPATCH-PROMPT.md` are read/no-op for V9. V9 CRUD does
not edit them.

## Proposed CRUD Operations

| CRUD | Surface | Operation after G-Omega |
|---|---|---|
| CRUD-3 LOCKS | `restart/locks/LOCKS.md` | Apply the explicit V9 addendum from `locks-diff.md`; verify 16 locks, five-shape BackendShape canon, and `FactStream` outside `BackendShape`. |
| CRUD-1 ARCHITECTURE | `restart/ARCHITECTURE.md` | Align implementation-status authority to SK-V15 T-P1/T-P2/T-P3 and PASS-IMPL V1 blockers; do not add any substrate, directive, BIR variant, public API, sidecar, lock, or shape. |
| CRUD-2 MASTER-PLAN | `restart/MASTER-PLAN.md` | Apply the four anchored operations in `master-plan-diff.md`; SK-V15 SPEC/DISPATCH remain read-only for V9. |
| CRUD-4 HANDOFF + MIGRATION | `restart/HANDOFF.md`, `restart/MIGRATION.md` | Replace stale SK-V14 W5B/Omega V8 current authority with SK-V15 W0-W11 current implementation authority; remove the absent T-P2 authority reference; make W0 the next dispatch. |
| CRUD-5 SKINNY CORPUS | `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md` | Limited text alignment where those surfaces still route current work through SK-V14/Omega V8 or omit SK-V15 overfit-prune constraints. |
| CRUD-6 AUDIT + CLEANUP | `restart/audit/totality/astral/V9/{CRUD-LOG,G-OMEGA-SIGNOFF}.md` | Record authorization, patch scope, verification, and CRUD-6 source-map cleanup; confirm no live source/generated/results/redress movement happened during V9 CRUD. |

Authorized V9 CRUD may touch only:

```text
restart/ARCHITECTURE.md
restart/MASTER-PLAN.md
restart/locks/LOCKS.md
restart/HANDOFF.md
restart/MIGRATION.md
restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md
restart/audit/totality/astral/V9/{CRUD-LOG,G-OMEGA-SIGNOFF}.md
```

V9 CRUD may not touch source, generated output, gates, `skinny/RESULTS.md`,
`skinny/REDRESS.md`, or SK-V15 SPEC/DISPATCH.

## Post-Authorization Directive

If authorized:

1. Apply CRUD-3 first.
2. Apply CRUD-1, CRUD-2, CRUD-4, and CRUD-5 as the authorized V1 corpus patch
   set while preserving unrelated dirty implementation files.
3. Apply CRUD-6 audit close.
4. Stop routine Omega/Alpha churn for this SK-V15 implementation authority.
5. Dispatch actual SK-V15 W0 through
   `restart/skinny/tranches/sk-v15/DISPATCH-PROMPT.md`, then execute W1 through
   W11 in strict `SPEC.md` order.

## Gate Question

Choose one:

1. Authorise: close G-Omega V9 and apply the proposed V1 corpus CRUD patches.
2. Hold for review: stop before applying any patch.
3. V3 extra confirming wave: run another Pass Omega V9 challenge/fold cycle
   before CRUD.
