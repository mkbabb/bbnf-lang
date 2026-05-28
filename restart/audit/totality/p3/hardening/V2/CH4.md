# CH4 COST - T-P3 V2

Verdict: REVISE.

Target packet: `7885b29ab` (`docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`).
Context commit: `d1d073a50`.

The V2 packet closes the V1 W4, W7-W9, W5/W6 CSSOM, and CRUD-4 cap defects in
substance, but it is not CH4-clean because the per-delta field contract is still
incomplete outside the repaired high-risk receiver rows.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 7885b29ab -- restart/audit/totality/p3` | `7885b29ab docs(sk-v15-t-p3): fold V1 hardening into V2 synthesis`; 7 files changed, 287 insertions, 206 deletions. |
| `git diff --check 7885b29ab^ 7885b29ab -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff piped to `git apply --check -` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| stale-pattern `rg` from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

These satisfy the invariant expectations at `restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md:95`-`119`.

## Accepted Repairs

- V1 `CH4-COST-01` is closed for the substantive W7-W9 budget defect: V1 required replacing understated W7/W8/W9 rows with 2D bands or explicit intrinsic-block scope (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:41`), and V2 now carries W7 `900-1400`, W8 `700-1100`, and W9 `850-1300` with consumer/gate, cap-fit, fail action, and no-W12 routes (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:130`-`132`; `restart/audit/totality/p3/3D-skinny-fold.md:88`).
- V1 `CH4-COST-02` is closed for W4 budget realism: V1 required splitting Pattern H into provenance gate, generator/check proof, runtime projection, destructive deletion, and close transcript (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:42`); V2 does that with bounded LOC, consumer/gate, fail action, and cap-fit statements (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:143`-`149`).
- V1 `CH4-COST-04` is closed for broad CSSOM scope: V2 states W5 is scoped typed provider work, W6 is retime/retirement, and broad CSSOM parity routes to intrinsic-block or G-Omega amendment rather than hidden W5/W6 work (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:128`-`129`; `restart/audit/totality/p3/3E-grammar-generalisation.md:39`-`42`, `restart/audit/totality/p3/3E-grammar-generalisation.md:125`).
- V1 `CH4-COST-05` is closed for CRUD-4 cap handling: V2 replaces follow-up cleanup with a blocked/extension decision that names exact remainder, receiver, blocker, and gate, and blocks SK-V15 W0 if current dispatch truth remains incomplete (`restart/audit/totality/p3/3F-migration-handoff.md:91`-`94`, `restart/audit/totality/p3/3F-migration-handoff.md:118`).

## Findings

| id | severity | owner | target artifact | conflicting evidence | finding | repair directive |
|---|---|---|---|---|---|---|
| CH4-V2-001 | High | 3A, 3B, 3D, 3E, 3F | `restart/audit/totality/p3/3A-architecture-synthesis.md:58`-`60`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:153`-`155`; `restart/audit/totality/p3/3D-skinny-fold.md:70`-`72`; `restart/audit/totality/p3/3E-grammar-generalisation.md:131`-`133`; `restart/audit/totality/p3/3F-migration-handoff.md:103`-`105`. | CH4 lens requires every delta to state LOC budget, propagation count/risk, wave alignment, same-wave consumer or gate, hard-cap fit, and fail action (`restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md:77`-`80`); SK-V15 cap rules require no W12 and row-level intrinsic-block, REDRESS/revert, or G-Omega amendment on non-fit work (`restart/skinny/tranches/sk-v15/SPEC.md:165`-`170`). | V2 repairs the high-risk W4/W7-W9 rows, but many carried proposal/governance deltas still state only LOC/risk/propagation/wave or "doc LOC" without a per-delta same-wave consumer/gate, hard-cap fit, and fail action. This leaves a paper-close aperture for lower-risk deltas even though the major cost carriers are cap-real. | Add a compact CH4 coverage matrix, or extend each consequences table, for every carried delta in 3A/3B/3D/3E/3F. Each row must name LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action. Doc/governance-only rows may use Pass Omega CRUD/G-Omega as the gate, but must still state the cap-fit and fail route. |
| CH4-V2-002 | High | 3C | `restart/audit/totality/p3/3C-locks-crystallisation.md:62`-`75`. | CH4 lens requires hard-cap fit and fail action for every delta (`restart/audit/totality/p3/hardening/V2/CHALLENGE-CONTEXT.md:77`-`80`); V1 specifically required the `D-L*` clauses to receive per-clause cost/risk/wave/gate propagation fields (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V1-CONSOLIDATED.md:43`). | The new 3C per-clause matrix closes the V1 LOC/risk/wave/gate/propagation defect, but the table still has no per-clause hard-cap-fit or fail-action columns. The global sentence at `3C-locks-crystallisation.md:60` is directionally correct but does not state the field per `D-L*` delta. | Add `hard-cap fit` and `fail action` columns to the 3C per-clause cost matrix. For each `D-L*`, state whether it is Pass Omega doc-only, which wave consumes it, and the exact non-fit action: intrinsic-block, REDRESS/revert, or G-Omega wave-graph amendment; no W12 or challenge-time implementation overflow. |

## Residual Risk

No `REJECT` condition is present: mechanical checks pass, lock/runtime invariants match, stale-pattern scan is clean, and the V1 budget defects are closed without W12, broad CSSOM parity, or doc-only implementation gates. A V3 fold is still required because CH4 field coverage must be per-delta, not only present on the highest-risk receiver rows.
