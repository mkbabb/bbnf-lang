# CH4 COST - T-P3 V3

Verdict: ACCEPT.

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Context commit: `5b85f7d5d`.

The V3 packet closes the V2 CH4 field-coverage defects. Every carried delta in
3A, 3B, 3D, 3E, and 3F now has row-level LOC, numeric propagation count, risk,
wave alignment, consumer/gate, hard-cap fit, and fail action. Every 3C `D-L*`
clause now has the same cost/fail-action coverage in the per-clause matrix. No
W12 route, broad CSSOM implementation parity route, challenge-time
implementation overflow, or doc-only implementation gate remains.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v3.diff`, then `git apply --check /tmp/tp3-locks-v3.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| stale-pattern `rg` from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

These satisfy the expected invariants in
`restart/audit/totality/p3/hardening/V3/CHALLENGE-CONTEXT.md:92`-`119`.
The dirty runtime/research/xtask state is outside this CH4 ownership slice and
was not used to explain any mismatch.

## V2 Repair Closure

| V2 item | result |
|---|---|
| `CH4-V2-001` | Closed. V2 required every carried 3A/3B/3D/3E/3F delta to name LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:35`-`40`; `restart/audit/totality/p3/hardening/V2/CH4.md:36`). V3 adds compact coverage matrices for 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:93`-`111`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:168`-`182`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:70`-`83`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:131`-`150`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:113`-`123`). |
| `CH4-V2-002` | Closed. V2 required per-clause hard-cap fit and fail action for every 3C `D-L*` clause (`restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:39`-`40`; `restart/audit/totality/p3/hardening/V2/CH4.md:37`). V3 adds `hard-cap fit` and `fail action` columns and rows for `D-L01` through `D-L16` (`restart/audit/totality/p3/3C-locks-crystallisation.md:59`-`76`). |

## Coverage Audit

| artifact | CH4 coverage |
|---|---|
| 3A | All 12 carried architecture deltas are covered by the matrix header and rows at `restart/audit/totality/p3/3A-architecture-synthesis.md:98`-`111`. High-risk W4/W7/W8/W9 rows carry T-P1/T-P2 bands, cap-fit limits, and fail actions (`restart/audit/totality/p3/3A-architecture-synthesis.md:103`-`107`). |
| 3B | All 11 carried MASTER deltas are covered at `restart/audit/totality/p3/3B-master-plan-reconciliation.md:170`-`182`. The W4 split separately preserves provenance gate, generator/check proof, runtime projection, destructive deletion, and close transcript costs with consumer/gate, fail action, and cap-fit statements (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:137`-`150`). |
| 3C | All 12 `D-L*` lock clauses have doc LOC, risk, affected waves, consumer/gate, numeric propagation count, hard-cap fit, and fail action at `restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`76`. |
| 3D | All 10 carried skinny-fold deltas are covered at `restart/audit/totality/p3/3D-skinny-fold.md:72`-`83`, with W4/W7-W9/CSS non-fit routes restated at `restart/audit/totality/p3/3D-skinny-fold.md:85`-`91`. |
| 3E | All 11 carried grammar-generalisation deltas are covered at `restart/audit/totality/p3/3E-grammar-generalisation.md:138`-`150`. W5/W6 are scoped to typed provider and same-workload retime, while lowerers inherit the 2D/3B W7-W9 carriers (`restart/audit/totality/p3/3E-grammar-generalisation.md:123`-`129`). |
| 3F | All 7 carried MIGRATION/HANDOFF deltas are covered at `restart/audit/totality/p3/3F-migration-handoff.md:115`-`123`, including CRUD-4/G-Omega gates and blocked/extension fail actions. |

## Prohibited Route Audit

- W12: blocked. The packet states no-W12 handling in 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`107`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:119`-`135`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:176`-`182`), 3C (`restart/audit/totality/p3/3C-locks-crystallisation.md:61`-`76`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:74`-`83`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:144`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:118`).
- Broad CSSOM parity: blocked as hidden W5/W6 scope. It is routed only as comparator pressure, intrinsic block, or G-Omega amendment (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:129`-`130`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:176`; `restart/audit/totality/p3/3D-skinny-fold.md:76`, `restart/audit/totality/p3/3D-skinny-fold.md:91`; `restart/audit/totality/p3/3E-grammar-generalisation.md:41`-`44`, `restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:141`, `restart/audit/totality/p3/3E-grammar-generalisation.md:159`).
- Challenge-time implementation overflow: blocked. 3A excludes challenge-time overflow globally for the coverage matrix (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`96`), 3C repeats no challenge-time implementation on every `D-L*` row (`restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`), and 3F's final directive replacement forbids challenge-time implementation (`restart/audit/totality/p3/3F-migration-handoff.md:123`).
- Doc-only implementation gate: blocked. Doc/governance rows name Pass Omega CRUD, G-Omega, or owning wave gates and explicit fail actions instead of claiming implementation closure from prose (`restart/audit/totality/p3/3A-architecture-synthesis.md:100`; `restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`174`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:182`; `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`; `restart/audit/totality/p3/3D-skinny-fold.md:74`; `restart/audit/totality/p3/3F-migration-handoff.md:117`-`123`).

## Findings

None.

Residual risk is future execution risk only: W4, W5/W6, and W7-W9 remain
high-cost implementation rows, but V3 now states their cap boundaries and
fail-closed routes explicitly enough for CH4.
