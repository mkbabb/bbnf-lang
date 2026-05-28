# CH4 COST - T-P3 V4

Verdict: ACCEPT.

Target packet: `e6c1c2a84` (`docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`).
Cycle: V4 confirmation.

The V4 cost challenge independently accepts the V3 CH4 fold. The V3 packet closes
`CH4-V2-001` and `CH4-V2-002`: every carried 3A/3B/3D/3E/3F delta and every 3C
`D-L*` clause now has row-level LOC, numeric propagation count, risk, wave
alignment, consumer/gate, hard-cap fit, and fail action. The closure does not
use W12, broad CSSOM parity hidden in W5/W6, challenge-time implementation
overflow, or doc-only implementation gates.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline e6c1c2a84 -- restart/audit/totality/p3` | `e6c1c2a84 docs(sk-v15-t-p3): fold V2 cost hardening into V3 synthesis`; 7 proposal files changed, 170 insertions, 123 deletions. |
| `git diff --check e6c1c2a84^ e6c1c2a84 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v4.diff`, then `git apply --check /tmp/tp3-locks-v4.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| stale-pattern `rg` from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

The expected invariants in
`restart/audit/totality/p3/hardening/V4/CHALLENGE-CONTEXT.md:92`-`119` are met.
Pre-existing dirty runtime/generated files are outside this CH4 ownership slice;
they do not alter the proposal artifact checks or the 67-file runtime count.

## V2 Finding Closure

| V2 item | V4 result |
|---|---|
| `CH4-V2-001` | Closed. V2 required every carried 3A/3B/3D/3E/3F delta to name LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action (`restart/audit/totality/p3/hardening/V2/CH4.md:36`; `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:39`). V3 supplies complete CH4 matrices for 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:98`-`111`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:170`-`182`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:72`-`83`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:138`-`150`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:115`-`123`). |
| `CH4-V2-002` | Closed. V2 required hard-cap fit and fail action columns for each 3C `D-L*` clause (`restart/audit/totality/p3/hardening/V2/CH4.md:37`; `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:40`). V3 adds those fields for all 12 clauses (`restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`76`) and records the fold in the 3C front matter (`restart/audit/totality/p3/3C-locks-crystallisation.md:20`-`21`). |

## Coverage Audit

| artifact | CH4 result |
|---|---|
| 3A | Accept. The matrix covers the 12 carried architecture deltas and its hard-cap rule globally excludes W12 and challenge-time implementation overflow (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`111`). High-cost W4/W7/W8/W9 rows name the larger bands, consumers, fit limits, and intrinsic-block/G-Omega fail routes (`restart/audit/totality/p3/3A-architecture-synthesis.md:103`, `restart/audit/totality/p3/3A-architecture-synthesis.md:106`-`107`). |
| 3B | Accept. The matrix covers 11 MASTER deltas (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:170`-`182`), with the W4 sub-row split preserving provenance, generator/check, projection, deletion, and transcript costs (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:144`-`150`). W5/W6 scoped CSS rows make broad CSSOM non-fit (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:129`-`130`, `restart/audit/totality/p3/3B-master-plan-reconciliation.md:176`). |
| 3C | Accept. The per-clause matrix has doc LOC, risk, affected waves, consumer/gate, propagation count, hard-cap fit, and fail action for all 12 lock clauses (`restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`76`). Each row is Pass Omega doc-only and consumed by explicit wave/governance gates, with no W12 or challenge-time implementation. |
| 3D | Accept. The matrix covers all 10 skinny-fold deltas (`restart/audit/totality/p3/3D-skinny-fold.md:72`-`83`). The non-fit fold keeps W4 and W7-W9 on T-P1/T-P2 cost carriers and blocks W12, silent overflow, and doc-only close (`restart/audit/totality/p3/3D-skinny-fold.md:85`-`91`). |
| 3E | Accept. The matrix covers all 11 grammar-generalisation deltas (`restart/audit/totality/p3/3E-grammar-generalisation.md:138`-`150`). CSS remains scoped typed-provider/retime work; broad CSSOM/value parity is comparator pressure unless intrinsic-blocked or G-Omega-routed (`restart/audit/totality/p3/3E-grammar-generalisation.md:41`-`44`, `restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:141`, `restart/audit/totality/p3/3E-grammar-generalisation.md:159`). |
| 3F | Accept. The matrix covers all 7 MIGRATION/HANDOFF deltas (`restart/audit/totality/p3/3F-migration-handoff.md:115`-`123`). Doc-only governance rows are not implementation gates: they route through CRUD-4/G-Omega and block W0 on incomplete current-state truth (`restart/audit/totality/p3/3F-migration-handoff.md:117`-`123`, `restart/audit/totality/p3/3F-migration-handoff.md:125`-`132`). |

## Prohibited Route Audit

- W12: blocked. V3 repeatedly routes over-cap work to intrinsic-block,
  REDRESS/revert, or G-Omega wave-graph amendment, with no W12 route
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`96`;
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:124`-`135`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`;
  `restart/audit/totality/p3/3D-skinny-fold.md:90`;
  `restart/audit/totality/p3/3E-grammar-generalisation.md:129`;
  `restart/audit/totality/p3/3F-migration-handoff.md:118`).
- Broad CSSOM parity: blocked as hidden W5/W6 scope. It appears only as
  comparator pressure, non-fit work, intrinsic-block, or G-Omega amendment
  (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:129`-`130`;
  `restart/audit/totality/p3/3D-skinny-fold.md:76`;
  `restart/audit/totality/p3/3E-grammar-generalisation.md:127`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:141`).
- Challenge-time implementation overflow: blocked by the 3A global matrix rule,
  the 3C per-clause fail actions, and the 3F directive replacement
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`96`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`;
  `restart/audit/totality/p3/3F-migration-handoff.md:123`).
- Doc-only implementation gates: blocked. Doc/governance rows are explicitly
  Pass Omega CRUD, G-Omega, W0, or wave-gate consumers with fail actions; none
  claims implementation closure from prose alone
  (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`174`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`;
  `restart/audit/totality/p3/3F-migration-handoff.md:117`-`123`).

## Findings

None.

Residual risk is future execution risk only: W4, W5/W6, and W7-W9 remain
high-cost implementation areas, but the V3 proposal now carries enough row-level
cost, cap-fit, consumer/gate, and fail-action coverage for CH4.
