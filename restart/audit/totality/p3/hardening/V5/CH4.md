# CH4 COST - T-P3 V5

Verdict: ACCEPT.

Target packet: `77b6e9fd7` (`docs(sk-v15-t-p3): repair V4 citation finding`).
Cycle: V5 final confirmation.

The V5 citation fold does not disturb the V3 CH4 cost repair. The target packet
changes only the 3A answered-row citation, replacing the out-of-range V2 CH4
range with the direct `CH4-V2-001` evidence row at
`restart/audit/totality/p3/hardening/V2/CH4.md:36`
(`restart/audit/totality/p3/3A-architecture-synthesis.md:56`). The V3 CH4
coverage matrices still close `CH4-V2-001` and `CH4-V2-002` without W12, broad
CSSOM parity, challenge-time implementation overflow, or doc-only implementation
gates.

## Required Checks

| check | result |
|---|---|
| `git show --stat --oneline 77b6e9fd7 -- restart/audit/totality/p3` | `77b6e9fd7 docs(sk-v15-t-p3): repair V4 citation finding`; 1 file changed, 1 insertion, 1 deletion, limited to `restart/audit/totality/p3/3A-architecture-synthesis.md`. |
| `git diff --check 77b6e9fd7^ 77b6e9fd7 -- restart/audit/totality/p3` | exit 0, no output. |
| Extracted `3C-locks-v+1-diff.md` diff to `/tmp/tp3-locks-v5.diff`, then `git apply --check /tmp/tp3-locks-v5.diff` | exit 0, no output. |
| `grep -cE '^[0-9]+\. \*\*' restart/locks/LOCKS.md` | `16`. |
| `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' \| wc -l` | `67`. |
| stale-pattern `rg` from `CHALLENGE-CONTEXT.md` | no matches, exit 1. |

The required local checks and expected invariants in
`restart/audit/totality/p3/hardening/V5/CHALLENGE-CONTEXT.md:80`-`104` are met.
Pre-existing dirty runtime/research/xtask files are outside this CH4 ownership
slice and were not used to explain any mismatch.

## V2 Finding Closure

| V2 item | V5 result |
|---|---|
| `CH4-V2-001` | Closed. V2 required every carried 3A/3B/3D/3E/3F delta to state LOC, numeric propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action (`restart/audit/totality/p3/hardening/V2/CH4.md:36`; `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:39`). Current V5 artifacts retain complete CH4 matrices for 3A (`restart/audit/totality/p3/3A-architecture-synthesis.md:93`-`111`), 3B (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:168`-`182`), 3D (`restart/audit/totality/p3/3D-skinny-fold.md:70`-`83`), 3E (`restart/audit/totality/p3/3E-grammar-generalisation.md:131`-`150`), and 3F (`restart/audit/totality/p3/3F-migration-handoff.md:113`-`123`). |
| `CH4-V2-002` | Closed. V2 required hard-cap fit and fail action columns for each 3C `D-L*` clause (`restart/audit/totality/p3/hardening/V2/CH4.md:37`; `restart/audit/totality/p3/hardening/HARDENING-T-P3-V2-CONSOLIDATED.md:40`). Current 3C retains those fields for all 12 clauses (`restart/audit/totality/p3/3C-locks-crystallisation.md:59`-`76`) and records the V3 fold in front matter (`restart/audit/totality/p3/3C-locks-crystallisation.md:20`-`21`). |

## Coverage Audit

| artifact | CH4 result |
|---|---|
| 3A | Accept. The matrix covers all 12 carried architecture deltas, and its global rule excludes W12 plus challenge-time implementation overflow (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`111`). The V5 citation fold is in-range and points to the exact V2 finding row (`restart/audit/totality/p3/3A-architecture-synthesis.md:56`; `restart/audit/totality/p3/hardening/V2/CH4.md:36`). |
| 3B | Accept. The matrix covers all 11 MASTER deltas with LOC, propagation count, risk, wave alignment, consumer/gate, hard-cap fit, and fail action (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:170`-`182`). W4 remains split into provenance, generator/check, projection, deletion, and transcript sub-rows (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:144`-`150`). |
| 3C | Accept. The per-clause matrix covers all 12 lock-addendum clauses with doc LOC, risk, affected waves, consumer/gate, propagation count, hard-cap fit, and fail action (`restart/audit/totality/p3/3C-locks-crystallisation.md:63`-`76`). |
| 3D | Accept. The matrix covers all 10 skinny-fold deltas (`restart/audit/totality/p3/3D-skinny-fold.md:72`-`83`) and keeps W4, W7-W9, and CSS non-fit routes outside W12 or docs-only closure (`restart/audit/totality/p3/3D-skinny-fold.md:85`-`91`). |
| 3E | Accept. The matrix covers all 11 grammar-generalisation deltas (`restart/audit/totality/p3/3E-grammar-generalisation.md:138`-`150`). CSS remains scoped typed-provider/retime work; broad CSSOM/value parity is comparator pressure unless intrinsic-blocked or G-Omega-routed (`restart/audit/totality/p3/3E-grammar-generalisation.md:127`, `restart/audit/totality/p3/3E-grammar-generalisation.md:141`, `restart/audit/totality/p3/3E-grammar-generalisation.md:159`). |
| 3F | Accept. The matrix covers all 7 MIGRATION/HANDOFF deltas and routes doc/governance work through CRUD-4, G-Omega, and W0/W11 gates instead of implementation closure by prose (`restart/audit/totality/p3/3F-migration-handoff.md:115`-`123`, `restart/audit/totality/p3/3F-migration-handoff.md:125`-`132`). |

## Prohibited Route Audit

- W12: blocked. 3A globally excludes W12 in the CH4 matrix, 3B rows repeatedly
  route non-fit work to intrinsic block, REDRESS/revert, or G-Omega amendment,
  and 3C rows explicitly say no W12 (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`96`;
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:174`-`182`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`).
- Broad CSSOM parity: blocked. W5/W6 are scoped typed-provider and retime work;
  larger CSSOM/value parity remains comparator pressure or a routed intrinsic
  block/G-Omega amendment (`restart/audit/totality/p3/3B-master-plan-reconciliation.md:176`;
  `restart/audit/totality/p3/3D-skinny-fold.md:76`;
  `restart/audit/totality/p3/3E-grammar-generalisation.md:127`,
  `restart/audit/totality/p3/3E-grammar-generalisation.md:141`).
- Challenge-time implementation overflow: blocked. 3A excludes it globally, 3C
  rows repeat no challenge-time implementation, and 3F's directive replacement
  bars challenge-time implementation (`restart/audit/totality/p3/3A-architecture-synthesis.md:95`-`96`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`;
  `restart/audit/totality/p3/3F-migration-handoff.md:123`).
- Doc-only implementation gates: blocked. Doc/governance rows are Pass Omega
  CRUD, G-Omega, W0, W11, or explicit wave-gate consumers with fail actions, and
  they do not claim implementation closure from prose alone
  (`restart/audit/totality/p3/3A-architecture-synthesis.md:100`;
  `restart/audit/totality/p3/3B-master-plan-reconciliation.md:172`-`174`;
  `restart/audit/totality/p3/3C-locks-crystallisation.md:65`-`76`;
  `restart/audit/totality/p3/3F-migration-handoff.md:117`-`123`).

## Findings

None.

Residual risk is future execution risk only: W4, W5/W6, and W7-W9 remain
high-cost implementation areas, but the current V5 target packet is citation-only
and the V3 row-level cost, cap-fit, consumer/gate, and fail-action coverage
remains CH4-clean.
