# SK-V12 W4 CHALLENGE V1 - CH3 Regression And REDRESS

Verdict: REVISE.

PLAN-V1 has one blocking discipline gap: it lets the retained REDRESS-125 CSS
SOTA report stand where W4 needs current post-W4 report/gate consumption.

## Findings

1. W4 PASS must use a W4-current CSS SOTA report/gate path. The retained
   W1b-2b report can remain baseline evidence, but it cannot be the W4 admit
   report after any production CSS scanner change.

2. JSON guard discipline is otherwise sound in A4: CSS companion validation is
   separate from JSON guard validation, CSS-only Criterion roots cannot serve
   as JSON proof, and a fresh populated JSON root is required when shared
   JSON-reachable paths move. PLAN-V2 must carry that condition into required
   commands; if W4 edits report/gate or production `bbnf-simd`, the W1a
   no-write root alone is insufficient.

3. `RESULTS.md` no-write proof is acceptable only at the W4 wave-disposition
   layer. W4 may close as a CSS ADMIT candidate without moving `RESULTS.md`,
   but SK-V12 cannot close PASS-ADMIT until W5 reconciles and promotes the
   row/result surface.

4. REDRESS requirements are coherent but must be explicit: W4 must record the
   selected candidate, post-W4 CSS/lightningcss numbers, JSON guard state,
   Lock 14/16 status, patch ownership, and the five-row orphan table.

W5 dependencies remain coherent. W3 is not required for PASS-ADMIT on the
existing REDRESS-125 candidate path, but becomes mandatory for FIXPOINT.
