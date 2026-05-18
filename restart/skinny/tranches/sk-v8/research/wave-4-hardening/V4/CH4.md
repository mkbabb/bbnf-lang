# SK-V8 W4 Hardening V4 CH4

Date: 2026-05-18.

Verdict: ACCEPT.

Confidence: 95%.

## Findings

1. The CH4 scope is satisfied for a rejection/routing close. ORCHESTRATOR CH4
   reviews LOC budget, risk class, wave alignment, hard cap, and same-wave
   consumer (`restart/prompts/ORCHESTRATOR.md:81`-`restart/prompts/ORCHESTRATOR.md:87`).
   SPEC caps W4 at `<=300` source/test LOC, `<=3` selected rows, and
   `<=90 min`, with LOC, rerun, and time budgets conjunctive
   (`restart/skinny/tranches/sk-v8/SPEC.md:216`-`restart/skinny/tranches/sk-v8/SPEC.md:245`).
   The W4 plan selected exactly three rows and one source owner,
   `skinny/crates/bbnf-bench/src/direct_struct.rs`
   (`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:9`-`restart/skinny/tranches/sk-v8/research/skv8-W4-plan.md:25`).
   Current HEAD is `53aecc2003670206ac605c18ffd4cc3156e604bc`;
   `git status --short` is empty, and the relevant source/RESULTS/gate/doc
   diff checks are clean.
2. Wave alignment is valid. SPEC allows W4 after W0/W1 and W2/W3 admission,
   rejection, or explicit route
   (`restart/skinny/tranches/sk-v8/SPEC.md:610`-`restart/skinny/tranches/sk-v8/SPEC.md:617`).
   HANDOFF records W0-W3 closed/routed and W4 as the active proposed
   rejection/routing disposition, with W5-W6 still conditional
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5`-`restart/skinny/tranches/sk-v8/HANDOFF.md:13`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:133`-`restart/skinny/tranches/sk-v8/HANDOFF.md:141`).
   The fact that HANDOFF remains pending before this V4 closure authority is
   not a CH4 blocker; V3 consolidated explicitly required an unchanged V4
   challenge before W4 may close
   (`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/HARDENING-W4-V3-CONSOLIDATED.md:35`-`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/HARDENING-W4-V3-CONSOLIDATED.md:38`).
3. Rejecting/routing instead of adding W4 report plumbing or a Lock 14
   allowance is cost-valid after selected-row falsification. SPEC Section 7
   requires every selected row to meet Track 1 and Track 2 floors for
   admission, and its revert protocol says failed behavior attempts revert
   behavior, RESULTS, and gate changes while keeping triage/REDRESS evidence
   (`restart/skinny/tranches/sk-v8/SPEC.md:626`-`restart/skinny/tranches/sk-v8/SPEC.md:648`).
   REDRESS 93 records that Apache cleared sonic/1.10, but `random` missed and
   `numbers` regressed by `+6.3287%`, before any row-table admission question
   (`skinny/REDRESS.md:2711`-`skinny/REDRESS.md:2716`). The V1 CH4
   requirements for a W4-aware checked report path, full-table maintain proof,
   and Lock 14 W4 parent-diff allowance were admission blockers, not mandatory
   work for a reverted failed candidate
   (`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V1/CH4.md:16`-`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V1/CH4.md:28`,
   `restart/skinny/tranches/sk-v8/research/wave-4-hardening/V2/CH4.md:9`-`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V2/CH4.md:24`).
4. The 90-minute cap is realistic only for the fail-closed route. An admission
   route would require source work plus W4-aware report/gate work, full-table
   maintain measurement, Lock 14 allowance/tests, RESULTS update, and rollback
   budget. The selected-row miss makes that spend unjustified in this wave. The
   accepted route has no surviving source, RESULTS, generated output, bench
   wiring, or gate change; `skinny/RESULTS.md` remains W0 authority with 38
   manifest rows and zero W4 markers, matching V3 CH4's repository-state
   finding
   (`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/CH4.md:19`-`restart/skinny/tranches/sk-v8/research/wave-4-hardening/V3/CH4.md:22`).
5. Same-wave consumer and revert protocol are coherent. The planned same-wave
   consumer was the selected direct rows consuming generated Track 1 direct/
   SinkOnly work and independent Track 2 proof
   (`restart/skinny/tranches/sk-v8/SPEC.md:637`-`restart/skinny/tranches/sk-v8/SPEC.md:638`).
   Because the selected rows failed, no source primitive survives and there is
   no orphan kernel. The live hand Track 2 parser still uses child digest
   construction in object/array paths
   (`skinny/crates/bbnf-bench/src/direct_struct.rs:502`,
   `skinny/crates/bbnf-bench/src/direct_struct.rs:529`), while the rejected
   patch remains archived at `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`
   and touches only `skinny/crates/bbnf-bench/src/direct_struct.rs`. REDRESS 93
   correctly routes any future reopen behind a W4/V9-aware checked gate,
   full-table maintain measurement, and independent Track 2 digest-arithmetic
   backstop (`skinny/REDRESS.md:2723`-`skinny/REDRESS.md:2729`).

## Required Folds

None for CH4. If the full V4 panel accepts, consolidation may update HANDOFF
from pending to closed/routed authority and allow W5 to become the next
candidate wave under its own gates.
