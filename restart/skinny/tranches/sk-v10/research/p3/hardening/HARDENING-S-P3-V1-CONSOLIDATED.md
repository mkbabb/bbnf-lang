# SK-V10 S-P3 V1 CHALLENGE Consolidation

Date: 2026-05-19.
Scope: consolidate six-lens CHALLENGE over the V1 SK-V10 S-P3 SPEC,
DISPATCH-PROMPT, and P3-A..F cohort.

## Verdict

Disposition: REVISE.

Lens outcomes:

| Lens | Verdict | Acceptance | Blocking disposition |
|---|---|---:|---|
| CH1 correctness | REVISE | 82% | P3-C/P3-E wave-number drift, W9 C8/C9 overreach, W9 dependency ambiguity |
| CH2 generality/Lock 14 | REVISE | 84% | Missing explicit SPEC Section 2.1 gate; W7-W9 needed stronger Lock 14 exits |
| CH3 regression/REDRESS | ACCEPT | 92% | No reopened rejected route |
| CH4 cost/micro-proof | REVISE | 78% | Missing LOC budgets, cap ambiguity, W9 too broad |
| CH5 hidden coupling/Lock 1 | REVISE | 82% | Generated-file hand-edit route, Track 2 dependency dilution, W5 Lock 14 loophole |
| CH6 anti-paper-close | REVISE | 88% | W7/W8 microbench exits needed threshold-bearing measured artifacts |

Mean lens score: 84.3%. ACCEPT lens rate: 1/6. V1 does not converge.

## Required Folds Applied Into V2

1. `SPEC.md` now names the cycle as V2 challenge-fold, adds binding per-wave
   LOC/edit budgets, and normalizes the manifest cap as the redress execution
   cap. Research and plan caps remain dispatch-protocol phase caps.
2. `SPEC.md` now contains Section 2.1, `Generality And Lock 14 Gate`, requiring
   CSS L4, Sheets, or BBNF-self proof for generic/codegen/runtime-outside-JSON
   behavior edits, and forbidding hand patches to generated output.
3. `SPEC.md` now contains Section 2.2, `Track 2 Independence Gate`, forbidding
   Track 2/oracle calls into generated Track 1, generated SinkOnly helpers,
   generated typed helpers, or benchmark-private shared parser code.
4. W1 and W2 now require the Track 2 independence proof or audit artifact.
5. W5 now requires named non-JSON proof for any codegen/generic behavior edit;
   "no generic behavior changed" is valid only when the diff has no such
   behavior edit.
6. W7 and W8 now treat generated artifacts as read-only evidence unless the
   same wave owns generator/schema inputs and regeneration.
7. W7 and W8 now close only on threshold-clearing caller microbench artifacts
   that record observed value, threshold, run id, host/build flags, feature
   gate, representative slices, sample count, scalar oracle identity, and
   differential harness identity. REDRESS records observed value versus
   threshold on miss.
8. W9 is narrowed to a relevant accepted W7 or W8 `C4`-`C7` proof, exactly one
   primitive, exactly one existing production caller, exactly one consumer
   plane, and one row-moving target set.
9. `C8` is routed out of the final W0-W10 executable plan until a future
   SPEC/CHALLENGE amendment adds a dedicated proof wave. `C9` remains
   unshortlisted maintain-only and cannot feed W9.
10. `DISPATCH-PROMPT.md` mirrors the budget table, the first-of-class and
    generic-edit CHALLENGE trigger, the W7/W8 threshold-bearing closure rule,
    the W9 one-primitive/one-caller/one-plane restriction, generated-boundary
    discipline, Track 2 dependency proof, and Section 2.1 Lock 14 proof.
11. P3-B, P3-C, P3-E, P3-F, and P3-A were aligned to the final W0-W10 topology
    and V2 routing so support artifacts no longer present stale dispatch wave
    numbers as authority.

## Next Cycle

Run S-P3 V2 CHALLENGE over the folded contract. The V2 acceptance target is
zero open critical defects and at least 95% ACCEPT; if V2 accepts, run one more
confirmation cycle unless the orchestrator pins final sign-off.
