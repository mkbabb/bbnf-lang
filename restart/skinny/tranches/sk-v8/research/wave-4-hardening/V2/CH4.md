# SK-V8 W4 Hardening V2 CH4

Verdict: ACCEPT.

Confidence: 92%.

## Findings

1. The V1 CH4 blockers were admission blockers, not rejection blockers. A
   W4-aware checked gate, full-table maintain proof, and Lock 14 parent-diff
   allowance are required before admitting source or updating
   `skinny/RESULTS.md`, but the fold now rejects the source path.
2. SPEC Section 7 supports this fail-closed route: selected rows failed the W4
   floor, the behavior patch was reverted, `skinny/RESULTS.md` is unchanged,
   and REDRESS records the failed attempt plus residual routing. That satisfies
   the revert/redress path instead of the admission exit gate.
3. The selected-row evidence is sufficient for rejection. `random` still
   misses sonic/1.10 and `numbers` regresses by +6.3287%, so W4 does not need
   to spend budget building unused W4-aware report plumbing or widening Lock 14
   for a source patch that is not admitted.
4. No additional CH4 performance evidence is needed after revert. Full-table
   maintain was required for a surviving global Track 2 parser change; with no
   source diff and no RESULTS diff, there is no changed behavior surface left
   to maintain.
5. Bookkeeping caveat: current handoff text cites a future/nonexistent V3
   consolidated authority. Final close docs should not rely on that citation
   until the authority exists.

## Required Folds

No CH4 fold required for the fail-closed rejection route.

Advisory only: when consolidating, cite the actual V2/V3 authority that exists,
or leave the V3 close reference provisional until generated.
