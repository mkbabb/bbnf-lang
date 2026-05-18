# SK-V8 W4 Hardening V3 CH4

Verdict: ACCEPT.

Confidence: 94%.

## Findings

1. SPEC Section 7 requires every selected W4 row to meet Track 1/Track 2
   floors, and its revert protocol says failed behavior attempts revert
   behavior, RESULTS, and gate changes while adding REDRESS.
2. W4 plan already encodes the fail-closed branch: if any selected row misses,
   revert the source patch and record REDRESS. The attempted scalar-parent fold
   failed that branch: `random` stayed below floor and `numbers` regressed by
   `+6.3287%`; `RESULTS.md` remains unchanged.
3. REDRESS item 93 explicitly states the admission blockers are not needed for
   this rejection: no source patch admitted, no Lock 14 allowance added, and
   `skinny/RESULTS.md` unchanged.
4. Repository checks match the text: relevant source/RESULTS/Lock14 files have
   no diff, `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` exists,
   `RESULTS.md` has 38 manifest rows and zero W4 markers, and
   `cargo test -p bbnf-bench lock14_baseline -- --nocapture` passed 10/10.

## Required Folds

None. Keep W4 as proposed rejected/routed disposition pending V3 convergence;
do not add W4-aware gate/report plumbing or a Lock14 W4 allowance for a
reverted candidate.
