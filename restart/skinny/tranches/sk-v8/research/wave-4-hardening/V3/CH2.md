# SK-V8 W4 Hardening V3 CH2

Verdict: ACCEPT.

Confidence: 95%.

## Findings

- W4 should reject/route the scalar-parent fold candidate. Apache passing does
  not save the three-row plan because `random` and `numbers` miss the selected
  row gate, and `numbers` also regresses.
- Numeric gate is correctly fail-closed: Apache passes
  `95.347us <= 92.643us * 1.10`, but `random` fails
  `569.57us > 463.26us * 1.10` and `numbers` fails
  `106.43us > 93.211us * 1.10` with `+6.3287%` Track 2 regression.
- SPEC/plan require all selected rows to pass; any miss triggers
  revert/redress, not partial admission.
- No source or RESULTS admission is present at HEAD. The rejected patch exists
  only at `/tmp/skv8-wave4-track2-scalar-fold-rejected.patch` and touches only
  `direct_struct.rs`. REDRESS records no source patch, no Lock 14 allowance,
  and unchanged RESULTS.
- `skinny/RESULTS.md` remains W0 authority: selected W4 rows are still
  `N-direct / NO-GO`, and the overall report remains `N-direct / NoGo`.
- Current authoritative docs are consistent after the V2 correction: HANDOFF
  says W4 is proposed/pending hardening convergence, not admitted or W5-active.

## Required Folds

None.
