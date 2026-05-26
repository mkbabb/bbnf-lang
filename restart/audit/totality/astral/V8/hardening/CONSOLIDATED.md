# Pass Omega V8 CHALLENGE V1 Consolidated

Date: 2026-05-26.
Cycle: V1.
Disposition: REVISE.
Acceptance: 5/6 lenses ACCEPT; one orphan REVISE remains.

## Lens Results

| Lens | Disposition | Required folds |
|---|---:|---|
| CH1 Correctness | REVISE | Owner file/type and exact test names per W5B construct; exact W5B.0 Lock 14 tests; per-test/per-log nonzero proof; redress/REDRESS LOC accounting |
| CH2 Generality | ACCEPT | NONE |
| CH3 Regression | ACCEPT | NONE |
| CH4 Cost | ACCEPT | NONE |
| CH5 Hidden Coupling | ACCEPT | NONE |
| CH6 Next-Tranche / Anti-Paper-Close | ACCEPT | NONE |

## Consolidated Finding

V8 correctly routes REDRESS-212 into a formal W5B.0 through W5B.4 sub-wave
amendment and preserves the V7 semantic ordering: W5B-FRONTEND precedes
W5C-GEN, W5D-DELETE, W6, W7, and new-admit waves. The packet also preserves
zero-delta LOCKS/ARCHITECTURE, no public syntax revival, no new substrate/BIR/
BackendShape, and no provider/template deletion in W5B.

The remaining defect is CH1 exactness. The packet names "exact tests" in broad
terms, but the V2 CH1 fold requires exact owner file/type and exact positive /
fail-closed tests per construct, exact W5B.0 Lock 14 tests, per-test/per-log
nonzero proof, and explicit LOC accounting for redress reports and reject-only
`skinny/REDRESS.md` edits when touched.

## Required V2 Fold

Fold CH1 into the V8 proposed amendment:

1. Add a W5B construct table with columns for construct, owner file/type,
   target representation, exact positive test, and exact fail-closed test.
2. Add exact W5B.0 Lock 14 test requirements for W5B roster admit, W5C/W5D
   subject rejection, provider/template modification rejection, all-template
   guarding, and the `grammar_provider.rs` exception.
3. Require per-test/per-log nonzero assertions for W5B.1 through W5B.4; wildcard
   aggregate log greps remain rejected.
4. Preserve the V8 maintain amendment: exact no-diff for W5B non-admit
   capability waves, or fresh SK-V14-open full-table maintain evidence inside
   W5B.4 if exact no-diff is rejected.
5. Count redress report edits and reject-only `skinny/REDRESS.md` edits in LOC
   accounting whenever touched.

## Next Action

Fold these requirements into the V8 source packet and rerun CH1 through CH6.
