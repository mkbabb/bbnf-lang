# SK-V12 S-P3 V2 CH3 - Regression / REDRESS

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Lens: CH3 regression and REDRESS.
Scope: read-only adversarial review of SK-V12 S-P3 V2 packet.

## Verdict

REVISE.

## Findings

1. Guard-floor language is weaker in promoted SPEC than P3-C. P3-C requires
   every behavior wave to rerun and maintain JSON guards or prove it did not
   touch JSON-producing paths and left `skinny/RESULTS.md` unchanged. SPEC
   currently says guard floors hold only "if JSON results are refreshed" in the
   global close condition and W1/W2 gates.
2. Candidate-local pre-block notes under-carry exact REDRESS families. P3-E and
   SPEC carry REDRESS 28/33 active TBL/NEON tiny-string dispatch, but P3-A C6
   omits it locally. P3-E and SPEC carry REDRESS 70/71 typed-output boundaries,
   but P3-A C1-C3 do not mirror the exact hand-authored typed sink,
   benchmark-private Track 1, and hidden-schema bans.

## Required Folds

1. Fold P3-C's guard rule into SPEC Section 0.1, W1, W2, and DISPATCH
   load-bearing facts: behavior waves must rerun and maintain all 4 direct + 7
   typed guards, or prove no JSON-producing path was touched and RESULTS stayed
   unchanged. Any guard miss fails unless recorded as explicit measured demotion
   in REDRESS.
2. Mirror exact REDRESS 28/33 language in P3-A C6 and any string/TBL/NEON
   candidate note.
3. Mirror exact REDRESS 70/71 typed-output boundary language in P3-A C1-C3.

## Residual Risk

The central REDRESS ledger otherwise carries REDRESS 96/97/98 and 111-120, and
JSON residual fixpoint floors are not broadly weakened.
