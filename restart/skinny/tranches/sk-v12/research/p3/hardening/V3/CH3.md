# SK-V12 S-P3 V3 CH3 - Regression / REDRESS

Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Date: 2026-05-20.
Lens: CH3 regression and REDRESS.
Scope: read-only adversarial review of SK-V12 S-P3 V3 packet.

## Verdict

ACCEPT.

## Findings

1. JSON guard rule is explicit in P3-C, SPEC, P3-F, and DISPATCH: behavior waves
   rerun and maintain all 4 direct + 7 typed guards, or prove no
   JSON-producing path was touched and RESULTS stayed unchanged; guard misses
   require measured REDRESS demotion.
2. REDRESS 28/33 active TBL/NEON tiny-string dispatch is locally mirrored in C6
   and centrally blocked.
3. REDRESS 70/71 typed-output boundary is locally mirrored in C1-C3 and
   centrally blocked.
4. REDRESS 96/97/98 and REDRESS 111-120 are carried without weakening the
   SK-V11 fixpoint.
5. W3 behavior requires fresh material evidence beyond REDRESS 114-119;
   otherwise it records a routed block with no source/RESULTS movement.

## Required Folds

None.

## Residual Risk

Low. Implementers should treat SPEC/P3-C as binding when P3-B uses stricter but
shorter guard wording.
