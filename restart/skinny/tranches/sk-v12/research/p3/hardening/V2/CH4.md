# SK-V12 S-P3 V2 CH4 - Cost / Caps

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Lens: CH4 cost and caps.
Scope: read-only adversarial review of SK-V12 S-P3 V2 packet.

## Verdict

REVISE.

## Findings

1. Unbudgeted W1a/W1b split remains authorized in P3-C. The packet names
   `G-W1a-GENERATOR-RUNTIME-UNBLOCK` and `G-W1b-GENERATED-NONJSON-BASELINE`
   without adding those sub-waves to the manifest with LOC, risk, wall cap,
   redress cap, rerun ceiling, gate, revert protocol, and same-wave consumer.
2. LOC budgets drift between P3-B and the promoted packet: P3-B uses W0
   `<=160 LOC`, W1 `<=520 LOC`, and W4 `<=220 LOC`, while SPEC/DISPATCH/P3-F
   use W0 `<=180`, W1 per-target caps `<=520 CSS / <=480 Sheets / <=460
   BBNF-self`, and W4 `<=120`.

## Verified Clean

- Wall cap vs redress cap is correctly separated for W0-W4.
- Rerun ceilings exist for manifest waves.
- W1 one-target redress discipline is explicit in SPEC/P3-B.
- W2 five-part cost table is present in SPEC and DISPATCH.
- Shortlist is exactly C1-C8, inside the <=8 cap.
- Base manifest is five waves, below the <=12 ceiling.

## Required Folds

1. Remove W1a/W1b split authority or promote it into the manifest with full caps
   and gates.
2. Normalize P3-B LOC budgets to match SPEC/DISPATCH/P3-F.
3. Mirror W1 one-target no-fallthrough in DISPATCH to reduce ambiguity.

## Residual Risk

DISPATCH can rely on SPEC for rerun details, but implementation-agent ambiguity
drops if no-fallthrough is mirrored in DISPATCH.
