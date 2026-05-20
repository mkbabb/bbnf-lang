# SK-V12 S-P3 V3 CH1 - Correctness

Pass: S-P3 Synthesis-Plan.
Cycle: V3.
Date: 2026-05-20.
Lens: CH1 correctness.
Scope: read-only adversarial review of SK-V12 S-P3 V3 packet.

## Verdict

REVISE.

## Findings

1. P3-F still weakens the W2 oracle/Track 2 threshold. The active V3 gates
   require W2 oracle/Track 2 >= 1 Mbps, independent, and strict-equal, but
   P3-F says only "finite, independent, and equal".
2. P3-B's W3 entry gate under-carries the W2 measured-reject path. P3-C and
   SPEC permit W1 admitted + W2 measured reject before W3 routed/adjudicated
   close, but P3-B names only W1+W2 admission or measured block.
3. P3-C close gate says "one of two forms" while defining admit, reject, and
   block forms.

## Required Folds

1. Change P3-F W2 gate language to require oracle/Track 2 >= 1 Mbps,
   independent, and strict-equal.
2. Add the W1-admitted/W2-measured-reject route to P3-B's W3 entry/topology
   language.
3. Change P3-C W4 close wording from two forms to three forms.

## Residual Risk

Low after these folds. The promoted SPEC and DISPATCH are mostly coherent; the
remaining defects are secondary-artifact drift.
