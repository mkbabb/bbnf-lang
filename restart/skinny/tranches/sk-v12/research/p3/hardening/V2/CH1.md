# SK-V12 S-P3 V2 CH1 - Correctness

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Lens: CH1 correctness.
Scope: read-only adversarial review of SK-V12 S-P3 V2 packet.

## Verdict

REVISE.

## Findings

1. W2 measured rejection is legal in SPEC/DISPATCH but P3-C W4 close cannot
   close it. SPEC Section 0.1 allows W2 to record a measured reject, and
   DISPATCH routes the remaining family to Alpha; P3-C close currently admits
   only W1+W2 admit or W1 baseline BLOCKED.
2. W3 routed-block semantics are inconsistent. SPEC and DISPATCH allow W3 to
   record no behavior dispatch with no source/RESULTS movement, but P3-C W3
   requires either W1+W2 admitted or W1 baseline BLOCKED and then an exactly
   selected residual row with Track 1/Track 2 floors.
3. W1 split gates `G-W1a-GENERATOR-RUNTIME-UNBLOCK` and
   `G-W1b-GENERATED-NONJSON-BASELINE` appear in P3-C without dispatch authority
   in SPEC/DISPATCH.
4. Baseline Mbps naming is mechanically drifted: `baseline_mbps`,
   `baseline_track1_mbps`, `W1_baseline_mbps`, and
   `W1_baseline_track1_mbps` all name the same W1 generated Track 1 value.

## Required Folds

1. Add a W4 close form for W1 baseline admitted, W2 measured reject recorded,
   guard floors preserved, W3 routed/adjudicated, and close docs agreed.
2. Rewrite P3-C W3 as two exit forms: behavior dispatch with selected residual
   row/floors, or routed block with no source/RESULTS movement and explicit
   material-reopen failure.
3. Remove W1a/W1b from P3-C as legal gates, or promote them into SPEC/DISPATCH
   as real sub-waves with caps and gates.
4. Standardize on `baseline_mbps`, defined as W1 generated Track 1 Mbps.

## Residual Risk

Gate names, canonical non-JSON row ids, W1 concrete thresholds, and JSON guard
floor arithmetic are aligned. The remaining risk is close-path ambiguity after
honest reject or routed outcomes.
