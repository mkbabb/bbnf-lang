# SK-V12 S-P3 V2 CH6 - Anti-Paper-Close

Pass: S-P3 Synthesis-Plan.
Cycle: V2.
Date: 2026-05-20.
Lens: CH6 anti-paper-close.
Scope: read-only adversarial review of SK-V12 S-P3 V2 packet.

## Verdict

REVISE.

## Findings

1. W1a split can close on compile/gate plumbing without a measured row. P3-C's
   split path says W1a may close by compiling the generated runtime path and
   proving gate/report consumption, while W1b owns throughput later.
2. P3-D still allows `track2_or_oracle_mbps` to be `n/a` for a pure equality
   oracle admitted by SPEC. That conflicts with W1/W2 gates requiring measured
   independent oracle/Track 2 throughput.

## Required Folds

1. Remove W1a/W1b split authority or make W1a an explicit wave whose exit gate
   includes generated Track 1 benchmark, independent oracle/Track 2 evidence,
   strict equality, sample count >= 30, same-wave gate consumption, and REDRESS
   failure handling.
2. Tighten P3-D so W1/W2 admission always requires measured
   `track2_or_oracle_mbps >= 1`. Permit `n/a` only for non-admitting support
   reports.
3. Ensure split/routed block paths record REDRESS evidence before W4/G-Alpha.

## Residual Risk

Low after these folds. Main SPEC and DISPATCH already reject docs-only G-Alpha
and future-phase promises; secondary P3 artifacts need the same clarity.
