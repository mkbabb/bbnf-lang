# SK-V8 W4 Hardening V3 CH3

Verdict: ACCEPT.

Confidence: 94%.

## Findings

1. No REDRESS 66-69/72/80/84 reopen found. W4's route screen keeps source-hook
   folding, parser-owned scratch, byte-output unescape, semantic string facts,
   direct cap-16, stale mantissa widening, and value-byte/control compaction
   closed; SPEC repeats those W4 preblocks.
2. W4 is fail-closed. REDRESS 93 records the scalar-parent fold as rejected,
   says the patch added none of the preblocked route families, leaves no source
   admitted, adds no Lock 14 allowance, and keeps `skinny/RESULTS.md`
   unchanged.
3. Source sanity matches the rejection: current `direct_struct.rs` hand Track 2
   still folds `self.value()?` child digests in object/array paths, so the
   rejected scalar-parent fold is not active. `git diff --stat` and
   `git diff --exit-code HEAD -- skinny/RESULTS.md` were clean.
4. Direct digest remains guard-only. SPEC classifies `direct_to_struct` as
   digest guard plane, W4 tasks forbid presenting digest as product proof, and
   REDRESS 93 routes residual misses to later direct-output-contract or
   control-path work.
5. Future reopen requirements are explicit: fresh `SK-V8-open` baseline,
   strict comparator, named thresholds, full-table maintain, same-wave
   consumer, revert/REDRESS, and hard cap are required before any reopen.
   Scalar-parent folding additionally requires a W4/V9-aware checked gate,
   full-table maintain measurement, and independent Track 2 digest-arithmetic
   backstop.

## Required Folds

None.
