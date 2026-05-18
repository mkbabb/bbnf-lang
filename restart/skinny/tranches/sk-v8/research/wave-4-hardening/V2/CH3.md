# SK-V8 W4 Hardening V2 CH3

Verdict: ACCEPT.

Confidence: 93%.

## Findings

1. The revised rejection is fail-closed. W4 records no source admission, no
   `skinny/RESULTS.md` update, no Lock 14 W4 allowance, and routes the failed
   scalar-parent fold through REDRESS 93.
2. Preblocked routes stay closed. REDRESS 93 explicitly excludes generated
   Track 1, runtime/codegen/BIR/directive/substrate movement, string
   materializer/fact work, cap-16, value-byte carry, source-hook, raw-f64, and
   mantissa routes, covering REDRESS 66-69, 72, 80, and 84.
3. Track 2 independence is not weakened by the final disposition because the
   patch is rejected and reverted. The saved patch touched only
   `skinny/crates/bbnf-bench/src/direct_struct.rs`; it did not call generated
   SinkOnly, generated typed helpers, generated Track 1, or shared parser code.
4. Residual direct rows are routed correctly: direct digest remains guard-plane
   evidence, not product proof; string/materializer misses stay under existing
   REDRESS families; numeric/control-path misses require later fresh hot-leaf
   evidence and a new checked gate.
5. The V1 digest-arithmetic concern is folded for a rejection disposition. If
   scalar parent folding is ever reopened, REDRESS 93 requires a W4/V9-aware
   checked gate, full-table maintain measurement, and independent Track 2
   digest-arithmetic backstop.

## Required Folds

None for CH3.
