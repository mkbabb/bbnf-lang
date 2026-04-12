# Tranche AO — Structural Dispatch + Scanner Generalization + Global Optimization

Post-AN audit reveals all optimization systems (CSP, e-graph, recognizer
mining, dispatch tables, cost model) are properly wired. The 27-40%
string-heavy gap to sonic-rs is from the per-byte dispatch model, not
from slow scanners. The structural scanner exists in parse-that but is
UNUSED by codegen.

## AO.0 — Structural Index Integration (P0)

Pre-scan input for structural bytes, filter quote parity, wire index
into dispatch emitter. Whitespace becomes implicit.

## AO.1 — Padded Buffer Mode

Eliminate SIMD boundary checks via input padding.

## AO.2 — parse-that Generalization (from AN)

Delete CSS re-exports, consolidate number scanners, parameterize
WS/quote, delete SpanParser wrappers, dedup nibble-LUT SIMD.

## AO.3 — SIMD Widening + Numeric SIMD

32-byte SIMD chunks (AVX2), SIMD digit-to-integer.

## AO.4 — Cost Model Calibration + Global CSP

Grid sweep CostWeights, global CSP solve for CSS L4.

## AO.5 — Correctness + Self-Hosting

CSS L4 tailwind fix, bootstrap audit, branch frequency ordering.
