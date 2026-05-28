# CH2 Generality — SK-V15 Alpha V1

Date: 2026-05-27.

## Verdict

REVISE, folded.

The packet blocks the principal shortcuts, but the first draft left native
platform and grammar-neutral command requirements too implicit.

## Folded Fixes

- `SYNTHESIS.md`, `HANDOFF.md`, `alpha-B`, and `alpha-F` now bind admission
  and SIMD evidence to Apple M5 Max / aarch64. x86 and AVX-512 rows are
  diagnostics only and cannot anchor admission.
- `alpha-E` now uses generic grammar-id regen/check consumers and requires a
  non-JSON/non-CSS smoke target from the existing template cohort.
- Codegen neutrality now explicitly forbids per-grammar regen enum/match
  fanout in addition to JSON/CSS runtime splits and CSS profile matches.

## Residual Risk

S-P3 must still select the exact generic commands and prove them at HEAD.
