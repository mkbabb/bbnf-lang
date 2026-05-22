# SK-V13 W13.7 CHALLENGE - Y String Unicode Typed Product Surface

Wave: W13.7.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.7/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT WITH CONDITION. The checksum must fold all 2,200 decoded strings in
order so skipped elements, decode drift, or borrowed/owned representation
differences are observable.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays inside the JSON host/API typed product
surface and bench gate. It does not touch generic crates, directives, BIR,
`BackendShape`, substrate APIs, or grammar policy.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted W13 typed rows must remain gate-valid.
W13.7 must record same-run measured admit or measured reject; unicode codec
history is not sufficient evidence.

## CH4 Cost

ACCEPT. Expected generated growth is one root and one scalar string vector
helper. Unexpected broad generated growth blocks redress until traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be serde typed oracle and sonic is the
strict SOTA comparator. A direct digest row, parse-only result, unicode decoder
microbench, or hidden checksum sink is REJECT.

## CH6 Anti-Paper-Close

ACCEPT. Synthetic-only tests, RESULTS-only edits, reused unicode decode
measurements, or proof-only codec claims are REJECT. The same-wave consumer is
the `bbnf-bench` `real_typed_struct` workload for `y_string_unicode`, measured
through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `y_string_unicode/real_typed_struct`
  lanes.
- The W13.7 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
