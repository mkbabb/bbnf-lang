# SK-V13 W13.8 CHALLENGE - Unicode Escapes Typed Product Surface

Wave: W13.8.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.8/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT WITH CONDITION. The checksum must fold the `meta` envelope and every
`records[*].id` plus decoded `records[*].v` string. Borrowed/owned string
representation must not affect equality. Invalid escape handling remains
strict through the existing parser and sidecars.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays inside the JSON host/API typed product
surface and bench gate. It does not touch generic crates, directives, BIR,
`BackendShape`, substrate APIs, or grammar policy. The Lock 14 owner-path
allowance must be W13.8-specific.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted W13 typed rows must remain gate-valid.
W13.8 must record same-run measured admit or measured reject; prior unicode
codec or direct-row history is not sufficient evidence.

## CH4 Cost

ACCEPT. Expected generated growth is one root and three product types. If
generated LOC expands beyond that shape, redress blocks until the O(N) cause is
traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be serde typed oracle and sonic is the
strict SOTA comparator. A direct digest row, parse-only result, unicode decoder
microbench, skipped record fields, or hidden checksum sink is REJECT.

## CH6 Anti-Paper-Close

ACCEPT. Synthetic-only tests, RESULTS-only edits, reused unicode decode
measurements, or proof-only codec claims are REJECT. The same-wave consumer is
the `bbnf-bench` `real_typed_struct` workload for `unicode_escapes`, measured
through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2,
  serde_json sidecar, and sonic.
- Native Criterion captures the four `unicode_escapes/real_typed_struct`
  lanes.
- The W13.8 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
