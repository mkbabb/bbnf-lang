# SK-V13 W13.6 CHALLENGE - Unicode Mixed Typed Product Surface

Wave: W13.6.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.6/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT WITH CONDITION. The selected product must cover root metadata,
metadata classes, and every record field: `id`, `type`, `value`, and `n`. The
checksum must fold all modeled fields and preserve decoded string equality
against Track 2, serde, and sonic.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays inside the JSON host/API typed product
surface and bench gate. It touches generated typed output and W13 gate/report
parents, but no generic crate, directive, BIR variant, `BackendShape`,
substrate API, or grammar policy surface.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted W13 typed rows (`numbers`,
`unicode_basic`, `random`, `instruments`) must remain gate-valid. REDRESS
history around unicode string kernels is not a substitute for this row; W13.6
must record a same-run measured admit or measured reject.

## CH4 Cost

ACCEPT. Expected generated growth is one root plus two nested product parsers.
If regeneration expands unrelated roots or creates unexpected O(N) generated
growth, redress blocks until the growth is traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be the serde typed oracle, but sonic remains
the strict SOTA comparator. A direct digest result, parse-only result,
unicode-decoder proof, hidden typed sink, or parser that drops modeled fields is
REJECT.

## CH6 Anti-Paper-Close

ACCEPT. A synthetic-only test, report-only row, RESULTS-only edit, reused
unicode codec number, or partial product root is REJECT. The same-wave consumer
is the `bbnf-bench` `real_typed_struct` workload for `unicode_mixed`, measured
through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `unicode_mixed/real_typed_struct` lanes.
- The W13.6 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
