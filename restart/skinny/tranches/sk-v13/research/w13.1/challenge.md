# SK-V13 W13.1 CHALLENGE - Numbers Typed Product Surface

Wave: W13.1.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.1/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT. The selected product surface is a top-level `Vec<f64>` over the full
`numbers` fixture. The plan requires generated Track 1, independent typed
Track 2, serde, and sonic to parse the same bytes and match checksums before a
row can admit. A numeric-bit checksum is appropriate because it falsifies
rounding/parsing drift instead of hiding it behind approximate equality.

## CH2 Generality / Lock 14

ACCEPT. The implementation stays in the JSON host/API typed schema and bench
gate surface. No generic-crate edit is planned, no JSON policy leaks into a
generic parser, and no new directive, BIR variant, `BackendShape`, or substrate
API is introduced. `lock14_baseline` must explicitly authorize the W13.1 owner
paths because generated typed output and report/gate parents are touched.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing typed admits must maintain their floors, and the
row cannot rely on the older typed slack contract. REDRESS must record either
strict admission above sonic + 1 Mbps or the measured failure class. The prior
W11 `numbers/direct_to_struct` admission is only historical context; it does
not prove this typed product row.

## CH4 Cost

ACCEPT. The row should add one generated root and one vector helper, well under
the W13 per-surface budget. Any unexpected generated-size expansion blocks
redress until traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. `track2_typed` currently delegates to serde, and
`serde_json_real_typed_struct` also uses serde. That is still independent from
generated Track 1, but the companion report must state the oracle model
explicitly: Track 2 is the independent serde typed oracle, sonic is the SOTA
comparator, and serde is the parity sidecar. The gate must reject Track 1 /
Track 2 coupling.

## CH6 Anti-Paper-Close

ACCEPT. The plan requires production Criterion lanes plus a companion
`gate-json` report. A report-only row, RESULTS-only edit, direct digest reuse,
or synthetic-test-only parser is REJECT. The same-wave consumer is the
`bbnf-bench` `real_typed_struct` workload for `numbers`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `numbers/real_typed_struct` lanes.
- The W13.1 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
