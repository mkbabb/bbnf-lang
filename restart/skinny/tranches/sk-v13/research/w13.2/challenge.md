# SK-V13 W13.2 CHALLENGE - Unicode Basic Typed Product Surface

Wave: W13.2.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.2/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT. The selected product surface is a regular record array over the full
`unicode_basic` fixture. The plan requires generated Track 1, independent typed
Track 2, serde, and sonic to parse the same bytes and match checksums before a
row can admit. The checksum must include ids, lengths, script/text bytes, and
tag bytes so UTF-8 or field-order drift is observable.

## CH2 Generality / Lock 14

ACCEPT. The implementation remains inside the JSON host/API typed product
surface and bench gate. No generic crate, directive, BIR variant,
`BackendShape`, or substrate API changes are planned. `lock14_baseline` must
authorize the W13.2 owner paths because generated typed output and gate/report
parents are touched.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted typed rows must maintain their
existing status, including W13.1 `numbers`. The row cannot inherit the older
1.10 slack contract; it admits only above same-run sonic strict + 1 Mbps.
REDRESS must record a measured admit or measured reject with Track 1 / Track 2
/ sonic evidence.

## CH4 Cost

ACCEPT. The expected generated growth is one root, one record type, and vector
helpers. If regeneration expands unrelated roots or exceeds the W13 surface
budget, redress blocks until the growth is traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be the serde typed oracle, but the gate must
state that explicitly and reject Track 1/Track 2 coupling. Sonic is the strict
SOTA comparator. The existing `unicode_basic/direct_to_struct` row is not typed
proof.

## CH6 Anti-Paper-Close

ACCEPT. A synthetic parser test, report-only row, RESULTS-only edit, direct
digest reuse, or old direct-row admission is REJECT. The same-wave consumer is
the `bbnf-bench` `real_typed_struct` workload for `unicode_basic`, measured
through Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `unicode_basic/real_typed_struct` lanes.
- The W13.2 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
