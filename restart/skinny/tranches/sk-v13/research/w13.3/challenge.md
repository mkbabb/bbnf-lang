# SK-V13 W13.3 CHALLENGE - Random Typed Product Surface

Wave: W13.3.
Plan under review: `restart/skinny/tranches/sk-v13/research/w13.3/plan.md`.
Disposition: ACCEPT.

## CH1 Correctness

ACCEPT. The selected product surface includes the root object, all 1,000
`result` records, and all friend records. The checksum must cover root fields,
user fields, friend fields, string bytes, booleans, and integers so a partial
payload or field rename drift is observable.

## CH2 Generality / Lock 14

ACCEPT. The implementation remains inside the JSON host/API typed product
surface and bench gate. No generic crate, directive, BIR variant,
`BackendShape`, or substrate API changes are planned. `lock14_baseline` must
authorize the W13.3 owner paths because generated typed output and gate/report
parents are touched.

## CH3 Regression / REDRESS

ACCEPT WITH CONDITION. Existing admitted typed rows must maintain their status,
including W13.1 `numbers` and W13.2 `unicode_basic`. The row cannot inherit
the older 1.10 slack contract; it admits only above same-run sonic strict + 1
Mbps. REDRESS must record a measured admit or measured reject with Track 1 /
Track 2 / sonic evidence.

## CH4 Cost

ACCEPT. The expected generated growth is one root and three nested struct
parsers. If regeneration expands unrelated roots or exceeds the W13 surface
budget, redress blocks until the growth is traced.

## CH5 Hidden Coupling

ACCEPT WITH CONDITION. Track 2 may be the serde typed oracle, but the gate must
state that explicitly and reject Track 1/Track 2 coupling. Sonic is the strict
SOTA comparator. A root-only parser that ignores `result`, or a user parser
that skips `friends`, is REJECT.

## CH6 Anti-Paper-Close

ACCEPT. A synthetic parser test, report-only row, RESULTS-only edit, direct
digest reuse, or partial typed root is REJECT. The same-wave consumer is the
`bbnf-bench` `real_typed_struct` workload for `random`, measured through
Criterion and consumed by `gate-json`.

## Accepted Redress Contract

Proceed to redress if and only if:

- `cargo xtask check-real-typed` passes after regeneration.
- Full-fixture typed parity passes for generated Track 1, serde Track 2, serde
  sidecar, and sonic.
- Native Criterion captures the four `random/real_typed_struct` lanes.
- The W13.3 companion report validates `Track 1 > sonic strict + 1 Mbps`,
  report artifact hash, strict equality provenance, and RESULTS/rolling
  alignment.
- REDRESS records the measured admit or reject.
