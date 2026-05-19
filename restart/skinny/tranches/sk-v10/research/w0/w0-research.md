# SK-V10 W0 Research - Telemetry Freeze

Pass: Wave Research.
Cycle: W0.
Date: 2026-05-19.
Scope: read-only verification of the SK-V10 opening telemetry surface.

## Inputs

- SPEC Section 3 authorizes W0 as a gate-only telemetry freeze.
- Opening authority remains the W1-rendered SK-V9 snapshot in
  `skinny/RESULTS.md` with run id
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
- The opening row surface is 17 `parse_only` rows, 17 `direct_to_struct`
  rows, and 6 `real_typed_struct` rows: 40 manifest rows total.
- Owner paths are the report/gate/metadata surfaces plus
  `skinny/RESULTS.md` and `skinny/REDRESS.md`; no behavior source is in scope.

## Findings

The report-rendering gate is already coherent when pointed at the frozen
criterion capture that produced the committed RESULTS file:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory
```

This passed and wrote the rendered report to
`/tmp/skv10-w0-gate-json-advisory.md`.

The default `skinny/target/criterion` cache is stale and was rejected before
row classification because its metadata was not a native W0 capture. That is a
cache-selection issue, not a RESULTS disposition change.

The cost-facts snapshot guard found the W0-owned stale invariant:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

The command failed with:

```text
RESULTS.md SK-V9 manifest row count moved from 38 to 40
```

The failure is valid: the SK-V10 opening surface inherits the W1 typed-row
admissions and therefore has 40 manifest rows. W0 should update the snapshot
guard to accept 40 rows while preserving the existing run-id grammar,
nonproducer marker, and uniform-run-id checks.

## Recommendation

Proceed to W0 plan with one narrow gate/report edit:

- Change the `gate-json --with-cost-facts --check-results` RESULTS row-count
  invariant from 38 to 40 and update its diagnostic text to name the SK-V10
  opening surface.
- Do not mint a new run id, rerender RESULTS, or move any row disposition.
- Re-run both W0 gate commands against `target/skv9-w1/criterion`.
