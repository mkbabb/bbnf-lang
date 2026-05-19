# SK-V10 W0 Redress - Telemetry Freeze

Pass: Wave Redress.
Cycle: W0.
Date: 2026-05-19.
Gate: `G-W0-TELEMETRY-FREEZE`.
Disposition: PASS.

## Patch

W0 updated the `gate-json --with-cost-facts --check-results` RESULTS snapshot
guard in `skinny/xtask/src/main.rs` from the stale pre-W1 38-row invariant to
the SK-V10 40-row opening manifest.

The change does not edit parser behavior, benchmark bodies, generated output,
`skinny/RESULTS.md`, or row dispositions.

## Evidence

Report/schema/run-id/metadata consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory
```

Result: PASS. Rendered report saved at
`/tmp/skv10-w0-gate-json-advisory.md`.

Cost-facts snapshot consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Result: PASS. JSON report saved at `/tmp/skv10-w0-cost-facts.json`.

## Gate Accounting

- The opening run identity remains
  `sk-v9-open:criterion-fnv64-a1e8a51ae806d386`.
- The 36-field report schema and 10-outcome enum remain unchanged.
- `RESULTS.md` keeps 40 manifest rows: 17 parse rows, 17 direct rows, and 6
  typed rows.
- No parse, direct, or typed row moved.
- The default `skinny/target/criterion` cache remains irrelevant to W0 because
  it was not the frozen W1-rendered SK-V9 capture.
