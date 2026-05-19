# SK-V10 W1 Redress - Direct Output Contract

Pass: Wave Redress.
Cycle: W1.
Date: 2026-05-19.
Gate: `G-W1-DIRECT-CONTRACT`.
Disposition: PASS.

## Patch

W1 added a direct row movement contract to
`skinny/crates/bbnf-bench/src/report.rs`.

The report validator now keeps unchanged W0 baseline rows under the inherited
W0 checks, but routes any baseline `N-direct / NO-GO` direct row movement
through a stricter direct contract predicate. A direct row can move only as
`A / GO` with digest output plane, strict row semantics, measured-row
validation, independent Track 2 status, non-gate-only consumer, REDRESS
provenance, a non-SK-V9-open wave id, and same-run native direct comparator
sources.

`skinny/RESULTS.md` was not edited.

## Evidence

Targeted contract tests:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench direct_contract -- --nocapture
```

Result: PASS, 2 tests.

Report validation tests:

```text
cargo test --manifest-path skinny/Cargo.toml -p bbnf-bench report::tests -- --nocapture
```

Result: PASS, 17 tests.

Report/schema/run-id/metadata consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --advisory
```

Result: PASS. Rendered report saved at
`/tmp/skv10-w1-gate-json-advisory.md`.

Cost-facts snapshot consumer:

```text
CRITERION_HOME=target/skv9-w1/criterion \
RUSTFLAGS="-C target-cpu=native" \
cargo xtask gate-json --with-cost-facts --check-results
```

Result: PASS. JSON report saved at `/tmp/skv10-w1-cost-facts.json`.

## Gate Accounting

- No `RESULTS.md` row moved.
- Direct digest evidence remains distinct from typed product proof.
- W2 is now authorized to re-evaluate direct rows, but any direct row movement
  must satisfy the W1 contract and the W2 numeric floors.
- The accidental generated-file formatting from the redress workspace was
  reverted before validation; Lock 14 was clean for the passing gate runs.
