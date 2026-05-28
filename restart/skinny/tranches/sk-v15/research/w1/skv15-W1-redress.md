# SK-V15 W1 Redress: CSS Broadcast Admission Hard Demotion

Date: 2026-05-28.
Plan: `restart/skinny/tranches/sk-v15/research/w1/skv15-W1-plan.md`.
Status: REDRESS-ADMITTED.

## Implementation

W1 closes `DEP-W1-CSS-BROADCAST` by making the W8R CSS full-parse tuple
diagnostic-only across every live admission surface.

Changes:

- `restart/skinny/ROLLING-SOTA-DELTA.md`: 24 CSS L4 rows remain present with
  the retained W8R numeric margins, but their `tranche_admitted` status is now
  `OPEN`.
- `skinny/xtask/src/main.rs`: rolling-delta validation now rejects
  audit-falsified CSS diagnostic evidence marked `ADMITTED`, and the legacy
  SK-V14 sustained validator rejects W8R CSS rows as admission authority.
- `skinny/crates/bbnf-bench/src/report.rs`: report-side legacy sustained
  validation rejects W8R CSS rows as admission authority.
- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`: the historical W8 harness now
  reports `W8Disposition::Diagnostic` and `admitted_rows=0` when the old
  numeric predicate is true.
- `skinny/REDRESS.md`: REDRESS-215 records the SK-V15 W1 supersession:
  retained W8R evidence is diagnostic broadcast evidence only; typed CSS proof
  remains routed to W5/W6.

No CSS providers, generated CSS runtime modules, codegen profile rosters,
`regen_css`, or root CSS runtime files were deleted or retired.

## Evidence

Executed from the skinny workspace unless noted:

```sh
cargo fmt --manifest-path skinny/Cargo.toml --all --check
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv13_rolling_delta
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p xtask skv15_w0
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench css_l4_w8
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- gate-json --check-results
RUSTFLAGS="-C target-cpu=native" cargo test --profile ax-iter -p bbnf-bench skv15_w0
```

Results:

- `skv13_rolling_delta`: 1 passed, 0 failed. The test now includes the negative
  assertion that audit-falsified CSS diagnostic rows must not remain
  `ADMITTED`.
- `xtask skv15_w0`: 5 passed, 0 failed.
- `bbnf-bench css_l4_w8`: 1 passed, 0 failed. The W8 full-parse path is
  diagnostic with zero admitted rows.
- `gate-json --check-results`: completed successfully against committed
  RESULTS plus rolling delta.
- `bbnf-bench skv15_w0`: 2 passed, 0 failed.

## Disposition

REDRESS-ADMITTED. No CSS row can close from the W8R broadcast tuple in RESULTS,
rolling delta, legacy sustained validators, or the W8 harness. The retained
W8R profile remains diagnostic evidence only. W2 may now dispatch against
Lock 14 / Lock 16 gate restoration.
