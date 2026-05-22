# SK-V13 W1 Redress - CSS Comparator/Oracle Harness Expansion

Wave: W1. Phase: Redress. Date: 2026-05-21.

## Disposition

`G-W1-CSS-COMPARATOR-ORACLE`: PASS.

W1 landed a gate-consumed SK-V13 CSS comparator/oracle matrix without admitting
new CSS behavior. The matrix records all 24 SK-V13 CSS parity features:

- `declaration_values` is the only measured/admitted row and is maintained via
  the existing Criterion-backed SK-V12 lightningcss/cssparser SOTA proof.
- the remaining 23 features are explicit `OPEN` rows with
  `absent_until_planned_wave`; none carries fake Mbps or admission status.

## Source Changes

- `bbnf-bench::report` now owns `sk-v13-css-comparator-oracle-v1`, validates
  the 24-feature matrix, rejects `PARTIAL`, stale totals, admitted absent rows,
  and unknown producer-only fields.
- `bbnf-bench` gate accepts `--skv13-css-comparator-oracle-report`, requires it
  to run with `--check-results`, validates the W1 matrix, then validates the
  referenced SK-V12 CSS SOTA report against Criterion lanes and retained
  artifacts.
- `xtask gate-json` passes the W1 companion flag through and now reads
  `threshold_mbps` only from the `lightningcss_strict[...]` comparator segment,
  checking that it equals `lightningcss_mbps + 1.0`.
- `wave-1-css-comparator-oracle.json` records the W1 matrix consumed by the
  gate.

## Verification

- `cargo test -p bbnf-bench --lib` — PASS, 97 tests.
- `cargo test -p bbnf-bench --bin gate` — PASS, 23 tests.
- `cargo test -p xtask` — PASS, 3 tests.
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-comparator-oracle-report ../restart/skinny/tranches/sk-v13/research/wave-1-css-comparator-oracle.json` — PASS.

The full gate printed `G-W1-CSS-COMPARATOR-ORACLE PASS ... feature_rows=24`
and then completed the JSON guard check successfully.

## Remainder

W1 intentionally does not admit stylesheet/selectors, declaration extensions,
visual functions, or other CSS features. W2/W3/W4/W10.N must populate the
same-plane facts named by the W1 plan before any of those rows can move out of
`OPEN`.
