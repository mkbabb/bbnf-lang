# SK-V13 W10.3 CHALLENGE - CSS Nested Layout Pack

Wave: W10.3. Phase: CHALLENGE. Date: 2026-05-22.

Disposition: ACCEPT.

## CH1 Correctness

The plan selects one grouped CSS-specific row,
`css_l4/nested_layout/direct_to_struct/main`, to close the five remaining CSS
feature rows: nested rules, logical properties, grid, flexbox, and typed
property groups. The fixture includes all five feature families and excludes
unrelated at-rule, selector, custom-property, and visual-function claims that
already have admitted rows.

The row must prove declaration order, nested-rule depth, parent-child
structure, property-family classification, and strict source spans. A parser
success flag without facts is insufficient.

## CH2 Generality / Lock 14

The plan keeps the implementation inside a CSS-specific generated profile and
runtime module. Generic edits are limited to profile registration, runtime
exports, report/gate plumbing, lock14 owner inventory, and xtask passthrough.
No generic grammar policy, JSON string/number policy, public substrate API,
directive, BIR variant, `BackendShape`, SIMD, or x86 change is allowed.

## CH3 Regression / REDRESS

W10.3 must maintain SK-V12 declaration-values, W2 stylesheet/selectors, W3
declaration-values-extended, W4 visual-functions, W10.1 at-rules/media, W10.2
vendor/custom, and JSON guard rows through the companion gate invocation. A
feature-row admission without the grouped W10.3 row and retained gate artifacts
is a reject.

REDRESS-112, REDRESS-113, and REDRESS-123 through REDRESS-127 remain
`GATE-FEED` history rather than close authority for full CSS parity. REDRESS
28/33, 50-55, 60-72, 82-84, 88/89, and 126 remain blocked if redress tries to
reuse string, escape, SIMD, substrate, or orphan-kernel routes instead of the
selected generated CSS row.

## CH4 Cost

Packing the five remaining rows is accepted because they share one stylesheet
fixture, one property-family fact stream, one report schema, and one Criterion
group. Splitting them into five subwaves would duplicate support plumbing
without increasing semantic coverage. The cap remains bounded by one generated
runtime profile and one source-sidecar comparator.

## CH5 Hidden Coupling

The grouped row and the five rolling feature rows are coupled by
`covered_feature_rows`. Gate validation must reject stale mappings and must not
silently admit already-covered selector, at-rule, declaration-value, visual, or
vendor/custom rows.

The row name `nested_layout` is intentionally narrower than full layout engine
semantics: it proves CSS parser parity and typed property-family facts, not
layout computation.

## CH6 Anti-Paper-Close

The lightningcss sidecar must inspect typed style/nesting/declaration presence
before returning the same-plane fact stream. The gate must reread Criterion
lanes, byte-compare Track 1/oracle/lightningcss retained fact streams, verify
fact-stream SHA-256, and consume rolling-delta updates in the same wave.

## Required Redress Checks

- `cargo test -p runtime css_l4_nested_layout`
- `cargo test -p codegen css_l4_nested_layout --lib`
- `cargo test -p bbnf-bench --lib nested_layout`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- W10.3 Criterion capture for the `nonjson_css_l4_w10_3` lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-nested-layout-report ../restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`
