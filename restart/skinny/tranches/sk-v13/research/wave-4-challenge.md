# SK-V13 W4 CHALLENGE - CSS Visual Functions Pack

Wave: W4. Phase: CHALLENGE. Date: 2026-05-22.

Disposition: ACCEPT.

## CH1 Correctness

The plan selects declaration-value visual functions rather than block-level
at-rules or nesting. This keeps W4 inside a fact-stream shape already exercised
by W3 while moving four open CSS parity rows. The fixture uses ordinary CSS
visual functions accepted by lightningcss in strict mode.

## CH2 Generality / Lock 14

The plan keeps visual-function semantics in a CSS-specific generated profile
and runtime module. Generic edits are limited to profile registration, runtime
exports, report/gate plumbing, lock14 owner-path inventory, and xtask
passthrough. The generic forbidden-token scan must remain intact.

## CH3 Regression / REDRESS

The plan explicitly maintains SK-V12 declaration-values, W2
stylesheet/selectors, W3 declaration-values-extended, and JSON guard rows
through the same gate invocation. No W4 feature row may remain admitted unless
the grouped W4 row passes strict equality and the lightningcss + 1 threshold.

## CH4 Cost

The selected pack is bounded: one fixture/report/artifact set, one runtime
profile, one report validator, and one Criterion group. At-rule, nesting, and
taxonomy rows are routed because they need a distinct block/property evidence
shape and would risk overflowing W4 if folded into the visual pack.

## CH5 Hidden Coupling

The grouped W4 row and four rolling feature rows are coupled by the report.
`RESULTS.md` may render both, but `ROLLING-SOTA-DELTA.md` remains feature-row
close accounting. The gate must consume this mapping and reject stale or
duplicated feature coverage.

## CH6 Anti-Paper-Close

Report-only throughput is insufficient. W4 requires Criterion lane reread,
retained Track 1/oracle/lightningcss artifacts, generated-source checksum,
same-plane strict equality, and rolling-delta consumption in one gate
invocation.

## Required Redress Checks

- `cargo test -p runtime css_l4_visual_functions`
- `cargo test -p codegen css_l4_visual_functions`
- `cargo test -p bbnf-bench --lib visual_functions`
- `cargo test -p bbnf-bench --bin gate skv13_css_visual_functions`
- `cargo test -p xtask skv13_css_visual_functions`
- W4 Criterion capture for the `nonjson_css_l4_w4` visual-functions lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-comparator-oracle-report ../restart/skinny/tranches/sk-v13/research/wave-1-css-comparator-oracle.json --skv13-css-stylesheet-selectors-report ../restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json --skv13-css-declaration-values-extended-report ../restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json --skv13-css-visual-functions-report ../restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json`
