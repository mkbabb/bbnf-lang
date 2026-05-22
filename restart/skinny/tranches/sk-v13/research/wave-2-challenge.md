# SK-V13 W2 CHALLENGE - CSS Stylesheet Root + Selectors

Wave: W2. Phase: CHALLENGE. Date: 2026-05-21.

Disposition: ACCEPT.

## CH1 Correctness

The fixture is strict-mode safe and contains one qualified rule with two
selector branches. It exercises the selector/root families W2 claims without
depending on recovery, nesting, functional pseudo arguments, or source-map
diagnostics. Byte-identical Track 1, golden, and lightningcss fact artifacts
are required before admission.

## CH2 Generality / Lock 14

The plan keeps selector semantics inside the CSS-specific generated profile and
runtime module. Generic edits are limited to runtime-profile registration and
runtime exports. The Lock 14 owner inventory must explicitly authorize the W2
profile paths; generic forbidden-token scans are not weakened.

## CH3 Regression / REDRESS

The plan requires declaration-values and JSON guard maintenance through the
same `gate-json --check-results` invocation. Partial selector/root feature
admissions are rejected unless the grouped W2 row passes.

## CH4 Cost

The row is intentionally small: one generated runtime module, one codegen
profile, one fixture/report/artifact set, and targeted gate extensions. The
work is within the W2 source/test budget if it avoids generalized CSS AST
extraction and does not refactor the existing declaration-values lane.

## CH5 Hidden Coupling

The grouped SPEC row and rolling feature rows are explicitly coupled by the W2
report. `RESULTS.md` may carry both the grouped row and the covered feature
rows; `ROLLING-SOTA-DELTA.md` remains feature-only. The gate must consume this
mapping rather than letting the grouped row paper-close five feature rows.

## CH6 Anti-Paper-Close

Report-only Mbps is insufficient. The accepted plan requires Criterion lane
reread, retained fact artifacts, current generated-source checksum, same-plane
lightningcss equality, and rolling-delta consumption in one gate invocation.

## Required Redress Checks

- `cargo test -p bbnf-bench --lib`
- `cargo test -p bbnf-bench --bin gate`
- `cargo test -p xtask`
- W2 Criterion capture for the `nonjson_css_l4` selector lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-comparator-oracle-report ../restart/skinny/tranches/sk-v13/research/wave-1-css-comparator-oracle.json --skv13-css-stylesheet-selectors-report ../restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json`
