# SK-V13 W3 CHALLENGE - CSS Declaration-Value Expansion

Wave: W3. Phase: CHALLENGE. Date: 2026-05-21.

Disposition: ACCEPT.

## CH1 Correctness

The plan selects a new grouped row instead of mutating the admitted SK-V12 row.
That keeps the old declaration-values evidence stable while W3 carries its own
fixture, fact stream, report schema, and Criterion lanes. The fixture exercises
the SPEC-named families and remains strict-mode parseable by lightningcss.

Escaped-token facts are normalized lexeme facts, not source-span facts. This
avoids the cssparser normalized-token span hazard identified in research.

## CH2 Generality / Lock 14

The plan keeps CSS feature semantics in a CSS-specific generated profile and
runtime module. Generic edits are limited to profile registration, runtime
exports, report/gate plumbing, and Lock 14 owner-path inventory. The generic
forbidden-token scan must not be weakened.

## CH3 Regression / REDRESS

The plan explicitly maintains the SK-V12 declaration-values row, W2
stylesheet/selectors row, and JSON guard rows through the same gate invocation.
No W3 feature row may remain admitted unless the grouped W3 row passes.

## CH4 Cost

The new profile is larger than W2 but still bounded: one fixture/report/artifact
set, one runtime profile, one report validator, and one Criterion group. The
plan avoids a full CSS AST implementation and avoids generic string/escape
policy movement, keeping W3 within the Section 6 budget.

## CH5 Hidden Coupling

The grouped W3 row and the five rolling feature rows are coupled by the report.
`RESULTS.md` may render both, but `ROLLING-SOTA-DELTA.md` remains feature-row
close accounting. The gate must consume this mapping.

## CH6 Anti-Paper-Close

Report-only throughput is insufficient. W3 requires Criterion lane reread,
retained Track 1/cssparser/lightningcss artifacts, generated-source checksum,
same-plane strict equality, and rolling-delta consumption in one gate
invocation.

## Required Redress Checks

- `cargo test -p runtime css_l4_declaration_values_extended`
- `cargo test -p codegen css_l4_declaration_values_extended`
- `cargo test -p bbnf-bench --lib`
- `cargo test -p bbnf-bench --bin gate`
- `cargo test -p xtask`
- W3 Criterion capture for the `nonjson_css_l4_w3` declaration-values-extended
  lanes
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-comparator-oracle-report ../restart/skinny/tranches/sk-v13/research/wave-1-css-comparator-oracle.json --skv13-css-stylesheet-selectors-report ../restart/skinny/tranches/sk-v13/research/w2/skv13-W2-css-l4-stylesheet-selectors.json --skv13-css-declaration-values-extended-report ../restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json`
