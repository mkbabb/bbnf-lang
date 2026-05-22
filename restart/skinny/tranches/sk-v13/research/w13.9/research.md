# SK-V13 W13.9 Research - Canada Typed Product Surface

Date: 2026-05-22.
Scope: `json/canada/real_typed_struct/main`.
Disposition: select for plan.

## Candidate

W13.9 selects the Canada GeoJSON corpus as a generated typed product row.
The corpus shape is a single FeatureCollection-like document with top-level
`type` and `features`; each feature carries `type`, `properties.name`,
`geometry.type`, and polygon `coordinates` as rings of coordinate pairs.

## Material Differential

This is not REDRESS 80's one-row mantissa widening and not REDRESS 119/120's
direct digest fixpoint. The row is a strict generated product surface:
`CanadaDocument -> CanadaFeature -> CanadaProperties -> CanadaGeometry ->
Vec<Vec<Vec<f64>>>`, checked against independent serde Track 2, serde_json,
and sonic-rs typed outputs. The W13.1 f64 parity admit makes the numeric
coordinate surface eligible; W13.9 tests the full GeoJSON object product
rather than a count-only coordinate checksum or a direct hash.

## Owner Paths

- `skinny/xtask/src/real_typed_schema.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `restart/skinny/tranches/sk-v13/research/w13.9/`
- `skinny/REDRESS.md`
- `skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` only on admit.

## Gate

`G-W13.9-TYPED-CANADA` admits only if Track 1 generated typed output is
strict-equal by checksum to Track 2, serde_json, and sonic-rs strict typed,
and native Criterion shows Track 1 greater than same-run sonic-rs strict typed
by at least 1 Mbps. A miss records a measured REDRESS reject and preserves the
rejected implementation patch under `/tmp`.
