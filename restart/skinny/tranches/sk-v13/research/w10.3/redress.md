# SK-V13 W10.3 Redress - CSS Nested Layout

## Scope

W10.3 admits the CSS L4 nested/layout generated row:

- `css_l4/nested_layout/direct_to_struct/main`
- covered feature rows: `nested_rules`, `logical_properties`, `grid`,
  `flexbox`, `typed_property_groups`
- gate: `G-W10-3-CSS-NESTED-LAYOUT`
- report:
  `restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`

## Implementation

The wave adds the `css_l4_nested_layout` grammar profile in codegen and
runtime, wires the benchmark/report/gate consumer, and records the feature rows
in `skinny/RESULTS.md` plus `restart/skinny/ROLLING-SOTA-DELTA.md`.

The generated parser emits schema `css-l4-nested-layout-facts-v1` over a
canonical fixture that exercises a nested child rule, logical properties, grid,
flexbox, and typed property value groups. The same-wave consumer is the W10.3
companion report gate, which compares Track 1, the golden oracle, and the
lightningcss same-plane source sidecar. The lightningcss sidecar validates this
release's typed AST projection for nested children while the fact stream remains
same-plane strict.

## Measurement

Criterion with `RUSTFLAGS="-C target-cpu=native"` records:

| metric | value |
|---|---:|
| Track 1 | 52233.53887747471 Mbps |
| golden oracle | 2503.5940289321406 Mbps |
| lightningcss | 421.16026478431274 Mbps |
| threshold | 422.16026478431274 Mbps |
| margin | 51811.3786126904 Mbps |

Strict equality: `pass:track1=golden=lightningcss`.

Fact-stream SHA-256:
`20296aab67b474ad3f333645378ddbf7acd7923cb71fa288b17ef93bb1ca4efb`.

Generated size guard: `generated_loc=111`, `generated_module_bytes=5932`,
`pass:generated_loc<=1050`.

## Verification

- `cargo test -p runtime css_l4_nested_layout`
- `cargo test -p codegen css_l4_nested_layout --lib`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib nested_layout`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson_css_l4_w10_3`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-nested-layout-report ../restart/skinny/tranches/sk-v13/research/w10.3/skv13-W10.3-css-l4-nested-layout.json`

## Disposition

PASS-ADMIT-CANDIDATE. `REDRESS-135` records the admission, and the rolling CSS
SOTA delta now marks `nested_rules`, `logical_properties`, `grid`, `flexbox`,
and `typed_property_groups` as ADMITTED.

Routed remainder: no SIMD/ASM claim lands in W10.3. Lock 16 remains
`n/a:no_simd_or_asm_claim`. The remaining campaign surface moves to the JSON
row reopening, union, decision-engine, and zero-orphan waves.
