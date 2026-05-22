# Rolling SOTA Delta

schema_version: sk-v13-rolling-sota-delta-v1
run_id: SK-V13-open
g_omega_status: signed
consumer_gate: cargo xtask gate-json --check-results
source_results: skinny/RESULTS.md
source_commit: 76f556124+w13.3-redress

## JSON Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| json/twitter/parse_only/main | parse_only | 16412.00 | 21018.00 | -4606.00 | OPEN |
| json/twitter/direct_to_struct/main | direct_to_struct | 11835.00 | 15071.00 | -3236.00 | OPEN |
| json/twitter/real_typed_struct/main | real_typed_struct | 18418.00 | 15608.00 | 2810.00 | ADMITTED |
| json/citm_catalog/parse_only/main | parse_only | 30893.00 | 25668.00 | 5225.00 | OPEN |
| json/citm_catalog/direct_to_struct/main | direct_to_struct | 21643.00 | 19693.00 | 1950.00 | ADMITTED |
| json/citm_catalog/real_typed_struct/main | real_typed_struct | 35263.00 | 21883.00 | 13380.00 | ADMITTED |
| json/canada/parse_only/main | parse_only | 17357.00 | 13973.00 | 3384.00 | OPEN |
| json/canada/direct_to_struct/main | direct_to_struct | 10607.00 | 12189.00 | -1582.00 | OPEN |
| json/canada/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/apache_builds/parse_only/main | parse_only | 12919.00 | 17575.00 | -4656.00 | OPEN |
| json/apache_builds/direct_to_struct/main | direct_to_struct | 11400.00 | 11171.00 | 229.00 | ADMITTED |
| json/apache_builds/real_typed_struct/main | real_typed_struct | 8744.00 | 8309.00 | 435.00 | ADMITTED |
| json/github_events/parse_only/main | parse_only | 15585.00 | 21959.00 | -6374.00 | OPEN |
| json/github_events/direct_to_struct/main | direct_to_struct | 12546.00 | 16293.00 | -3747.00 | OPEN |
| json/github_events/real_typed_struct/main | real_typed_struct | 12980.00 | 12722.00 | 258.00 | ADMITTED |
| json/update_center/parse_only/main | parse_only | 11810.00 | 19850.00 | -8040.00 | OPEN |
| json/update_center/direct_to_struct/main | direct_to_struct | 8595.00 | 11238.00 | -2643.00 | OPEN |
| json/update_center/real_typed_struct/main | real_typed_struct | 12145.00 | 12889.00 | -744.00 | OPEN |
| json/mesh/parse_only/main | parse_only | 13444.00 | 11753.00 | 1691.00 | OPEN |
| json/mesh/direct_to_struct/main | direct_to_struct | 9631.00 | 9582.00 | 49.00 | ADMITTED |
| json/mesh/real_typed_struct/main | real_typed_struct | 9788.00 | 9110.00 | 678.00 | ADMITTED |
| json/random/parse_only/main | parse_only | 9935.00 | 15679.00 | -5744.00 | OPEN |
| json/random/direct_to_struct/main | direct_to_struct | 7894.00 | 8788.00 | -894.00 | OPEN |
| json/random/real_typed_struct/main | real_typed_struct | 8559.00 | 7261.00 | 1298.00 | ADMITTED |
| json/gsoc-2018/parse_only/main | parse_only | 23333.00 | 49847.00 | -26514.00 | OPEN |
| json/gsoc-2018/direct_to_struct/main | direct_to_struct | 15318.00 | 23900.00 | -8582.00 | OPEN |
| json/gsoc-2018/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/marine_ik/parse_only/main | parse_only | 13049.00 | 10162.00 | 2887.00 | OPEN |
| json/marine_ik/direct_to_struct/main | direct_to_struct | 9566.00 | 8494.00 | 1072.00 | ADMITTED |
| json/marine_ik/real_typed_struct/main | real_typed_struct | 11819.00 | 9309.00 | 2510.00 | ADMITTED |
| json/instruments/parse_only/main | parse_only | 17312.00 | 19615.00 | -2303.00 | OPEN |
| json/instruments/direct_to_struct/main | direct_to_struct | 12200.00 | 12786.00 | -586.00 | OPEN |
| json/instruments/real_typed_struct/main | real_typed_struct | 21026.00 | 15996.00 | 5030.00 | ADMITTED |
| json/numbers/parse_only/main | parse_only | 19110.00 | 13336.00 | 5774.00 | OPEN |
| json/numbers/direct_to_struct/main | direct_to_struct | 13875.00 | 12919.00 | 956.00 | ADMITTED |
| json/numbers/real_typed_struct/main | real_typed_struct | 13157.00 | 11871.00 | 1286.00 | ADMITTED |
| json/unicode_mixed/parse_only/main | parse_only | 8836.00 | 17336.00 | -8500.00 | OPEN |
| json/unicode_mixed/direct_to_struct/main | direct_to_struct | 4808.00 | 10497.00 | -5689.00 | OPEN |
| json/unicode_mixed/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_escapes/parse_only/main | parse_only | 13006.00 | 18902.00 | -5896.00 | OPEN |
| json/unicode_escapes/direct_to_struct/main | direct_to_struct | 5127.00 | 14041.00 | -8914.00 | OPEN |
| json/unicode_escapes/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_basic/parse_only/main | parse_only | 11631.00 | 15874.00 | -4243.00 | OPEN |
| json/unicode_basic/direct_to_struct/main | direct_to_struct | 9189.00 | 8821.00 | 368.00 | ADMITTED |
| json/unicode_basic/real_typed_struct/main | real_typed_struct | 6584.00 | 6033.00 | 551.00 | ADMITTED |
| json/distinct_values/parse_only/main | parse_only | 9687.00 | 17709.00 | -8022.00 | OPEN |
| json/distinct_values/direct_to_struct/main | direct_to_struct | 6281.00 | 11668.00 | -5387.00 | OPEN |
| json/distinct_values/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/y_string_unicode/parse_only/main | parse_only | 6285.00 | 13609.00 | -7324.00 | OPEN |
| json/y_string_unicode/direct_to_struct/main | direct_to_struct | 4997.00 | 8680.00 | -3683.00 | OPEN |
| json/y_string_unicode/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |

## CSS L4 Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| css_l4/declaration_values/direct_to_struct/main | css_l4_parity | 434.13 | 169.23 | 264.90 | ADMITTED |
| css_l4/declarations/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | ADMITTED |
| css_l4/stylesheet_root/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/at_rules_keyframes/direct_to_struct/main | css_l4_parity | 21584.64 | 254.22 | 21330.42 | ADMITTED |
| css_l4/nested_rules/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | ADMITTED |
| css_l4/css_variables/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | ADMITTED |
| css_l4/calc_expressions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | ADMITTED |
| css_l4/var_url_functions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | ADMITTED |
| css_l4/color_functions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | ADMITTED |
| css_l4/gradients/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | ADMITTED |
| css_l4/transforms/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | ADMITTED |
| css_l4/filters/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | ADMITTED |
| css_l4/easing_functions/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | ADMITTED |
| css_l4/media_queries/direct_to_struct/main | css_l4_parity | 21584.64 | 254.22 | 21330.42 | ADMITTED |
| css_l4/vendor_prefixes/direct_to_struct/main | css_l4_parity | 34635.22 | 278.74 | 34356.48 | ADMITTED |
| css_l4/custom_at_rules/direct_to_struct/main | css_l4_parity | 34635.22 | 278.74 | 34356.48 | ADMITTED |
| css_l4/pseudo_classes/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/pseudo_elements/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/attribute_selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/logical_properties/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | ADMITTED |
| css_l4/grid/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | ADMITTED |
| css_l4/flexbox/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | ADMITTED |
| css_l4/typed_property_groups/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | ADMITTED |

## Gate Notes

- `T1_sota` is the pinned admission threshold: `sonic-rs strict + 1 Mbps` for JSON rows and `lightningcss + 1 Mbps` for CSS rows.
- `tranche_admitted` records current strict admission status only; positive diagnostic parse margins remain `OPEN` until a SK-V13 wave lands the required equality and gate provenance.
- Missing real typed rows are explicit `MISSING` rows so the 51-row JSON universe cannot silently shrink.
