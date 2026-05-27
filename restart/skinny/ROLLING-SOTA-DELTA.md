# Rolling SOTA Delta

schema_version: sk-v13-rolling-sota-delta-v1
run_id: SK-V13-open
g_omega_status: signed
consumer_gate: cargo xtask gate-json --check-results
source_results: skinny/RESULTS.md
source_commit: 209fb0363+w10v-current-head-resweep

## JSON Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| json/twitter/parse_only/main | parse_only | 15561.00 | 21014.00 | -5453.00 | OPEN |
| json/twitter/direct_to_struct/main | direct_to_struct | 11908.00 | 15111.00 | -3203.00 | OPEN |
| json/twitter/real_typed_struct/main | real_typed_struct | 10705.05 | 8953.25 | 1751.80 | ADMITTED |
| json/citm_catalog/parse_only/main | parse_only | 9079.838 | 8336.772 | 743.066 | ADMITTED |
| json/citm_catalog/direct_to_struct/main | direct_to_struct | 21414.00 | 19939.00 | 1475.00 | OPEN |
| json/citm_catalog/real_typed_struct/main | real_typed_struct | 20512.60 | 12663.29 | 7849.31 | ADMITTED |
| json/canada/parse_only/main | parse_only | 16709.901 | 12971.929 | 3737.972 | ADMITTED |
| json/canada/direct_to_struct/main | direct_to_struct | 10962.00 | 12206.00 | -1244.00 | OPEN |
| json/canada/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/apache_builds/parse_only/main | parse_only | 12767.00 | 17352.00 | -4585.00 | OPEN |
| json/apache_builds/direct_to_struct/main | direct_to_struct | 11428.00 | 11106.00 | 322.00 | OPEN |
| json/apache_builds/real_typed_struct/main | real_typed_struct | 4352.26 | 3391.81 | 960.45 | ADMITTED |
| json/github_events/parse_only/main | parse_only | 14966.00 | 23010.00 | -8044.00 | OPEN |
| json/github_events/direct_to_struct/main | direct_to_struct | 12483.00 | 16198.00 | -3715.00 | OPEN |
| json/github_events/real_typed_struct/main | real_typed_struct | 6643.66 | 5976.17 | 667.49 | ADMITTED |
| json/update_center/parse_only/main | parse_only | 11791.00 | 19662.00 | -7871.00 | OPEN |
| json/update_center/direct_to_struct/main | direct_to_struct | 8546.00 | 11184.00 | -2638.00 | OPEN |
| json/update_center/real_typed_struct/main | real_typed_struct | 6776.28 | 5846.38 | 929.90 | ADMITTED |
| json/mesh/parse_only/main | parse_only | 11669.30 | 6590.82 | 5078.48 | ADMITTED |
| json/mesh/direct_to_struct/main | direct_to_struct | 9661.00 | 9758.00 | -97.00 | OPEN |
| json/mesh/real_typed_struct/main | real_typed_struct | 4580.29 | 4344.22 | 236.07 | ADMITTED |
| json/random/parse_only/main | parse_only | 9946.00 | 15666.00 | -5720.00 | OPEN |
| json/random/direct_to_struct/main | direct_to_struct | 7801.00 | 8945.00 | -1144.00 | OPEN |
| json/random/real_typed_struct/main | real_typed_struct | 4354.29 | 3042.02 | 1312.27 | ADMITTED |
| json/gsoc-2018/parse_only/main | parse_only | 23587.00 | 50364.00 | -26777.00 | OPEN |
| json/gsoc-2018/direct_to_struct/main | direct_to_struct | 15385.00 | 23881.00 | -8496.00 | OPEN |
| json/gsoc-2018/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/marine_ik/parse_only/main | parse_only | 9505.49 | 5339.94 | 4165.55 | ADMITTED |
| json/marine_ik/direct_to_struct/main | direct_to_struct | 10513.00 | 8455.00 | 2058.00 | OPEN |
| json/marine_ik/real_typed_struct/main | real_typed_struct | 5515.10 | 5241.98 | 273.12 | ADMITTED |
| json/instruments/parse_only/main | parse_only | 4281.770 | 3458.276 | 823.494 | ADMITTED |
| json/instruments/direct_to_struct/main | direct_to_struct | 12060.00 | 12732.00 | -672.00 | OPEN |
| json/instruments/real_typed_struct/main | real_typed_struct | 9550.11 | 7780.12 | 1769.99 | ADMITTED |
| json/numbers/parse_only/main | parse_only | 14472.31 | 7453.77 | 7018.53 | ADMITTED |
| json/numbers/direct_to_struct/main | direct_to_struct | 14125.00 | 12748.00 | 1377.00 | OPEN |
| json/numbers/real_typed_struct/main | real_typed_struct | 6608.57 | 6023.91 | 584.66 | ADMITTED |
| json/unicode_mixed/parse_only/main | parse_only | 7379.340 | 7012.268 | 367.072 | ADMITTED |
| json/unicode_mixed/direct_to_struct/main | direct_to_struct | 5062.00 | 10654.00 | -5592.00 | OPEN |
| json/unicode_mixed/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_escapes/parse_only/main | parse_only | 7897.45 | 2985.08 | 4912.37 | ADMITTED |
| json/unicode_escapes/direct_to_struct/main | direct_to_struct | 5523.00 | 14299.00 | -8776.00 | OPEN |
| json/unicode_escapes/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_basic/parse_only/main | parse_only | 9445.73 | 7060.90 | 2384.83 | ADMITTED |
| json/unicode_basic/direct_to_struct/main | direct_to_struct | 9317.00 | 8977.00 | 340.00 | OPEN |
| json/unicode_basic/real_typed_struct/main | real_typed_struct | 3221.33 | 2481.52 | 739.81 | ADMITTED |
| json/distinct_values/parse_only/main | parse_only | 9920.00 | 18161.00 | -8241.00 | OPEN |
| json/distinct_values/direct_to_struct/main | direct_to_struct | 6540.00 | 11949.00 | -5409.00 | OPEN |
| json/distinct_values/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/y_string_unicode/parse_only/main | parse_only | 3169.90 | 2418.91 | 750.99 | ADMITTED |
| json/y_string_unicode/direct_to_struct/main | direct_to_struct | 5061.00 | 8999.00 | -3938.00 | OPEN |
| json/y_string_unicode/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |

## CSS L4 Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| css_l4/declaration_values/direct_to_struct/main | css_l4_parity | 434.13 | 169.23 | 264.90 | OPEN |
| css_l4/declarations/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | OPEN |
| css_l4/stylesheet_root/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | OPEN |
| css_l4/selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | OPEN |
| css_l4/at_rules_keyframes/direct_to_struct/main | css_l4_parity | 21584.64 | 254.22 | 21330.42 | OPEN |
| css_l4/nested_rules/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | OPEN |
| css_l4/css_variables/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | OPEN |
| css_l4/calc_expressions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | OPEN |
| css_l4/var_url_functions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | OPEN |
| css_l4/color_functions/direct_to_struct/main | css_l4_parity | 265.72 | 55.91 | 209.81 | OPEN |
| css_l4/gradients/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | OPEN |
| css_l4/transforms/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | OPEN |
| css_l4/filters/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | OPEN |
| css_l4/easing_functions/direct_to_struct/main | css_l4_parity | 225.89 | 115.53 | 110.37 | OPEN |
| css_l4/media_queries/direct_to_struct/main | css_l4_parity | 21584.64 | 254.22 | 21330.42 | OPEN |
| css_l4/vendor_prefixes/direct_to_struct/main | css_l4_parity | 34635.22 | 278.74 | 34356.48 | OPEN |
| css_l4/custom_at_rules/direct_to_struct/main | css_l4_parity | 34635.22 | 278.74 | 34356.48 | OPEN |
| css_l4/pseudo_classes/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | OPEN |
| css_l4/pseudo_elements/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | OPEN |
| css_l4/attribute_selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | OPEN |
| css_l4/logical_properties/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | OPEN |
| css_l4/grid/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | OPEN |
| css_l4/flexbox/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | OPEN |
| css_l4/typed_property_groups/direct_to_struct/main | css_l4_parity | 52233.54 | 422.16 | 51811.38 | OPEN |

## Gate Notes

- `T1_sota` is the pinned admission threshold: `sonic-rs strict + 1 Mbps` for JSON rows and `lightningcss + 1 Mbps` for CSS rows.
- `tranche_admitted` records current strict admission status only; positive diagnostic parse margins remain `OPEN` until a SK-V13 wave lands the required equality and gate provenance.
- Missing real typed rows are explicit `MISSING` rows so the 51-row JSON universe cannot silently shrink.
