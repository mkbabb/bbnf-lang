# Rolling SOTA Delta

schema_version: sk-v13-rolling-sota-delta-v1
run_id: SK-V13-open
g_omega_status: signed
consumer_gate: cargo xtask gate-json --check-results
source_results: skinny/RESULTS.md
source_commit: 591eafb07+w1-prune1

## JSON Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| json/twitter/parse_only/main | parse_only | 15561.00 | 21014.00 | -5453.00 | OPEN |
| json/twitter/direct_to_struct/main | direct_to_struct | 11908.00 | 15111.00 | -3203.00 | OPEN |
| json/twitter/real_typed_struct/main | real_typed_struct | 17898.00 | 15503.00 | 2395.00 | OPEN |
| json/citm_catalog/parse_only/main | parse_only | 30150.00 | 25566.00 | 4584.00 | OPEN |
| json/citm_catalog/direct_to_struct/main | direct_to_struct | 21414.00 | 19939.00 | 1475.00 | OPEN |
| json/citm_catalog/real_typed_struct/main | real_typed_struct | 36719.00 | 22858.00 | 13861.00 | OPEN |
| json/canada/parse_only/main | parse_only | 16977.00 | 14102.00 | 2875.00 | OPEN |
| json/canada/direct_to_struct/main | direct_to_struct | 10962.00 | 12206.00 | -1244.00 | OPEN |
| json/canada/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/apache_builds/parse_only/main | parse_only | 12767.00 | 17352.00 | -4585.00 | OPEN |
| json/apache_builds/direct_to_struct/main | direct_to_struct | 11428.00 | 11106.00 | 322.00 | OPEN |
| json/apache_builds/real_typed_struct/main | real_typed_struct | 8127.00 | 8092.00 | 35.00 | OPEN |
| json/github_events/parse_only/main | parse_only | 14966.00 | 23010.00 | -8044.00 | OPEN |
| json/github_events/direct_to_struct/main | direct_to_struct | 12483.00 | 16198.00 | -3715.00 | OPEN |
| json/github_events/real_typed_struct/main | real_typed_struct | 13040.00 | 12628.00 | 412.00 | OPEN |
| json/update_center/parse_only/main | parse_only | 11791.00 | 19662.00 | -7871.00 | OPEN |
| json/update_center/direct_to_struct/main | direct_to_struct | 8546.00 | 11184.00 | -2638.00 | OPEN |
| json/update_center/real_typed_struct/main | real_typed_struct | 13191.00 | 12624.00 | 567.00 | OPEN |
| json/mesh/parse_only/main | parse_only | 12987.00 | 11759.00 | 1228.00 | OPEN |
| json/mesh/direct_to_struct/main | direct_to_struct | 9661.00 | 9758.00 | -97.00 | OPEN |
| json/mesh/real_typed_struct/main | real_typed_struct | 9686.00 | 8868.00 | 818.00 | OPEN |
| json/random/parse_only/main | parse_only | 9946.00 | 15666.00 | -5720.00 | OPEN |
| json/random/direct_to_struct/main | direct_to_struct | 7801.00 | 8945.00 | -1144.00 | OPEN |
| json/random/real_typed_struct/main | real_typed_struct | 8151.00 | 7394.00 | 757.00 | OPEN |
| json/gsoc-2018/parse_only/main | parse_only | 23587.00 | 50364.00 | -26777.00 | OPEN |
| json/gsoc-2018/direct_to_struct/main | direct_to_struct | 15385.00 | 23881.00 | -8496.00 | OPEN |
| json/gsoc-2018/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/marine_ik/parse_only/main | parse_only | 12357.00 | 9903.00 | 2454.00 | OPEN |
| json/marine_ik/direct_to_struct/main | direct_to_struct | 10513.00 | 8455.00 | 2058.00 | OPEN |
| json/marine_ik/real_typed_struct/main | real_typed_struct | 12164.00 | 9199.00 | 2965.00 | OPEN |
| json/instruments/parse_only/main | parse_only | 17468.00 | 19631.00 | -2163.00 | OPEN |
| json/instruments/direct_to_struct/main | direct_to_struct | 12060.00 | 12732.00 | -672.00 | OPEN |
| json/instruments/real_typed_struct/main | real_typed_struct | 21464.00 | 16210.00 | 5254.00 | OPEN |
| json/numbers/parse_only/main | parse_only | 19267.00 | 13667.00 | 5600.00 | OPEN |
| json/numbers/direct_to_struct/main | direct_to_struct | 14125.00 | 12748.00 | 1377.00 | OPEN |
| json/numbers/real_typed_struct/main | real_typed_struct | 13281.00 | 12250.00 | 1031.00 | OPEN |
| json/unicode_mixed/parse_only/main | parse_only | 9294.00 | 18859.00 | -9565.00 | OPEN |
| json/unicode_mixed/direct_to_struct/main | direct_to_struct | 5062.00 | 10654.00 | -5592.00 | OPEN |
| json/unicode_mixed/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_escapes/parse_only/main | parse_only | 13550.00 | 19274.00 | -5724.00 | OPEN |
| json/unicode_escapes/direct_to_struct/main | direct_to_struct | 5523.00 | 14299.00 | -8776.00 | OPEN |
| json/unicode_escapes/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/unicode_basic/parse_only/main | parse_only | 12041.00 | 16126.00 | -4085.00 | OPEN |
| json/unicode_basic/direct_to_struct/main | direct_to_struct | 9317.00 | 8977.00 | 340.00 | OPEN |
| json/unicode_basic/real_typed_struct/main | real_typed_struct | 6753.00 | 6045.00 | 708.00 | OPEN |
| json/distinct_values/parse_only/main | parse_only | 9920.00 | 18161.00 | -8241.00 | OPEN |
| json/distinct_values/direct_to_struct/main | direct_to_struct | 6540.00 | 11949.00 | -5409.00 | OPEN |
| json/distinct_values/real_typed_struct/main | real_typed_struct | absent:product-surface-not-generated | absent:product-surface-not-generated | absent:product-surface-not-generated | MISSING |
| json/y_string_unicode/parse_only/main | parse_only | 6590.00 | 13861.00 | -7271.00 | OPEN |
| json/y_string_unicode/direct_to_struct/main | direct_to_struct | 5061.00 | 8999.00 | -3938.00 | OPEN |
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
