# Rolling SOTA Delta

schema_version: sk-v13-rolling-sota-delta-v1
run_id: SK-V14-W11W-current
g_omega_status: signed
consumer_gate: cargo xtask gate-json --check-results
source_results: skinny/RESULTS.md
source_commit: bae430dcf

## JSON Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| json/twitter/parse_only/main | parse_only | 8349.290 | 4914.095 | 3435.195 | ADMITTED |
| json/twitter/direct_to_struct/main | direct_to_struct | 17585.679 | 14858.624 | 2727.055 | ADMITTED |
| json/twitter/real_typed_struct/main | real_typed_struct | 10705.05 | 8953.25 | 1751.80 | ADMITTED |
| json/citm_catalog/parse_only/main | parse_only | 9079.838 | 8336.772 | 743.066 | ADMITTED |
| json/citm_catalog/direct_to_struct/main | direct_to_struct | 33366.495 | 21251.015 | 12115.480 | ADMITTED |
| json/citm_catalog/real_typed_struct/main | real_typed_struct | 20512.60 | 12663.29 | 7849.31 | ADMITTED |
| json/canada/parse_only/main | parse_only | 16709.901 | 12971.929 | 3737.972 | ADMITTED |
| json/canada/direct_to_struct/main | direct_to_struct | 4749.599 | 2734.746 | 2014.853 | ADMITTED |
| json/canada/real_typed_struct/main | real_typed_struct | 4761.909 | 2737.418 | 2024.491 | ADMITTED |
| json/apache_builds/parse_only/main | parse_only | 13129.331 | 12952.668 | 176.663 | ADMITTED |
| json/apache_builds/direct_to_struct/main | direct_to_struct | 7483.813 | 6328.769 | 1155.044 | ADMITTED |
| json/apache_builds/real_typed_struct/main | real_typed_struct | 4352.26 | 3391.81 | 960.45 | ADMITTED |
| json/github_events/parse_only/main | parse_only | 8148.582 | 5015.433 | 3133.149 | ADMITTED |
| json/github_events/direct_to_struct/main | direct_to_struct | 12501.469 | 11013.854 | 1487.615 | ADMITTED |
| json/github_events/real_typed_struct/main | real_typed_struct | 6643.66 | 5976.17 | 667.49 | ADMITTED |
| json/update_center/parse_only/main | parse_only | 5671.345 | 4708.613 | 962.732 | ADMITTED |
| json/update_center/direct_to_struct/main | direct_to_struct | 12820.158 | 10888.271 | 1931.887 | ADMITTED |
| json/update_center/real_typed_struct/main | real_typed_struct | 6776.28 | 5846.38 | 929.90 | ADMITTED |
| json/mesh/parse_only/main | parse_only | 11669.30 | 6590.82 | 5078.48 | ADMITTED |
| json/mesh/direct_to_struct/main | direct_to_struct | 9036.398 | 7876.325 | 1160.073 | ADMITTED |
| json/mesh/real_typed_struct/main | real_typed_struct | 4580.29 | 4344.22 | 236.07 | ADMITTED |
| json/random/parse_only/main | parse_only | 3093.724 | 2938.264 | 155.460 | ADMITTED |
| json/random/direct_to_struct/main | direct_to_struct | 7977.902 | 5755.672 | 2222.230 | ADMITTED |
| json/random/real_typed_struct/main | real_typed_struct | 4354.29 | 3042.02 | 1312.27 | ADMITTED |
| json/gsoc-2018/parse_only/main | parse_only | 13213.304 | 11356.449 | 1856.855 | ADMITTED |
| json/gsoc-2018/direct_to_struct/main | direct_to_struct | 7228.198 | 6670.742 | 557.456 | ADMITTED |
| json/gsoc-2018/real_typed_struct/main | real_typed_struct | 7176.742 | 6628.652 | 548.090 | ADMITTED |
| json/marine_ik/parse_only/main | parse_only | 9505.49 | 5339.94 | 4165.55 | ADMITTED |
| json/marine_ik/direct_to_struct/main | direct_to_struct | 11162.218 | 8831.443 | 2330.775 | ADMITTED |
| json/marine_ik/real_typed_struct/main | real_typed_struct | 5515.10 | 5241.98 | 273.12 | ADMITTED |
| json/instruments/parse_only/main | parse_only | 4281.770 | 3458.276 | 823.494 | ADMITTED |
| json/instruments/direct_to_struct/main | direct_to_struct | 18191.796 | 14489.541 | 3702.255 | ADMITTED |
| json/instruments/real_typed_struct/main | real_typed_struct | 9550.11 | 7780.12 | 1769.99 | ADMITTED |
| json/numbers/parse_only/main | parse_only | 14472.31 | 7453.77 | 7018.53 | ADMITTED |
| json/numbers/direct_to_struct/main | direct_to_struct | 12574.721 | 11310.297 | 1264.424 | ADMITTED |
| json/numbers/real_typed_struct/main | real_typed_struct | 6608.57 | 6023.91 | 584.66 | ADMITTED |
| json/unicode_mixed/parse_only/main | parse_only | 7379.340 | 7012.268 | 367.072 | ADMITTED |
| json/unicode_mixed/direct_to_struct/main | direct_to_struct | 5903.562 | 5341.219 | 562.343 | ADMITTED |
| json/unicode_mixed/real_typed_struct/main | real_typed_struct | 5837.942 | 5310.589 | 527.353 | ADMITTED |
| json/unicode_escapes/parse_only/main | parse_only | 7897.45 | 2985.08 | 4912.37 | ADMITTED |
| json/unicode_escapes/direct_to_struct/main | direct_to_struct | 2357.459 | 1853.453 | 504.006 | ADMITTED |
| json/unicode_escapes/real_typed_struct/main | real_typed_struct | 2244.473 | 2037.703 | 206.770 | ADMITTED |
| json/unicode_basic/parse_only/main | parse_only | 9445.73 | 7060.90 | 2384.83 | ADMITTED |
| json/unicode_basic/direct_to_struct/main | direct_to_struct | 6177.340 | 4693.661 | 1483.679 | ADMITTED |
| json/unicode_basic/real_typed_struct/main | real_typed_struct | 3221.33 | 2481.52 | 739.81 | ADMITTED |
| json/distinct_values/parse_only/main | parse_only | 5155.207 | 3234.781 | 1920.426 | ADMITTED |
| json/distinct_values/direct_to_struct/main | direct_to_struct | 8755.197 | 3908.274 | 4846.923 | ADMITTED |
| json/distinct_values/real_typed_struct/main | real_typed_struct | 8827.520 | 3896.064 | 4931.456 | ADMITTED |
| json/y_string_unicode/parse_only/main | parse_only | 3169.90 | 2418.91 | 750.99 | ADMITTED |
| json/y_string_unicode/direct_to_struct/main | direct_to_struct | 5493.522 | 4264.646 | 1228.876 | ADMITTED |
| json/y_string_unicode/real_typed_struct/main | real_typed_struct | 5361.584 | 4267.896 | 1093.688 | ADMITTED |

## CSS L4 Targets

| row | plane | T1_current | T1_sota | margin | tranche_admitted |
|---|---|---:|---:|---:|---|
| css_l4/declaration_values/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/declarations/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/stylesheet_root/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/selectors/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/at_rules_keyframes/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/nested_rules/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/css_variables/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/calc_expressions/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/var_url_functions/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/color_functions/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/gradients/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/transforms/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/filters/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/easing_functions/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/media_queries/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/vendor_prefixes/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/custom_at_rules/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/pseudo_classes/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/pseudo_elements/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/attribute_selectors/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/logical_properties/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/grid/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/flexbox/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |
| css_l4/typed_property_groups/direct_to_struct/main | css_l4_full_parse | 2319.04 | 930.28 | 1388.76 | OPEN |

## Gate Notes

- `T1_sota` is the pinned admission threshold: `sonic-rs strict + 1 Mbps` for JSON rows. CSS table values remain retained W8R diagnostic full-parse evidence only; SK-V15 W6 supersedes live CSS admission with a typed same-workload rejection (`Track1=4.317 Mbps`, `cssparser=2051.911 Mbps`, margin `-2048.594 Mbps`, Track1 `2/4` corpus passes), so CSS stays `OPEN` until a fresh typed row beats `cssparser` with typed-summary equality.
- `tranche_admitted` records current strict admission status only; positive diagnostic parse margins remain `OPEN` until a same-wave implementation lands the required equality and gate provenance.
- Missing real typed rows are explicit `MISSING` rows so the 51-row JSON universe cannot silently shrink.

## SK-V17 close note (2026-05-30, HEAD `6bb4b2a6c`)

SK-V17 supersedes the CSS L4 plane above. The CSS rows in this table are the
pre-SK-V17 `css_l4_full_parse` diagnostic (cssparser flaw-probe, `T1_current=2319.04`
/ `T1_sota=930.28`, one tuple broadcast across 24 conceptual rows) and remain `OPEN`
under the current schema. SK-V17 re-bases the CSS >SOTA claim on the FAIR
full-CSSOM-materializing comparator (lightningcss), per-corpus, at the rich typed
plane with EXACT 9-field cssparser equality — NOT the cssparser flaw-probe. The
canonical SK-V17 close medians (N=200 cold, rich-typed Track-1 vs lightningcss
full-CSSOM, re-baselined same-run; `restart/skinny/tranches/sk-v17/research/w5/skv17-W5-close-ledger.md` §3):

| corpus | class | rich-typed Mbps | lightningcss Mbps | rich/lcss |
|---|---|---:|---:|---:|
| bootstrap | regular | 2473.1 | 1119.1 | **2.210×** |
| animate | regular | 2937.9 | 1247.7 | **2.355×** |
| tailwindcss | utility | 2773.4 | 828.5 | **3.348×** |
| material-components-web | irregular | 2618.5 | 1312.0 | **1.996×** |

Both regular corpora cross decisively; max ratio 3.348×. The 24-row CSS broadcast
above is NOT re-stamped (pre-blocked route); a schema migration to per-corpus
lightningcss rows is an SK-V18 RESULTS-plane fold. The JSON 51-row universe (this
table) is unchanged and ADMITTED throughout SK-V17.
