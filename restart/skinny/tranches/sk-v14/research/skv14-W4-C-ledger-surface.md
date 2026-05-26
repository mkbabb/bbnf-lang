# SK-V14 W4-C: CSS L4 Ledger Surface

Date: 2026-05-26.
Wave: W4 PRUNE-2.
Phase: research.

## Required Row Keys

W4 owns the 24 operational CSS L4 rows:

```text
css_l4/declaration_values/direct_to_struct/main
css_l4/declarations/direct_to_struct/main
css_l4/css_variables/direct_to_struct/main
css_l4/calc_expressions/direct_to_struct/main
css_l4/var_url_functions/direct_to_struct/main
css_l4/color_functions/direct_to_struct/main
css_l4/stylesheet_root/direct_to_struct/main
css_l4/selectors/direct_to_struct/main
css_l4/pseudo_classes/direct_to_struct/main
css_l4/pseudo_elements/direct_to_struct/main
css_l4/attribute_selectors/direct_to_struct/main
css_l4/gradients/direct_to_struct/main
css_l4/transforms/direct_to_struct/main
css_l4/filters/direct_to_struct/main
css_l4/easing_functions/direct_to_struct/main
css_l4/at_rules_keyframes/direct_to_struct/main
css_l4/media_queries/direct_to_struct/main
css_l4/vendor_prefixes/direct_to_struct/main
css_l4/custom_at_rules/direct_to_struct/main
css_l4/nested_rules/direct_to_struct/main
css_l4/logical_properties/direct_to_struct/main
css_l4/grid/direct_to_struct/main
css_l4/flexbox/direct_to_struct/main
css_l4/typed_property_groups/direct_to_struct/main
```

## Current RESULTS State

`skinny/RESULTS.md` already carries these rows as:

- `verdict` / gate state: `not_admitted:pre-W8-css-full-parse-equality`
- `audit_overlay_verdict`: `AUDIT-FALSIFIED`
- `audit_overlay_refs`: `sk-v13/v1-css-l4-validation:§1-6`

No W4 source attempt should rewrite these rows before the provider-cycle
amendment lands.

## Current Rolling Delta State

`restart/skinny/ROLLING-SOTA-DELTA.md` still lists the 24 CSS L4 rows with
`tranche_admitted = ADMITTED`. That is the remaining ledger correction W4 must
perform once its deletion/generator sequencing is made coherent.

## Redress Requirement

W4 as currently specified requires 24 row-keyed REDRESS entries, each citing
`v1 §1-6` and naming both the deleted template/provider path and the generated
replacement path. Because provider deletion is blocked before replacement
exists, the current wave cannot honestly create those 24 per-row closure
entries. The correct immediate entry is a single W4 rejection entry that
routes the spec cycle to Pass Omega V4.
