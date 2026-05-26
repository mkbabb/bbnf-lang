# SK-V14 W4R-C: REDRESS Row Map

Date: 2026-05-26.
Wave: W4R.
Phase: research.
Agent: Dewey.
Scope: read-only inspection of `skinny/REDRESS.md` and CSS L4 validation pack.

## Question

Assign row-keyed REDRESS ids for the amended W4 ledger prune.

## Finding

The current highest REDRESS id is 184. Amended W4 should add
REDRESS-185 through REDRESS-208, one entry for each reverted CSS L4 row.
Each entry cites
`restart/skinny/tranches/sk-v13/audit-overfit/validation/v1-css-l4-validation.md`
§1-6 and states that W4 performs no CSS source, generator, provider,
template, runtime-twin, or `regen_css` deletion.

## ID Map

| REDRESS | Row key | Provider family |
|---:|---|---|
| 185 | `css_l4/declaration_values/direct_to_struct/main` | `css_l4_declaration_values` |
| 186 | `css_l4/declarations/direct_to_struct/main` | `css_l4_declaration_values_extended` |
| 187 | `css_l4/css_variables/direct_to_struct/main` | `css_l4_declaration_values_extended` |
| 188 | `css_l4/calc_expressions/direct_to_struct/main` | `css_l4_declaration_values_extended` |
| 189 | `css_l4/var_url_functions/direct_to_struct/main` | `css_l4_declaration_values_extended` |
| 190 | `css_l4/color_functions/direct_to_struct/main` | `css_l4_declaration_values_extended` |
| 191 | `css_l4/stylesheet_root/direct_to_struct/main` | `css_l4_stylesheet_selectors` |
| 192 | `css_l4/selectors/direct_to_struct/main` | `css_l4_stylesheet_selectors` |
| 193 | `css_l4/pseudo_classes/direct_to_struct/main` | `css_l4_stylesheet_selectors` |
| 194 | `css_l4/pseudo_elements/direct_to_struct/main` | `css_l4_stylesheet_selectors` |
| 195 | `css_l4/attribute_selectors/direct_to_struct/main` | `css_l4_stylesheet_selectors` |
| 196 | `css_l4/gradients/direct_to_struct/main` | `css_l4_visual_functions` |
| 197 | `css_l4/transforms/direct_to_struct/main` | `css_l4_visual_functions` |
| 198 | `css_l4/filters/direct_to_struct/main` | `css_l4_visual_functions` |
| 199 | `css_l4/easing_functions/direct_to_struct/main` | `css_l4_visual_functions` |
| 200 | `css_l4/at_rules_keyframes/direct_to_struct/main` | `css_l4_at_rules_and_media` |
| 201 | `css_l4/media_queries/direct_to_struct/main` | `css_l4_at_rules_and_media` |
| 202 | `css_l4/vendor_prefixes/direct_to_struct/main` | `css_l4_vendor_and_custom_atrules` |
| 203 | `css_l4/custom_at_rules/direct_to_struct/main` | `css_l4_vendor_and_custom_atrules` |
| 204 | `css_l4/nested_rules/direct_to_struct/main` | `css_l4_nested_layout` |
| 205 | `css_l4/logical_properties/direct_to_struct/main` | `css_l4_nested_layout` |
| 206 | `css_l4/grid/direct_to_struct/main` | `css_l4_nested_layout` |
| 207 | `css_l4/flexbox/direct_to_struct/main` | `css_l4_nested_layout` |
| 208 | `css_l4/typed_property_groups/direct_to_struct/main` | `css_l4_nested_layout` |

## Template

Each entry should follow this form:

```text
Item N closes ROW under G-SK-V14-W4-PRUNE-2 as PRUNE. The prior CSS L4
admission is audit-falsified by sk-v13/v1-css-l4-validation:§1-6; W4
reclassifies the row to AUDIT-FALSIFIED/OPEN as ledger state only. No CSS
source, generator, provider, template, runtime-twin, or regen_css deletion is
performed in W4; deletion remains routed to W5 after replacement provider
generation exists.
```

## Consumer

The amended W4 redress gate must prove REDRESS-185 through REDRESS-208 are
present and row-keyed before W5 dispatch.
