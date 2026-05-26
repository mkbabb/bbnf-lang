# SK-V14 W4R-A: Rolling Delta Ledger Surface

Date: 2026-05-26.
Wave: W4R.
Phase: research.
Agent: Godel.
Scope: read-only inspection of `restart/skinny/ROLLING-SOTA-DELTA.md`.

## Question

Identify the minimum amended-W4 ledger edit required to restore CSS L4 to
0/24 ADMITTED without touching metric cells or JSON rows.

## Finding

`restart/skinny/ROLLING-SOTA-DELTA.md` currently lists 24 CSS L4 row keys
under `## CSS L4 Targets`, and every row ends with `ADMITTED`.

The minimum W4R edit is status-only:

- change the final `tranche_admitted` cell on the 24 `css_l4/...` rows from
  `ADMITTED` to `OPEN`;
- preserve every numeric metric cell;
- preserve the JSON rows, which are already at 0/17 admitted for parse-only,
  direct-to-struct, and real-typed-struct classes after W1.

## Row Keys

1. `css_l4/declaration_values/direct_to_struct/main`
2. `css_l4/declarations/direct_to_struct/main`
3. `css_l4/stylesheet_root/direct_to_struct/main`
4. `css_l4/selectors/direct_to_struct/main`
5. `css_l4/at_rules_keyframes/direct_to_struct/main`
6. `css_l4/nested_rules/direct_to_struct/main`
7. `css_l4/css_variables/direct_to_struct/main`
8. `css_l4/calc_expressions/direct_to_struct/main`
9. `css_l4/var_url_functions/direct_to_struct/main`
10. `css_l4/color_functions/direct_to_struct/main`
11. `css_l4/gradients/direct_to_struct/main`
12. `css_l4/transforms/direct_to_struct/main`
13. `css_l4/filters/direct_to_struct/main`
14. `css_l4/easing_functions/direct_to_struct/main`
15. `css_l4/media_queries/direct_to_struct/main`
16. `css_l4/vendor_prefixes/direct_to_struct/main`
17. `css_l4/custom_at_rules/direct_to_struct/main`
18. `css_l4/pseudo_classes/direct_to_struct/main`
19. `css_l4/pseudo_elements/direct_to_struct/main`
20. `css_l4/attribute_selectors/direct_to_struct/main`
21. `css_l4/logical_properties/direct_to_struct/main`
22. `css_l4/grid/direct_to_struct/main`
23. `css_l4/flexbox/direct_to_struct/main`
24. `css_l4/typed_property_groups/direct_to_struct/main`

## Consumer

The amended W4 redress gate can verify the ledger with:

```sh
awk 'BEGIN{in_css=0;rows=0;admitted=0} /^## CSS L4 Targets/{in_css=1; next} /^## Gate Notes/{in_css=0} in_css && /^\| css_l4\// {rows++; if ($0 ~ /\| ADMITTED \|$/) admitted++} END{print rows, admitted}' restart/skinny/ROLLING-SOTA-DELTA.md
```

Expected close value: `24 0`.
