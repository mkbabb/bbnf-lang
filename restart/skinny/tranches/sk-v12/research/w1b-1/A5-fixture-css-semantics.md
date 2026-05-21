# SK-V12 W1b-1 A5 - Fixture CSS Semantics

Scope: read-only fixture and CSS declaration-value semantic research. No source
edits.

## Finding

The requested fixture is missing:
`restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`
does not exist, and the `w1b/` research directory is not present. Existing CSS
assets are either too broad (`data/css/{normalize,bootstrap,tailwind}.css`) or
negative/recovery oriented (`grammar/tests/css/complex-errors.css`), so none
should be promoted automatically as the W1b-1 strict fixture.

## Candidate Corpus

Use one intentionally small fixture:

```css
a { color: #ff00ff; width: 50%; opacity: .5; margin-left: -10px; }
b { background-color: rgb(255 128 0 / 0.5) !important; }
@media (min-width: 640px) { c { height: 100px; color: red; } }
```

This exercises colors, dimensions, percentages, unitless numbers, negative
length, `!important`, and nested media context without requiring shorthand
expansion or full stylesheet canonicalization.

## Strict Equality Facts

W1b-1 should compare byte-identical `css_l4_declaration_value_fact_stream`
output between generated Track 1 and an independent oracle. The fact stream
should encode declaration ordinal, context, property, important flag, and
stable value facts. Equality must not be declaration counts, parse admission,
prettified CSS, digest-only equality, or `token_normalize` output.

The existing root CSS tests are useful background but not skinny Track 1
authority.

## Semantic Risks

Shorthands can reorder, expand, or collapse under `lightningcss`; keep them
out unless facts preserve raw tokens. Vendor-prefixed properties can route
through special vendor prefix handling; defer them from the first fixture.
Functions are acceptable only where both sides emit stable facts; `calc()`
simplification, `var()` fallbacks, gradients, and custom functions should wait
until after the first row is measurable.

## Exact Fallback

If the fixture is still missing at W1b-1 plan/redress entry, create exactly the
candidate corpus above at
`restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`.
If that path cannot feed generated Track 1 plus independent oracle, W1b-1
should record `BLOCKED/FAIL` for `G-W1b-1-CSS-L4-ORACLE`; do not substitute
Sheets, BBNF-self, JSON rows, root CSS runtime, or `complex-errors.css`.
