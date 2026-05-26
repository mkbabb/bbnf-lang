# SK-V14 W4R-B: RESULTS Overlay Surface

Date: 2026-05-26.
Wave: W4R.
Phase: research.
Agent: Goodall.
Scope: read-only inspection of `skinny/RESULTS.md`.

## Question

Determine whether amended W4 needs to edit `skinny/RESULTS.md` to preserve
the CSS L4 audit overlay.

## Finding

The 24 CSS L4 rows in `skinny/RESULTS.md` already retain
`AUDIT-FALSIFIED`. The amended W4 gate does not need a RESULTS edit for the
overlay state.

W4 is a ledger-only PRUNE wave after Pass Omega V4. It does not regenerate or
refresh CSS metrics, and it does not own the dirty SK-V13 JSON artifacts in
the current working tree. Therefore W4 should not use JSON drift as a reason
to rewrite RESULTS rows.

## W4-Specific Rows

The W4 visual-functions projection rows are:

- `css_l4/gradients/direct_to_struct/main`
- `css_l4/transforms/direct_to_struct/main`
- `css_l4/filters/direct_to_struct/main`
- `css_l4/easing_functions/direct_to_struct/main`

Those rows retain:

- `SK-V14-open:retained-css-l4-audit-overlay`;
- `not_admitted:pre-W8-css-full-parse-equality`;
- `AUDIT-FALSIFIED`;
- `sk-v13/v1-css-l4-validation:§1-6`.

## Risk

The SK-V13 JSON files do not carry an `audit_overlay_verdict` key. A future
full-table regeneration must preserve the SK-V14 overlay layer explicitly
instead of treating the JSON rows as sufficient authority. W4 avoids that
risk by not regenerating RESULTS.

## Consumer

The amended W4 redress gate can verify retained overlay state with:

```sh
rg '^\| css_l4/' skinny/RESULTS.md | rg -c 'AUDIT-FALSIFIED'
```

Expected close value: `24`.
