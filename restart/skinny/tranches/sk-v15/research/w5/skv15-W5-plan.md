# SK-V15 W5 Plan: CSS Typed Value Provider

Status: PLAN-V1.
Wave: W5 CSS typed Value provider.
Owner paths:
`xtask/src/regen_css.rs`,
`xtask/runtime-projections/css_l4.toml`,
`crates/core/src/runtime/css_l4/{mod.rs,visitor.rs}`,
`crates/core/src/runtime/mod.rs`,
`crates/core/tests/typed_accessor_surface.rs`.

## Intervention

Add a generated CSS visitor surface to the existing root CSS L4 typed runtime.
The root provider already emits typed value, document, and view surfaces; W5
closes the named visitor gap without touching skinny fact-stream proof.

Generator work:

- Extend the CSS projection module exports with `visitor_exports`.
- Emit `visitor.rs` from `xtask regen-css`.
- Re-export `CssVisitor` and `visit_document` from
  `runtime::css_l4`; re-export `CssVisitor` and
  `visit_document as visit_css_document` at `runtime`.

Visitor shape:

- `CssVisitor<'p>` default no-op callbacks for stylesheet, rules, style/media/
  keyframes/generic-at variants, keyframe blocks, declarations, selectors,
  typed values, colors, and functions.
- `visit_document(&CssDocument, &mut impl CssVisitor)` walks rules,
  declarations, selector lists, keyframe blocks, nested media rules, value
  lists, function argument/fallback lists, and color-mix color references.

## Boundaries

Do not delete or retire `CSS_GENERATED_RS`, `CssFullParseSummary`,
fact-stream-only `parse()`, brace-counter proof, W8R tuples, or
`LegacyPath` / `LegacySegment`. They remain diagnostic/routed until W6.

Do not stage the pre-existing dirty skinny CSS `generated.rs` files,
`skinny/crates/bbnf-bench/src/generated_real_typed.rs`, prior-tranche JSON
research files, or `docs/precepts`.

## Gates

Run on Apple M5 Max / aarch64:

```sh
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- regen-css
RUSTFLAGS="-C target-cpu=native" cargo run --profile ax-iter -p xtask -- check-runtime --grammar css_l4
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test typed_accessor_surface css_l4_document_visitor_reaches_typed_values
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_substrate
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_parity
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test projection_totality
cargo fmt --check --package xtask --package bbnf
grep -cE "^[0-9]+\. \*\*" restart/locks/LOCKS.md
find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' | wc -l
git diff --check
```

## Dependency Rows

W5 consumes `DEP-W6-CSS-GENERATED-RS`,
`DEP-W6-CSS-SUMMARY-FACT-STREAM`,
`DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, and
`DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM` only by providing replacement typed
provider output. Deletion/retirement remains blocked until W6.
