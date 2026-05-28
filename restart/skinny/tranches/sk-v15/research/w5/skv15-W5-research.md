# SK-V15 W5 Research: CSS Typed Value Provider

W5 authority: build typed CSS value/document/view/visitor output and prove it
with executable tests/gates. Do not retire `CSS_GENERATED_RS`,
`CssFullParseSummary`, fact-stream-only `parse()`, brace counters, W8R tuples,
or `LegacyPath` / `LegacySegment`; those remain diagnostic/routed until W6.

## A - Root Typed Provider

Root CSS L4 already has generated typed value, document, and view surfaces:
`CssTypedValue`, typed dimensions/colors/functions, `CssDocument::values`,
`walk_values`, `CssView`, and `RuntimeView` implementation. The generator owns
`value.rs`, `arena.rs`, `builder.rs`, `document.rs`, `view.rs`,
`parse_with.rs`, and `mod.rs`.

Gap: no visitor module or `CssVisitor` / `visit_*` API exists in
`crates/core/src/runtime/css_l4` or `xtask/src/regen_css.rs`.

Candidate proof commands:

```sh
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_substrate
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test css_l4_parity
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test typed_accessor_surface
RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf --test projection_totality
```

## B - Skinny Old Proof Inventory

Seven skinny CSS profiles still emit `generated.rs` from
`CSS_GENERATED_RS`. `parse()` returns `Result<String, CssFactError>` via
`emit_fact_stream`; `parse_full()` also returns `String`; `CssFullParseSummary`
has only four counters: rules, at-rules, qualified-rules, declarations.

The seven generated CSS files are currently dirty from pre-existing formatting
diffs. W5 must not stage them unless it intentionally rewrites generated CSS
output with matching W5 proof.

## C - Bench/Gate State

Skinny CSS bench rows still use fact-stream planes and W8 full-parse
diagnostics. `gate-json --check-results` preserves JSON guardrails but is not a
typed CSS provider gate by itself. Existing core tests are the strongest W5
typed-provider proof; W6 owns same-workload retime and old-proof retirement.

## D - Generic Boundaries

W3 converted CSS profile behavior to request-carried metadata. W5 must not
reintroduce static CSS profile/provider branches. If generic skinny provider
code is touched, prove CSS plus Sheets or BBNF-self stability.

## E - Dependency Rows

W5 consumes `DEP-W6-CSS-GENERATED-RS`,
`DEP-W6-CSS-SUMMARY-FACT-STREAM`,
`DEP-W3-W6-CSS-PROVIDER-TEMPLATE`, and
`DEP-W4-W6-CSS-LEGACY-RUNTIME-SHIM`, but only by building replacement typed
provider output and keeping old proof diagnostic. Deletion/retirement is W6.

Budgets: High risk, manual LOC `180-360`, generated LOC `220-440` only from a
named provider, docs LOC `80-180`, redress `<=30m`.

## F - Dirty/Staging

Unowned dirty paths after W4: `docs/precepts`, prior SK-V12/SK-V13 research
JSON files, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, and seven
skinny CSS `generated.rs` files. Root CSS runtime/generator/test paths are
clean at W5 entry. Stage only W5-owned root CSS generator/runtime/test/docs.
