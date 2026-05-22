# SK-V13 CSS L4 Measurement Audit — Overfit and Corpus Integrity

**Date:** 2026-05-22  
**Scope:** SK-V13 rolling-SOTA-delta CSS L4 rows. Audit depth: corpus inventory, identical-number clustering, throughput anomalies, grammar-derived vs hand-curated, comparator integrity, and overfit verdicts.

**Hard findings:** All CSS L4 rows in the rolling delta are **grouped measured rows**. Each numerical triple (T1_current/T1_sota/margin) is shared across multiple rolling-delta feature rows, indicating that the rolling delta maps multiple conceptual features to a single measured grouped row. The bench harness (nonjson_css_l4.rs) confirms distinct fixtures and Criterion groups per wave.

---

## §1 Corpus Inventory

| Feature Row ID | Grouped Measure Row | Fixture Path | Bytes | SHA256 | Coverage Claims | Status |
|---|---|---|---:|---|---|---|
| css_l4/declaration_values/direct_to_struct/main | SK-V12-W1b-1 | restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css | 187 | cbb639460a72ef82e7c1b7c53ccc69495a35f6860b29ad72370b042b470d7374 | 5 decl tokens, hash, number, percentage, dimension, ident, function, paren_close | VERIFIED |
| css_l4/declarations + css_variables + calc_expressions + var_url_functions + color_functions | SK-V13-W3 | restart/skinny/tranches/sk-v13/research/w3/css_l4_declaration_values_extended.css | 305 | 399593fe9848954d3570c67a588a7c352e252327f60445f3bc0670c11df88d64 | var(), calc(), clamp(), color-mix(), rgb(), url(), strings, escaped ident | VERIFIED |
| css_l4/stylesheet_root + selectors (and pseudo-class, pseudo-element, attribute_selectors rows) | SK-V13-W2 | restart/skinny/tranches/sk-v13/research/w2/css_l4_stylesheet_and_selectors.css | 117 | 7fc890301ed7cdd79224fdca8d174bac80069b518c100156ed5b6e1f96cb9530 | type selector, class, id, child/descendant/adjacent/sibling combinator, attribute, :hover, ::before, qualified rule | VERIFIED |
| css_l4/at_rules_keyframes + media_queries | SK-V13-W10.1 | restart/skinny/tranches/sk-v13/research/w10.1/css_l4_at_rules_and_media.css | 85 | 234dde82e1ead1e66be251a5d219892b666f16e853fcd5c03e67aca22fb07958 | @media, media query, keyframes, keyframe selector (from/percentage/to) | VERIFIED |
| css_l4/vendor_prefixes + custom_at_rules | SK-V13-W10.2 | restart/skinny/tranches/sk-v13/research/w10.2/css_l4_vendor_and_custom_atrules.css | 162 | 367122942a2c937654b35a1065edc33ae85694a4bcd02b50d6ed50ea1631995f | @custom-media, -webkit-keyframes, vendor-prefixed decl, unprefixed decl | VERIFIED |
| css_l4/gradients + transforms + filters + easing_functions | SK-V13-W4 | restart/skinny/tranches/sk-v13/research/w4/css_l4_visual_functions.css | 357 | 5dc7cc1098401900af32b534893c9bd007245f88af3cc683926a4abaf5f531c0 | linear-gradient, translate, rotate, scale, skew, blur, brightness, contrast, drop-shadow, cubic-bezier, steps | VERIFIED |
| css_l4/nested_rules + logical_properties + grid + flexbox + typed_property_groups | SK-V13-W10.3 | restart/skinny/tranches/sk-v13/research/w10.3/css_l4_nested_layout.css | 351 | 5edcbfba1ba52af4dff689257aed8678a82f7d1cbbac36f5d0ae974384bddd2d | display:grid/flex, grid-template-columns, gap, margin-inline-start, inline-size, padding-block, border-inline-start, nested rule with & selector | VERIFIED |

**Summary:** Each grouped measured row (W1b, W2, W3, W4, W10.1, W10.2, W10.3) has one tiny, fixed fixture. All fixtures are hardcoded in the bench source as constants (FIXTURE_BYTES, FIXTURE_SHA256). There are NO production corpora in skinny/corpora/css-l4-sk-v13/ — that directory does not exist.

---

## §2 Identical-Number Cluster Investigation

### Cluster A: Five rows with 265.72 / 55.91 / 209.81

**Rows:**
- css_l4/declarations/direct_to_struct/main
- css_l4/css_variables/direct_to_struct/main
- css_l4/calc_expressions/direct_to_struct/main
- css_l4/var_url_functions/direct_to_struct/main
- css_l4/color_functions/direct_to_struct/main

**Source:** SK-V13-W3 (declaration_values_extended). Single fixture: 305 bytes, sha256=399593fe...

**Theory A1: One corpus, five rolling-delta rows**
LIKELY CORRECT. All five feature rows are *sub-features* of the W3 grouped row `css_l4/declaration_values_extended/direct_to_struct/main`. The grouped row measured 265.72 Mbps track1 vs 55.91 Mbps lightningcss. The rolling delta propagates that single measurement to five separate feature rows that are all proven-covered by the W3 fixture.

**Evidence:**
- W3 research document (wave-3-declaration-values-research.md) explicitly lists W3 coverage: "declarations, css_variables, calc_expressions, var_url_functions, color_functions".
- Bench source: `nonjson_css_l4.rs` lines 604-606 show `declaration_values_extended_track1_facts()` is the single parser called for W3.
- Report generation: `write_declaration_values_extended_report_with_quick_measurement()` (lines ~1300) measures one grouped row and writes to a single report JSON.

**Verdict: NOT OVERFIT by row duplication. This is intended: grouped measurement + multi-feature rolling delta rows per SPEC Section 14.**

---

### Cluster B: Four rows with 225.89 / 115.53 / 110.37

**Rows:**
- css_l4/gradients/direct_to_struct/main
- css_l4/transforms/direct_to_struct/main
- css_l4/filters/direct_to_struct/main
- css_l4/easing_functions/direct_to_struct/main

**Source:** SK-V13-W4 (visual_functions). Single fixture: 357 bytes, sha256=5dc7cc...

**Theory B1: One corpus, four rolling-delta rows**
LIKELY CORRECT. W4 research document (wave-4-visual-pack-research.md) section "Selected Research Conclusion" explicitly states:

> "The row should cover exactly these feature rows: gradients, transforms, filters, easing_functions."

**Evidence:**
- Bench source lines 138-179 show a single `visual_input` fixture measured against `visual_functions_track1_facts()`, `visual_functions_oracle_facts()`, and `visual_functions_lightningcss_facts()`.
- Report generation `write_visual_functions_report_with_quick_measurement()` produces one row with `covered_feature_rows = ["gradients", "transforms", "filters", "easing_functions"]` (lines ~1529-1534).

**Verdict: NOT OVERFIT. Grouped design is intentional.**

---

### Cluster C: Two rows with 26894.88 / 596.05 / 26298.83

**Rows:**
- css_l4/stylesheet_root/direct_to_struct/main
- css_l4/selectors/direct_to_struct/main
- css_l4/pseudo_classes/direct_to_struct/main
- css_l4/pseudo_elements/direct_to_struct/main
- css_l4/attribute_selectors/direct_to_struct/main

**Actually five rows; rolling delta shows only two.** The delta table in ROLLING-SOTA-DELTA.md cuts the list after line 88. Expanding:

```
| css_l4/stylesheet_root/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
...
| css_l4/pseudo_classes/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/pseudo_elements/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
| css_l4/attribute_selectors/direct_to_struct/main | css_l4_parity | 26894.88 | 596.05 | 26298.83 | ADMITTED |
```

**Source:** SK-V13-W2 (stylesheet_selectors). Single fixture: 117 bytes, sha256=7fc890...

**Theory C1: Five rolling-delta rows from one grouped measurement**
LIKELY CORRECT. W2 research (wave-2-stylesheet-selectors-research.md) section 5 states:

> "A small W2 fixture can exercise the required selector families without relying on lightningcss recovery mode: type selectors, classes, ids, child/descendant/adjacent/sibling combinators, attributes, pseudo-classes, and pseudo-elements."

**Evidence:**
- Bench source lines 40-84 show single `selector_input` fixture.
- Report generation `write_stylesheet_selectors_report_with_quick_measurement()` should produce one row. Examining the bench source, the writer is not shown in excerpt, but the Criterion group is `nonjson_css_l4_w2`, single group.

**Verdict: NOT OVERFIT. Grouped by design.**

---

## §3 Suspicious-Throughput Rows

### Row: stylesheet_root, selectors (+ pseudo_classes, pseudo_elements, attribute_selectors) @ 26894.88 Mbps

**Raw data:**
- Fixture: 117 bytes
- Track1 Mbps: 26894.88
- lightningcss Mbps: 596.05

**Calculation check (formula: mbps = bytes * 8_000 / elapsed_ns):**
- To achieve 26894.88 Mbps on 117 bytes:
  - elapsed_ns = 117 * 8_000 / 26894.88 ≈ 34.7 nanoseconds per parse

**Is this plausible?**
- lightningcss_mbps = 596.05 → elapsed_ns ≈ 1563 ns
- Track1 is 45× faster → ~34.7 ns per parse
- lightningcss parses the full stylesheet AST; Track1 parses a fact stream
- For a 117-byte input with minimal nesting, 34.7 ns is plausible if Track1 is a hand-optimized token scanner, not a full AST builder

**Concern: measure_mbps uses black_box and iterations. Check sample_cost from report.**
Bench source line 1752-1759 shows `sample_cost` includes `track1_ns`, `oracle_ns`, `lightningcss_ns`. The W2 report (if generated) should record these.

**Verdict: UNVERIFIABLE from static analysis. Benchmark artifact (Criterion report + sample_cost field in JSON report) would verify. If W2 report exists in restart/skinny/tranches/sk-v13/research/w2/, cross-check the elapsed_ns and iteration count.**

---

### Row: at_rules_keyframes, media_queries @ 21584.64 Mbps

**Raw data:**
- Fixture: 85 bytes
- Track1 Mbps: 21584.64
- lightningcss Mbps: 254.22

**Calculation check:**
- To achieve 21584.64 Mbps on 85 bytes:
  - elapsed_ns = 85 * 8_000 / 21584.64 ≈ 31.5 ns per parse

**Concern:** Even faster than W2, on a smaller fixture (85 vs 117 bytes). lightningcss is only 84× slower (21584 / 254).

**Verdict: UNVERIFIABLE from static analysis. Criterion artifact would verify.**

---

### Row: nested_rules (+ logical_properties, grid, flexbox, typed_property_groups) @ 52233.54 Mbps

**Raw data:**
- Fixture: 351 bytes (largest CSS fixture)
- Track1 Mbps: 52233.54
- lightningcss Mbps: 422.16

**Calculation check:**
- To achieve 52233.54 Mbps on 351 bytes:
  - elapsed_ns = 351 * 8_000 / 52233.54 ≈ 53.7 ns per parse

**Concern:** lightningcss is only 123× slower. With nested rules, AST traversal, grid/flex layout facts, and typed property groups, lightningcss overhead is understandable. But 52 ns to parse a 351-byte stylesheet with 3 top-level rules, 1 nested rule, and 14 declarations seems low.

**Comparator check:** W10.3 research (wave-10-3-nested-layout-research.md) section "Candidate Row" lists the fixture structure. Bench source lines 750-764 show the lightningcss call is `StyleSheet::parse(...) + validate_nested_layout_lightningcss_ast()`. The AST validation is non-trivial.

**Verdict: UNVERIFIABLE from static analysis. Criterion artifact + sample_cost field required.**

---

## §4 Grammar-Derived vs Hand-Curated

**All CSS L4 rows use generated parsers.** No hand-curated scanners or Ad-hoc parsers.

| Wave | Row ID | Generated Module | Templates | Provider | Status |
|---|---|---|---|---|---|
| W1b-1 | css_l4/declaration_values/... | runtime::generated_css_l4_declaration_values | crates/codegen/src/css_l4_declaration_values_templates/ | RuntimeProvider::CssL4DeclarationValues | GENERATED |
| W2 | css_l4/stylesheet_and_selectors/... | runtime::generated_css_l4_stylesheet_selectors | crates/codegen/src/css_l4_stylesheet_selectors_templates/ | RuntimeProvider::CssL4StylesheetSelectors | GENERATED |
| W3 | css_l4/declaration_values_extended/... | runtime::generated_css_l4_declaration_values_extended | crates/codegen/src/css_l4_declaration_values_extended_templates/ | RuntimeProvider::CssL4DeclarationValuesExtended | GENERATED |
| W4 | css_l4/visual_functions/... | runtime::generated_css_l4_visual_functions | crates/codegen/src/css_l4_visual_functions_templates/ | RuntimeProvider::CssL4VisualFunctions | GENERATED |
| W10.1 | css_l4/at_rules_and_media/... | runtime::generated_css_l4_at_rules_and_media | crates/codegen/src/css_l4_at_rules_and_media_templates/ | RuntimeProvider::CssL4AtRulesAndMedia | GENERATED |
| W10.2 | css_l4/vendor_and_custom_atrules/... | runtime::generated_css_l4_vendor_and_custom_atrules | crates/codegen/src/css_l4_vendor_and_custom_atrules_templates/ | RuntimeProvider::CssL4VendorCustom | GENERATED |
| W10.3 | css_l4/nested_layout/... | runtime::generated_css_l4_nested_layout | crates/codegen/src/css_l4_nested_layout_templates/ | RuntimeProvider::CssL4NestedLayout | GENERATED |

**Generator mechanism:** Each row's codegen path is registered in skinny/crates/codegen/src/lib.rs under a RuntimeProvider branch. The provider instantiates a GrammarConfig and template expansion. No grammar metadata audit is in scope here (that is W0/W1a validation); we verify that *measured rows exist and map to generators*.

**Source proof:**
- nonjson_css_l4.rs imports `use runtime::generated_css_l4_*` for each row (lines 25-31).
- The generated_css_l4_* modules are in skinny/crates/runtime/src/grammars/css_l4_*/generated.rs (confirmed by generated_module_stats() functions).

**Verdict: All measured rows are generated, not hand-curated. Grammar-driven generation is confirmed by import statements and module directory structure.**

---

## §5 Comparator Integrity

All CSS rows use lightningcss as the same-run strict comparator. Evidence:

| Wave | lightningcss binding | Same-run? | Plane match? |
|---|---|---|---|
| W1b-1 | lightningcss_facts() calls StyleSheet::parse() + fixture sidecar | YES | css_l4_declaration_value_fact_stream |
| W2 | stylesheet_selectors_lightningcss_facts() calls StyleSheet::parse() + oracle facts | YES | css_l4_stylesheet_selector_fact_stream |
| W3 | declaration_values_extended_lightningcss_facts() calls StyleSheet::parse() + oracle facts | YES | css_l4_declaration_value_extended_fact_stream |
| W4 | visual_functions_lightningcss_facts() calls StyleSheet::parse() + oracle facts | YES | css_l4_visual_function_fact_stream |
| W10.1 | at_rules_and_media_lightningcss_facts() calls StyleSheet::parse() + AST validation | YES | css_l4_at_rules_media_fact_stream |
| W10.2 | vendor_custom_lightningcss_facts() calls StyleSheet::parse() + AST validation | YES | css_l4_vendor_custom_fact_stream |
| W10.3 | nested_layout_lightningcss_facts() calls StyleSheet::parse() + AST validation | YES | css_l4_nested_layout_fact_stream |

**Same-run verification:** All lightningcss calls are in the bench harness, not sidecar/historical. Lines 1520-1521 (W4 example):
```rust
let lightning_measure = measure_mbps(input.as_str(), |input| {
    visual_functions_lightningcss_facts(input).map_err(|error| error.to_string())
});
```

The measure_mbps function (lines 3093-3116) runs the parser in the same bench session, with black_box, iterations, and elapsed timing.

**Threshold validation:** Each report sets `threshold_mbps = lightning_measure.mbps + 1.0` (line 1524, W4 example). The rolling delta margin is `T1_current - T1_sota = Track1_mbps - (lightningcss_mbps + 1)`. All admissions show positive margins, consistent with threshold passing.

**Verdict: lightningcss comparator is same-run, strictly measured, and threshold-gated. No sidecar/historical artifacts.**

---

## §6 Overfit Verdict

**SUMMARY TABLE**

| Feature Row(s) | Grouped Row | Fixture Bytes | Track1 Mbps | lightningcss Mbps | Margin | Verdict |
|---|---|---:|---:|---:|---:|---|
| declaration_values | W1b-1 | 187 | 434.13 | 169.23 | 264.90 | HONEST |
| declarations, css_variables, calc_expr, var_url_func, color_func | W3 | 305 | 265.72 | 55.91 | 209.81 | HONEST-GROUPED |
| stylesheet_root, selectors, pseudo_{class,elem}, attr_selectors | W2 | 117 | 26894.88 | 596.05 | 26298.83 | SUSPICIOUS-THROUGHPUT |
| at_rules_keyframes, media_queries | W10.1 | 85 | 21584.64 | 254.22 | 21330.42 | SUSPICIOUS-THROUGHPUT |
| vendor_prefixes, custom_at_rules | W10.2 | 162 | 34635.22 | 278.74 | 34356.48 | SUSPICIOUS-THROUGHPUT |
| gradients, transforms, filters, easing_func | W4 | 357 | 225.89 | 115.53 | 110.37 | HONEST |
| nested_rules, logical_prop, grid, flexbox, typed_prop_groups | W10.3 | 351 | 52233.54 | 422.16 | 51811.38 | OVERFIT-THROUGHPUT |

### Verdict Details

**HONEST (declaration_values, W4 visual_functions):**
- Track1 Mbps are in reasonable range (225–434) relative to fixture size and feature complexity.
- Ratios to lightningcss are moderate (2.5–2.6×).
- No evidence of corpus reuse; each has distinct grammar features.

**HONEST-GROUPED (W3 declaration_values_extended):**
- Multiple rolling-delta feature rows mapped to one grouped measurement is intentional per SPEC Section 14.
- The fixture exercise all five claimed feature families (var(), calc(), color-mix(), url(), string, escaped ident).
- No overfit; design is as-specified.

**SUSPICIOUS-THROUGHPUT (W2, W10.1, W10.2):**
- Track1 Mbps are extremely high relative to fixture size: 26894 (117 bytes), 21584 (85 bytes), 34635 (162 bytes).
- lightningcss is 45–124× slower; the delta is suspiciously large.
- **Root cause:** These are *micro-benchmarks on toy fixtures*, not production corpora. The fixtures are hardcoded to minimum viable size (e.g., W10.1 is 85 bytes: `@media... { ... } @keyframes...`). With such tiny inputs, measurement overhead (Criterion setup, black_box, cache effects) dominates, inflating Mbps.
- **Risk:** If the rolling delta is intended to represent production throughput, these rows are NOT representative. If the rolling delta is a micro-benchmark gate, they are HONEST.
- **Mitigation:** Criterion artifacts (target/criterion/nonjson_css_l4_w2, etc.) should show iteration count, warmup time, sample_size. If iterations are <100 or sample_count is artificially low, throughput is inflated.
- **Verdict:** Require artifact verification. Flag as SUSPICIOUS until Criterion reports are audited.

**OVERFIT-THROUGHPUT (W10.3 nested_layout @ 52233.54 Mbps):**
- Fixture is largest (351 bytes), but Track1 Mbps is also largest (52233).
- Normalized to bytes: 52233 / 351 ≈ 148.8 Mbps per 100 bytes. Compare to W4 visual (225.89 / 357 ≈ 63.3 per 100 bytes).
- W10.3 is 2.35× faster per byte despite identical feature complexity to W4.
- lightningcss is 123× slower, suggesting lightningcss does significantly more work (nested rule traversal, typed property group classification).
- **Concern:** Track1 parser for W10.3 may be using a fast-fail return or token-only counter, not full semantic parsing. The grammar module should be audited for sink behavior (does it actually build facts, or count tokens?).
- **Mitigation:** Inspect skinny/crates/runtime/src/grammars/css_l4_nested_layout/parser.rs and sink.rs. Verify that every declared fact (nested_rules, logical_properties, grid declarations, flexbox declarations, typed_property_groups) is actually emitted, not skipped.
- **Verdict:** OVERFIT unless parser audits confirm full semantic fact emission.

---

## Largest-Flagged Row

**css_l4/nested_layout/direct_to_struct/main (W10.3) @ 52233.54 Mbps**

- Fixture: 351 bytes
- T1_current: 52233.54 Mbps
- T1_sota: 422.16 Mbps (lightningcss)
- Margin: 51811.38 Mbps

**Flag reason:** Extreme throughput anomaly combined with highest-complexity feature set (5 sub-features, nested structures). Requires sink verification to confirm honest measurement.

---

## Recommended Actions

1. **§3 Verification:** Retrieve Criterion reports from target/criterion/nonjson_css_l4_w2, nonjson_css_l4_w10_1, nonjson_css_l4_w10_2, nonjson_css_l4_w10_3. Check iteration count, sample_size, and elapsed_ns_per_iteration.

2. **§6 Nested-Layout Audit:** Read skinny/crates/runtime/src/grammars/css_l4_nested_layout/sink.rs. Confirm all fact emissions (nested_rules, logical_properties, grid/flex/color/font/border property groups) are unconditionally emitted, not conditional-fast-fail.

3. **Margin Revalidation:** If Criterion artifacts confirm inflated sample_count or low iteration, re-measure with production-like corpora (>1 KB, realistic nesting depth, property diversity).

4. **Rolling-Delta Contract:** Clarify whether rolling-delta feature rows are:
   - (A) Individual measured rows (each has independent throughput), or
   - (B) Rolling references to grouped measured rows (feature rows share Mbps).
   
   Current design is (B); ensure admission gate enforces that each feature row cites its group row source.

