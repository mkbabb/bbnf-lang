# SK-V14 W8 Redress: CSS L4 Production Re-Admit Full-Parse Gap

Date: 2026-05-26.

Disposition: REJECT.

## Gate

`G-SK-V14-W8-CSS-L4-READMIT`

W8 requires every CSS L4 re-admit candidate to run against
`skinny/corpora/css-l4-sk-v14/` with at least 800 KiB of production CSS, a
lightningcss full-parse comparator, a cssparser full-parse oracle, and a Track
1 generated grammar-derived path on the same full-parse plane. Fact-stream vs
full-AST asymmetry is pre-blocked.

## Executable Probe

W8 added `skinny/crates/bbnf-bench/src/css_l4_w8.rs`, exported from
`skinny/crates/bbnf-bench/src/lib.rs`.

The probe performs the W8 attempt directly:

- loads all four SK-V14 CSS L4 production corpus files;
- verifies the corpus byte floor (`979638 >= 819200`);
- parses every corpus with `lightningcss::stylesheet::StyleSheet::parse`;
- parses every corpus with a cssparser stylesheet probe;
- executes all seven post-W7 generated CSS runtime profiles over every corpus;
- classifies the Track 1 outputs by their generated fact-stream markers.

## Evidence

Command:

```sh
cargo test -p bbnf-bench css_l4_w8 -- --nocapture
```

Result:

- `css_l4_w8::tests::css_l4_w8_production_attempt_rejects_fact_stream_track1`
  passed.
- corpus files: 4.
- corpus bytes: 979638.
- lightningcss full-parse passes: 4 / 4.
- cssparser full-parse passes: 4 / 4.
- Track 1 generated profile runs: 28 / 28.
- Track 1 fact-stream runs: 28 / 28.
- admitted rows: 0 / 24.

## Finding

The production corpus and both full-parse comparators are available. The
post-W7 Track 1 CSS runtime surface is not a CSS full-parse plane. It emits
generated fact-stream rows with W7 policy and frontend metadata for all seven
CSS profiles:

- `css_l4_declaration_value_fact_stream`
- `css_l4_declaration_value_extended_fact_stream`
- `css_l4_stylesheet_selector_fact_stream`
- `css_l4_visual_function_fact_stream`
- `css_l4_at_rules_media_fact_stream`
- `css_l4_vendor_custom_fact_stream`
- `css_l4_nested_layout_fact_stream`

That is useful PRUNE evidence, but it is not W8 admit evidence. Re-labelling
those rows as full-parse equality would repeat the SK-V13 fact-stream/full-AST
asymmetry failure.

## Disposition

All 24 CSS L4 W8 rows remain non-admitted. `skinny/RESULTS.md` and
`restart/skinny/ROLLING-SOTA-DELTA.md` remain at their existing
`AUDIT-FALSIFIED` / `OPEN` posture for CSS L4; no CSS row is flipped to
`AUDIT-SUSTAINED`.

## Corrective Route

A future CSS re-admit wave needs a generated Track 1 CSS full-parse path on the
same plane as the lightningcss/cssparser comparators. The acceptable route is
to extend the grammar-derived generator/runtime surface so the emitted CSS
consumer validates full stylesheet structure and exposes a same-plane equality
surface. Hand-authored profile templates, CANONICAL_FIXTURE short-circuits,
tiny fixtures, or fact-stream adapters remain rejected.
