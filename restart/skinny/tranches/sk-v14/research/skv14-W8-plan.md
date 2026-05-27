# SK-V14 W8 Plan - CSS L4 Re-Admit Attempt

Status: DISPATCHED from post-W7 HEAD `672b927d5`.

## Selected Rows

W8 selects all 24 CSS L4 rows listed in `skinny/xtask/src/main.rs`
`SKV13_CSS_FEATURES` and reflected in `skinny/RESULTS.md` under
`css_l4_bench`.

## Exact File Set

Implementation/proof files:

- `skinny/crates/bbnf-bench/src/css_l4_w8.rs`
- `skinny/crates/bbnf-bench/src/lib.rs`

Disposition files, only if the executable attempt does not admit a row:

- `restart/skinny/tranches/sk-v14/research/skv14-W8-redress.md`
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v14/HANDOFF.md`

No generated CSS runtime file is hand-edited in W8. If W8 admits a row,
generated output must come from the existing post-W7 `cargo xtask regen-css`
path. If W8 rejects, W8 records the missing generated full-parse capability
instead of substituting a hand-authored parser or a fact-stream/full-AST
adapter.

## Track Paths

Track 1:

- `runtime::generated_css_l4_declaration_values::parser::parse`
- `runtime::generated_css_l4_declaration_values_extended::parser::parse`
- `runtime::generated_css_l4_stylesheet_selectors::parser::parse`
- `runtime::generated_css_l4_visual_functions::parser::parse`
- `runtime::generated_css_l4_at_rules_and_media::parser::parse`
- `runtime::generated_css_l4_vendor_and_custom_atrules::parser::parse`
- `runtime::generated_css_l4_nested_layout::parser::parse`

Track 2 / comparator:

- `lightningcss::stylesheet::StyleSheet::parse` full parse over
  `skinny/corpora/css-l4-sk-v14/`.
- `cssparser` full-parse probe over the same corpus bytes.

## Admission Rule

No row admits unless the production-corpus attempt proves all of:

- corpus bytes are at least `800 KiB`;
- lightningcss full-parse succeeds for every corpus file;
- cssparser full-parse succeeds for every corpus file;
- Track 1 is a generated grammar-derived full-parse path on the same plane,
  not a fact-stream/full-AST asymmetry;
- per-iteration equality can be evaluated on the same plane.

If any condition fails, W8 is `REJECTED` with an architectural-block proof for
the 24 CSS L4 rows. Tiny fixture parity, old SK-V13 fact-stream rows, and
generated metadata rows are not W8 admit evidence.

## Rollback Boundary

Revert this wave by removing `css_l4_w8.rs`, the `lib.rs` module export, and
the W8 disposition entries. Do not revert W7 policy/union runtime wiring or
pre-existing user dirty files.
