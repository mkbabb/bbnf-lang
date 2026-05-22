# SK-V13 W4 Redress - CSS Visual Functions

Disposition: `PASS-ADMIT-CANDIDATE`.

W4 lands the generated `css_l4_visual_functions` runtime row and
gate-consumed report for the visual-functions feature set: `gradients`,
`transforms`, `filters`, and `easing_functions`.

Measured row:

| row | Track 1 Mbps | cssparser/golden Mbps | lightningcss Mbps | threshold | margin |
|---|---:|---:|---:|---:|---:|
| `css_l4/visual_functions/direct_to_struct/main` | 225.893652 | 164.868370 | 114.526478 | 115.526478 | 110.367174 |

Strict equality:

- `track1=cssparser=lightningcss`.
- Fact-stream SHA-256:
  `309b08f3da0867a5494316fc5e1ae0d29a1db580a8d508f0c385f63785c262d5`.
- Input SHA-256:
  `5dc7cc1098401900af32b534893c9bd007245f88af3cc683926a4abaf5f531c0`.

Verification:

- `cargo test -p runtime css_l4_visual_functions`
- `cargo test -p codegen css_l4_visual_functions --lib`
- `cargo test -p bbnf-bench --lib visual_functions`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-visual-functions-report ../restart/skinny/tranches/sk-v13/research/w4/skv13-W4-css-l4-visual-functions.json`
