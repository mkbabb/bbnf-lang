# SK-V13 W3 Redress — CSS Declaration-Values Extended

Disposition: `PASS-ADMIT-CANDIDATE`.

W3 lands the generated `css_l4_declaration_values_extended` runtime row and
gate-consumed report for the declaration-values extended feature set:
`declarations`, `css_variables`, `calc_expressions`, `var_url_functions`, and
`color_functions`.

Measured row:

| row | Track 1 Mbps | cssparser Mbps | lightningcss Mbps | threshold | margin |
|---|---:|---:|---:|---:|---:|
| `css_l4/declaration_values_extended/direct_to_struct/main` | 265.724931 | 94.409640 | 54.913964 | 55.913964 | 209.810967 |

Strict equality:

- `track1=cssparser=lightningcss`.
- Fact-stream SHA-256:
  `a39c3cf33479015fa1195f857ac2c2d84cf43977489cc97fe4a1f6f3b99038c9`.
- Input SHA-256:
  `399593fe9848954d3570c67a588a7c352e252327f60445f3bc0670c11df88d64`.

Verification:

- `cargo test -p codegen css_l4_declaration_values_extended --lib`
- `cargo test -p bbnf-bench --lib nonjson_css_l4::tests::declaration_values_extended`
- `cargo test -p bbnf-bench --lib nonjson_css_l4::tests::writes_gate_consumed_declaration_values_extended_report`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-declaration-values-extended-report ../restart/skinny/tranches/sk-v13/research/w3/skv13-W3-css-l4-declaration-values-extended.json`
