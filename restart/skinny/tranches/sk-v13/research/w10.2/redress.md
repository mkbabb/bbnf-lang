# SK-V13 W10.2 Redress - CSS Vendor And Custom At-Rules

## Scope

W10.2 admits the CSS L4 vendor/custom generated row:

- `css_l4/vendor_and_custom_atrules/direct_to_struct/main`
- covered feature rows: `vendor_prefixes`, `custom_at_rules`
- gate: `G-W10-2-CSS-VENDOR-CUSTOM`
- report:
  `restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`

## Implementation

The wave adds the `css_l4_vendor_and_custom_atrules` grammar profile in codegen
and runtime, wires the benchmark/report/gate consumer, and records the feature
rows in `skinny/RESULTS.md` plus `restart/skinny/ROLLING-SOTA-DELTA.md`.

The generated parser emits schema `css-l4-vendor-custom-facts-v1` over a
canonical fixture plus a scanner fallback for the same profile. The fact stream
records `@custom-media`, a vendor-prefixed `@-webkit-keyframes`, and vendor
prefixed declarations while the lightningcss sidecar validates the same-plane
source shape. In this lightningcss release, `@custom-media` is retained as a
typed unknown at-rule; the gate checks that prelude explicitly instead of
claiming a false `CssRule::CustomMedia` projection.

## Measurement

Criterion with `RUSTFLAGS="-C target-cpu=native"` records:

| metric | value |
|---|---:|
| Track 1 | 34635.2188713192 Mbps |
| golden oracle | 1053.882780028159 Mbps |
| lightningcss | 277.74217938286023 Mbps |
| threshold | 278.74217938286023 Mbps |
| margin | 34356.47669193634 Mbps |

Strict equality: `pass:track1=golden=lightningcss`.

Fact-stream SHA-256:
`b5e80e079438e9adbd478aee73e33fb6d02d69ebe1bf32e939db7a59ffe88da3`.

Generated size guard: `generated_loc=996`, `generated_module_bytes=32404`,
`pass:generated_loc<=1050`.

## Verification

- `cargo test -p runtime css_l4_vendor_and_custom_atrules`
- `cargo test -p codegen css_l4_vendor_and_custom_atrules --lib`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib vendor_custom`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson_css_l4_w10_2`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-vendor-custom-report ../restart/skinny/tranches/sk-v13/research/w10.2/skv13-W10.2-css-l4-vendor-custom.json`

## Disposition

PASS-ADMIT-CANDIDATE. `REDRESS-134` records the admission, and the rolling CSS
SOTA delta now marks `vendor_prefixes` and `custom_at_rules` as ADMITTED.

Routed remainder: no SIMD/ASM claim lands in W10.2. Lock 16 remains
`n/a:no_simd_or_asm_claim`, and the remaining CSS parity rows stay in the W10
fan-out.
