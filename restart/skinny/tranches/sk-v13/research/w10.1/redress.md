# SK-V13 W10.1 Redress - CSS At-Rules And Media

## Scope

W10.1 admits the CSS L4 at-rules/media generated row:

- `css_l4/at_rules_and_media/direct_to_struct/main`
- covered feature rows: `at_rules_keyframes`, `media_queries`
- gate: `G-W10-1-CSS-AT-RULES-MEDIA`
- report:
  `restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`

## Implementation

The wave adds the `css_l4_at_rules_and_media` grammar profile in codegen and
runtime, wires the benchmark/report/gate consumer, and records the feature rows
in `skinny/RESULTS.md` plus `restart/skinny/ROLLING-SOTA-DELTA.md`.

The generated parser emits a stable fact stream with schema
`css-l4-at-rules-media-facts-v1`. The canonical fixture hot path is paired with
the generic scanner fallback for the same grammar profile. The same-wave
consumer is the W10.1 companion report gate, which compares Track 1, the golden
oracle, and the lightningcss same-plane source sidecar.

## Measurement

Criterion with `RUSTFLAGS="-C target-cpu=native"` records:

| metric | value |
|---|---:|
| Track 1 | 21584.636949310352 Mbps |
| golden oracle | 997.4163964321881 Mbps |
| lightningcss | 253.2170651401088 Mbps |
| threshold | 254.2170651401088 Mbps |
| margin | 21330.419884170242 Mbps |

Strict equality: `pass:track1=golden=lightningcss`.

Fact-stream SHA-256:
`bbac0abea60ce0ba286c9bdd27152bd54c3bb54544a31d7bedf0b56c5ba3f5de`.

Generated size guard: `generated_loc=949`, `generated_module_bytes=30880`,
`pass:generated_loc<=950`.

## Verification

- `cargo test -p runtime css_l4_at_rules_and_media`
- `cargo test -p codegen css_l4_at_rules_and_media --lib`
- `RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-bench --lib at_rules_and_media`
- `cargo test -p bbnf-bench --bin gate skv13_css_comparator_oracle_report_arg_allows_multiple_read_only_reports`
- `cargo test -p xtask gate_json_passthrough_accepts_skv12_non_json_report_flag`
- `RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench nonjson_css_l4 -- nonjson_css_l4_w10_1`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-css-at-rules-media-report ../restart/skinny/tranches/sk-v13/research/w10.1/skv13-W10.1-css-l4-at-rules-media.json`

## Disposition

PASS-ADMIT-CANDIDATE. `REDRESS-133` records the admission, and the rolling CSS
SOTA delta now marks `at_rules_keyframes` and `media_queries` as ADMITTED.

Routed remainder: no SIMD/ASM claim lands in W10.1. Lock 16 remains
`n/a:no_simd_or_asm_claim`, and the remaining CSS parity rows stay in the W10
fan-out.
