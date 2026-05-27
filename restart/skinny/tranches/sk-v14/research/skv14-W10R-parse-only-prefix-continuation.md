# SK-V14 W10R Redress: JSON parse_only Prefix Continuation

Date: 2026-05-27.

Status: post-W11 implementation residual close. REDRESS-218 admits one
additional JSON `parse_only` row and leaves ten rows open.

## Source Change

`parse_that_regex::match_string_at_quote_after_plain_prefix_trusted_utf8`
continues JSON string matching from a prefix that the caller already proved
contains no quote, escape, or control byte. The generated JSON `parse_only`
path now keeps its tiny-string fast path and, for longer strings, resumes the
SIMD trusted matcher at the first unscanned byte instead of rescanning the same
prefix from the opening quote.

This is a source-level terminal scanner improvement. It does not build an
offset tape, does not alter the comparator plane, and does not touch JSON
templates by hand; `cargo xtask regen-json` refreshed
`skinny/crates/runtime/src/grammars/json/` from
`skinny/crates/codegen/src/runtime_generator.rs`.

## Cold Evidence

Evidence artifact:
`restart/skinny/tranches/sk-v14/research/skv14-W10R-parse-only-profile-direct.tsv`.

All rows were measured with `iters=400`, `warmup_iters=0`,
`RUSTFLAGS=-C target-cpu=native`, and the release `profile_direct` binary.

| row | Track 1 Mbps | Skipper Mbps | margin vs Skipper + 1 | disposition |
|---|---:|---:|---:|---|
| json/canada/parse_only/main | 16709.901 | 12970.929 | 3737.972 | ADMIT |
| json/twitter/parse_only/main | 11353.193 | 14221.309 | -2869.116 | OPEN |
| json/citm_catalog/parse_only/main | 19003.760 | 21705.402 | -2702.642 | OPEN |
| json/apache_builds/parse_only/main | 11163.964 | 12133.978 | -971.014 | OPEN |
| json/github_events/parse_only/main | 12818.237 | 16806.903 | -3989.666 | OPEN |
| json/update_center/parse_only/main | 9530.868 | 14061.055 | -4531.187 | OPEN |
| json/random/parse_only/main | 7557.895 | 10984.641 | -3427.746 | OPEN |
| json/gsoc-2018/parse_only/main | 20758.238 | 33632.883 | -12875.645 | OPEN |
| json/instruments/parse_only/main | 10973.947 | 12988.300 | -2015.353 | OPEN |
| json/unicode_mixed/parse_only/main | 6309.961 | 6351.340 | -42.379 | OPEN |
| json/distinct_values/parse_only/main | 4444.974 | 8783.870 | -4339.896 | OPEN |

The prior six W10 admitted rows remain above Skipper + 1. W10R adds only
`canada`; it does not admit the ten remaining residual rows by implication.

## Verification

- `cargo xtask check-json`
- `RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' cargo build --release -p bbnf-bench --bin profile_direct`
- `cargo test -p parse-that-regex trusted_string_matcher_continues_after_plain_prefix -- --nocapture`
- `cargo test -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`

## Routed Remainder

JSON `parse_only` now stands at 7 / 17 admitted. The remaining ten rows stay
open as measured residuals under REDRESS-218; none has an architectural-level
intrinsic-block proof.
