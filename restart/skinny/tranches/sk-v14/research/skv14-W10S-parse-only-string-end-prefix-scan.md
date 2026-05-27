# SK-V14 W10S Redress: JSON parse_only String-End Prefix Scan

Date: 2026-05-27.

Status: post-W10R implementation residual close. REDRESS-219 admits one
additional JSON `parse_only` row and leaves nine rows open.

## Source Change

`parse_that_regex` now exposes a parse-only raw-end trusted string matcher and
a word-mask tiny-string prefix scanner. The generated JSON `parse_only` path
uses the prefix scan to return tiny plain strings without a byte loop and to
resume the trusted raw-end scanner at the first unconsumed byte for longer or
escaped strings.

The change stays on the strict parse_only plane. It does not build an offset
tape, does not allocate a DOM, and does not alter comparator binding.
`cargo xtask regen-json` refreshed `skinny/crates/runtime/src/grammars/json/`
from `skinny/crates/codegen/src/runtime_generator.rs`.

## Cold Evidence

Evidence artifact:
`restart/skinny/tranches/sk-v14/research/skv14-W10S-parse-only-string-end-profile-direct.tsv`.

All rows were measured with `iters=400`, `warmup_iters=0`,
`RUSTFLAGS=-C target-cpu=native`, and the release `profile_direct` binary.

| row | Track 1 Mbps | Skipper Mbps | margin vs Skipper + 1 | disposition |
|---|---:|---:|---:|---|
| json/unicode_mixed/parse_only/main | 7379.340 | 7011.268 | 367.072 | ADMIT |

## Verification

- `cargo xtask check-json`
- `RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' cargo build --release -p bbnf-bench --bin profile_direct`
- `cargo test -p parse-that-regex trusted_ -- --nocapture`
- `cargo test -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`

## Routed Remainder

JSON `parse_only` now stands at 8 / 17 admitted. The remaining nine rows stay
open as measured residuals under REDRESS-219; none has an architectural-level
intrinsic-block proof.
