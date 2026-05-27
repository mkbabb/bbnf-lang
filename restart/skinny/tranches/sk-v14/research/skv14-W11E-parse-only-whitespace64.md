# SK-V14 W11E JSON parse_only 64-Byte Whitespace Reject

Date: 2026-05-27.

## Disposition

W11E tested a grammar-neutral 64-byte JSON whitespace skip route using the
existing `bbnf-simd` `byte_class_from_eq_set_64` primitive over the four JSON
whitespace bytes. The candidate replaced the scalar/SWAR space-only helper in
`parse-that-regex::skip_ascii_whitespace`, so it affected generated Track 1
parse_only and the other consumers of that shared primitive.

The route is rejected. Correctness and primitive parity held, but cold native
measurement regressed every remaining parse_only residual row and admitted no
row. The transient source patch was reverted and retained at
`/tmp/skv14-W11E-parse-only-whitespace64-rejected.patch` with SHA-256
`0d07dd3120d54cbf2424c90ba861f134b85081f10840d5df254049ecbad4d47f`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 7279.499 | 15393.239 | 15394.239 | -8114.740 |
| github_events | 8738.756 | 15912.253 | 15913.253 | -7174.497 |
| update_center | 9956.990 | 14299.827 | 14300.827 | -4343.837 |
| random | 4492.659 | 10465.257 | 10466.257 | -5973.598 |
| gsoc-2018 | 16187.535 | 34136.162 | 34137.162 | -17949.627 |
| distinct_values | 4284.404 | 11310.197 | 11311.197 | -7026.793 |

All rows used 1000 cold iterations with `warmup_iters=0`.

## Verification

- `cargo test --profile ax-iter -p parse-that-regex ascii_whitespace_skip_matches_json_space_set -- --nocapture`
- `cargo test --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test --profile ax-iter -p bbnf-simd --test checkasm_byte_class_from_eq_set_64 -- --nocapture`
- `cargo build --release -p bbnf-bench --bin profile_direct`

## Residual State

JSON parse_only remains 11 / 17 ADMITTED and 6 / 17 OPEN:
`twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
`distinct_values`. Future whitespace attempts must not repeat this full
64-byte set-member skip shape without a fresh material differential.
