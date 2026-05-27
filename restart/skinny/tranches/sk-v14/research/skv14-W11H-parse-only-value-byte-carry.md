# SK-V14 W11H JSON parse_only Value-Byte Carry Reject

Date: 2026-05-27.

## Disposition

W11H tested generated `parse_only` object-member value-byte carry. The
candidate changed `parse_only_key_colon` to validate the key string, consume
the colon, skip post-colon whitespace, return the first value byte, and feed
that byte into all value arms through a new `parse_only_begin_value_with_byte`
helper.

The route is distinct from W11F and W11G: it is not limited to string/object
member fast arms and it does carry the post-colon value byte. It still avoids
W11D delimiter threading, array comma carry, object comma key carry,
structural pre-scans, cursor-return ABI changes, and W10AA object-loop
cleanup.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11H-parse-only-value-byte-carry-rejected.patch` with SHA-256
`eb79dd2154f972812478f2b191583b8a457fb8740fc4d14979fddb2dd81f08d8`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12633.652 | 16225.132 | 16226.132 | -3592.480 |
| github_events | 14161.872 | 17500.027 | 17501.027 | -3339.155 |
| update_center | 10388.135 | 14653.580 | 14654.580 | -4266.445 |
| random | 8512.116 | 10837.393 | 10838.393 | -2326.277 |
| gsoc-2018 | 22746.743 | 36312.402 | 36313.402 | -13566.659 |
| distinct_values | 6694.096 | 12064.448 | 12065.448 | -5371.352 |

All rows used 1000 cold iterations with `warmup_iters=0`.

## Verification

- `cargo xtask regen-json`
- `cargo xtask check-json`
- `cargo test --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test --profile ax-iter -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`
- `cargo build --release -p bbnf-bench --bin profile_direct`

## Residual State

JSON parse_only remains 11 / 17 ADMITTED and 6 / 17 OPEN:
`twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
`distinct_values`. Future value-byte routes must add a fresh material
differential beyond object-member key-colon carry.
