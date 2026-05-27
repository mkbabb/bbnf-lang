# SK-V14 W11I JSON parse_only Array Value-Byte Carry Reject

Date: 2026-05-27.

## Disposition

W11I tested generated `parse_only` array comma-to-next-value byte carry. The
candidate changed `parse_only_consume_array_next` to return the already-found
next value byte after comma whitespace and changed `ArrayAfterValue` to feed
that byte into all value arms through `parse_only_begin_value_with_byte`.

The route is distinct from W11H: it does not alter object key-colon handling
or carry the post-colon value byte. It also avoids W11D delimiter threading,
W11F object-member string/object fast arms, object comma key specialization,
structural pre-scans, cursor-return ABI changes, and W10AA object-loop
cleanup.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11I-parse-only-array-value-carry-rejected.patch` with SHA-256
`2ad5a499b1f4deae57aa0fd2cdf4ea733bd49627a5efbf89c02066090c185c64`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12477.296 | 16267.349 | 16268.349 | -3791.053 |
| github_events | 14067.197 | 17130.163 | 17131.163 | -3063.966 |
| update_center | 10326.688 | 14636.777 | 14637.777 | -4311.089 |
| random | 8598.184 | 10783.814 | 10784.814 | -2186.630 |
| gsoc-2018 | 22762.956 | 36220.848 | 36221.848 | -13458.892 |
| distinct_values | 6726.396 | 11824.367 | 11825.367 | -5098.971 |

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
`distinct_values`. Future array byte-carry routes must add a fresh material
differential beyond returning the already-found post-comma value byte.
