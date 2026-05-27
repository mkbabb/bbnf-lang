# SK-V14 W11J JSON parse_only Object Key Specialization Reject

Date: 2026-05-27.

## Disposition

W11J tested generated `parse_only` object comma-to-next-key specialization.
The candidate split the post-object-value delimiter state so a comma path
could skip following whitespace, require the next key quote, and dispatch
directly into key parsing instead of returning through the generic
`ObjectExpectKey` state.

The route is distinct from W11G and W11H: it does not fuse key-string plus
colon handling and does not carry the post-colon value byte. It is also
distinct from W11D delimiter threading, W11F object-member string/object fast
arms, W11I array value-byte carry, structural pre-scans, cursor-return ABI
changes, and W10AA object-loop cleanup.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11J-parse-only-object-key-specialization-rejected.patch` with
SHA-256
`a1428c1561d4baaaff5dc8049796aaa87a6aa5cdcbef95199f557a8b075ecb5b`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12796.489 | 16444.898 | 16445.898 | -3649.409 |
| github_events | 14102.875 | 17519.443 | 17520.443 | -3417.568 |
| update_center | 10451.067 | 14448.873 | 14449.873 | -3998.806 |
| random | 8548.923 | 10704.985 | 10705.985 | -2157.062 |
| gsoc-2018 | 22744.353 | 36518.232 | 36519.232 | -13774.879 |
| distinct_values | 6805.900 | 12062.376 | 12063.376 | -5257.476 |

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
`distinct_values`. Future object key-start specialization routes must add a
fresh material differential beyond splitting the comma-to-next-key path.
