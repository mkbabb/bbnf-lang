# SK-V14 W11G JSON parse_only Key-Colon Fusion Reject

Date: 2026-05-27.

## Disposition

W11G tested a generated `parse_only` key-string plus colon fusion. The
candidate changed `parse_only_key_colon` to validate the key string directly
with `parse_only_string_end`, check for the colon at the key end or after
intervening whitespace, and then stop after colon whitespace.

The route is distinct from W11D/W11F/W10AA: it carries no next value byte,
keeps the existing `ObjectAfterValue` delimiter state, does not dispatch
object-member values through string/object fast arms, does not add structural
pre-scans, and does not change the cursor-return ABI or object-loop cleanup
shape.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11G-parse-only-key-colon-fusion-rejected.patch` with SHA-256
`c538adcc2abd703d7fc77a39e546dcfff0e12a15f9ba9edc7d9a21826d42f210`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12679.983 | 16340.466 | 16341.466 | -3661.483 |
| github_events | 14161.888 | 17504.216 | 17505.216 | -3343.328 |
| update_center | 10281.716 | 14301.312 | 14302.312 | -4020.596 |
| random | 8578.566 | 10825.866 | 10826.866 | -2248.300 |
| gsoc-2018 | 22788.403 | 36270.819 | 36271.819 | -13483.416 |
| distinct_values | 6689.145 | 12053.735 | 12054.735 | -5365.590 |

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
`distinct_values`. Future key-colon routes must add a fresh material
differential, with value-byte carry as the next distinct candidate.
