# SK-V14 W11F JSON parse_only Object-Member Fast Arm Reject

Date: 2026-05-27.

## Disposition

W11F tested a generated `parse_only` object-member fast arm. After
`parse_only_key_colon`, object member values beginning with a string or object
dispatch directly to the string parser or object opener, while arrays,
numbers, literals, and other values fall back to the generic value dispatcher.
The candidate keeps the existing `ObjectAfterValue` delimiter state, so it is
not W11D value-context delimiter threading; it also does not carry a value byte
from key-colon, does not add a structural pre-scan, and does not change the
cursor-return ABI.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11F-parse-only-object-member-fast-arm-rejected.patch` with SHA-256
`78e72f694a683de1a54c4f877205ada36e37e2376e89b904eaf541b28dee9aee`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12586.763 | 16023.519 | 16024.519 | -3437.756 |
| github_events | 13982.841 | 17337.903 | 17338.903 | -3356.062 |
| update_center | 10341.735 | 14430.580 | 14431.580 | -4089.845 |
| random | 8326.377 | 10766.996 | 10767.996 | -2441.619 |
| gsoc-2018 | 22143.235 | 36247.462 | 36248.462 | -14105.227 |
| distinct_values | 6564.118 | 11905.764 | 11906.764 | -5342.646 |

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
`distinct_values`. Future object-member fast-arm attempts must add a fresh
material differential rather than repeating this string/object dispatch split.
