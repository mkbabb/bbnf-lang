# SK-V14 W11D JSON parse_only Threaded Context Reject

Date: 2026-05-27.

## Disposition

W11D tested a generated JSON `parse_only` route that threads value context
through the iterative parser. The candidate lets completed scalar values and
empty containers immediately consume their enclosing object/array delimiter
instead of returning through the generic after-value frame first. Key-colon
parsing still stops after colon whitespace; the candidate does not carry the
next value byte, does not add a structural pre-scan, does not change the
cursor-return ABI, and does not reattempt the W10AA fused string/object-loop
route.

The route is rejected. Correctness held, but same-binary cold native
`profile_direct` evidence did not admit any of the six remaining parse_only
rows. The transient source patch was reverted and retained at
`/tmp/skv14-W11D-parse-only-threaded-context-rejected.patch` with SHA-256
`98b9494008e0d810699788c1ed8c667b2de29727301be6d27b3f6cf65d2b7146`.

## Evidence

| corpus | Track 1 Mbps | sonic Mbps | floor | margin |
|---|---:|---:|---:|---:|
| twitter | 12546.497 | 16444.461 | 16445.461 | -3898.964 |
| github_events | 14024.554 | 17239.857 | 17240.857 | -3216.303 |
| update_center | 10331.072 | 14561.737 | 14562.737 | -4231.665 |
| random | 8426.348 | 10758.538 | 10759.538 | -2333.190 |
| gsoc-2018 | 22287.200 | 36130.468 | 36131.468 | -13844.268 |
| distinct_values | 6635.355 | 11892.741 | 11893.741 | -5258.386 |

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
`distinct_values`. Future parse_only source attempts must avoid repeating this
context-threaded delimiter-consumption route unless they add a fresh material
differential and rerun same-binary cold evidence.
