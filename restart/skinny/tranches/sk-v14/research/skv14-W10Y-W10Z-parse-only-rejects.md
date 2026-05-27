# SK-V14 W10Y/W10Z JSON parse_only Residual Rejects

Date: 2026-05-27.

Status: REJECT. No source patch lands.

## Scope

After W10W and W10X, six JSON `parse_only` residual rows remained open:
`twitter`, `github_events`, `update_center`, `random`, `gsoc-2018`, and
`distinct_values`.

Two materially distinct source candidates were tested:

1. W10Y plain-string structural fast path. The generated parse-only template
   tried a strict structural-index validator for inputs whose strings had no
   escapes, falling back to the W10W validator for escaped strings.
2. W10Z cursor-return ABI. The generated parse-only template removed the
   mutable `ParseOnlyState` hot-loop ABI and returned cursor offsets from
   helpers, preserving the W10W iterative container stack and all string,
   number, literal, and delimiter semantics.

Both candidates were abrogated after measurement. `runtime_generator.rs`,
`generated_json`, and `scan.rs` were restored before this packet was retained.

## Evidence

Correctness checks run during the candidate cycle:

- `cargo test -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `cargo test -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`
- `cargo xtask check-json`

W10Y raw evidence:
`restart/skinny/tranches/sk-v14/research/skv14-W10Y-parse-only-plain-structural-reject.raw.log`

SHA-256:
`819ae104c4de55e206bc75c82a660a8c91c243977a8becdebaa96e34f5773f45`

W10Z raw evidence:
`restart/skinny/tranches/sk-v14/research/skv14-W10Z-parse-only-cursor-return-reject.raw.log`

SHA-256:
`f79bd1a1203ae0a9be0251753249aeb56622b365af98736441ad27853289f7d2`

## W10Y Result

The structural fast path did not admit its intended no-escape residual rows:

| row | Track 1 Mbps | Skipper Mbps | margin |
|---|---:|---:|---:|
| `random/parse_only` | 8244.645 | 10690.947 | -2446.302 |
| `distinct_values/parse_only` | 9855.480 | 11756.262 | -1900.782 |

It also regressed guard rows in the same binary:

| guard row | Track 1 Mbps | Skipper Mbps | margin |
|---|---:|---:|---:|
| `canada/parse_only` | 10677.550 | 12597.519 | -1919.969 |
| `instruments/parse_only` | 10661.782 | 14747.186 | -4085.404 |

Disposition: reject. Do not reattempt a whole-input structural pre-scan for
`parse_only` unless it eliminates the guard-row regression and admits at least
one residual under the same cold gate.

## W10Z Result

The cursor-return ABI preserved the generated parse-only contract but did not
admit any residual row in the cold 400-iteration sweep:

| row | Track 1 Mbps | Skipper Mbps | margin |
|---|---:|---:|---:|
| `twitter/parse_only` | 12675.609 | 16365.742 | -3690.133 |
| `github_events/parse_only` | 14178.315 | 17366.844 | -3188.529 |
| `update_center/parse_only` | 10360.392 | 14585.898 | -4225.506 |
| `random/parse_only` | 8434.140 | 10759.664 | -2325.524 |
| `gsoc-2018/parse_only` | 22445.264 | 35933.541 | -13488.277 |
| `distinct_values/parse_only` | 6643.804 | 11958.817 | -5315.013 |

Guard rows remained mixed: `canada` and `apache_builds` stayed above Skipper,
while `instruments` remained below same-run Skipper as an unstable guard row.
Because no residual admitted, the source patch was not retained.

## Close State

Current JSON `parse_only` remains 11 / 17 admitted and 6 / 17 open. The open
rows are unchanged: `twitter`, `github_events`, `update_center`, `random`,
`gsoc-2018`, and `distinct_values`.
