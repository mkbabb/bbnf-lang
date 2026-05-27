# SK-V14 W11Q Parse-Only Indexed Strings Reject

Date: 2026-05-27.

Status: REJECT. No source patch lands, no `skinny/RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Candidate

W11Q tested an indexed parse_only route for JSON strings. The transient source
extended the JSON structural scanner to produce structural quote/punctuation
positions plus string starts that contain escapes or control bytes. Generated
`parse_only` then used the index to skip full string validation for proven
plain strings while preserving the existing validator for risky strings. UTF-8
validation remained in `parse_only_bytes`; numbers, literals, delimiters, and
EOF stayed on the existing generated parse_only validators.

This is materially distinct from REDRESS-224 through REDRESS-240: it is not
inline frame-stack work, whitespace64, object-member fast arms, key-colon
fusion, value-byte carry, array carry, or object key-start specialization. It
also differs from W10Y/W10Z because it carried a scanner-proven risky-string
side table instead of assuming plain strings globally.

The transient patch was reverted after measurement and retained at
`/tmp/skv14-W11Q-parse-only-indexed-strings-rejected.patch` with SHA-256
`cd8620ba8f53caa51851069eb83d114ce73968f1edfff6231d32b5d422436a52`.

## Correctness Gates

- `CARGO_TARGET_DIR=/tmp/skv14-w11q-regen-json3-target cargo run --profile ax-iter -p xtask -- regen-json`
- `CARGO_TARGET_DIR=/tmp/skv14-w11q-check-json3-target cargo run --profile ax-iter -p xtask -- check-json`
- `CARGO_TARGET_DIR=/tmp/skv14-w11q-runtime-test3-target cargo test --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11q-codegen-test4-target cargo test --profile ax-iter -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`

All correctness gates passed before measurement.

## Cold Evidence

Release-native no-warm profile command:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11q-profile-target RUSTFLAGS="-C target-cpu=native" cargo build --release -p bbnf-bench --bin profile_direct
for corpus in twitter github_events update_center random gsoc-2018 distinct_values canada instruments apache_builds citm_catalog; do
  for mode in parse_only_track1 parse_only_track2 parse_only_sonic parse_only_serde; do
    /tmp/skv14-w11q-profile-target/release/profile_direct 400 "$corpus" "$mode" 0
  done
done
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11Q-parse-only-indexed-strings.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11Q-parse-only-indexed-strings.raw.log`

Residual row results against the `sonic + 1.0` floor:

| Row | Track 1 Mbps | Sonic Mbps | Floor | Margin |
|---|---:|---:|---:|---:|
| `twitter/parse_only` | 7452.818 | 10576.937 | 10577.937 | -3125.119 |
| `github_events/parse_only` | 8989.536 | 11618.018 | 11619.018 | -2629.482 |
| `update_center/parse_only` | 7224.976 | 12737.485 | 12738.485 | -5513.509 |
| `random/parse_only` | 7271.271 | 9726.409 | 9727.409 | -2456.138 |
| `gsoc-2018/parse_only` | 11251.547 | 27975.548 | 27976.548 | -16725.001 |
| `distinct_values/parse_only` | 8951.894 | 10467.320 | 10468.320 | -1516.426 |

The guard rows `canada`, `instruments`, `apache_builds`, and `citm_catalog`
also failed to clear same-run sonic under this route, so the candidate gives
no compensating guard-row evidence.

## Disposition

W11Q proves that indexed plain-string skipping is not sufficient to admit any
remaining parse_only residual row. The route is pre-blocked without a fresh
material differential. Current JSON parse_only state remains 11 / 17
ADMITTED and 6 OPEN: `twitter`, `github_events`, `update_center`, `random`,
`gsoc-2018`, and `distinct_values`.
