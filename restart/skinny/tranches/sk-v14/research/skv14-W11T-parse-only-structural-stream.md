# SK-V14 W11T Parse-Only Structural Stream Reject

Date: 2026-05-27.

Status: REJECT. No source patch lands, no `skinny/RESULTS.md` row moves, and
`restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

## Candidate

W11T tested a generated parse_only structural-stream route for JSON. The
transient source changed the JSON scanner to emit punctuation plus real quote
positions and a scanner-owned risky-string-start side table. Generated
parse_only then drove object/array delimiters and string close positions from
that structural stream, while preserving fallback validation for escaped or
control-bearing strings.

This differs from W11Q: W11Q kept the existing byte-loop parser and used the
index only to skip plain-string validation. W11T used the structural stream as
the parser driver for containers, delimiters, and string boundaries. It also
differs from W11D through W11J because it does not rely on delimiter context,
whitespace64, value-byte carry, object-member fast arms, key-colon fusion,
array carry, or object key-start specialization.

The transient patch was reverted after measurement and retained at
`/tmp/skv14-W11T-parse-only-structural-stream-rejected.patch` with SHA-256
`fb7788d2b376efb91f61c08eae030c55613e355e368e884c820731de245da25b`.

## Correctness Gates

- `CARGO_TARGET_DIR=/tmp/skv14-w11t-regen-json-target RUSTC_WRAPPER= cargo run --profile ax-iter -p xtask -- regen-json`
- `CARGO_TARGET_DIR=/tmp/skv14-w11t-check-json-target RUSTC_WRAPPER= cargo run --profile ax-iter -p xtask -- check-json`
- `CARGO_TARGET_DIR=/tmp/skv14-w11t-runtime-test-target RUSTC_WRAPPER= cargo test --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11t-codegen-test-target RUSTC_WRAPPER= cargo test --profile ax-iter -p codegen emits_distinct_json_parse_only_path_without_tape_builder -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11t-scan-test-target RUSTC_WRAPPER= cargo test --profile ax-iter -p runtime match_scalar -- --nocapture`

All correctness gates passed before measurement.

## Cold Evidence

Release-native no-warm profile command:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11t-profile-target RUSTC_WRAPPER= \
  RUSTFLAGS="-C target-cpu=native" \
  cargo build --release -p bbnf-bench --bin profile_direct

for corpus in twitter github_events update_center random gsoc-2018 distinct_values canada instruments apache_builds citm_catalog; do
  for mode in parse_only_track1 parse_only_track2 parse_only_sonic parse_only_serde; do
    /tmp/skv14-w11t-profile-target/release/profile_direct 400 "$corpus" "$mode" 0
  done
done
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11T-parse-only-structural-stream.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11T-parse-only-structural-stream.raw.log`

Residual row results against the `sonic + 1.0` floor:

| Row | Track 1 Mbps | Sonic Mbps | Floor margin |
|---|---:|---:|---:|
| `twitter/parse_only` | 3996.677 | 6132.269 | -2136.592 |
| `github_events/parse_only` | 5681.323 | 9691.079 | -4010.756 |
| `update_center/parse_only` | 4267.644 | 7745.797 | -3479.153 |
| `random/parse_only` | 3009.075 | 5799.474 | -2791.399 |
| `gsoc-2018/parse_only` | 3465.988 | 7814.061 | -4349.073 |
| `distinct_values/parse_only` | 4083.236 | 6565.786 | -2483.550 |

Guard rows also failed the same floor: `canada` margin `-883.804` Mbps,
`instruments` margin `-824.606` Mbps, `apache_builds` margin `-1395.578`
Mbps, and `citm_catalog` margin `-7206.933` Mbps.

## Disposition

W11T proves that scanner-backed structural-stream parse_only driving is not
sufficient to admit any remaining parse_only residual row. The route is
pre-blocked without a fresh material differential. Current JSON parse_only
state remains 11 / 17 ADMITTED and 6 OPEN: `twitter`, `github_events`,
`update_center`, `random`, `gsoc-2018`, and `distinct_values`.
