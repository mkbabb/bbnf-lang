# SK-V14 W11W JSON parse_only Memchr Trusted-String Split

## Disposition

W11W is `ADMIT`. The source patch lands, `skinny/RESULTS.md` moves the six
remaining JSON `parse_only` rows, and
`restart/skinny/ROLLING-SOTA-DELTA.md` records 17 / 17 JSON `parse_only`
admission.

The material differential replaces the trusted plain-string end search in
`parse-that-regex` with a split scanner: `memchr2` finds the next quote or
backslash, and a separate SWAR control-byte scan checks only the exact prefix
before that syntax byte. This keeps raw-control rejection exact while avoiding
the custom 64-byte string-special primitive rejected by W11V. It is also
distinct from W11T because it does not add a structural stream, side table, or
replacement parse_only driver.

## Correctness Gates

- `CARGO_TARGET_DIR=/tmp/skv14-w11w-regex-test-target RUSTC_WRAPPER= cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p parse-that-regex trusted_string -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11w-runtime-test-target RUSTC_WRAPPER= cargo test --manifest-path skinny/Cargo.toml --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`

## Cold Profile

Profile build:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11w-profile-target RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' cargo build --manifest-path skinny/Cargo.toml --release -p bbnf-bench --bin profile_direct
```

Profile sweep, run from `skinny/` so fixture paths resolve cold per parse:

```sh
for corpus in twitter github_events update_center random gsoc-2018 distinct_values canada instruments apache_builds citm_catalog; do
  for mode in parse_only_track1 parse_only_track2 parse_only_sonic parse_only_serde; do
    /tmp/skv14-w11w-profile-target/release/profile_direct 400 "$corpus" "$mode" 0
  done
done
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11W-parse-only-memchr.raw.log`
  sha256 `a0b667cbf9f2282366a1e645d7d6e1b7b67d2a56ebe1cd1bf9aafdd2895ef00a`
- `restart/skinny/tranches/sk-v14/research/skv14-W11W-parse-only-memchr.tsv`
  sha256 `13c65586e7d634503454c3170680578e549f71ab3686b6510d67f63006aeb902`

## Admission Rows

| row | Track 1 Mbps | Track 2 Mbps | sonic Mbps | sonic + 1 Mbps | margin Mbps | verdict |
|---|---:|---:|---:|---:|---:|---|
| `twitter/parse_only` | 8349.290 | 4558.264 | 4913.095 | 4914.095 | 3435.195 | ADMIT |
| `github_events/parse_only` | 8148.582 | 5092.727 | 5014.433 | 5015.433 | 3133.149 | ADMIT |
| `update_center/parse_only` | 5671.345 | 2837.898 | 4707.613 | 4708.613 | 962.732 | ADMIT |
| `random/parse_only` | 3093.724 | 2414.011 | 2937.264 | 2938.264 | 155.460 | ADMIT |
| `gsoc-2018/parse_only` | 13213.304 | 6976.158 | 11355.449 | 11356.449 | 1856.855 | ADMIT |
| `distinct_values/parse_only` | 5155.207 | 2406.940 | 3233.781 | 3234.781 | 1920.426 | ADMIT |

All rows use generated Track 1 `runtime::generated_json::parse_only`, the
independent Track 2 structural oracle, strict `parse_only/sonic_rs::Skipper`,
and cold measurement with `warmup_iters=0`.

## Guard Rows

Previously admitted guard rows also remain above the same-run `sonic + 1.0`
floor under the W11W binary:

| row | Track 1 Mbps | sonic Mbps | sonic + 1 Mbps | margin Mbps |
|---|---:|---:|---:|---:|
| `canada/parse_only` | 3532.967 | 2709.958 | 2710.958 | 822.009 |
| `instruments/parse_only` | 4536.891 | 3747.053 | 3748.053 | 788.838 |
| `apache_builds/parse_only` | 6573.481 | 4520.817 | 4521.817 | 2051.664 |
| `citm_catalog/parse_only` | 10516.060 | 9196.957 | 9197.957 | 1318.103 |

## Close State

W11W closes the remaining JSON `parse_only` residual queue. Current SK-V14
state is JSON `parse_only` 17 / 17 ADMITTED, JSON `direct_to_struct` 17 / 17
ADMITTED, JSON `real_typed_struct` 17 / 17 ADMITTED, and CSS L4 24 / 24
ADMITTED.
