# SK-V14 W11V Parse-Only String64 Reject

## Disposition

W11V is `REJECT`. No source patch lands, no `skinny/RESULTS.md` row moves,
and `restart/skinny/ROLLING-SOTA-DELTA.md` remains unchanged.

The attempted material differential added a 64-byte aarch64 JSON string-special
mask primitive to `bbnf-simd` and routed
`parse_that_regex::match_string_end_at_quote_after_plain_prefix_trusted_utf8`
through it via the trusted plain-string scanner. This is distinct from W11T:
it does not add a structural stream, a side table, or a replacement parse_only
driver. It targets the W11S rank-1 hot leaf directly.

The source patch was reverted after measurement and retained as
`/tmp/skv14-W11V-string64-rejected.patch` with SHA-256
`74bd6832bfc243e7a44ba6584ff316e44f8fccc99eb032dbec3b1f3c06ee163c`.

## Correctness Gates

- `CARGO_TARGET_DIR=/tmp/skv14-w11v-simd-test-target RUSTC_WRAPPER= BBNF_SIMD_STRICT=1 cargo test --profile ax-iter -p bbnf-simd sk_v3_intrinsic_parity_aarch64 --test checkasm_parity -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11v-regex-test-target RUSTC_WRAPPER= cargo test --profile ax-iter -p parse-that-regex trusted_string -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv14-w11v-runtime-test-target RUSTC_WRAPPER= cargo test --profile ax-iter -p runtime generated_parse_only_accepts_and_rejects_json -- --nocapture`

## Cold Profile

Command:

```sh
for corpus in twitter github_events update_center random gsoc-2018 distinct_values canada instruments apache_builds citm_catalog; do
  for mode in parse_only_track1 parse_only_track2 parse_only_sonic parse_only_serde; do
    /tmp/skv14-w11v-profile-target/release/profile_direct 400 "$corpus" "$mode" 0
  done
done
```

Profile build:

```sh
CARGO_TARGET_DIR=/tmp/skv14-w11v-profile-target RUSTC_WRAPPER= RUSTFLAGS='-C target-cpu=native' cargo build --release -p bbnf-bench --bin profile_direct
```

Retained evidence:

- `restart/skinny/tranches/sk-v14/research/skv14-W11V-parse-only-string64.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11V-parse-only-string64.tsv`
- `restart/skinny/tranches/sk-v14/research/skv14-W11V-parse-only-string64-baseline.raw.log`
- `restart/skinny/tranches/sk-v14/research/skv14-W11V-parse-only-string64-baseline.tsv`

## Residual Margins

| row | Track 1 Mbps | sonic + 1 Mbps | margin Mbps | verdict |
|---|---:|---:|---:|---|
| `twitter/parse_only` | 7394.633 | 10090.942 | -2696.309 | REJECT |
| `github_events/parse_only` | 5919.577 | 7862.597 | -1943.020 | REJECT |
| `update_center/parse_only` | 4258.687 | 8574.849 | -4316.162 | REJECT |
| `random/parse_only` | 2798.992 | 5038.784 | -2239.792 | REJECT |
| `gsoc-2018/parse_only` | 7641.852 | 18183.891 | -10542.039 | REJECT |
| `distinct_values/parse_only` | 2918.153 | 6048.749 | -3130.596 | REJECT |

## Guard Rows

The source patch also fails the guard condition because one previously
admitted parse_only guard row misses the same-run floor:

| row | Track 1 Mbps | sonic + 1 Mbps | margin Mbps |
|---|---:|---:|---:|
| `canada/parse_only` | 8086.475 | 7152.280 | 934.195 |
| `instruments/parse_only` | 6664.616 | 7848.626 | -1184.010 |
| `apache_builds/parse_only` | 7387.596 | 7376.717 | 10.879 |
| `citm_catalog/parse_only` | 10610.161 | 6623.945 | 3986.216 |

## Route Effect

The 64-byte trusted string-special primitive improves Track 1 throughput on
the open rows relative to the immediately measured pre-patch baseline, but the
same-run `sonic_rs::Skipper` floor remains higher for every open row. W11V
therefore pre-blocks this exact string64 trusted-skip route without a fresh
material differential.
