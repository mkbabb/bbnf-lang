# SK-V13 W14.3 Redress - Canada Parse-Only Admission

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W14.3-JSON-PARSE-CANADA`.

## Result

`json/canada/parse_only/main` moves from `S / NO-GO` to `A / GO`. The wave
does not change parser runtime, generated JSON parser bodies, union substrate,
SIMD primitives, output digests, or decision-engine policy. It adds one
configured W14 parse-only row spec and supplies the gate-consumed strict DOM
report for Canada.

## Measurement

Native Criterion was refreshed with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/canada/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan
```

The companion facts measured:

| lane | Mbps |
|---|---:|
| Track 1 generated mean | 17052.470 |
| Track 1 generated lower confidence | 17006.626 |
| Track 2 handcoded oracle | 17101.405 |
| sonic-rs strict | 14078.954 |
| serde_json | 5351.603 |
| threshold | 14079.954 |
| mean margin | 2972.516 |

The gate-generated RESULTS slope row records Track 1 `16977` Mbps, Track 2
`17119` Mbps, sonic strict `14101` Mbps, and rolling margin `2875.00` Mbps.

## Verification

```text
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json
```

`gate-json --update-results` initially rejected stale
`simd_structural_scan/*_simd/metadata.toml` capture identity after the Canada
Criterion lanes were refreshed. The SIMD scan bench was rerun only to refresh
required gate metadata; W14.3 does not claim a SIMD admission.

## Artifacts

- `restart/skinny/tranches/sk-v13/research/w14.3/canada-parse-facts.json`
- `restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json`
- SHA-256: `f1a34375b0126fbb3c2d9fe273f3f32fb8d4d9d8c169b43b73940fe8b8a41df2`

## Remainder

`marine_ik` and `mesh` remain positive-margin OPEN parse-only rows until they
receive their own W14.N plan, CHALLENGE, report artifact, and REDRESS
citation. No implementation-limited miss is closed by this admit.
