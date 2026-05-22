# SK-V13 W14.2 Redress - CITM Catalog Parse-Only Admission

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W14.2-JSON-PARSE-CITM-CATALOG`.

## Result

`json/citm_catalog/parse_only/main` moves from `S / NO-GO` to `A / GO`.
The intervention does not change parser runtime, union substrate, generated
JSON parser bodies, or SIMD code. It generalizes the W14 parse-only
admission gate to a row-spec table and supplies a gate-consumed strict DOM
report for `citm_catalog`.

## Measurement

Native Criterion was refreshed with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/citm_catalog/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan
```

The companion facts measured:

| lane | Mbps |
|---|---:|
| Track 1 generated mean | 30196.751 |
| Track 1 generated lower confidence | 30134.073 |
| Track 2 handcoded oracle | 20598.200 |
| sonic-rs strict | 25567.165 |
| serde_json | 7607.770 |
| threshold | 25568.165 |
| mean margin | 4628.586 |

The gate-generated RESULTS slope row records Track 1 `30150` Mbps, Track 2
`20574` Mbps, sonic strict `25565` Mbps, and rolling margin `4584.00` Mbps.

## Verification

```text
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json
```

`gate-json --update-results` initially rejected the capture because
`simd_structural_scan/*_simd/metadata.toml` was from an older capture. The
SIMD scan bench was refreshed; no SIMD row is admitted or claimed by W14.2.

## Artifacts

- `restart/skinny/tranches/sk-v13/research/w14.2/citm-catalog-parse-facts.json`
- `restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json`
- SHA-256: `47f0e3c552f8ac3dc7f408549ef8a1416bb1fd780b1f86725193ff1550aff44f`

## Remainder

`canada`, `marine_ik`, and `mesh` remain positive-margin OPEN parse-only rows
until they receive their own W14.N plan, CHALLENGE, report artifact, and
REDRESS citation. No implementation-limited miss is closed by this admit.
