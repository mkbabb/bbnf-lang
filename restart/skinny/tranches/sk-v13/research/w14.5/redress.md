# SK-V13 W14.5 Redress - Mesh Parse-Only Admission

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W14.5-JSON-PARSE-MESH`.

## Result

`json/mesh/parse_only/main` moves from `S / NO-GO` to `A / GO`. The wave
does not change parser runtime, generated JSON parser bodies, union substrate,
SIMD primitives, output digests, or decision-engine policy. It adds one
configured W14 parse-only row spec and supplies the gate-consumed strict DOM
report for Mesh.

## Measurement

Native Criterion was refreshed with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/mesh/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan
```

The companion facts measured:

| lane | Mbps |
|---|---:|
| Track 1 generated mean | 12897.188 |
| Track 1 generated lower confidence | 12865.574 |
| Track 2 handcoded oracle | 11513.615 |
| sonic-rs strict | 11760.010 |
| serde_json | 5060.545 |
| threshold | 11761.010 |
| mean margin | 1136.179 |

The gate-generated RESULTS slope row records Track 1 `12987` Mbps, Track 2
`11522` Mbps, sonic strict `11758` Mbps, and rolling margin `1228.00` Mbps.
The `json_parity` regex also refreshed adjacent Mesh serde direct/typed
comparator lanes; W14.5 claims only the parse-only row.

## Verification

```text
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.5/skv13-W14.5-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.1/skv13-W14.1-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.4/skv13-W14.4-json-parse-only.json
```

`gate-json --update-results` initially rejected stale
`simd_structural_scan/*_simd/metadata.toml` capture identity after the Mesh
Criterion lanes were refreshed. The SIMD scan bench was rerun only to refresh
required gate metadata; W14.5 does not claim a SIMD admission.

## Artifacts

- `restart/skinny/tranches/sk-v13/research/w14.5/mesh-parse-facts.json`
- `restart/skinny/tranches/sk-v13/research/w14.5/skv13-W14.5-json-parse-only.json`
- SHA-256: `271d51a7e795e821982780f65020028980d53e67ee7212bada13ad0bb3345fd5`

## Remainder

W14.5 exhausts the positive-margin report-only parse admission pattern. No
remaining JSON row should be closed by status/report plumbing alone. The next
campaign wave must land a real implementation or a measured architectural
block, with `json/instruments/direct_to_struct/main` and
`json/update_center/real_typed_struct/main` the closest pinned-margin targets.
