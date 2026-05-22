# SK-V13 W14.4 Redress - Marine IK Parse-Only Admission

Date: 2026-05-22.
Disposition: ADMIT.
Gate: `G-W14.4-JSON-PARSE-MARINE-IK`.

## Result

`json/marine_ik/parse_only/main` moves from `S / NO-GO` to `A / GO`. The wave
does not change parser runtime, generated JSON parser bodies, union substrate,
SIMD primitives, output digests, or decision-engine policy. It adds one
configured W14 parse-only row spec and supplies the gate-consumed strict DOM
report for Marine IK.

## Measurement

Native Criterion was refreshed with:

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/marine_ik/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan
```

The companion facts measured:

| lane | Mbps |
|---|---:|
| Track 1 generated mean | 12272.307 |
| Track 1 generated lower confidence | 12196.346 |
| Track 2 handcoded oracle | 12311.606 |
| sonic-rs strict | 9901.936 |
| serde_json | 3916.999 |
| threshold | 9902.936 |
| mean margin | 2369.371 |

The gate-generated RESULTS slope row records Track 1 `12357` Mbps, Track 2
`12302` Mbps, sonic strict `9902` Mbps, and rolling margin `2454.00` Mbps.
The `json_parity` regex also refreshed adjacent Marine IK serde
direct/typed comparator lanes; W14.4 claims only the parse-only row.

## Verification

```text
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.4/skv13-W14.4-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.1/skv13-W14.1-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json
```

`gate-json --update-results` initially rejected stale
`simd_structural_scan/*_simd/metadata.toml` capture identity after the Marine
IK Criterion lanes were refreshed. The SIMD scan bench was rerun only to
refresh required gate metadata; W14.4 does not claim a SIMD admission.

## Artifacts

- `restart/skinny/tranches/sk-v13/research/w14.4/marine-ik-parse-facts.json`
- `restart/skinny/tranches/sk-v13/research/w14.4/skv13-W14.4-json-parse-only.json`
- SHA-256: `01a4ce96289ab0efadb88c55d3817f84da05dfe1cde8fe0b2f908475dcf9f1bc`

## Remainder

`mesh` remains the only positive-margin OPEN parse-only row in the W14
table-admission pattern. After mesh receives its own packet, remaining JSON
rows need real implementation work, not status/report-only admission.
