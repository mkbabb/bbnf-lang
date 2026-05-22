# SK-V13 W14.5 Plan - Mesh Parse-Only Admission

Date: 2026-05-22.
Wave: W14.5.
Gate: `G-W14.5-JSON-PARSE-MESH`.

## Selected Intervention

Admit `json/mesh/parse_only/main` under the 2026-05-21 parse-only re-pin by
adding one configured W14 parse-only admission row and a gate-consumed strict
DOM report. This is a measured-row admission, not a parser-runtime change.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.5/`

Any parser runtime, generated JSON parser body, SIMD primitive, union
substrate, output digest, or decision-engine edit is out of W14.5 scope.

## Gate Contract

`G-W14.5-JSON-PARSE-MESH` admits only if:

- `JsonParseOnlyAdmissionSpec` contains exactly one W14.5 entry for
  `json/mesh/parse_only/main`, `mesh`, `json_mesh`, and `723597` bytes.
- A fresh native Criterion run measures:
  `json/mesh/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)`.
- The report schema is `sk-v13-json-parse-only-v1`, with wave
  `SK-V13-W14.5`, gate `G-W14.5-JSON-PARSE-MESH`, route
  `generated-json-parse-only-mesh`, and REDRESS entry `REDRESS-158`.
- Track 1 DOM parse throughput is at least `sonic_rs_anchor + 1 Mbps`.
- Track 2 is independent and strict equality is `pass`.
- `gate-json --update-results` marks only `json/mesh/parse_only/main` as
  `A / GO` for W14.5 and updates the rolling table consistently.
- Existing W14.1-W14.4 report gates remain valid.

## Test And Measurement Plan

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/mesh/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
cargo test -p xtask gate_json_passthrough_accepts_skv13_json_parse_only_report_flag -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.5/skv13-W14.5-json-parse-only.json
```

If the mesh Criterion refresh changes the required SIMD capture identity, rerun
`RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench simd_scan`
only as metadata refresh, with no SIMD admission claim.

## Revert Protocol

If Track 1 misses `sonic_rs_anchor + 1 Mbps`, strict equality fails, or
`gate-json` cannot consume the report, revert the source/report patch, save the
rejected patch at `/tmp/skv13-waveW14.5-rejected.patch`, append a measured
REDRESS reject, and leave `json/mesh/parse_only/main` as `S / NO-GO`.

## Remainder

W14.5 is the last planned report-only parse admission. After it admits or
rejects, the next wave must be an implementation wave, with
`instruments/direct_to_struct` and `update_center/real_typed_struct` as the
closest pinned gaps from the post-W14.4 audit.
