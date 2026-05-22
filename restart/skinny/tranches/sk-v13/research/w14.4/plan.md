# SK-V13 W14.4 Plan - Marine IK Parse-Only Admission

Date: 2026-05-22.
Wave: W14.4.
Gate: `G-W14.4-JSON-PARSE-MARINE-IK`.

## Intervention

Admit `json/marine_ik/parse_only/main` under the 2026-05-21 parse-only re-pin
by adding Marine IK to the W14 table-driven parse-only admission surface and
supplying a row-specific `sk-v13-json-parse-only-v1` report.

The implementation introduces only:

- One W14.4 `JsonParseOnlyAdmissionSpec` for `marine_ik`.
- A native `json_marine_ik` Criterion lane refresh for the report.
- A W14.4 companion report and measurement facts artifact.
- RESULTS, rolling delta, and REDRESS updates for
  `json/marine_ik/parse_only/main`.

No parser runtime, generated JSON parser body, SIMD primitive, union
substrate, output digest, decision-engine route, or non-selected parse-only
row changes in this wave. `mesh` remains routed to a later W14.N packet.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.4/`
- `skinny/REDRESS.md`

## Acceptance

The wave admits only if:

- The companion `sk-v13-json-parse-only-v1` report validates
  `json/marine_ik/parse_only/main`, `SK-V13-W14.4`,
  `G-W14.4-JSON-PARSE-MARINE-IK`, `REDRESS-157`, DOM output, strict equality,
  measured UTF-8, escape completeness, independent Track 2, Lock 14 status,
  and prior REDRESS 102 material differential.
- Native Criterion lanes are present for `json_marine_ik/track1_generated`,
  `track2_handcoded`, `sonic_rs_anchor`, and `serde_json`.
- `threshold_mbps == sonic_strict_mbps_after + 1.0`.
- Track 1 lower-confidence throughput is greater than the threshold.
- Track 2 remains present as an independent oracle and is not allowed to
  substitute for Track 1 as the throughput admission lane.
- No non-selected W14 parse-only row moves to `A / GO`.

## Revert Protocol

If report validation, artifact hashing, Criterion confirmation, Lock 14, or
`gate-json` validation fails, revert source/status edits, save the rejected
patch at `/tmp/skv13-waveW14.4-rejected.patch`, append a measured REDRESS
reject, and leave `json/marine_ik/parse_only/main` as `S / NO-GO`.

If the gate passes, keep the Marine IK row spec, admit only
`json/marine_ik/parse_only/main`, update the rolling delta, and append REDRESS
with the measurement artifact hash.

## Measurement

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/marine_ik/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.4/skv13-W14.4-json-parse-only.json
```
