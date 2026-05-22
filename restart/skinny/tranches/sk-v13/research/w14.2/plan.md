# SK-V13 W14.2 Plan - CITM Catalog Parse-Only Admission

Date: 2026-05-22.
Wave: W14.2.
Gate: `G-W14.2-JSON-PARSE-CITM-CATALOG`.

## Intervention

Admit `json/citm_catalog/parse_only/main` under the 2026-05-21 parse-only
re-pin by generalizing the W14 parse-only report/gate plumbing from a
numbers-only one-off into a small table-driven admission surface.

The implementation introduces:

- A configured W14 parse-only admission spec for W14.1 `numbers` and W14.2
  `citm_catalog`.
- Shared report validation keyed by row identity, wave id, run prefix,
  consumer gate, route id, Criterion group, byte count, and REDRESS entry.
- Shared RESULTS row validation and gate row marking for configured W14.N
  rows.
- A W14.2 companion report and measurement facts artifact for
  `json/citm_catalog/parse_only/main`.

The W14.2 row movement remains narrow: only `citm_catalog` may move in this
wave. Adding future candidate rows (`canada`, `marine_ik`, `mesh`) requires a
later plan/CHALLENGE/redress packet with its own report and REDRESS citation.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.2/`
- `skinny/REDRESS.md`

## Acceptance

The wave admits only if:

- The companion `sk-v13-json-parse-only-v1` report validates
  `json/citm_catalog/parse_only/main`, `SK-V13-W14.2`,
  `G-W14.2-JSON-PARSE-CITM-CATALOG`, `REDRESS-155`, DOM output, strict
  equality, measured UTF-8, escape completeness, independent Track 2, Lock 14
  status, and prior REDRESS 102 material differential.
- Native Criterion lanes are present for `json_citm_catalog/track1_generated`,
  `track2_handcoded`, `sonic_rs_anchor`, and `serde_json`.
- `threshold_mbps == sonic_strict_mbps_after + 1.0`.
- The Track 1 lower-confidence throughput is greater than the threshold.
- Track 2 is present and independent, but is not treated as the throughput
  admission lane.
- No non-selected W14 parse-only row moves to `A / GO`.

## Revert Protocol

If report validation, artifact hashing, Criterion confirmation, Lock 14, or
gate-json validation fails, revert source changes, save the rejected patch at
`/tmp/skv13-waveW14.2-rejected.patch`, append a measured REDRESS reject, and
leave `RESULTS.md` / `ROLLING-SOTA-DELTA.md` unchanged for `citm_catalog`.

If the gate passes, keep the table-driven W14 parse-only validator, admit only
`json/citm_catalog/parse_only/main`, update the rolling delta, and append
REDRESS with the measurement artifact hash.

## Measurement

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/citm_catalog/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.2/skv13-W14.2-json-parse-only.json
```
