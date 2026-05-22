# SK-V13 W14.3 Plan - Canada Parse-Only Admission

Date: 2026-05-22.
Wave: W14.3.
Gate: `G-W14.3-JSON-PARSE-CANADA`.

## Intervention

Admit `json/canada/parse_only/main` under the 2026-05-21 parse-only re-pin by
adding Canada to the W14 table-driven parse-only admission surface and
supplying a row-specific `sk-v13-json-parse-only-v1` report.

The implementation is intentionally narrow:

- Add one W14.3 `JsonParseOnlyAdmissionSpec` for `canada`.
- Refresh the native `json_canada` Criterion lanes used by the report.
- Add a W14.3 companion report and measurement facts artifact.
- Let the existing W14 report validator, `gate-json` marker, and RESULTS row
  validator consume the new row spec.
- Update only `json/canada/parse_only/main` in `RESULTS.md` and the rolling
  SOTA delta, plus the matching REDRESS entry.

No parser runtime, generated JSON parser body, SIMD primitive, union
substrate, output digest, or decision-engine route is changed by this wave.
Future positive-margin rows (`marine_ik`, `mesh`) remain closed to admission
until their own W14.N packets land.

## Owner Paths

- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/RESULTS.md`
- `restart/skinny/ROLLING-SOTA-DELTA.md`
- `restart/skinny/tranches/sk-v13/research/w14.3/`
- `skinny/REDRESS.md`

## Acceptance

The wave admits only if:

- The companion `sk-v13-json-parse-only-v1` report validates
  `json/canada/parse_only/main`, `SK-V13-W14.3`,
  `G-W14.3-JSON-PARSE-CANADA`, `REDRESS-156`, DOM output, strict equality,
  measured UTF-8, escape completeness, independent Track 2, Lock 14 status,
  and prior REDRESS 102 material differential.
- Native Criterion lanes are present for `json_canada/track1_generated`,
  `track2_handcoded`, `sonic_rs_anchor`, and `serde_json`.
- `threshold_mbps == sonic_strict_mbps_after + 1.0`.
- Track 1 lower-confidence throughput is greater than the threshold.
- Track 2 remains present as an independent oracle and is not allowed to
  substitute for Track 1 as the throughput admission lane.
- No non-selected W14 parse-only row moves to `A / GO`.

## Revert Protocol

If the report, artifact hash, Criterion capture, Lock 14 owner-scope check, or
`gate-json` validation fails, revert the source/status edits, save the
rejected patch at `/tmp/skv13-waveW14.3-rejected.patch`, append a measured
REDRESS reject, and leave `json/canada/parse_only/main` as `S / NO-GO`.

If the gate passes, keep the Canada row spec, admit only
`json/canada/parse_only/main`, update the rolling delta, and append REDRESS
with the measurement artifact hash.

## Measurement

```text
RUSTFLAGS="-C target-cpu=native" cargo bench -p bbnf-bench --bench json_parity -- 'json/canada/(track1_generated|track2_handcoded|sonic_rs_anchor|serde_json)'
cargo test -p bbnf-bench skv13_json_parse_only_report_accepts_configured_corpus_admit -- --nocapture
cargo test -p bbnf-bench json_parse_only_admission_passes_configured_corpora_only -- --nocapture
cargo test -p bbnf-bench validate_sk_v8_w0_accepts_configured_parse_only_admission_rows -- --nocapture
cargo test -p bbnf-bench admits_sk_v13_w14_parent_diff_under_w14_scope -- --nocapture
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --update-results --advisory
RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results --advisory --skv13-json-parse-only-report ../restart/skinny/tranches/sk-v13/research/w14.3/skv13-W14.3-json-parse-only.json
```
