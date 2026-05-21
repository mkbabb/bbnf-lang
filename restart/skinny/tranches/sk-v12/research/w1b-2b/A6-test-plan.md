# SK-V12 W1b-2b A6 - Test Plan And Backward Compatibility

Date: 2026-05-20.
Phase: W1b-2b research.
Scope: tests for the CSS L4 lightningcss SOTA companion report and gate.

## Grounding

SPEC Section 7.2 adds a W1b-2-specific report/gate surface for the existing
CSS L4 declaration-value row. The gate consumes W1b-2a Criterion/equality
artifacts, validates `sk-v12-css-l4-sota-v1`, derives
`threshold_mbps = lightningcss_mbps + 1`, and emits either
`PASS-ADMIT-CANDIDATE` or `PASS-MEASURED-BASELINE`. It must fail closed for
stale/missing comparator evidence, failed equality, generated-size failure,
JSON guard failure/demotion, and illegal write/probe combinations.

This test plan is additive. It must not weaken the existing W1b-1
`sk-v12-nonjson-generated-v1` gate, W1a non-JSON gate, JSON `RESULTS.md`
writer, Lock 14 baseline, or current CSS fixture/equality tests.

## Focused Unit Tests

Add `report.rs` tests under the existing `#[cfg(test)] mod tests` pattern:

- `skv12_css_l4_sota_report_accepts_admit_candidate`: construct one valid
  `SkV12CssL4SotaReport` row with `track1_mbps > lightningcss_mbps + 1`,
  `admission_status = PASS-ADMIT-CANDIDATE`, strict three-way equality,
  `sample_count >= 30`, W1b-2b identity, generated-size fields, and JSON guard
  consumed.
- `skv12_css_l4_sota_report_accepts_measured_baseline`: same valid row but
  `track1_mbps <= lightningcss_mbps + 1` and
  `admission_status = PASS-MEASURED-BASELINE`; equality exactly at
  `lightningcss_mbps + 1` must stay measured-baseline.
- `skv12_css_l4_sota_report_derives_threshold_and_margin`: reject reports whose
  serialized `threshold_mbps` or `admission_margin_mbps` disagrees with
  `lightningcss_mbps + 1` and `track1_mbps - threshold_mbps`.
- `skv12_css_l4_sota_report_rejects_required_failure_classes`: mutate one field
  at a time and require `validate_gate().is_err()` for wrong schema id, wrong
  wave/run identity, wrong row id, non-CSS grammar/domain, wrong output plane,
  non-strict row, missing `lightningcss_mbps`, `sample_count < 30`, missing or
  stale lightningcss version/build/artifact fields, failed
  `strict_output_equality`, failed `three_way_equality`, non-independent
  cssparser oracle, generated-size guard failure, Lock 14 failure, and
  unconsumed/failed JSON guard state.
- `skv12_css_l4_sota_report_rejects_unknown_producer_fields`: mirror the
  existing deny-unknown-fields test by injecting a `producer_only_field` into
  the row JSON and asserting `from_json_str()` fails.

Keep helpers local to `report.rs` and parallel to `skv12_non_json_report()` /
`skv12_reject()` so old tests remain readable and unchanged.

## CSS L4 Comparator Tests

Keep the current `nonjson_css_l4.rs` tests and add only W1b-2b-specific checks
if implementation adds a new writer such as `write_sota_report_with_*`:

- preserve `cssparser_oracle_matches_generated_track1`;
- preserve `lightningcss_sidecar_matches_generated_track1_and_cssparser`;
- preserve fixture-drift fail-closed behavior;
- add one writer test that builds the W1b-2b SOTA report from the current
  fixture, calls `validate_gate()`, and asserts retained artifact path fields
  name Track 1, cssparser, lightningcss, and strict-equality outputs.

Do not convert these tests into broad file-system snapshot tests. The unit
contract is equality, fixture drift rejection, report validation, and named
artifact references.

## CLI Tests

Extend `skinny/crates/bbnf-bench/src/bin/gate.rs` tests next to the existing
`skv12_non_json_report_arg_*` cases:

- `skv12_css_l4_sota_report_arg_extracts_single_path`: verify
  `--skv12-css-l4-sota-report <path>` returns exactly that path.
- `skv12_css_l4_sota_report_arg_allows_no_write_json_check_flags`: verify the
  flag may be combined with `--advisory --check-results` and triggers the
  companion JSON check path.
- `skv12_css_l4_sota_report_arg_rejects_update_results_combination`: reject
  `--update-results` and `--write-results`.
- `skv12_css_l4_sota_report_arg_rejects_probe_combination`: reject
  `--include-volatile-probes`.
- `skv12_css_l4_sota_report_arg_rejects_mixed_companion_reports`: reject mixing
  the new flag with `--w1a-non-json-report` or `--skv12-non-json-report`.

If a small print-decision helper is introduced, unit-test both emitted labels:
`G-W1b-2-CSS-L4-LIGHTNINGCSS PASS-ADMIT-CANDIDATE` and
`G-W1b-2-CSS-L4-LIGHTNINGCSS PASS-MEASURED-BASELINE`.

## Artifact No-Write Tests

The no-write contract is load-bearing because Section 7.2 says the companion
report must fail closed with write/probe flags and JSON guards must run against
an existing accepted JSON Criterion root or a fresh populated JSON guard
capture.

Required tests:

- unit-level CLI arg tests above prove the companion flag cannot be combined
  with `--update-results`, `--write-results`, or `--include-volatile-probes`;
- an integration-style command check should run from `skinny/` with a temporary
  `CRITERION_HOME` that contains or points at a populated JSON guard capture,
  `--skv12-css-l4-sota-report`, `--advisory`, and `--check-results`, then
  verify `skinny/RESULTS.md` is byte-identical before/after;
- a negative command check should use an empty CSS-only Criterion directory and
  assert the gate fails instead of treating missing JSON guards as success;
- if the report writer emits retained facts, it may write only the named W1b
  artifact files and the companion SOTA report, never `skinny/RESULTS.md` or
  `skinny/REDRESS.md`.

## Backward Compatibility

The new schema and flag must be isolated:

- `--skv12-non-json-report` keeps accepting W1b-1
  `sk-v12-nonjson-generated-v1` reports and keeps printing the existing gate
  label.
- `--w1a-non-json-report` behavior remains unchanged.
- JSON `gate-json` without the CSS SOTA flag still follows the existing W0/W2
  result path and existing stale-results handling.
- The existing `Report` markdown schema, probe row rendering, and strict
  admission checks remain unchanged.
- Lock 14 tests should continue to pass without allowlist edits if W1b-2b stays
  inside Section 7.2 owner paths.

## What Not To Touch

- Do not edit `skinny/RESULTS.md` for `PASS-MEASURED-BASELINE`.
- Do not add placeholder CSS SOTA rows to the JSON results table.
- Do not edit `skinny/REDRESS.md` in tests; REDRESS 124 is outcome evidence for
  the implementation run, not a unit-test fixture target.
- Do not relax the W1b-1 `sk-v12-nonjson-generated-v1` validator to fit W1b-2b.
- Do not weaken fixture checksum/byte-length checks in `nonjson_css_l4.rs`.
- Do not touch `lock14_baseline.rs` unless implementation actually changes a
  frozen/generic root and the plan is amended.
- Do not treat lightningcss transitive cssparser use as a failure; only direct
  cssparser API calls in the lightningcss comparator are forbidden.

## Minimum Command Set

```sh
cargo test -p bbnf-bench nonjson_css_l4 -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report -- --nocapture
cargo test -p bbnf-bench skv12_css_l4_sota_report_arg -- --nocapture
cargo test -p bbnf-bench lock14 -- --nocapture
```

No benchmark is required for the unit-test gate, but the implementation close
still needs the Section 7.2 native Criterion command and the no-write JSON guard
command against a populated JSON guard root.
