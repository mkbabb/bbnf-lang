# SK-V8 W2 Hardening V4 CH3

Reviewed target: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

Verdict: ACCEPT

Confidence: 93%

## Findings

1. The exact V3 CH3 blocker is folded. V3 failed standard
   `gate-json --advisory --check-results` before rendering because Apache/CITM
   source-only typed fixtures made the W0 checked report require unadmitted
   `track1_real_typed_struct`, `track2_real_typed_struct`,
   `sonic_rs_real_typed_struct`, and `serde_json_real_typed_struct` metadata.
   In `74fe4e1b`, `validate_w0_capture_metadata` now receives
   `w0_real_typed_metadata_expected(&fixture.name)`, and that predicate derives
   from `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")` rather
   than `real_typed_struct::fixture_for_name`. Apache/CITM therefore stay
   source/product parity fixtures and no longer force benchmark row-table
   admission metadata.

2. The regression test covers the intended split. The new
   `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`
   test asserts `twitter` and `update_center` require real-typed metadata while
   `apache_builds` and `citm_catalog` do not. That is the V3-required
   distinction between measured W0 rows and source-only W2 fixtures.

3. The W0 run-id strict validator was not weakened. `report.rs` still requires
   every row's `run_id` to equal `SK_V8_OPEN_RUN_ID`, and the baseline report
   test now rejects both a single mutated row and a uniform non-fingerprint
   value such as `sk-v8-open:test`. The `74fe4e1b` code diff does not edit
   `report.rs`; the checked gate still exits on run-id drift before it can
   compare checked-in `RESULTS.md`.

4. The live standard checked report failure is now W0 run-id drift, not W2
   metadata failure. My `cargo xtask gate-json --advisory --check-results` run
   rendered the report and failed with:
   `json/twitter/parse_only/main run_id moved from SK-V8-open baseline
   sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-b9435757f85b6da0`. It did not fail with the V3
   `citm_catalog metadata invalid: missing coherent metadata for
   track1_real_typed_struct` error.

5. Row-table admission remains unchanged. `skinny/RESULTS.md` still has exactly
   four measured `real_typed_struct` rows: `twitter`, `update_center`, `mesh`,
   and `marine_ik`. It has no measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows, and the live Criterion tree I checked
   has no Apache/CITM `real_typed_struct` metadata directories.

## Verification

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`: PASS.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS, including negative run-id mutations.
- `cargo xtask gate-json --advisory --check-results`: expected FAIL at the W0
  run-id strict validator, with rendered run id
  `sk-v8-open:criterion-fnv64-b9435757f85b6da0` versus committed baseline
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; no Apache/CITM real-typed
  metadata failure observed.
- `rg` over `skinny/RESULTS.md`: only `twitter`, `update_center`, `mesh`, and
  `marine_ik` appear as measured `real_typed_struct` rows.
- `find skinny/target/criterion ... apache_builds/citm_catalog ... real_typed_struct`: no Apache/CITM real-typed Criterion metadata directories found.

## Required Folds

None for the V3 CH3 blocker. Keep the W0 run-id drift routed separately; do not
weaken the strict baseline validator to make this W2 source-only packet pass.
