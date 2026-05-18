# SK-V8 W2 Hardening V5 CH3

Reviewed target: `bf2f073d`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`).

V4 folded code target re-challenged: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

Verdict: ACCEPT

Confidence: 94%

## Findings

1. The V4-folded implementation is unchanged at HEAD. `bf2f073d` adds only the
   V4 hardening reports and consolidated note on top of `74fe4e1b`; there is no
   diff from `74fe4e1b..HEAD` in `skinny/crates/bbnf-bench/src`,
   `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`, or `HANDOFF.md`.

2. The prior V3 CH3 blocker remains folded. The checked report gate now passes
   `w0_real_typed_metadata_expected(&fixture.name)` into
   `validate_w0_capture_metadata`, and that predicate derives from measured
   `SK-V8-open` real-typed baseline rows:
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`. It no
   longer treats every source fixture returned by
   `real_typed_struct::fixture_for_name` as requiring unadmitted Criterion
   `real_typed_struct` metadata.

3. The focused regression test proves the intended split: `twitter` and
   `update_center` require W0 real-typed metadata, while `apache_builds` and
   `citm_catalog` do not. This directly covers the V3 failure mode where
   Apache/CITM source-only typed fixtures made standard
   `gate-json --advisory --check-results` fail before report validation.

4. The live standard checked report now fails only on the known W0 run-id strict
   drift. It renders the report, then rejects
   `json/twitter/parse_only/main run_id moved from SK-V8-open baseline
   sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-b9435757f85b6da0`. I did not observe the V3
   `citm_catalog metadata invalid: missing coherent metadata for
   track1_real_typed_struct` failure or any Apache/CITM real-typed metadata
   error.

5. The W0 run-id validator is still strict. `report.rs` rejects any row whose
   run id differs from `SK_V8_OPEN_RUN_ID`, and the baseline report test rejects
   both a single bad run id and a uniform `sk-v8-open:test` run id. This is the
   expected route: do not weaken W0 strictness to make W2 source-only parity
   pass the local checked report.

6. Row-table admission remains unchanged. `skinny/RESULTS.md` still has exactly
   four measured `real_typed_struct` rows: `twitter`, `update_center`, `mesh`,
   and `marine_ik`. It has no measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows, and the local Criterion tree has no
   Apache/CITM real-typed metadata directories.

## Verification

- `git rev-parse HEAD`: `bf2f073d99309c84adf0dd3770e5071778422125`.
- `git diff --stat 74fe4e1b..HEAD`: V4 review docs only.
- `git diff --exit-code 74fe4e1b..HEAD -- skinny/crates/bbnf-bench/src skinny/RESULTS.md skinny/REDRESS.md restart/skinny/tranches/sk-v8/SPEC.md restart/skinny/tranches/sk-v8/HANDOFF.md`: PASS.
- `git diff --check 74fe4e1b..HEAD`: PASS.
- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`: PASS.
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS.
- `cargo xtask gate-json --advisory --check-results`: expected FAIL at W0
  run-id strict drift, expected
  `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`, rendered
  `sk-v8-open:criterion-fnv64-b9435757f85b6da0`; no Apache/CITM metadata
  failure observed.
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`: PASS,
  schema `sk-v8-costfacts-v1`, wave `SK-V8-W1`, 15 manifest rows.
- `cargo test -p bbnf-bench real_typed -- --nocapture`: PASS, seven tests
  including Apache/CITM sidecar parity and full real-typed fixture parity.
- `cargo xtask check-real-typed`: PASS.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`: PASS, ten tests.
- `cargo xtask check-json`: PASS.
- `cargo xtask check-conformance`: PASS, 21 valid fixtures accepted and 7
  invalid fixtures rejected.
- `cargo test -p codegen typed_direct -- --nocapture`: PASS.
- `find skinny/target/criterion -path '*apache_builds*real_typed_struct*' -o -path '*citm_catalog*real_typed_struct*'`: no Apache/CITM real-typed Criterion metadata paths found.
- `git status --short`: clean before writing this report.

## Required Folds

None for CH3. Keep the W0 run-id strict drift routed separately; do not weaken
the strict baseline validator to make the W2 source-only Apache/CITM parity
packet pass standard checked report admission.
