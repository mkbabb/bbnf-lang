# SK-V8 W2 Hardening V5 CH1

Reviewed target: HEAD `bf2f073d99309c84adf0dd3770e5071778422125`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`), re-challenging the
unchanged V4-folded target after `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0
rows`).

Lens: CH1 report-gate/metadata semantics and REDRESS/HANDOFF consistency.

Verdict: ACCEPT

Confidence: 96%

## Findings

1. The V4 report-gate fold is correctly scoped. `gate.rs` passes
   `w0_real_typed_metadata_expected(&fixture.name)` into
   `validate_w0_capture_metadata`, and that predicate is now derived from
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`, not from the
   source fixture map. The live regression test asserts the intended split:
   `twitter` and `update_center` require W0 real-typed metadata, while
   `apache_builds` and `citm_catalog` do not.

2. Apache/CITM remain source/product parity fixtures, not unadmitted measured
   rows. `real_typed_struct::fixture_for_name` and generated real-typed output
   include `apache_builds` and `citm_catalog`, and the real-typed parity suite
   passes for both. `skinny/RESULTS.md` still has only the four W0 measured
   `real_typed_struct` rows: `twitter`, `update_center`, `mesh`, and
   `marine_ik`. The local Criterion tree has no Apache/CITM real-typed metadata
   paths.

3. W0 run-id strictness is not weakened. `report.rs` still rejects any telemetry
   row whose `run_id` differs from `SK_V8_OPEN_RUN_ID`, and
   `validate_sk_v8_w0` still enforces exact row count, known row ids, baseline
   outcome/verdict equality, and baseline throughput deltas. The standard
   checked report run fails closed at the already recorded W0 run-id drift:
   `json/twitter/parse_only/main run_id moved from SK-V8-open baseline
   sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-b9435757f85b6da0`. It does not fail on
   `apache_builds` or `citm_catalog` missing `track1_real_typed_struct`
   metadata.

4. The run-id whitelist remains row-manifest scoped, not fixture-source scoped.
   `is_w0_criterion_input` accepts real-typed Criterion inputs only when the
   matching `json/{corpus}/real_typed_struct/main` baseline row exists. The
   focused fingerprint test proves unrelated future groups and valid-fixture
   unvalidated real-typed estimates such as `json_canada/sonic_rs_real_typed_struct`
   do not perturb the committed run id, while admitted W0 row inputs still do.

5. REDRESS and HANDOFF agree with executable behavior. REDRESS 91 records
   Apache/CITM source/product admission, Canada routing, unchanged
   `skinny/RESULTS.md`, rejected W2 benchmark row-table admission, and the V3
   gate fold that binds real-typed metadata requirements to measured W0 rows.
   HANDOFF repeats the same W2 disposition and explicitly says Apache/CITM no
   longer require unadmitted Criterion metadata rows while the W0 run-id strict
   validator remains intact.

6. The unchanged-target condition holds. `74fe4e1b..HEAD` adds only the V4
   challenge reports and V4 consolidated artifact; the V4 gate fold itself is
   unchanged at HEAD.

## Verification

- `git rev-parse HEAD`: `bf2f073d99309c84adf0dd3770e5071778422125`.
- `git diff --name-status 74fe4e1b..HEAD`: only V4 hardening artifacts.
- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`: PASS.
- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS.
- `cargo test -p bbnf-bench w0_criterion_fingerprint_excludes_derendered_probe_estimates -- --nocapture`: PASS.
- `cargo test -p bbnf-bench real_typed -- --nocapture`: PASS, including Apache/CITM typed parity tests.
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`: PASS; schema `sk-v8-costfacts-v1`, wave `SK-V8-W1`, 15 manifest rows, zero gate-level diagnostics.
- `cargo xtask gate-json --advisory --check-results`: expected FAIL at W0 run-id drift; no Apache/CITM missing real-typed metadata failure.
- `rg` over `skinny/RESULTS.md`: only `twitter`, `update_center`, `mesh`, and `marine_ik` appear as measured `real_typed_struct` rows.
- `find target/criterion -path '*apache_builds*real_typed_struct*' -o -path '*citm_catalog*real_typed_struct*'`: no paths.
- `git diff --exit-code HEAD -- skinny/RESULTS.md`: PASS, no diff.
- `git diff --check`: PASS.

## Required Folds

None. No CH1 blocker remains. Preserve the current split: Apache/CITM are W2
source/product parity fixtures only, W2 benchmark row-table admission remains
rejected/routed for this wave, and the W0 run-id validator must continue to fail
closed on the recorded metadata drift.
