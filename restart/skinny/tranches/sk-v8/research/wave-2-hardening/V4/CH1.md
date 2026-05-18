# SK-V8 W2 Hardening V4 CH1

Reviewed target: `74fe4e1b`
(`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

Lens: CH1 report-gate/metadata semantics and REDRESS/HANDOFF consistency.

Verdict: ACCEPT

Confidence: 95%

## Findings

1. The V3 CH3 checked-report blocker is folded in the executable gate. The
   metadata validator no longer derives required `real_typed_struct` Criterion
   rows from `real_typed_struct::fixture_for_name(..)`. It now passes
   `w0_real_typed_metadata_expected(&fixture.name)`, and that predicate checks
   `sk_v8_open_baseline("json/{fixture}/real_typed_struct/main")`. That binds
   real typed metadata requirements to measured W0 baseline rows, not to the W2
   source/product typed fixture map.

2. The regression test covers the exact source-only versus measured-row split.
   `w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures`
   requires metadata for measured W0 typed rows such as `twitter` and
   `update_center`, while explicitly not requiring it for W2 source-only
   `apache_builds` and `citm_catalog`.

3. The live standard checked report path now reaches the known W0 strict
   run-id validator instead of failing on Apache/CITM metadata. My
   `cargo xtask gate-json --advisory --check-results` run rendered the report
   and failed with
   `json/twitter/parse_only/main run_id moved from SK-V8-open baseline
   sk-v8-open:criterion-fnv64-9a37562ed3d0383a to
   sk-v8-open:criterion-fnv64-b9435757f85b6da0`. It did not fail with the V3
   `citm_catalog metadata invalid: missing coherent metadata for
   track1_real_typed_struct` error. This is not a W2 blocker because the W0
   run-id drift is already recorded/routed and the fold did not weaken that
   validator.

4. REDRESS and HANDOFF are consistent with the executable behavior. REDRESS
   still says W2 admits Apache/CITM as source/product parity only, rejects
   benchmark row-table admission for this wave, leaves `skinny/RESULTS.md`
   unchanged, and does not claim six measured `real_typed_struct A / GO` rows.
   HANDOFF repeats the same W2 disposition and now records the V3 fold as a
   report-gate fix: Apache/CITM no longer require unadmitted Criterion metadata
   rows, while the W0 run-id strict validator remains intact.

5. Strict-vs-strict comparator discipline is preserved. The commit does not
   edit `report.rs`; W0 validation still requires native `sonic_rs_strict` and
   `serde_json` comparators to be strict, same-run-native, on the expected
   workload plane, and sourced from the expected Criterion artifacts. Sidecar
   comparators remain historical or absent slots, and lossy sonic remains a
   parse-only permissive flaw probe. The CostFacts gate still reports schema
   `sk-v8-costfacts-v1`, wave `SK-V8-W1`, 15 manifest rows, and zero gate-level
   diagnostics.

6. Row-table non-admission remains visible. `skinny/RESULTS.md` has no diff in
   `74fe4e1b` and still contains exactly four measured `real_typed_struct`
   rows: `twitter`, `update_center`, `mesh`, and `marine_ik`. There are no
   measured `apache_builds/real_typed_struct` or
   `citm_catalog/real_typed_struct` rows, and the live Criterion tree has no
   Apache/CITM real-typed metadata directories. If unadmitted real-typed
   estimates are created later, the fixed W0 row-count and baseline row-id
   validator still reject them rather than silently admitting new rows.

7. Grammar-neutral, Lock 14, and no-new-surface constraints are intact for this
   fold. The targeted active-surface diff is limited to the report gate and
   REDRESS/HANDOFF text; `skinny/RESULTS.md` is unchanged, and no directive,
   BIR, `BackendShape`, substrate, generic runtime, parser, codegen, SIMD, or
   generated typed product surface changed. The focused Lock 14 suite passed,
   including W2 typed-owner allowance and out-of-owner rejection tests.

8. ORCHESTRATOR hardening discipline is satisfied for CH1. The fold is narrow,
   carries an executable regression test, keeps the remaining W0 run-id drift
   visible instead of papering it over, and aligns REDRESS/HANDOFF with the
   actual report-gate semantics.

## Verification

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`: PASS.
- `cargo test -p bbnf-bench w0_report_accepts_exact_opening_baseline -- --nocapture`: PASS, including negative run-id cases in the test.
- `cargo xtask gate-json --advisory --check-results`: expected FAIL at W0
  run-id strict drift; no Apache/CITM missing real-typed metadata failure.
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`: PASS;
  15 manifest rows and zero gate-level diagnostics.
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`: PASS, 10 tests.
- `git diff --check 74fe4e1b^ 74fe4e1b`: PASS.
- `git diff --exit-code 74fe4e1b^ 74fe4e1b -- skinny/RESULTS.md`: PASS, no diff.
- `rg` over `skinny/RESULTS.md`: only `twitter`, `update_center`, `mesh`, and
  `marine_ik` appear as measured `real_typed_struct` rows.
- `find skinny/target/criterion ... apache_builds/citm_catalog ... real_typed_struct`: no Apache/CITM real-typed metadata directories found.

## Required Folds

None. Preserve the current split: W2 source/product parity stays admitted for
Apache/CITM, benchmark row-table admission stays rejected/routed for this wave,
and the W0 run-id validator must remain strict.
