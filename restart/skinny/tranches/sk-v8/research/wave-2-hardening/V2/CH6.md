# SK-V8 W2 Hardening V2 - CH6

Role: CH6 anti-paper-close audit.
Target reviewed: current HEAD `8ce03af4`
(`fix(sk-v8-wave2-gate): fold typed hardening disposition`).

## Verdict

ACCEPT.

Confidence: 96%.

## Findings

1. Measured row admission is no longer implied as W2 closure. `HANDOFF.md`
   says W2 has source/product parity admitted and benchmark row-table admission
   rejected for this wave, then moves the next active wave to W3
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:5-8`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:175-187`). The current measured
   authority remains W0-rendered `skinny/RESULTS.md` with only four
   `real_typed_struct` rows (`HANDOFF.md:33-40`). REDRESS 91 uses the same
   distinction: `apache_builds/real_typed_struct` and
   `citm_catalog/real_typed_struct` are admitted source/product rows, not
   measured rows in the current W0 manifest (`skinny/REDRESS.md:2622-2625`).

2. Strict-vs-strict performance victory is not claimed for W2. The W2 proof is
   generated Track 1 DirectBuild checksum parity against serde_json as the
   Track 2/oracle path plus a separate sonic-rs checksum parity lane
   (`skinny/REDRESS.md:2632-2636`). The research and plan explicitly remove the
   extra independent Track 2 parser claim and state that `track2_typed`
   delegates to serde_json
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-typed-product-expansion.md:11-15`,
   `restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:41-43`). No
   Apache/CITM strict performance row is added to `skinny/RESULTS.md`.

3. There is no hidden benchmark deferral. W2 does not say "benchmark later" or
   preserve an unclosed row-table TODO; it rejects benchmark row-table admission
   for this wave because the W0 run-id validator was already known to reject
   local Criterion metadata drift unrelated to W2 source
   (`skinny/REDRESS.md:2648-2652`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:184-187`). The W2 plan's
   conditional benchmark-refresh wording now routes any failed refresh to
   unchanged `RESULTS.md`, explicit row-table rejection, and source/product
   parity evidence instead of weakening validation
   (`restart/skinny/tranches/sk-v8/research/skv8-W2-plan.md:46-50`).

4. The W0 run-id validator remains intact. `TelemetryRow::validate_sk_v8_w0`
   still rejects any row whose `run_id` differs from
   `SK_V8_OPEN_RUN_ID` (`skinny/crates/bbnf-bench/src/report.rs:336-340`),
   and report validation still requires the exact W0 baseline row set,
   outcomes, verdicts, and throughput deltas
   (`skinny/crates/bbnf-bench/src/report.rs:494-532`). The constant remains
   `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
   (`skinny/crates/bbnf-bench/src/report.rs:655`). The focused regression test
   still covers both single-row and uniform run-id drift rejection
   (`skinny/crates/bbnf-bench/src/report.rs:2031-2039`).

5. The V1 folds landed without broadening Lock 14. The parent-diff allowance is
   limited to W2 subjects and the three typed owner paths
   (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:399-404`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:445-466`), with tests for
   W2-only allowance, non-W2 rejection, path normalization, and out-of-scope W2
   rejection (`skinny/crates/bbnf-bench/src/lock14_baseline.rs:620-646`).
   The schema identity was bumped to `sk-v8-real-typed-w2` in both the schema
   source and generated output (`skinny/xtask/src/real_typed_schema.rs:8-24`,
   `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1-4`).

Focused verification run during this audit:

- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --exact`
- `cargo test -p bbnf-bench lock14_baseline::tests::admits_w2_typed_owner_parent_diff_only_under_w2_scope -- --exact`
- `cargo test -p bbnf-bench lock14_baseline::tests::rejects_w2_scope_parent_diff_outside_typed_owner_paths -- --exact`

All three passed.

## Required Folds

None.
