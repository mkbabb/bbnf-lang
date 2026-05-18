# SK-V8 W2 Hardening V4 Consolidated

Date: 2026-05-18.
Target: `74fe4e1b` (`fix(sk-v8-wave2-gate): bind real typed metadata expectations to measured W0 rows`).

## Verdict

ACCEPT, 6/6.

Minimum confidence: 93%.

## Disposition

V4 is the first post-fold ACCEPT cycle after the V3 REVISE. The V3 blocker is
closed: the standard checked report gate no longer requires Apache/CITM
`real_typed_struct` Criterion metadata before benchmark row-table admission.
The gate derives W0 real typed metadata requirements from measured
`SK-V8-open` baseline rows while preserving the strict W0 run-id validator.

Apache/CITM remain source/product parity rows only. `skinny/RESULTS.md` still
contains the W0 four measured `real_typed_struct` rows, Canada remains routed
out on full-fixture DirectBuild-versus-serde checksum mismatch, and W2
benchmark row-table admission remains rejected/routed for this wave.

## Verification Cited By Challenge

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo xtask check-real-typed`
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`
- `cargo xtask gate-json --advisory --check-results` expected-failed only at the known W0 run-id strict drift
- `git diff --exit-code HEAD -- skinny/RESULTS.md`
- `git diff --check`

## Required Folds

None.

## Convergence Status

V4 is qualifying ACCEPT cycle 1 after the V3 reset. Run V5 against the unchanged
V4-folded target before closing W2.
