# SK-V8 W2 Hardening V5 Consolidated

Date: 2026-05-18.
Target: unchanged V4-folded target at `bf2f073d`
(`docs(sk-v8-wave2-hardening): record V4 accept cycle`).

## Verdict

ACCEPT, 6/6.

Minimum confidence: 94%.

## Disposition

V5 is the second consecutive post-fold ACCEPT cycle after the V3 REVISE reset.
Together with V4, it closes W2 hardening convergence.

The V3 blocker remains folded: W0 real typed metadata requirements are derived
from measured `SK-V8-open` baseline rows, not from the source/product typed
fixture map. Apache/CITM source-only typed fixtures therefore do not require
unadmitted Criterion `real_typed_struct` rows. The standard checked report path
still fails closed on the known W0 run-id drift when run against the local
Criterion tree; that strict validator is not weakened.

W2 closes with Apache/CITM source/product parity admitted, Canada routed out,
`skinny/RESULTS.md` unchanged, and benchmark row-table admission rejected for
this wave.

## Verification Cited By Challenge

- `cargo test -p bbnf-bench w0_real_typed_metadata_expectation_uses_measured_baseline_not_source_fixtures -- --nocapture`
- `cargo test -p bbnf-bench report::tests::w0_report_accepts_exact_opening_baseline -- --nocapture`
- `cargo test -p bbnf-bench real_typed -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `cargo xtask check-real-typed`
- `cargo xtask check-json`
- `cargo xtask check-conformance`
- `cargo xtask gate-json --with-cost-facts --advisory --check-results`
- `cargo xtask gate-json --advisory --check-results` expected-failed only at the known W0 run-id strict drift
- `git diff --exit-code HEAD -- skinny/RESULTS.md`
- `git diff --check`

## Required Folds

None.

## Close Status

W2 is converged and closed as source/product parity admitted with benchmark
row-table admission rejected/routed. W3 is the next wave.
