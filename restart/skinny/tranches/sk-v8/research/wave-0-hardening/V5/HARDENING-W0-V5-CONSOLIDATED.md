# SK-V8 W0 Hardening V5 Consolidated

Date: 2026-05-18.

Target: `0c49fabd6d6facd136e1e69b8482aa4f239561ae`
(`fix(sk-v8-wave0): fold hardening V4 gate blockers`).

## Verdict

REJECT.

Accept rate: 4/6 = 66.7%.

Convergence: not achieved. V5 is not a qualifying ACCEPT cycle under
`restart/prompts/ORCHESTRATOR.md` Section 3Z because CH1 and CH4 found material
W0 gate-honesty blockers. W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | REJECT | 98% | W0 validates broad outcome allowlists but does not bind exact opening outcome/verdict identity per row; the baseline test accepts impossible `K` parse rows and `A / GO` direct misses. |
| CH2 | ACCEPT | 96% | Comparator allowlist, native strict-only admission, sidecar-same-run rejection, and flaw-probe isolation held after the V4 fold. |
| CH3 | ACCEPT | 96% | Lock 14, frozen-root diffs, fixture-scoped fingerprinting, and no directive/BIR/substrate drift held. |
| CH4 | REJECT | 96% | `run_id` is fixture-name scoped but not exact row-manifest scoped; a valid fixture with an unvalidated real-typed comparator estimate still perturbs `run_id`. |
| CH5 | ACCEPT | 95% | Packet/RESULTS consistency and W1-W6 blocking posture held, with only residual stale planning prose risks. |
| CH6 | ACCEPT | 96% | Replay, copied-root checks, SIMD mutation rejection, unvalidated-corpus check, and anti-paper-close evidence held. |

## Required V6 Fold

1. Extend `SK_V8_OPEN_BASELINE` to bind exact `outcome_id` and `verdict` per
   opening row, in addition to Track 1 and Track 2 baselines.
2. Make `Report::validate_sk_v8_w0()` reject any row whose `row_id`,
   `outcome_id`, `verdict`, Track 1, or Track 2 differs from the W0 opening
   identity beyond the existing numeric tolerance.
3. Replace the permissive `w0_report_accepts_exact_opening_baseline` fixture so
   the accepted rows use the actual W0 outcome/verdict tuple. Add negative tests
   for at least `twitter/parse_only` `S -> K` and `twitter/direct_to_struct`
   `N-direct / NO-GO -> A / GO`.
4. Build `criterion_fingerprint` from exact W0 row membership rather than
   `fixture_names x W0_CRITERION_BENCHES`. Real-typed benchmark/comparator
   inputs must be accepted only for the four W0 real-typed rows, not every valid
   fixture. Keep `json_unvalidated_future` and `json_probes_*` exclusions.
5. Add a focused negative for a valid fixture with an unvalidated row input,
   such as `json_canada/sonic_rs_real_typed_struct/new/estimates.json`.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench`
- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- dynamic `json_unvalidated_future` Criterion mutation
- dynamic valid-fixture/unvalidated-row Criterion mutation
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- `git diff --check`

## Governance

V5 rejection resets the consecutive ACCEPT counter. After the V6 fold, W0 must
receive two consecutive challenge cycles at at least 95% ACCEPT, with no open
critical defects, before W0 can close and W1-W6 can dispatch.
