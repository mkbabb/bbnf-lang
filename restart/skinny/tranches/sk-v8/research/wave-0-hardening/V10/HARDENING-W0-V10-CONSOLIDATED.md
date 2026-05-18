# SK-V8 W0 Hardening V10 Consolidated

Date: 2026-05-18.

Target: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Verdict

REJECT.

Accept rate: 5/6 = 83.3%.

Convergence: not achieved. V10 cannot count as the first qualifying ACCEPT
cycle after the V9 reset because CH4 found material cost/reproducibility
blockers. The consecutive ACCEPT counter remains reset. W1-W6 remain blocked.

## Dispositions

| Lens | Verdict | Confidence | Disposition |
|---|---|---:|---|
| CH1 | ACCEPT | 97% | The V8 strict hard-failure blocker remains closed after the telemetry fold; W0 rows stay deferred/view-boundary and non-GO outcomes cannot reach strict admission. |
| CH2 | ACCEPT | 96% | Lock 14, grammar neutrality, no-new-surface, frozen behavior, strict-vs-strict, and non-JSON proof held. |
| CH3 | ACCEPT | 95% | The V9 telemetry-consumption blocker is folded for CostFacts sentinels, redress, Track 2 independence, substrate tuples, and structured run metadata groups. |
| CH4 | REJECT | 88% | The source fold exceeds the live post-V6 `<=120` W0 fold cap and still permits empty `arch`, `cpu`, `os`, and `simd` metadata values. |
| CH5 | ACCEPT | 96% | No hidden coupling or paper-close route found; V10 would have been only the first qualifying post-V9 accept if all lenses accepted. |
| CH6 | ACCEPT | 95% | Integration checks passed from the skinny workspace, including focused/full tests, gate replay, xtask checks, row counts, diff checks, and frozen behavior-surface checks. |

## Required V11 Fold

1. Bring the live V10 telemetry-consumption source footprint under the existing
   `<=120` post-V6 W0 fold cap, or explicitly reauthorize a larger fold with
   why/what/evidence/revert accounting. The preferred fold is reduction, not
   governance expansion.
2. Tighten `validate_w0_build_metadata()` so `host_triple` carries a non-empty
   host triple plus non-empty `arch` and `cpu` facts, and `feature_mask` carries
   non-empty `arch`, `os`, and `simd` facts plus exact
   `target_cpu=native`.
3. Add focused negative tests for empty host/feature metadata payloads while
   preserving row id, outcome/verdict, throughput, run id, and comparator
   evidence.
4. Preserve accepted V10 evidence: CostFacts sentinels, redress sentinel,
   Track 2 independence, exact workload substrate tuples, run-id/content
   fingerprinting, Criterion-root filtering, frozen behavior-surface diff,
   strict hard-failure rejection, and commit-sliced rollback.

## Evidence To Rerun After Fold

- `cargo test -p bbnf-bench w0_ -- --nocapture`
- `cargo test -p bbnf-bench strict -- --nocapture`
- `cargo test -p bbnf-bench sidecar_same_run -- --nocapture`
- `cargo test -p bbnf-bench`
- `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`
- dynamic Criterion mutation probes for non-W0 injected groups and admitted-row
  run-id drift
- `cargo xtask check-json`
- `cargo xtask check-real-typed`
- `cargo xtask check-conformance`
- frozen behavior-surface diff
- `git diff --check`

## Governance

V10 rejection preserves the reset consecutive ACCEPT counter. After the V11
fold, W0 must receive two consecutive challenge cycles at >=95% ACCEPT, with
zero critical defects and no unresolved REVISE, before W0 can close and W1-W6
can dispatch.
