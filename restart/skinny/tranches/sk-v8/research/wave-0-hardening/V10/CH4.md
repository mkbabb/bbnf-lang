# SK-V8 W0 Hardening V10 - CH4 COST

Verdict: REJECT.

Confidence: 88%.

Target reviewed: `3a9fa32675cedb1f8a0d31247df229fe505068be`
(`fix(sk-v8-wave0): fold hardening V9 telemetry consumption blocker`).

## Scope Reviewed

- V9 consolidated blocker and required V10 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V9/HARDENING-W0-V9-CONSOLIDATED.md`.
- SK-V8 cost, cap, rerun, rollback, and W0-only constraints:
  `restart/skinny/tranches/sk-v8/SPEC.md`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md`.
- Target patch:
  `skinny/crates/bbnf-bench/src/report.rs`.

## Evidence

1. The fold addresses the V9 semantic-consumption shape in the right owner
   file. `TelemetryRow::validate_sk_v8_w0()` now delegates to
   `validate_w0_manifest_semantics()` after run id, profile artifact, hot leaf,
   and sample-cost checks. The new helper validates W0 CostFacts sentinels,
   redress sentinel, Track 2 independence, build metadata, and workload-specific
   substrate tuples. The substrate tuples are exact by workload:
   `parse_only` requires
   `borrowed_view_over_offset_tape / discarded_after_capacity / one`,
   `direct_to_struct` requires
   `sink_only_digest / n/a / zero_or_inert`, and `real_typed_struct` requires
   `typed_direct_projection / n/a / zero_or_inert`.

2. The patch is still telemetry/report validation only. `git show --stat
   3a9fa326` reports one touched file,
   `skinny/crates/bbnf-bench/src/report.rs`, with `169 insertions / 13
   deletions`. The frozen behavior-surface diff over runtime, SIMD, codegen,
   generated/product, bench fixture, and related W0 behavior roots from
   `0bd16f6d..3a9fa326` returned empty. `git diff --check
   00c3485a..3a9fa326 -- skinny/crates/bbnf-bench/src/report.rs` returned
   clean.

3. Cost governance does not pass as written. The live packet still says W0
   post-V6 folds are `<=120 report/gate/test/doc LOC`, and LOC budgets are
   conjunctive with the 90-minute cap and rerun ceilings. V9 named a critical
   W0 gate defect, which authorizes this kind of fold, but I found no
   reauthorization that raises or waives the `<=120` post-V6 fold cap. The
   target commit's `169 insertions / 13 deletions` in `report.rs` exceeds that
   cap before counting deletions or touched-line churn.

4. Reproducibility is materially improved but still under-consumed for malformed
   host/feature metadata. `validate_w0_build_metadata()` requires exact
   `profile=bench`, `rustflags=-C target-cpu=native`, and `target_cpu=native`.
   It also requires a semicolon-structured host string and `arch=`, `os=`,
   `simd=`, and `target_cpu=native` in the feature mask. However, it checks
   `arch=`, `cpu=`, `os=`, and `simd=` with `starts_with()` only, so
   `a-b;arch=;cpu=` and `arch=;os=;simd=;target_cpu=native` would pass despite
   carrying empty architecture/OS/SIMD/CPU facts. V9 required build/run metadata
   to be consumed enough to avoid producer-only text and specifically called for
   a non-empty host triple plus architecture.

5. The focused negative tests cover the broad field groups but not the empty
   value case. The new W0 full-baseline test mutates CostFacts, redress,
   Track 2 independence, old/default build flags, unstructured host, missing
   feature-mask field, and a wrong substrate surface. It does not mutate
   `host_triple` to empty-key payloads or `feature_mask` to empty `arch/os/simd`
   payloads while preserving row id, outcome/verdict, throughput, run id, and
   comparator evidence.

6. Run id and Criterion-root volatility remain intact. The W0 run id remains
   `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`; row validation still compares
   every row against that constant, and the existing fingerprint test excludes
   derendered probe estimates, future unvalidated fixtures, and unmatched
   workload estimates while moving on a validated W0 estimate.

7. Rollback is feasible and commit-sliced. In a detached temporary worktree,
   `git revert --no-commit 3a9fa326 00c3485a f452e837 6c0bc15d 0c49fabd
   077aadad 61d5d304 cb0fdba0 6d8cb701` exited 0 with the expected W0 slice
   staged: SK-V8 packet docs, `skinny/RESULTS.md`, `bbnf-bench` gate/report/lib
   files, `lock14_baseline.rs`, and `skinny/xtask/src/main.rs`.

8. Verification is green but not enough to override the two blockers:
   `cargo test -p bbnf-bench w0_ -- --nocapture` passed 20 tests in 2.37s;
   `cargo test -p bbnf-bench strict -- --nocapture` passed 5 tests in 2.35s;
   `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed 1 test in
   2.33s; `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C
   target-cpu=native' cargo xtask gate-json --advisory --check-results` passed
   in 6.21s.

## Blockers

1. `3a9fa326` exceeds the live post-V6 W0 fold budget: `169 insertions / 13
   deletions` in `report.rs` versus `<=120 report/gate/test/doc LOC`.

2. Build/run metadata validation still permits empty `arch`, `cpu`, `os`, and
   `simd` values, leaving a reproducibility paper-close route inside the new
   report validator.

## Required Fold If Rejecting

1. Bring the V10 fold under the existing `<=120` post-V6 fold cap, split it, or
   add an explicit SK-V8 governance fold that reauthorizes the larger LOC with
   why/what/evidence/revert accounting.

2. Tighten `validate_w0_build_metadata()` so `host_triple` has a non-empty host
   triple and non-empty `arch`/`cpu` facts, and `feature_mask` has non-empty
   `arch`, `os`, and `simd` facts plus exact `target_cpu=native`.

3. Add focused negative tests for empty host/feature metadata payloads while
   preserving row id, outcome/verdict, throughput, run id, and comparator
   evidence.

4. Preserve the accepted V10 evidence: CostFacts sentinels, redress sentinel,
   Track 2 independence, exact workload substrate tuples, run-id/content
   fingerprinting, Criterion-root filtering, frozen behavior-surface diff, and
   commit-sliced rollback.
