# SK-V8 W0 Hardening V12 - CH4 COST

Verdict: ACCEPT.

Confidence: 96%.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Scope Reviewed

- CH4 COST lens and convergence rule:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:86`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 W0 cap, rerun, same-wave consumer, and rollback authority:
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:226`,
  `restart/skinny/tranches/sk-v8/SPEC.md:251`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`,
  `restart/skinny/tranches/sk-v8/SPEC.md:341`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`.
- Dispatch and handoff cost constraints:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:155`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`.
- V10 blockers and V11 first qualifying cycle:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:97`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:10`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:26`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:70`.

## Evidence

1. The unchanged V11 target remains under the live post-V6 W0 cap under the
   accepted V11 accounting. `git diff --numstat 00c3485a..61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` returns
   `118 13 skinny/crates/bbnf-bench/src/report.rs`; the governing W0 row still
   says post-V6 folds are `<=120 report/gate/test/doc LOC`
   (`restart/skinny/tranches/sk-v8/SPEC.md:218`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`). V10 rejected `169`
   insertions as over cap before counting deletions or churn
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`);
   V11 accepted the live insertion footprint as `118 <= 120`
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/CH4.md:44`).

2. The target remains source-scoped to report validation. `git show --stat
   --oneline --decorate --no-renames 61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` reports one file changed with `58
   insertions / 109 deletions`, and `git diff --name-only 61d5cc3b^
   61d5cc3b --` returns only `skinny/crates/bbnf-bench/src/report.rs`. Current
   W0 source inputs are unchanged after the V11 docs commit:
   `git diff --name-only 61d5cc3b..HEAD -- skinny/crates/bbnf-bench/src/report.rs
   skinny/crates/bbnf-bench/src/bin/gate.rs skinny/RESULTS.md` returns no paths.
   The frozen behavior-surface diff from `0bd16f6d..61d5cc3b` over runtime,
   SIMD, codegen, Track 2, scan, materialization, parity, generated typed,
   benches, and parity-oracle paths also returns no paths, satisfying the W0
   behavior-freeze rule (`restart/skinny/tranches/sk-v8/SPEC.md:335`,
   `restart/skinny/tranches/sk-v8/SPEC.md:357`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:139`).

3. Empty metadata rejects remain executable in `report.rs`. The W0 manifest
   validator defines `has_nonempty` by stripping a key prefix and rejecting empty
   tails (`skinny/crates/bbnf-bench/src/report.rs:1021`). It rejects host
   metadata without a host triple, dash, non-empty `arch=`, and non-empty `cpu=`
   (`skinny/crates/bbnf-bench/src/report.rs:1039`,
   `skinny/crates/bbnf-bench/src/report.rs:1043`,
   `skinny/crates/bbnf-bench/src/report.rs:1045`). It also rejects feature masks
   missing non-empty `arch=`, `os=`, or `simd=`, and requires exact
   `target_cpu=native` (`skinny/crates/bbnf-bench/src/report.rs:1053`,
   `skinny/crates/bbnf-bench/src/report.rs:1056`). The W0 full-baseline test
   mutates both empty host metadata and empty feature-mask metadata and expects
   `validate_sk_v8_w0()` to fail
   (`skinny/crates/bbnf-bench/src/report.rs:2065`,
   `skinny/crates/bbnf-bench/src/report.rs:2068`).

4. The same-wave consumer and gate remain present. The spec requires every
   emitted telemetry field to be consumed by `gate-json` and rejects
   producer-only telemetry (`restart/skinny/tranches/sk-v8/SPEC.md:142`); the W0
   exit gate repeats that `gate-json` consumes every emitted telemetry field in
   the same slice (`restart/skinny/tranches/sk-v8/SPEC.md:360`). The gate binary
   adds the W0 telemetry note and then runs
   `report.validate_schema_v3().and_then(|_| report.validate_sk_v8_w0())`,
   exiting invalid on any validation error
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:315`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`). Inside `report.rs`, required
   W0 text fields reject empty values before row identity, run id, profile/hot
   leaf, manifest semantics, and `same_wave_consumer_class == "gate_only"` are
   checked (`skinny/crates/bbnf-bench/src/report.rs:317`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:356`).

5. The V10 semantic-consumption fold remains intact after the V11 compaction.
   CostFacts, redress, and Track 2 sentinels reject unsupported W0 values
   (`skinny/crates/bbnf-bench/src/report.rs:1009`,
   `skinny/crates/bbnf-bench/src/report.rs:1013`). The substrate tuple is exact
   per workload (`skinny/crates/bbnf-bench/src/report.rs:1063`,
   `skinny/crates/bbnf-bench/src/report.rs:1074`), with `parse_only`,
   `direct_to_struct`, and `real_typed_struct` mapped separately
   (`skinny/crates/bbnf-bench/src/report.rs:1083`,
   `skinny/crates/bbnf-bench/src/report.rs:1090`). The negative test still
   mutates CostFacts, redress, Track 2 independence, and substrate surface
   (`skinny/crates/bbnf-bench/src/report.rs:2058`,
   `skinny/crates/bbnf-bench/src/report.rs:2061`,
   `skinny/crates/bbnf-bench/src/report.rs:2069`).

6. Rerun evidence is current and realistic for this COST re-challenge. From
   `/Users/mkbabb/Programming/bbnf-lang/skinny`, these commands passed:
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v12-target cargo test -p bbnf-bench w0_ --
   --nocapture` (12 report W0 tests and 8 gate-bin W0 tests),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v12-target cargo test -p bbnf-bench strict
   -- --nocapture` (5 tests),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v12-target cargo test -p bbnf-bench
   sidecar_same_run -- --nocapture` (1 test),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v12-target cargo test -p bbnf-bench` (52 lib
   tests, 8 gate-bin tests, 0 doctests),
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo
   xtask gate-json --advisory --check-results`,
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-json`,
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-real-typed`, and
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-conformance`
   (`conformance: 21 valid fixtures accepted; 7 invalid fixtures rejected`).
   `awk` over `skinny/RESULTS.md` reports `main_rows=38 manifest_rows=38`, and
   `git diff --check 00c3485a..61d5cc3b --
   skinny/crates/bbnf-bench/src/report.rs` returned clean.

7. Rollback remains commit-sliced. SPEC requires reverting the named W0
   implementation commits together, then any post-V6 W0 fold
   (`restart/skinny/tranches/sk-v8/SPEC.md:341`,
   `restart/skinny/tranches/sk-v8/SPEC.md:367`). In a detached throwaway
   worktree, `git revert --no-commit 61d5cc3b 3a9fa326 00c3485a f452e837
   6c0bc15d 0c49fabd 077aadad 61d5d304 cb0fdba0 6d8cb701` exited 0. The staged
   slice was the expected W0 packet/docs plus `skinny/RESULTS.md`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs`,
   `skinny/crates/bbnf-bench/src/gate.rs`,
   `skinny/crates/bbnf-bench/src/lib.rs`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
   `skinny/crates/bbnf-bench/src/report.rs`, and `skinny/xtask/src/main.rs`.

## Blockers

None.

## Required Fold If Rejecting

Not applicable. I am not rejecting this target.

## Residual Risk

- The `118 / 13` result is accepted as the live insertion-footprint budget, as
  in V11. If later governance redefines the `<=120` cap as insertions plus
  deletions, the same diff would be `131` changed lines and would require a
  governance fold.
- `gate-json --check-results` is a replay over the existing W0 Criterion capture
  artifacts in `/tmp/skv8-w0-target`; it is valid for W0 telemetry replay, but it
  is not a fresh performance benchmark run.
- This ACCEPT supplies the unchanged second qualifying CH4 COST cycle for V12,
  but W0 closure still depends on the full V12 consolidated result reaching the
  ORCHESTRATOR two-cycle convergence rule with zero critical defects and no
  unresolved REVISE (`restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:120`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V11/HARDENING-W0-V11-CONSOLIDATED.md:14`).
