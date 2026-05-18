# SK-V8 W0 Hardening V11 - CH4 COST

Verdict: ACCEPT.

Confidence: 95%.

Target reviewed: `61d5cc3b4312883e026060174e876a0c18b34703`
(`fix(sk-v8-wave0): fold hardening V10 cost and metadata blockers`).

## Scope Reviewed

- Challenge governance and COST lens:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:86`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 W0 caps, rerun ceilings, owner paths, same-wave consumer, and rollback:
  `restart/skinny/tranches/sk-v8/SPEC.md:142`,
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:226`,
  `restart/skinny/tranches/sk-v8/SPEC.md:251`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:339`,
  `restart/skinny/tranches/sk-v8/SPEC.md:341`,
  `restart/skinny/tranches/sk-v8/SPEC.md:360`.
- Dispatch/handoff cost constraints:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`.
- V10 blockers and required V11 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:31`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:35`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:39`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:94`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:97`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:103`.

## Evidence

1. The live post-V6 W0 cap is now satisfied under the requested accounting.
   `git diff --numstat 00c3485a -- skinny/crates/bbnf-bench/src/report.rs`
   returns `118 13 skinny/crates/bbnf-bench/src/report.rs`. The governing W0
   row still says post-V6 folds are `<=120 report/gate/test/doc LOC`
   (`restart/skinny/tranches/sk-v8/SPEC.md:218`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`). I count the cap as
   live insertion footprint, consistent with the V10 blocker wording that the
   old `169` insertion footprint exceeded the cap before deletions or churn
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:45`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/CH4.md:50`).
   Under that live rule, `118 <= 120`.

2. The target fold is source-scoped and behavior-frozen. `git show --stat
   --oneline --decorate --no-renames 61d5cc3b` reports one touched file,
   `skinny/crates/bbnf-bench/src/report.rs`, with `58 insertions / 109
   deletions`. `git diff --name-only 61d5cc3b^ 61d5cc3b --` returns only
   `skinny/crates/bbnf-bench/src/report.rs`. `git diff --name-only
   0bd16f6d..HEAD -- skinny/crates/runtime skinny/crates/bbnf-simd
   skinny/crates/codegen skinny/crates/bbnf-bench/src/track2
   skinny/crates/bbnf-bench/src/scan.rs
   skinny/crates/bbnf-bench/src/materialization.rs
   skinny/crates/bbnf-bench/src/parity.rs
   skinny/crates/bbnf-bench/src/generated_real_typed.rs
   skinny/crates/bbnf-bench/benches skinny/xtask/src/bin/parity_oracle.rs`
   returns no paths, satisfying the W0 behavior-freeze requirement
   (`restart/skinny/tranches/sk-v8/SPEC.md:335`,
   `restart/skinny/tranches/sk-v8/SPEC.md:357`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:139`).

3. Empty metadata acceptance is closed. `validate_w0_manifest_semantics()`
   defines `has_nonempty` by stripping each key prefix and rejecting empty
   tails (`skinny/crates/bbnf-bench/src/report.rs:1021`). It then rejects an
   empty host triple, a host without `-`, and empty/missing `arch=` or `cpu=`
   host facts (`skinny/crates/bbnf-bench/src/report.rs:1043`,
   `skinny/crates/bbnf-bench/src/report.rs:1046`). It separately
   rejects empty/missing `arch=`, `os=`, or `simd=` feature facts and requires
   exact `target_cpu=native`
   (`skinny/crates/bbnf-bench/src/report.rs:1053`). The W0 baseline negative
   test now mutates both empty host metadata and empty feature-mask metadata
   while preserving the rest of the accepted row shape
   (`skinny/crates/bbnf-bench/src/report.rs:2053`,
   `skinny/crates/bbnf-bench/src/report.rs:2065`,
   `skinny/crates/bbnf-bench/src/report.rs:2068`).

4. The accepted V10 semantic-consumption checks survive the compaction.
   CostFacts, redress, and Track 2 sentinels still reject unsupported W0
   manifest values (`skinny/crates/bbnf-bench/src/report.rs:1009`,
   `skinny/crates/bbnf-bench/src/report.rs:1014`). The workload substrate tuple
   is still compared against exact expected W0 values
   (`skinny/crates/bbnf-bench/src/report.rs:1063`,
   `skinny/crates/bbnf-bench/src/report.rs:1074`), with `parse_only`,
   `direct_to_struct`, and `real_typed_struct` mapped separately
   (`skinny/crates/bbnf-bench/src/report.rs:1083`,
   `skinny/crates/bbnf-bench/src/report.rs:1091`). The W0 baseline negative
   test still mutates CostFacts, redress, Track 2 independence, and substrate
   surface (`skinny/crates/bbnf-bench/src/report.rs:2058`,
   `skinny/crates/bbnf-bench/src/report.rs:2061`,
   `skinny/crates/bbnf-bench/src/report.rs:2069`).

5. The same-wave consumer remains present. The W0 schema requires every
   emitted field to be consumed by `gate-json`
   (`restart/skinny/tranches/sk-v8/SPEC.md:142`) and the W0 exit gate repeats
   the same-wave consumer requirement
   (`restart/skinny/tranches/sk-v8/SPEC.md:360`). The gate binary adds the W0
   telemetry note and then calls `report.validate_schema_v3().and_then(|_|
   report.validate_sk_v8_w0())`, failing the process on validation error
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:315`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:319`). Inside the report
   validator, required SK-V8 text fields reject empty values before row
   identity, run id, profile, hot leaf, manifest semantics, and
   `same_wave_consumer_class == "gate_only"` are checked
   (`skinny/crates/bbnf-bench/src/report.rs:317`,
   `skinny/crates/bbnf-bench/src/report.rs:328`,
   `skinny/crates/bbnf-bench/src/report.rs:355`,
   `skinny/crates/bbnf-bench/src/report.rs:356`).

6. Rerun evidence is realistic for the W0 artifact replay. From
   `/Users/mkbabb/Programming/bbnf-lang/skinny`, these commands passed:
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v11-target cargo test -p bbnf-bench w0_ --
   --nocapture` (12 report W0 tests and 8 gate-bin W0 tests),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v11-target cargo test -p bbnf-bench strict
   -- --nocapture` (5 tests),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v11-target cargo test -p bbnf-bench
   sidecar_same_run -- --nocapture` (1 test),
   `CARGO_TARGET_DIR=/tmp/skv8-ch4-v11-target cargo test -p bbnf-bench`
   (52 lib tests, 8 gate-bin tests, 0 doctests),
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native'
   cargo xtask gate-json --advisory --check-results`,
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-json`,
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-real-typed`, and
   `CARGO_TARGET_DIR=/tmp/skv8-w0-target cargo xtask check-conformance`
   (`conformance: 21 valid fixtures accepted; 7 invalid fixtures rejected`).
   `awk` over `skinny/RESULTS.md` reports `main_rows=38 manifest_rows=38`.
   `git diff --check 00c3485a -- skinny/crates/bbnf-bench/src/report.rs`
   returned clean.

7. Rollback remains commit-sliced. SPEC requires reverting the named W0
   implementation commits together, then any post-V6 W0 fold commit
   (`restart/skinny/tranches/sk-v8/SPEC.md:341`,
   `restart/skinny/tranches/sk-v8/SPEC.md:367`). In a throwaway clone,
   `git revert --no-commit 61d5cc3b 3a9fa326 00c3485a f452e837 6c0bc15d
   0c49fabd 077aadad 61d5d304 cb0fdba0 6d8cb701` exited 0. The staged slice
   was the expected W0 packet/docs plus `skinny/RESULTS.md`,
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

- The `118 / 13` cap result is accepted as a live insertion-footprint budget.
  If the orchestrator later redefines this cap as additions plus deletions,
  the same diff would be `131` changed lines and would require a governance
  fold. I found no live instruction applying that stricter churn metric to this
  post-V6 W0 fold.
- `gate-json --check-results` is a replay over W0 Criterion capture artifacts.
  It passes against `/tmp/skv8-w0-target`, the documented W0 target root. A
  fresh empty `CARGO_TARGET_DIR` fails with `missing Criterion metadata rows`,
  which is consistent with the gate consuming captured metadata rather than
  generating benchmarks itself, but it should stay explicit in rerun notes.
- W0 still needs two consecutive >=95% ACCEPT challenge cycles before W1-W6 can
  dispatch (`restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:123`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V10/HARDENING-W0-V10-CONSOLIDATED.md:64`).
