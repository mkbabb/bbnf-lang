# SK-V8 W0 Hardening V7 - CH4 COST

Verdict: ACCEPT.

Confidence: 93%.

Current HEAD reviewed: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).

## Reviewed Surfaces

- Orchestrator CH4 and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:86`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:118`.
- SK-V8 packet cost, W0 scope, rollback, and downstream gates:
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:226`,
  `restart/skinny/tranches/sk-v8/SPEC.md:241`,
  `restart/skinny/tranches/sk-v8/SPEC.md:288`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:341`,
  `restart/skinny/tranches/sk-v8/SPEC.md:357`,
  `restart/skinny/tranches/sk-v8/SPEC.md:367`,
  `restart/skinny/tranches/sk-v8/SPEC.md:372`.
- Handoff and dispatch constraints:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:97`.
- V6 CH4 and consolidation:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:71`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:82`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:117`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:201`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:39`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`.
- W0 code and report surfaces:
  `skinny/crates/bbnf-bench/src/bin/gate.rs:42`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:490`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1770`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:1976`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`.
- Current W0 results:
  `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:141`.

## Findings

1. V6 CH4's cost blocker is folded in the governing packet. The old `<=350`
   W0 cap is explicitly superseded only for the measured telemetry
   gate/report/Lock 14 slice, with file-by-file accounting from `0bd16f6d` to
   `6c0bc15d`: 3532 insertions and 253 deletions across the seven W0 files
   (`restart/skinny/tranches/sk-v8/SPEC.md:322`,
   `restart/skinny/tranches/sk-v8/SPEC.md:324`,
   `restart/skinny/tranches/sk-v8/SPEC.md:326`,
   `restart/skinny/tranches/sk-v8/SPEC.md:333`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:154`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:58`).

2. The post-V6 fold fits the new limit. The current W0 row budget is
   `0 production behavior LOC`, the reauthorized Section 3 scope, and
   `post-V6 folds <=120 report/gate/test/doc LOC`
   (`restart/skinny/tranches/sk-v8/SPEC.md:218`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`). The actual fold from
   the V6 archive commit `40e8c6a7` to HEAD is 67 insertions / 7 deletions:
   8 / 1 in `DISPATCH-PROMPT.md`, 11 / 1 in `HANDOFF.md`, 29 / 4 in `SPEC.md`,
   and 19 / 1 in `skinny/crates/bbnf-bench/src/report.rs`. The code portion is
   the exact run-id binding and focused test update: `validate_sk_v8_w0` rejects
   any manifest row whose run id differs from `SK_V8_OPEN_RUN_ID`
   (`skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:660`), and the accepted-baseline test
   now rejects both one-row and uniform stale run ids
   (`skinny/crates/bbnf-bench/src/report.rs:1976`,
   `skinny/crates/bbnf-bench/src/report.rs:1980`).

3. Rollback is now realistic and commit-sliced. SPEC names the six W0
   implementation commits and any post-V6 fold commit as the rollback slice,
   then requires restoring the opening `skinny/RESULTS.md` while preserving
   hardening docs as evidence (`restart/skinny/tranches/sk-v8/SPEC.md:341`,
   `restart/skinny/tranches/sk-v8/SPEC.md:343`,
   `restart/skinny/tranches/sk-v8/SPEC.md:367`). A temporary-worktree rollback
   simulation of `git revert --no-commit f452e837 6c0bc15d 0c49fabd 077aadad
   61d5d304 cb0fdba0 6d8cb701` exited 0 with no stderr conflicts. The resulting
   touched paths were exactly the expected W0 packet/report/gate slice plus the
   `SYNTHESIS.md` doc touched by `077aadad`.

4. Gate runtime is practical. A warmed `gate-json` replay over the existing W0
   criterion root passed in 8.34s. Focused W0 tests passed in 9.67s with a cold
   target: 12 `report.rs` W0 tests and 8 gate-bin W0 tests. This is well inside
   the 90-minute implementation/redress cap and consistent with the W0 rerun
   ceiling (`restart/skinny/tranches/sk-v8/SPEC.md:241`,
   `restart/skinny/tranches/sk-v8/SPEC.md:251`).

5. I found no production behavior LOC. The frozen behavior-surface diff from
   `0bd16f6d..HEAD` exited 0 over grammar input, runtime JSON/tape, SIMD,
   codegen, generated/product helpers, Track 2, parity, scan, materialization,
   and typed-schema roots. That matches the V7 cost condition
   (`restart/skinny/tranches/sk-v8/SPEC.md:335`,
   `restart/skinny/tranches/sk-v8/SPEC.md:338`) and W0 exit prohibition on
   parser/scanner/SIMD/asm/codegen/product/generated-output changes
   (`restart/skinny/tranches/sk-v8/SPEC.md:357`). Lock 14 also validates the
   allowlist and frozen roots before gate execution
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:42`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`).

6. Same-wave telemetry consumption is still present. `gate-json` builds the
   report, validates schema plus W0 telemetry, then only writes or compares
   `RESULTS.md` after validation
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:329`). `Report::validate_sk_v8_w0`
   requires exact row count, per-row validation, exact row ids, outcome/verdict
   identity, and baseline deltas (`skinny/crates/bbnf-bench/src/report.rs:499`,
   `skinny/crates/bbnf-bench/src/report.rs:509`,
   `skinny/crates/bbnf-bench/src/report.rs:517`,
   `skinny/crates/bbnf-bench/src/report.rs:529`). The committed manifest has 38
   rows, all with `gate_only`, and no run-id mismatch; the first row shows the
   bound `sk-v8-open:criterion-fnv64-9a37562ed3d0383a`
   (`skinny/RESULTS.md:44`, `skinny/RESULTS.md:48`).

7. W1-W6 remain blocked. V6 rejection reset the consecutive ACCEPT counter and
   requires two consecutive >=95% ACCEPT challenge cycles before W0 can close
   and W1-W6 dispatch
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:62`).
   This matches ORCHESTRATOR convergence governance
   (`restart/prompts/ORCHESTRATOR.md:118`,
   `restart/prompts/ORCHESTRATOR.md:120`) and the live packet still grants only
   W0 authority while blocking W1-W6 until W0 closes and later entry gates are
   satisfied (`restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:238`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:103`).

## Commands And Evidence

- `git rev-parse HEAD`: `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `git status --short`: clean before and after review.
- `git diff --numstat 0bd16f6d..6c0bc15d -- skinny/RESULTS.md skinny/crates/bbnf-bench/src/bin/gate.rs skinny/crates/bbnf-bench/src/gate.rs skinny/crates/bbnf-bench/src/lib.rs skinny/crates/bbnf-bench/src/lock14_baseline.rs skinny/crates/bbnf-bench/src/report.rs skinny/xtask/src/main.rs`: matched the packet exactly: `95/173`, `1215/69`, `159/2`, `1/0`, `611/0`, `1431/4`, `20/5`; total `3532/253`.
- `git diff --numstat 0bd16f6d..HEAD -- ...same seven files...`: `3550/253`, reflecting only the post-V6 report fold beyond the reauthorized V6 target.
- `git diff --numstat 40e8c6a7..HEAD`: `67/7` total, under the `<=120` post-V6 fold cap.
- `git diff --exit-code 0bd16f6d..HEAD -- <frozen behavior surfaces>`: exit 0.
- `git diff --exit-code HEAD^..HEAD -- <frozen behavior surfaces>`: exit 0.
- `rg -c '^\\| json/' skinny/RESULTS.md`: `38`.
- Manifest awk check over `skinny/RESULTS.md`: `manifest_rows=38 gate_only=38 bad_run_id=0`.
- `/usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-ch4-v7-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_ -- --nocapture`: PASS, 20 W0 tests, `real 9.67`.
- `/usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results`: PASS, `real 8.34`.
- Temporary worktree rollback simulation of the named W0 commits plus HEAD: `rollback_revert_exit=0`, no stderr conflicts.

## Material Blockers

None for CH4 COST.

## Residual Risks

- The run-id binding intentionally freezes W0 to the captured criterion
  fingerprint. A legitimate future baseline refresh will need an explicit W0
  fold rather than a casual `--update-results`.
- Rollback is conflict-free in a temp worktree, but one named implementation
  commit (`077aadad`) also touches packet docs including `SYNTHESIS.md`; rollback
  still needs the SPEC-required opening `RESULTS.md` restoration and document
  reconciliation.
- I replayed the gate against the existing `/tmp/skv8-w0-target` evidence root;
  I did not run a fresh full benchmark capture in this CH4 review.
