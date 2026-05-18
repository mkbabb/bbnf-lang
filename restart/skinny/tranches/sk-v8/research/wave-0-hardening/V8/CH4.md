# SK-V8 W0 Hardening V8 - CH4 COST

Verdict: ACCEPT.

Confidence: 94%.

Target reviewed: `f452e8373ed717731dd5e720c1d947c086cc22c9`
(`fix(sk-v8-wave0): fold hardening V6 run identity and cost governance`).
Current HEAD `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd` was treated only
as the V7 documentation archive; it is not implementation evidence for this
review.

## Reviewed Surfaces

- V7/V6 CH4 challenge state and convergence reset:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/CH4.md:3`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/CH4.md:7`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:10`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:49`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:82`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:117`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:201`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/HARDENING-W0-V6-CONSOLIDATED.md:61`.
- Cost governance, W0 scope, rollback, and downstream block:
  `restart/prompts/ORCHESTRATOR.md:118`,
  `restart/prompts/ORCHESTRATOR.md:120`,
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:226`,
  `restart/skinny/tranches/sk-v8/SPEC.md:241`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:324`,
  `restart/skinny/tranches/sk-v8/SPEC.md:333`,
  `restart/skinny/tranches/sk-v8/SPEC.md:335`,
  `restart/skinny/tranches/sk-v8/SPEC.md:341`,
  `restart/skinny/tranches/sk-v8/SPEC.md:357`,
  `restart/skinny/tranches/sk-v8/SPEC.md:367`,
  `restart/skinny/tranches/sk-v8/SPEC.md:372`.
- Dispatch and handoff constraints:
  `restart/skinny/tranches/sk-v8/HANDOFF.md:5`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:154`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:238`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:6`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:103`.
- W0 implementation and validation surfaces:
  `skinny/crates/bbnf-bench/src/bin/gate.rs:42`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:489`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:499`,
  `skinny/crates/bbnf-bench/src/report.rs:517`,
  `skinny/crates/bbnf-bench/src/report.rs:529`,
  `skinny/crates/bbnf-bench/src/report.rs:660`,
  `skinny/crates/bbnf-bench/src/report.rs:1976`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`,
  `skinny/RESULTS.md:44`,
  `skinny/RESULTS.md:48`,
  `skinny/RESULTS.md:141`.

## Findings

1. V7 folded the V6 COST blocker. V6 rejected W0 because the implemented
   telemetry gate no longer fit the old `<=350` report/gate/schema/test/doc cap
   and because rollback had outgrown the promised one-slice protocol
   (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:82`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:117`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V6/CH4.md:201`).
   The target packet now explicitly reauthorizes the measured W0
   telemetry/gate/report/Lock 14 scope from `0bd16f6d` to `6c0bc15d`: 3532
   insertions and 253 deletions across seven named W0 files
   (`restart/skinny/tranches/sk-v8/SPEC.md:324`,
   `restart/skinny/tranches/sk-v8/SPEC.md:326`,
   `restart/skinny/tranches/sk-v8/SPEC.md:333`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`).

2. The post-V6 fold is under the revised cap and does not create a later-wave
   budget loophole. W0 is budgeted as `0 production behavior LOC`, the
   reauthorized Section 3 scope, and `post-V6 folds <=120
   report/gate/test/doc LOC` (`restart/skinny/tranches/sk-v8/SPEC.md:218`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`). The actual fold from
   the V6 archive commit `40e8c6a7` to target `f452e837` is 67 insertions and 7
   deletions: 8/1 in `DISPATCH-PROMPT.md`, 11/1 in `HANDOFF.md`, 29/4 in
   `SPEC.md`, and 19/1 in `skinny/crates/bbnf-bench/src/report.rs`. Later waves
   retain separate caps and conditional dispatch rows rather than inheriting the
   W0 reauthorization (`restart/skinny/tranches/sk-v8/SPEC.md:219`,
   `restart/skinny/tranches/sk-v8/SPEC.md:224`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:38`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:43`).

3. The code portion of the fold is narrowly tied to run identity. W0 row
   validation rejects any run id that differs from the captured
   `SK_V8_OPEN_RUN_ID` (`skinny/crates/bbnf-bench/src/report.rs:336`,
   `skinny/crates/bbnf-bench/src/report.rs:660`), and the focused negative
   tests reject both a single-row stale run id and a uniform stale run id
   (`skinny/crates/bbnf-bench/src/report.rs:1976`,
   `skinny/crates/bbnf-bench/src/report.rs:1980`). The gate computes the run id
   from W0 Criterion inputs and validates schema plus W0 telemetry before
   writing or comparing `RESULTS.md`
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:319`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:390`,
   `skinny/crates/bbnf-bench/src/bin/gate.rs:673`).

4. I found no production behavior LOC. The governing packet still prohibits
   parser, scanner, SIMD, asm, codegen, product-plane, and generated parser
   output changes in W0 (`restart/skinny/tranches/sk-v8/SPEC.md:357`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:139`). The frozen behavior diff
   over grammar input, runtime JSON/tape, SIMD, codegen, generated/product
   helpers, Track 2, parity, scan, materialization, and typed-schema roots was
   empty from `0bd16f6d..f452e837` and from `f452e837^..f452e837`. Lock 14 also
   runs the allowlist and frozen-root validation before gate execution
   (`skinny/crates/bbnf-bench/src/bin/gate.rs:42`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:336`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:375`,
   `skinny/crates/bbnf-bench/src/lock14_baseline.rs:399`).

5. Rollback is realistic for the implemented slice. SPEC now requires reverting
   `6d8cb701`, `cb0fdba0`, `61d5d304`, `077aadad`, `0c49fabd`, and `6c0bc15d`
   together, then any post-V6 W0 fold, restoring the opening `RESULTS.md`, and
   preserving hardening docs as evidence
   (`restart/skinny/tranches/sk-v8/SPEC.md:341`,
   `restart/skinny/tranches/sk-v8/SPEC.md:343`,
   `restart/skinny/tranches/sk-v8/SPEC.md:367`). A temporary-worktree rollback
   simulation from `f452e837` reverted those six commits plus `f452e837` with
   exit 0 and no stderr conflicts.

6. Gate runtime is practical. The implementation/redress cap includes source
   edits, generation, verification, RESULTS/REDRESS updates, and rollback
   (`restart/skinny/tranches/sk-v8/SPEC.md:241`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`). Focused W0 tests
   passed in 10.31s using a cold target, and warmed `gate-json --check-results`
   passed in 6.01s against the existing W0 evidence root. That is practical
   relative to the 90-minute cap.

7. W1-W6 are still blocked until this second accept completes at the cycle
   level. V7 consolidated as 6/6 ACCEPT but explicitly says it is the first
   qualifying ACCEPT after the V6 reset and requires one more unchanged
   re-challenge (`restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:12`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:14`,
   `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:49`).
   ORCHESTRATOR requires two consecutive challenge cycles at >=95% ACCEPT with
   no open critical defects and no unresolved REVISE
   (`restart/prompts/ORCHESTRATOR.md:118`,
   `restart/prompts/ORCHESTRATOR.md:120`). Handoff and dispatch still grant only
   W0 authority and keep W1-W6 conditional
   (`restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
   `restart/skinny/tranches/sk-v8/HANDOFF.md:238`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`,
   `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:103`).

## Commands And Evidence

- `git rev-parse HEAD`: `ff6d09c6ef53283e38b20626aa7f83aa0b85d3bd`.
- `git rev-parse f452e837`: `f452e8373ed717731dd5e720c1d947c086cc22c9`.
- `git show --name-only --pretty='format:%H%n%s' ff6d09c6 --`: only V7
  hardening docs were added.
- `git diff --name-status f452e837..HEAD`: seven added V7 artifact files only.
- `git diff --numstat f452e837..HEAD --`: V7 docs only, 1081 insertions and 0
  deletions; not counted as implementation evidence.
- `git diff --numstat 0bd16f6d..6c0bc15d -- <seven W0 files>`: `95/173`
  `skinny/RESULTS.md`, `1215/69` `src/bin/gate.rs`, `159/2` `src/gate.rs`,
  `1/0` `src/lib.rs`, `611/0` `src/lock14_baseline.rs`, `1431/4`
  `src/report.rs`, `20/5` `skinny/xtask/src/main.rs`; total 3532/253.
- `git diff --numstat 0bd16f6d..f452e837 -- <seven W0 files>`: same as above
  except `src/report.rs` is `1449/4`; total 3550/253 through target.
- `git diff --numstat 40e8c6a7..f452e837 --`: 67/7 total, under the <=120
  post-V6 fold cap.
- `git diff --exit-code --stat 0bd16f6d..f452e837 -- <frozen behavior roots>`:
  exit 0.
- `git diff --exit-code --stat f452e837^..f452e837 -- <frozen behavior roots>`:
  exit 0.
- `git diff --check 0bd16f6d..f452e837 -- <W0/report/gate/docs paths>`:
  exit 0.
- Manifest awk check over `skinny/RESULTS.md`: `manifest_rows=38 gate_only=38
  bad_run_id=0`.
- `(cd skinny && /usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-ch4-v8-test-target RUSTFLAGS='-C target-cpu=native' cargo test -p bbnf-bench w0_ -- --nocapture)`:
  PASS, 20 W0 tests, `real 10.31`.
- `(cd skinny && /usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-w0-target RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory --check-results)`:
  PASS, `real 6.01`.
- Temporary detached worktree rollback simulation:
  `git revert --no-commit f452e837 6c0bc15d 0c49fabd 077aadad 61d5d304 cb0fdba0 6d8cb701`
  exited 0 with empty stderr. The resulting touched paths were
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md`,
  `restart/skinny/tranches/sk-v8/SPEC.md`,
  `restart/skinny/tranches/sk-v8/SYNTHESIS.md`, `skinny/RESULTS.md`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs`,
  `skinny/crates/bbnf-bench/src/gate.rs`,
  `skinny/crates/bbnf-bench/src/lib.rs`,
  `skinny/crates/bbnf-bench/src/lock14_baseline.rs`,
  `skinny/crates/bbnf-bench/src/report.rs`, and `skinny/xtask/src/main.rs`.
- `git status --short --branch` before review and after rollback simulation:
  no dirty worktree entries.

## Material Blockers

None for CH4 COST.

## Residual Risks

- The V7 consolidated artifact has a non-resolving full target hash at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:5`.
  V7 CH4 itself names the correct target at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/CH4.md:7`, and
  the consolidated artifact also dispatches V8 against short target
  `f452e837` at
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V7/HARDENING-W0-V7-CONSOLIDATED.md:50`.
  I treated this as a doc hygiene residual, not an implementation COST blocker.
- Rollback is conflict-free in a temporary worktree, but completing an actual W0
  rejection still requires the SPEC-required opening `RESULTS.md` restoration
  and doc reconciliation. The simulation shows `SYNTHESIS.md` is also in the
  reverted commit slice because one named W0 fold commit touched it.
- The run-id binding intentionally freezes W0 to the captured Criterion
  fingerprint. A legitimate future W0 baseline refresh would need an explicit
  fold instead of a casual `--update-results`.
- I replayed `gate-json` against the existing `/tmp/skv8-w0-target` Criterion
  root; I did not run a fresh full benchmark capture during this V8 CH4 review.

## Second Accept Status

Yes for CH4 COST: this V8 CH4 ACCEPT can count toward the second consecutive
unchanged W0 ACCEPT cycle. It does not close W0 by itself; W0 can close only if
the consolidated V8 challenge cycle also reaches >=95% ACCEPT with zero open
critical defects and no unresolved REVISE under
`restart/prompts/ORCHESTRATOR.md:120`.
