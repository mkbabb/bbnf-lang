# SK-V8 W0 Hardening V9 - CH4 COST

Verdict: ACCEPT.

Confidence: 95%.

Target reviewed: `00c3485a8774296e796c2f68b74fd3d559627f0a`
(`fix(sk-v8-wave0): fold hardening V8 strict hard-failure blocker`).

## Scope Reviewed

- Orchestrator CH4 and convergence governance:
  `restart/prompts/ORCHESTRATOR.md:74`,
  `restart/prompts/ORCHESTRATOR.md:86`,
  `restart/prompts/ORCHESTRATOR.md:104`,
  `restart/prompts/ORCHESTRATOR.md:120`.
- SK-V8 W0 cost, rerun, rollback, and W0-only dispatch constraints:
  `restart/skinny/tranches/sk-v8/SPEC.md:218`,
  `restart/skinny/tranches/sk-v8/SPEC.md:241`,
  `restart/skinny/tranches/sk-v8/SPEC.md:251`,
  `restart/skinny/tranches/sk-v8/SPEC.md:322`,
  `restart/skinny/tranches/sk-v8/SPEC.md:338`,
  `restart/skinny/tranches/sk-v8/SPEC.md:340`,
  `restart/skinny/tranches/sk-v8/SPEC.md:341`,
  `restart/skinny/tranches/sk-v8/SPEC.md:357`,
  `restart/skinny/tranches/sk-v8/SPEC.md:367`,
  `restart/skinny/tranches/sk-v8/SPEC.md:372`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:131`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:139`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:142`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:148`,
  `restart/skinny/tranches/sk-v8/HANDOFF.md:236`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:37`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:47`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:56`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:66`,
  `restart/skinny/tranches/sk-v8/DISPATCH-PROMPT.md:99`.
- V8 rejection and required V9 fold:
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:8`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:16`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:25`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/HARDENING-W0-V8-CONSOLIDATED.md:34`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/CH1.md:59`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/CH1.md:102`,
  `restart/skinny/tranches/sk-v8/research/wave-0-hardening/V8/CH1.md:129`.
- V9 code surfaces:
  `skinny/crates/bbnf-bench/src/gate.rs:135`,
  `skinny/crates/bbnf-bench/src/gate.rs:139`,
  `skinny/crates/bbnf-bench/src/gate.rs:459`,
  `skinny/crates/bbnf-bench/src/report.rs:275`,
  `skinny/crates/bbnf-bench/src/report.rs:336`,
  `skinny/crates/bbnf-bench/src/report.rs:1012`,
  `skinny/crates/bbnf-bench/src/report.rs:1954`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:384`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:673`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:717`,
  `skinny/crates/bbnf-bench/src/bin/gate.rs:1770`.

## Findings

1. The V9 fold is a focused response to the V8 critical blocker, not a new
   behavior wave. V8 rejected because a current hard-failure row could be
   relabeled strict/measured while preserving row id, outcome, verdict,
   throughput, and run id. The V9 commit changes only
   `skinny/crates/bbnf-bench/src/gate.rs` and
   `skinny/crates/bbnf-bench/src/report.rs`, with 24 insertions / 7 deletions
   in `gate.rs` and 37 insertions / 57 deletions in `report.rs` from
   `84f885a4..00c3485a`. This fits the SPEC exception allowing post-V6 fold
   work when a later challenge names a critical W0 gate defect.

2. Cost governance remains bounded. The original W0 implementation
   reauthorization is still the seven-file telemetry/gate/report/Lock 14 slice
   from `0bd16f6d` to `6c0bc15d`; V7/V8 already accepted that accounting. V9
   does not add parser, scanner, SIMD, asm, codegen, generated parser,
   product-plane, or benchmark behavior LOC. The frozen behavior-surface diff
   from `0bd16f6d..HEAD` over runtime, SIMD, codegen, generated/product,
   Track 2, parity, scan, materialization, fixture, and grammar roots returned
   empty.

3. The strict hard-failure blocker is folded at both enforcement points. The
   helper now rejects every outcome whose verdict is not `GO` before checking
   comparator evidence, which covers `G`, `I`, `J`, `K`, `L`, `M`, `N-direct`,
   and `S`, plus conditional/focus outcomes. W0 row validation now freezes all
   current opening rows to `Strictness=deferred`,
   `measured_validation_path=view-boundary`, `parse_utf8=view-boundary`, and
   `escape_complete=yes`; it no longer tries to upgrade a W0 row into strict
   admission through comparator evidence.

4. Run identity and content fingerprinting remain fail-closed. The W0 run id is
   still computed from filtered Criterion inputs and compared to
   `SK_V8_OPEN_RUN_ID`. The existing unit test covers the intended volatility
   boundary: excluded probe estimates, future unvalidated fixtures, and
   unvalidated rows do not move the fingerprint, while a validated W0 estimate
   does. I also reran a temp-only probe: mutating
   `json_probes_twitter/host_call_dispatch_overhead/new/estimates.json`
   preserved `gate-json` success (`exit 0`), while a valid one-digit JSON
   mutation to `json_canada/track1_generated/new/estimates.json` failed
   `gate-json` (`exit 1`) with run id moved from
   `sk-v8-open:criterion-fnv64-9a37562ed3d0383a` to
   `sk-v8-open:criterion-fnv64-13222ff1629829cd`.

5. Rollback remains realistic and commit-sliced. A detached temp worktree
   successfully ran `git revert --no-commit 00c3485a f452e837 6c0bc15d
   0c49fabd 077aadad 61d5d304 cb0fdba0 6d8cb701` with exit 0 and no conflicts.
   The touched paths were the expected W0 packet/report/gate slice:
   `DISPATCH-PROMPT.md`, `HANDOFF.md`, `SPEC.md`, `SYNTHESIS.md`,
   `skinny/RESULTS.md`, `src/bin/gate.rs`, `src/gate.rs`, `src/lib.rs`,
   `src/lock14_baseline.rs`, `src/report.rs`, and `skinny/xtask/src/main.rs`.

6. Runtime cost is still practical under the 90-minute implementation/redress
   cap and W0 rerun ceiling. Focused tests and gate replay completed quickly:
   `cargo test -p bbnf-bench w0_ -- --nocapture` passed 20 tests in 9.29s;
   `cargo test -p bbnf-bench strict -- --nocapture` passed 5 tests in 9.28s;
   `cargo test -p bbnf-bench sidecar_same_run -- --nocapture` passed 1 test in
   9.26s; warmed `cargo xtask gate-json --advisory --check-results` against
   `/tmp/skv8-w0-target` passed in 6.01s.

7. W1-W6 remain blocked. V8 rejection reset the consecutive ACCEPT counter, so
   this CH4 ACCEPT can be at most part of the first qualifying V9 cycle after
   the reset. W0 still needs two consecutive consolidated cycles at >=95%
   ACCEPT with no open critical defects and no unresolved REVISE before later
   waves can dispatch.

## Commands And Evidence

- `git rev-parse --short HEAD`: `00c3485a`.
- `git diff --numstat 84f885a4..00c3485a --`: `24/7`
  `skinny/crates/bbnf-bench/src/gate.rs`, `37/57`
  `skinny/crates/bbnf-bench/src/report.rs`.
- `git diff --check 84f885a4..HEAD -- skinny/crates/bbnf-bench/src/gate.rs
  skinny/crates/bbnf-bench/src/report.rs`: exit 0.
- `git diff --exit-code --stat 0bd16f6d..HEAD -- <frozen behavior roots>`:
  exit 0.
- `/usr/bin/time -p env RUSTFLAGS='-C target-cpu=native' cargo test -p
  bbnf-bench w0_ -- --nocapture`: PASS, 20 tests, `real 9.29`.
- `/usr/bin/time -p env RUSTFLAGS='-C target-cpu=native' cargo test -p
  bbnf-bench strict -- --nocapture`: PASS, 5 tests, `real 9.28`.
- `/usr/bin/time -p env RUSTFLAGS='-C target-cpu=native' cargo test -p
  bbnf-bench sidecar_same_run -- --nocapture`: PASS, 1 test, `real 9.26`.
- `/usr/bin/time -p env CARGO_TARGET_DIR=/tmp/skv8-w0-target
  RUSTFLAGS='-C target-cpu=native' cargo xtask gate-json --advisory
  --check-results`: PASS, `real 6.01`.
- Temp Criterion probe: excluded probe mutation accepted; validated W0
  estimate mutation failed closed on run-id drift.
- Temp rollback simulation: full W0 implementation slice plus `f452e837` and
  `00c3485a` reverted with exit 0.
- `git status --short` before writing this report showed only other untracked
  V9 challenge files; no source dirty state from this CH4 review.

## Blockers

None for CH4 COST.

## Required Fold If Rejecting

Not applicable; CH4 accepts V9.

## Residual Risks

- V9 is the first post-rejection challenge cycle after V8 reset; it cannot
  close W0 alone under ORCHESTRATOR Section 3Z.
- A future legitimate W0 baseline refresh still requires an explicit fold that
  updates the frozen run id/content fingerprint, not a casual Criterion rerun.
