# B3 — Agent Dispatch

Dispatch B3 immediately after this plan lands on master. B3 is the R3
revert tranche per the W0c status snapshot at
`docs/tranches/B2/audit/W0c-status-2026-04-25-04h.md`. Master HEAD at
B3 open is `b8cacedd` (the W0c status-snapshot commit on top of the
B2.W0.a/b/c partial-close commits).

This is not a research brief. B3's design space is fully constrained
by the status snapshot's R3 path + the per-wave specs under
`waves/W<N>.md`. The job now is execution: revert the 14 W0'-scope
commits, verify build + test green, bench parity, write FINAL, hand
off to B2 resume.

## Read first

1. `docs/instructions/README.md` — operational directives.
2. `docs/instructions/PROFILING.md` — profiling workflow.
3. `docs/instructions/tranche/SPEC.md` — tranche spec, §Hard gates,
   §Cherry-pick conflict resolution, §Diagnostic-loop relinquish.
4. `docs/instructions/tranche/WAVE_SPEC.md` — wave-spec format.
5. `docs/instructions/tranche/AGENT_DISPATCH_TEMPLATE.md` — sub-agent
   prompt template.
6. `docs/tranches/B3/B3.md` — tranche plan, invariants, wave summary.
7. `docs/tranches/B3/waves/W<N>.md` for the wave being dispatched.
8. `docs/tranches/B2/audit/W0c-status-2026-04-25-04h.md` — the W0.c
   diagnostic record that motivates B3.
9. `docs/tranches/B2/B2.md` — predecessor tranche; B2 stays paused
   through B3 and resumes from W0.c re-execution post-B3 close.
10. `docs/tranches/AY-II/audit/W0p-PAUSE-SNAPSHOT.md` — the original
    AY-II.W0' pause record (commits being reverted are inventoried
    here).
11. The orchestrator's prior B2 commit history
    (`git log master | grep -E '\(B2\.W0|AY-II.W0'\''`) for the
    cherry-pick reference patterns.

## Program order

1. **B3.W0** — Revert sequence + build verification; 1 agent +
   1 closer; ~120 min wall.
2. **B3.W1** — Parity bench + B3 FINAL + B2 resume handoff; 1 agent;
   ~60 min wall.
3. **B2 W0.c re-execution** — orchestrator dispatches B2 from W0.c on
   post-B3 substrate; ~4 hours wall (B2's existing W0.c hard cap).
4. **B2.W1 → W4** — sequential, per existing B2 plan.
5. **B4** — opens AFTER B2 closes; plan authored at that point;
   re-lands W0'.a/b/c content under post-B2 xtask substrate.
6. **AY-II.W0' close ceremony** — shifts to **B4 close** (not at
   AY-II directly).
7. **AY-II.W1-W5** — sequential, per existing AY-II plan; operates on
   post-B4 substrate.

## Non-negotiables

- **No quick solutions, no workarounds, no fallbacks.** The revert
  is a `git revert` chain producing forward commits. No
  `git reset --hard`. No squash-revert. No revert-then-fix.
- **The revert chain stays in a worktree** (per `tranche/SPEC.md`
  §Worktree isolation); orchestrator cherry-picks onto master at W0
  close gate.
- **`-X theirs` / `-X ours` are forbidden** for conflict resolution.
  Conflicts resolve manually with content-aware editing; resolution
  rationale lands in `audit/W0p-revert-record.md` §Conflicts.
- **B2.W0.a/b/c partial-close commits** (`dec67806`, `3c68e8c4`,
  `21881591`) are NOT reverted under any circumstance. They are
  B2's substrate; B2 resumes against them post-B3.
- **W0'.d4-d7 commits** (`5c737bd1`, `f5cdcd52`, `2e5e3ff5`,
  `700501f5`) are NOT reverted. They touch independent surfaces
  (gorgeous gating, dev-dep posture, build.rs fingerprint, iter-check
  exclude pattern); their AY-II.W0'.d-tag was sequencing convenience,
  not architectural coupling.
- **`generated.rs` is not touched in B3.** The reverts restore source-
  level types; `generated.rs` content was never updated post-W0',
  so it remains as-is. Touching it would re-introduce drift. B4 owns
  the regen window post-B2.
- **B4 is not pre-authored in B3.** Only a forward pointer in
  REMAINING-TRAJECTORY + AY-II/PATH-FORWARD names B4 as the W0'
  re-land destination. B4's plan is authored after B2 closes.
- **No mid-tranche thesis pivot.** If revert is insufficient,
  escalate to escape-clause scope expansion (W0-fix, W0 base) per
  `B3.md` §Escape clause. Beyond W0 base is hard environmental
  blocker — relinquish to user direction.

## Wave-level dispatch templates

Every dispatched sub-agent receives:

1. **Hard cap** (default by wave): W0.a = 90 min; W0.b = 30 min
   (orchestrator-owned, no separate sub-agent); W1.a = 60 min. At
   0.9× cap, the agent commits; at 1.0× cap, the agent halts and
   returns.
2. **File-bound disjointness** per the wave spec's file-bounds table.
3. **Read-first list** from §Read first above.
4. **Return discipline** from §Return discipline below.
5. **Triumvirate escalation**: if the JSONL quiets > 15 min OR the
   first pass produces no commit, the orchestrator dispatches the
   3-agent triumvirate (research / plan / redress) before redispatch
   per `tranche/SPEC.md` §Diagnostic-loop relinquish.

### W0 dispatch (1 agent + 1 closer)

- **W0.a — Revert sequence** (sub-agent on a worktree)
  Files (owner-only): the source files under `crates/core/src/`
  affected by the 14 reverts; `docs/tranches/B3/audit/W0p-revert-record.md`;
  `docs/tranches/B3/audit/diffs/*.diff`; `docs/tranches/B3/audit/W0-cargo-check.txt`;
  `docs/tranches/B3/audit/W0-test-output.txt`.
  Sub-gate: 14 forward-revert commits in the worktree branch; cargo
  check + nextest exit 0; manifest + diffs land; agent reports per
  §Return discipline.
  Hard cap: 90 min.

- **W0.b — Cherry-pick + master integrity** (orchestrator-owned; no
  separate sub-agent)
  Mechanism: orchestrator cherry-picks the 14 reverts + the W0.a
  manifest commit onto master in order; verifies B2.W0.a/b/c intact;
  re-runs cargo check + nextest on master; updates PROGRESS.md +
  wave-status; tears down the W0.a worktree.

### W1 dispatch (1 agent)

- **W1.a — Parity bench + close** (sub-agent on a worktree)
  Files (owner-only): `docs/benchmarks/post-B3-W1*` (3 files);
  `docs/tranches/B3/audit/W1-test-output.txt`;
  `docs/tranches/B3/FINAL.md`; `docs/tranches/B3/PROGRESS.md`;
  `docs/tranches/B3/B3.md` (wave summary table only);
  `docs/tranches/B3/waves/W1.md` (`**Status**` line only);
  `docs/tranches/REMAINING-TRAJECTORY.md`;
  `docs/tranches/AY-II/PATH-FORWARD.md`.
  Sub-gate: 4 verification artefacts exist with gates met; FINAL.md +
  cross-tranche updates committed in the worktree; agent reports per
  §Return discipline.
  Hard cap: 60 min.

## Return discipline

Every sub-agent returns:

1. Worktree path + branch name.
2. Commit SHAs in order with one-line descriptions.
3. Exact artefact paths (file paths the agent created or modified).
4. Hard-gate status per item from the wave's hard-gate list.
5. Wall-clock measurements (where wall is part of the gate).
6. `git status --short` (must be empty or contain only `target/`
   symlink).
7. **For W0.a**: revert-SHA range + per-revert wall measurements +
   conflict count + resolution summaries; cargo-check + nextest exit
   status.
8. **For W1.a**: parser-test wall; compile_bbnf median;
   cargo-xtask-regen wall; FINAL.md path; cross-tranche doc-update
   diff summaries.

## Empty-return redispatch

Per `redispatch-empty-return` feedback memory: if a sub-agent returns
empty, the orchestrator redispatches the original brief verbatim with
a prior-worktree pointer. The empty return is not scope-revelation.

## Anti-patterns to avoid

Per the lessons from B1 + B2's agent dispatches:

- **Do NOT use `ScheduleWakeup`** — that's a `/loop` dynamic-mode
  tool; outside that mode it does nothing useful for the agent.
- **Do NOT use `Monitor` to wait for command exit** — Monitor streams
  stdout-line events, not exit events. A long-running `cargo` invoked
  from Monitor doesn't trigger completion when it finishes.
- **Use `Bash(run_in_background=true)` + `TaskOutput(block=true,
  timeout=600000)` for long commands** — the runtime delivers
  completion notifications automatically; agent should wait via
  TaskOutput or for the auto-notification.
- **Do NOT exit prematurely with cargo running** — the agent must
  collect cargo output before returning.
- **Do NOT touch `target/.bbnf-cache/`** during W0/W1; cache
  preservation matters for reproducible cycle measurements (B3 is
  pre-B2-W3, when the cache still exists).
- **Worktree target symlink fix**: `scripts/seed-worktree.sh` creates
  a self-referential symlink (B1 + B2 lesson). On worktree creation,
  run `rm -f target && ln -s /Users/mkbabb/Programming/bbnf-lang/target
  target` before any cargo invocation.
- **Single cargo per CARGO_TARGET_DIR**: B3 has at most one sub-agent
  per wave, but the orchestrator must NOT spawn its own cargo
  invocations against the main target while a wave's agent is active.
- **Do NOT use `--strategy recursive -X theirs`** for revert
  conflicts. Manual resolution is mandatory; rationale lands in the
  audit manifest.
- **Do NOT `git reset --hard`** during the revert chain. Each
  `git revert` lands as its own forward commit; the chain is reversible
  via `git revert` of the reverts (B4's mechanism).

## B2 resume after B3 close

When B3.W1 closes, the orchestrator dispatches B2 W0.c re-execution.
B2's existing W0.c brief at `docs/tranches/B2/waves/W0.md` is the
same brief that produced `21881591` (the partial-close commit); with
the parser regression resolved, the brief closes cleanly. B2 then
continues W1 → W4 per its existing plan, in `docs/tranches/B2/`.

B2's W0.c brief does NOT change as a result of B3. The brief assumes
a working `BbnfBootstrap::parse`; B3 makes that assumption true again.

## B4 forward pointer

After B2 closes, B4 opens for W0' re-land. B4's plan is authored at
that point, citing the post-B2 xtask substrate as its substrate, the
W0'-content diff snapshots at `docs/tranches/B3/audit/diffs/*.diff` as
its source-of-truth, and bisect-and-fix as its operational mode.

B4 does NOT exist as a placeholder in this tranche; only a forward
pointer in REMAINING-TRAJECTORY + AY-II/PATH-FORWARD names it.
