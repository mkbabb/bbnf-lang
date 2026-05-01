# B2 — Agent Dispatch

Dispatch B2 after the pre-B2 trivial fixes (commit `81195656`) land.
The pre-B2 commit restores `cargo iter-check` warm to the d7 baseline
(0.14 s) so B2's measurements are honest.

This is not a research brief. B2's design space is fully constrained
by `docs/audit/2026-04-25-deep-audit/AUDIT-D-architectural-
transposition.md` (T3 selection: xtask + checked-in generation), the
boundary inventory at `B2.md §Critical files`, and the per-wave specs
under `waves/W<N>.md`. The job now is execution: land the xtask
substrate, migrate consumers, retire the proc-macro, retire the
script, gate CI.

## Read first

1. `docs/instructions/README.md`
2. `docs/instructions/PROFILING.md`
3. `docs/instructions/tranche/SPEC.md`
4. `docs/instructions/tranche/WAVE_SPEC.md`
5. `docs/audit/2026-04-25-deep-audit/AUDIT-A-B1-retrospective.md`
6. `docs/audit/2026-04-25-deep-audit/AUDIT-B-AY-II-viability.md`
7. `docs/audit/2026-04-25-deep-audit/AUDIT-C-AZ-coherence.md`
8. `docs/audit/2026-04-25-deep-audit/AUDIT-D-architectural-transposition.md`
9. `docs/tranches/B2/B2.md`
10. `docs/tranches/B2/waves/W<N>.md` for the wave being dispatched
11. The orchestrator's prior B1 commit history (`git log master | grep B1`) for reference patterns on cherry-pick discipline + commit style

## Program order

1. **Pre-B2 fixes** (already landed at `81195656`): drop rustflags
   regression + rename `json_monolithic_value` to `json_value`.
2. **B2.W0** — xtask substrate + first per-grammar emission (BBNF
   self-host); 2 parallel + 1 closer; ~5 hours wall.
3. **B2.W1** — consumer cutover; 4 parallel + 1 closer;
   delete-then-swap window; ~5-7 hours wall.
4. **B2.W2** — proc-macro retirement; `crates/derive/` deletes;
   1 + 1 closer; ~2 hours wall.
5. **B2.W3** — script simplification; 1 agent; ~1.5 hours wall.
6. **B2.W4** — CI gate + FINAL + AY-II handoff; 1 consolidator;
   ~1.5 hours wall.
7. **AY-II.W0' close ceremony** (compressed-honest per AUDIT-B);
   ~15 min on the post-B2 substrate.
8. **AY-II.W1-W5** sequential.

## Non-negotiables

- No quick solutions, no workarounds, no fallbacks, no dual-paths.
- The proc-macro DOES retire at W2.a. Not "deferred to a successor",
  not "kept behind a feature flag", not "shrunk to a thin shim". Gone.
- `crates/derive/` directory deletes outright. The tree's history
  remains in git but the path no longer resolves in worktree.
- `bbnf_derive` dep entries purge from EVERY `Cargo.toml`. Zero
  results in `rg -nF 'bbnf_derive\|bbnf-derive' --type toml` post-W2.
- `BBNF_SCHEMA_VERSION` constant retires (the entire file it lived in
  deletes).
- `scripts/bootstrap-bbnf.sh` deletes at W3 (not "kept for legacy
  invocations"; not "renamed"). Logic absorbs into `xtask::regen`.
- `target/.bbnf-cache/` is no longer created or read by any code path
  post-W3. The directory's role ceases.
- No mid-tranche scope absorption. If consumer cutover surfaces a
  resistant site, halt + relinquish + open B3 for the residual scope.
- Master green at every wave boundary EXCEPT W1.b's named
  delete-then-swap window (W1.c is the named restoration wave).

## Wave-level dispatch templates

Every dispatched sub-agent receives:

1. **Hard cap** (default by wave): W0.a/b = 60 min, W0.c = 4 hours;
   W1.a = 60 min, W1.b/c/d = 75 min, W1.e = 60 min; W2.a = 90 min,
   W2.b = 30 min; W3 = 90 min; W4 = 90 min. At 0.9× cap, the agent
   commits; at 1.0× cap, the agent halts and returns.
2. **File-bound disjointness** per the wave spec's file-bounds table.
3. **Read-first list** from §Read first above.
4. **Return discipline** from §Return discipline below.
5. **Triumvirate escalation**: if the JSONL quiets >15 min OR the
   first pass produces no commit, the orchestrator dispatches the
   3-agent triumvirate (research / plan / redress) before redispatch.

### W0 dispatch (2 parallel + 1 serial closer)

- **W0.a — xtask substrate** (parallel with W0.b)
  Files (owner-only): `xtask/Cargo.toml`, `xtask/src/main.rs`,
  `xtask/src/regen.rs` (scaffold), workspace `Cargo.toml` (members
  + `[workspace.metadata.bbnf]` table).
  Sub-gate: `cargo xtask --help` resolves; workspace builds clean
  with the new member.
  Hard cap: 60 min.

- **W0.b — Per-grammar emission target audit** (parallel with W0.a)
  Files (owner-only): `docs/tranches/B2/audit/W0-bbnf-surface-
  snapshot.rs`, `docs/tranches/B2/audit/W0-per-grammar-boundary.md`.
  Sub-gate: snapshot is parseable Rust; boundary spec covers shared-
  infra vs grammar-specific items.
  Hard cap: 45 min.

- **W0.c — Implementation closer** (serial; opens after W0.a + W0.b)
  Files (owner-only): `xtask/src/regen.rs` (full impl), per-grammar
  output files at `crates/core/src/grammar/generated/bbnf.rs`,
  `crates/core/src/grammar/generated/mod.rs`, `crates/core/src/
  grammar/mod.rs` (modify), `crates/bootstrap/src/lib.rs` +
  `crates/bootstrap/Cargo.toml`, `docs/tranches/B2/audit/W0-byte-
  equivalent-diff.txt`, `docs/benchmarks/archive/post-B2-W0-walls.txt`.
  Sub-gate: `cargo xtask regen --grammar bbnf` succeeds; byte-
  equivalent gate green; bbnf-bootstrap migrated to `include!`;
  iter-check-full ≤ 30 min cold.
  Hard cap: 4 hours (the heaviest wave; pipeline integration takes
  time).

### W1 dispatch (4 parallel + 1 closer)

Each agent owns disjoint consumer slices; see `waves/W1.md` §File
bounds for exact assignment.

- W1.a (60 min): gorgeous's 5 derive sites.
- W1.b (75 min): JSON-family core tests (~12 sites).
- W1.c (75 min): CSS + Sheets-family core tests (~18 sites).
- W1.d (75 min): BBNF + cross-grammar core tests (~20 sites).
- W1.e closer (60 min): per-grammar regen sweep + workspace test
  matrix verification + post-W1-walls measurement.

### W2 dispatch (1 + 1 closer)

- W2.a (90 min): proc-macro crate deletion + dep purge from every
  Cargo.toml + `[patch.crates-io]` line removal + `Cargo.lock`
  regeneration + bbnf-bootstrap retirement-or-shim decision.
- W2.b (30 min): legacy `crates/core/src/grammar/generated.rs`
  monolith retirement; post-W2-walls record.

### W3 dispatch (1 agent)

- W3.a (90 min): `bootstrap-bbnf.sh` + `check-bootstrap-clean.sh`
  deletion; xtask absorbs post-process logic; Makefile
  `ay-prime`/`clean-cache` retirement; `regen` + `regen-check`
  Make targets added; PROFILING.md §Bootstrap regen + §Dev-host
  setup updated.

### W4 dispatch (1 consolidator)

- W4.a (90 min): CI workflow `cargo xtask regen --check` step;
  pre-commit hook; B2 FINAL.md; AY-II handoff updates (PATH-
  FORWARD, AY-II.md, waves/W0p.md compressed-honest spec); AZ-I.W0
  amendment (drop derive-cache + Watt); REMAINING-TRAJECTORY.md +
  RISK-PERF-MATRIX.md revisions; post-B2.json aggregate.

## Return discipline

Every sub-agent returns:

1. Worktree path + branch name.
2. Commit SHAs in order with one-line descriptions.
3. Exact artefact paths (file paths the agent created or modified).
4. Hard-gate status per item from the wave's hard-gate list.
5. Wall-clock measurements (where wall is part of the gate).
6. `git status --short` (must be empty or contain only `target/`
   symlink).
7. For W0.c: byte-equivalent diff path + `cargo iter-check-full` cold
   wall.
8. For W1.e: `rg -nF '#[derive(Parser' --type rust` count (must be 0);
   workspace nextest run exit; iter-check warm + iter-check-full cold
   walls.
9. For W2.a: `ls crates/derive/` output (must be empty / no such dir);
   `rg -nF 'bbnf_derive\|bbnf-derive' --type toml` count (must be 0).

## Empty-return redispatch

Per `redispatch-empty-return` feedback memory: if a sub-agent returns
empty, the orchestrator redispatches the original brief verbatim with
a prior-worktree pointer. The empty return is not scope-revelation.

## Anti-patterns to avoid

Per the lessons from B1's agent dispatches:

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
- **Do NOT touch `target/.bbnf-cache/`** during W0/W1/W2; cache
  preservation matters for cycle-2 measurements until W3 retires the
  cache entirely.
- **Worktree target symlink fix**: `scripts/seed-worktree.sh` creates
  a self-referential symlink (B1 lesson). On worktree creation, run
  `rm target && ln -s /Users/mkbabb/Programming/bbnf-lang/target target`
  before any cargo invocation.
- **Single cargo per CARGO_TARGET_DIR**: parallel agents sharing
  target via symlink serialize on the cargo lock. Sequence cargo
  invocations within a wave; parallelize file edits.
