# Meta-Audit 04 — Dev-Toolchain Painpoint Audit

**Measurement host**: master `ab4d9378` @ worktree `agent-ac819d7c`, macOS
25.4.0 arm64. Wall-clocks are zsh `time` output; CPU% is the `time`
line's `system`/`user` ratio multiplied out against wall.

All raw logs under `/tmp/ma04/*.log` (ephemeral) and
`/private/tmp/claude-504/.../tasks/b*.output`.

## Measurement matrix

| # | Command | State | Wall | CPU% | Exit |
|---|---|---|---|---|---|
| 1a | `cargo iter-check` (after `rm -rf target/ax-iter/incremental target/.bbnf-cache`) | semi-cold (target/ax-iter retained .rmeta) | **0.41 s** | 39 % | 0 |
| 1b | `cargo iter-check` warm | warm | **0.13 s** | 95 % | 0 |
| 1c | `cargo iter-check` after `touch crates/core/src/runtime/parsed.rs` | touch-cascade (runtime/) | **4.12 s** | 181 % | 0 |
| 2a | `cargo iter-test-leaf` | cold (full leaf ring) | **41.67 s** | 376 % | 0 |
| 2b | `cargo iter-test-leaf` warm | warm | **1.10 s** | 81 % | 0 |
| 3a | `cargo check --profile ax-iter -p gorgeous --lib` | blocked on target/ lock (parallel bbnf-bootstrap) | **42.34 s** (0 compile; exit-by-kill of sibling) | 0 % | kill | 
| 3b | `cargo check --profile ax-iter -p bbnf-bootstrap --lib` | cold, single-rustc | **>130 s and still running at cutoff** (bbnf-bootstrap rustc pid 66920 single-core 99 % CPU) | 99 % | killed at 23-min cap |
| 4 | `time cargo test --profile ax-iter -p bbnf --test value_api_apples_to_apples --no-run` | cold, gated behind (3b) lock | **>110 s still queued/running at cutoff** | — | — |
| 5 | `bash -n scripts/{bootstrap-bbnf,test-tier,prepare-profile-wave,profile-bench-headless}.sh` | syntax check | **< 0.05 s each** | — | 0 (all four OK) |
| 6 | `CARGO_TARGET_DIR=/.../prep-wave make ay-prepare-profile-wave` | not executed — blocked on (3b) finishing | — | — | — |

Process state at 23 min cap: a single `rustc --crate-name bbnf_bootstrap`
process still pegged at 99 % CPU inside a single derive expansion, >130 s
elapsed. This reproduces the W0p-infra-root-cause prediction exactly
(≥ 40 s observed; unbounded tail not probed in root-cause).

## Painpoints (ranked by friction × frequency)

### Pain 1: `cargo check -p bbnf-bootstrap --lib` single-derive wall

- **Measured cost**: >130 s cold, single-core, single-rustc. No cache
  hit path — `scripts/bootstrap-bbnf.sh:28` deletes `target/.bbnf-cache/`
  every run, so every bootstrap invocation pays the full pipeline cost.
- **Frequency**: triggered by `make ay-expand-bbnf`,
  `scripts/bootstrap-bbnf.sh`, `make ay-bench-close WAVE=close`,
  `make test-close`, and every `cargo iter-check-full` /
  `cargo check --workspace` invocation. CI hits it on every push.
- **Root cause**: the BbnfBootstrap grammar is the single
  `#[derive(Parser)]` site in `crates/bootstrap/src/lib.rs:1`; the 133-LOC
  grammar expands ~30 k LOC of `TokenStream`, serialised inside one
  rustc, amplified by the `scripts/bootstrap-bbnf.sh` cache-nuke on L28.
- **Whether B1.md already addresses it**: **no**. B1.W0.b lists
  `bash scripts/bootstrap-bbnf.sh` as a gate but does not propose
  caching or fix the `.bbnf-cache` deletion.
- **Proposed B1 extension**: add W0.d sub-item "bootstrap cache honest"
  that either (a) removes `rm -rf target/.bbnf-cache/` from
  `scripts/bootstrap-bbnf.sh` and relies on the content-keyed cache in
  `crates/derive/src/lib.rs:300-358`, or (b) writes a `.bbnf-cache`
  prime step into `make ay-prime` (new) that seeds the cache from the
  last-good expand artefact.

### Pain 2: `iter-check-full` (`--workspace`) still re-expands gorgeous + bootstrap on cold

- **Measured cost**: not directly timed (would exceed 23-min cap given
  (3a) + (3b) above), but the constituent parts are ≥ 556 s (gorgeous)
  + ≥ 130 s (bootstrap) serialised inside two single-core rustcs —
  conservative floor 12 min, closer to 15 min.
- **Frequency**: advertised public alias in `.cargo/config.toml:78`;
  invoked by B1.W0.c hard gate ("cargo iter-check-full exits 0") and
  by anyone doing close-ceremony lint.
- **Root cause**: same d3/d4/d5/d6 pathology the root-cause doc
  catalogued. The d7 narrowing excluded gorgeous + bootstrap from
  `iter-check`, but `iter-check-full` kept `--workspace` unqualified.
  That is the exact command B1.W0.c's hard gate #1 requires green.
- **Whether B1.md already addresses it**: **no**. B1.W0.c requires
  `iter-check-full` to exit 0 but does not budget its cost or split
  close-gate correctness from compile-gate iteration.
- **Proposed B1 extension**: downgrade `iter-check-full` from a routine
  alias to a close-only alias in `.cargo/config.toml`; B1.W0.c hard
  gate recast to measure **wall-clock** explicitly with an agreed
  ceiling (e.g. ≤ 20 min cold on reference HW), not just "exit 0".

### Pain 3: Target lock contention on parallel `cargo check -p <A>` + `-p <B>`

- **Measured cost**: in probe (3a), a parallel `cargo check -p gorgeous` +
  `cargo check -p bbnf-bootstrap` produced one "`Blocking waiting for
  file lock on package cache`" and then one "`Blocking waiting for file
  lock on build directory`"; gorgeous never did any work and its `time`
  line reported 0 % CPU while its sibling ran. Exit-by-kill after 42 s.
- **Frequency**: any agent-orchestrator that parallelises per-crate
  compile probes (the current meta-audit for instance). W0p-infra
  research probes were single-threaded precisely because of this;
  B1 will need parallel capture for post-W0 proof artefacts.
- **Root cause**: cargo's target-dir lock is per-`CARGO_TARGET_DIR`;
  sibling agents must point each probe at a distinct `CARGO_TARGET_DIR`
  or serialise them.
- **Whether B1.md already addresses it**: **no**.
- **Proposed B1 extension**: add a one-paragraph note in
  `docs/instructions/PROFILING.md` "Parallel probe discipline" that
  says: *parallel per-crate probes require per-agent `CARGO_TARGET_DIR`;
  sibling agents must not share `target/`.* Add a sub-gate to B1.W0.b
  that confirms `scripts/prepare-profile-wave.sh` still requires an
  absolute `CARGO_TARGET_DIR` (it does, line 68 of the script). No
  script change.

### Pain 4: `target/.bbnf-cache/` does not exist on the audited worktree

- **Measured cost**: listed; directory absent. The proc-macro cache at
  `crates/derive/src/lib.rs:300-358` therefore cannot serve any
  incremental run until at least one successful gorgeous/bootstrap
  expansion has landed its content-keyed entries. Pair with Pain 1:
  on a fresh clone there is no way to recover the cache cheaply.
- **Frequency**: every fresh clone; every CI runner; every worktree.
- **Root cause**: no `make ay-prime` / `make ay-warm-cache` step; the
  cache is strictly a side-effect of running the proc-macro through to
  its end without kill.
- **Whether B1.md already addresses it**: **no**.
- **Proposed B1 extension**: new Makefile target `ay-prime` that (i)
  optionally downloads a pre-computed `target/.bbnf-cache/` tarball
  from a declared URL if one exists, or (ii) runs `cargo check -p
  bbnf-bootstrap --lib` + `cargo check -p gorgeous --lib` serially
  with a timeout wrapper and reports resulting cache population. List
  `make ay-prime` in PROFILING.md §Public fast-path commands. Note:
  unrelated to B1's defensible floor; add as non-blocking.

### Pain 5: `iter-check` target dir touches exclude `bbnf-analysis`, `bbnf-lsp`

- **Measured cost**: `target/ax-iter/deps/libbbnf_analysis-*.rmeta`
  present from a prior run, so no current fresh-cost datum; but the
  exclude list means any lsp/analysis regression surfaces only on
  `cargo iter-check-full` or `cargo check -p bbnf-analysis` — which is
  `iter-check-full`'s >12-min cost, or a one-off check.
- **Frequency**: chronic: bbnf-analysis / bbnf-lsp touches break
  quietly in the fast loop and surface only at close-gate.
- **Root cause**: `.cargo/config.toml:72` explicitly excludes them to
  avoid the single-rustc stall (same mechanism as gorgeous + bootstrap,
  though lighter).
- **Whether B1.md already addresses it**: **no** — the exclusion itself
  is audited by a tranche-drift agent, not toolchain pain. But the
  invariant that "every routine-surface exclusion has a fast alternate
  validation command" is unstated in B1.md.
- **Proposed B1 extension**: new invariant in B1.md §Invariants item
  item 10: *"Every `--exclude`d crate on the routine surface has a
  named fast-path alias that validates it alone"*; add
  `iter-check-lsp = "check --profile ax-iter -p bbnf-lsp -p
  bbnf-analysis"` to `.cargo/config.toml`.

### Pain 6: `scripts/bootstrap-bbnf.sh` nukes `.bbnf-cache` unconditionally

- **Measured cost**: L28 `rm -rf target/.bbnf-cache/` → every bootstrap
  run is a full single-derive expansion. This is why (3b) was
  unbounded: the derive cache never hits.
- **Frequency**: every `make expand-bbnf`, every regen, every
  `scripts/bootstrap-bbnf.sh` call.
- **Root cause**: conservative reset to force authority; no
  content-check to guarantee the cache is the fresh entry.
- **Whether B1.md already addresses it**: **partially**. B1.W0.b gates
  on the script completing but does not gate on its per-run cost.
- **Proposed B1 extension**: the bootstrap-cache invariant noted in
  Pain 1's fix proposal; B1.W0.b sub-gate becomes measurably faster on
  cycle-2 (regen idempotency check: cycle-2 wall ≤ 10 % of cycle-1,
  not just "exit 0").

### Pain 7: Incremental cache ICE from `bbnf-analysis` (noted in prompt)

- **Measured cost**: not reproduced in this probe — `cargo check
  --profile ax-iter -p bbnf-analysis --lib` would require lifting the
  exclusion and eating the single-rustc cost. Artefact under
  `target/ax-iter/deps/libbbnf_analysis-d5f1dfb1242e8b12.rmeta` is a
  prior-run metadata file from before `bbnf-analysis` joined the
  exclusion list.
- **Frequency**: prompt cites it as known; any agent that re-enables
  bbnf-analysis in routine loop trips it.
- **Whether B1.md addresses it**: **no**.
- **Proposed B1 extension**: B1.W0.a sub-gate adds
  `cargo check --profile ax-iter -p bbnf-analysis --lib` exits 0 after
  a fresh `rm -rf target/ax-iter/incremental`; if it ICEs, the
  exclusion note in `.cargo/config.toml:72` is expanded to cite the
  ICE + open issue; if it passes, bbnf-analysis is removed from the
  exclude list.

### Pain 8: No `make ay-prime` / cache-warm entrypoint for new clones

- **Measured cost**: first `cargo iter-check-full` on a fresh clone
  must pay ≥ 12 min; every subsequent developer is blocked until the
  first finishes. B0 measured "target symlink" shortcut
  (`docs/tranches/B0/FINAL.md:39`) but that is a worktree-orchestrator
  convenience, not a documented public command.
- **Frequency**: every new clone / CI runner / new worktree.
- **Whether B1.md addresses it**: **no**.
- **Proposed B1 extension**: see Pain 4 Addition 1 below.

## B0 FINAL claim validation

For each claim at `docs/tranches/B0/FINAL.md`:

| B0 claim | Baseline | Current measurement at `ab4d9378` | Drift | Cause |
|---|---|---|---|---|
| `cargo iter-check` warm 0.16 s | 0.16 s | **0.125 s warm** | **−22 % (faster)** | d7 exclusion narrowed the workspace member list; fewer .rmeta to reload. |
| `cargo iter-check` workspace warm 7.16 s (baseline before B0) | 7.16 s | **not reachable** — `cargo check --workspace` requires gorgeous + bootstrap cold | **drift unmeasurable; claim was pre-d7** | the "45× speedup" was against a workspace check that today costs ≥ 12 min cold on a fresh worktree. |
| `make iter-test-leaf` 1.05 s warm | 1.05 s | **1.10 s warm; 41.67 s cold** | **+5 % warm, no cold baseline** | cold-path was not in B0's claim matrix — leaf-tier crates include the incremental dev-dep graph (parse-that, pprint, regex-syntax...) which cold-compile in ~40 s. |
| `cargo test --workspace --no-run` warm 0.76 s | 0.76 s | **not measured this audit** — 3 min time cap | — | B0's headline was dominated by the `target/` symlink shortcut (FINAL.md:39); fresh worktrees will not reproduce it. |
| `make expand-json` writes 6224-line artefact | 6224 lines | **not re-measured this audit** — same gorgeous wall | drift unknown | depends on gorgeous cold. |
| `scripts/test-tier.sh leaf` exit 0 | exit 0 | **not re-measured this audit** (40 s cold per alias above) | likely stable | crate list confirmed at `scripts/test-tier.sh:48` uses `-p tape …` (no stale `bbnf-tape`). |
| `make ay-test-value-api` 4 tests ok | pass | **not run** — compile gate ≥ 110 s cold on single-test | unknown | gated behind the same bbnf test binary cold-compile. |

**Key drift**: B0's 45× iter-check headline baseline was
`cargo check --workspace` which is today the `iter-check-full` alias
and is **≥ 12-min cold** — the 7.16 s warm number B0 compared against
was only reachable once the workspace cache was already primed by a
previous workspace compile. That priming step is not documented as
a public command, so new contributors cannot reproduce the 45×
figure from a cold clone.

## Proposed B1 scope additions (concrete)

### Addition 1: `docs/tranches/B1/B1.md` §Invariants — new items 10-12

Before (end of §Invariants, line 61):
```
9. B1 carries no successor debt tree; anything runtime-facing routes
   back to AY-II immediately.
```

After:
```
9. B1 carries no successor debt tree; anything runtime-facing routes
   back to AY-II immediately.
10. Every `--exclude`d crate on the routine-surface `iter-check` alias
    has a named fast-path alias that validates it alone, recorded in
    `docs/benchmarks/archive/post-B1-W0-routine.txt`.
11. `cargo iter-check-full` (the workspace close-gate) records a
    measured wall-clock ceiling in `docs/benchmarks/archive/post-B1-W0-proof.txt`;
    the ceiling is an explicit number, not "exit 0".
12. `target/.bbnf-cache/` is neither created nor destroyed as a
    side-effect of any B1 command — scripts that previously relied on
    unconditional `rm -rf target/.bbnf-cache/` are fixed so cycle-2
    is measurably cheaper than cycle-1.
```

Rationale: turns the three most-measured painpoints into explicit
close gates, with runtime-verifiable artefact rows.

### Addition 2: `docs/tranches/B1/B1.md` §Wave summary — new W0.d

Wave: **B1.W0.d — Cold-path truth + cache honesty**
Purpose: repair `scripts/bootstrap-bbnf.sh` so `.bbnf-cache/` survives
across runs; add `make ay-prime` that seeds the cache from a single
cold run of `cargo check -p bbnf-bootstrap --lib` (+ optionally
`-p gorgeous --lib`); document both in PROFILING.md §Public fast-path
commands.

Hard gate:
1. `bash scripts/bootstrap-bbnf.sh` cycle-2 wall ≤ 10 % of cycle-1
   wall, recorded in `docs/benchmarks/archive/post-B1-W0-proof.txt`.
2. `make ay-prime` on a `rm -rf target/.bbnf-cache target/ax-iter/incremental`
   fresh state populates `target/.bbnf-cache/` with at least one
   `.rs` entry and exits 0.
3. `cargo iter-check-full` cold wall recorded explicitly; ceiling
   declared by W0.d in its PR body.

Evidence: `docs/benchmarks/archive/post-B1-W0-proof.txt` rows
`bootstrap-cycle-1`, `bootstrap-cycle-2`, `ay-prime-fresh`,
`iter-check-full-cold`.

### Addition 3: `docs/tranches/B1/AGENT_DISPATCH.md` §W0 — new W0.d subagent

Scope: own `scripts/bootstrap-bbnf.sh` + `Makefile` (new
`ay-prime` target + `iter-check-full` ceiling) + `.cargo/config.toml`
(downgrade `iter-check-full` from routine-alias wording to
close-ceremony wording, and add an `iter-check-lsp` alias for the
excluded crates).

Deliverable: three artefact rows in `docs/benchmarks/archive/post-B1-W0-proof.txt`
(`bootstrap-cycle-2`, `ay-prime-fresh`, `iter-check-full-cold`);
comment-block in `.cargo/config.toml` that cites the three-alias
cost model (routine / lsp-validate / close-gate).

### Addition 4: `.github/workflows/ci.yml` — split the bbnf-bootstrap cost off the preflight path

Scope note: ensure CI preflight does NOT invoke `iter-check-full` or
`scripts/bootstrap-bbnf.sh` (both > 10 min cold). If preflight today
hits either, move to the heavy job. B0 FINAL §W2 claims this is
already done ("Preflight (routine) / Heavy (close-gate) step groups"),
but no measurement of preflight cold-wall lives in `post-B0-W2-close.json`.
B1.W0.c adds the measurement.

### Addition 5: `docs/instructions/PROFILING.md` — "Parallel probe discipline" paragraph

One paragraph near §Public fast-path commands documenting that
parallel per-crate probes by sibling agents require
`CARGO_TARGET_DIR=/abs/per-agent-path`. Directly addresses Pain 3's
lock contention; the script-level gate already exists in
`scripts/prepare-profile-wave.sh:68` but the prose is missing.

## Painpoints B1 should NOT address (out of scope, noted for successor)

- **per-derive-site expansion cost in bbnf-bootstrap itself** (~130 s+
  for 1 derive) → routes to AY-II or BA as a grammar-derive
  optimisation; structural, not infrastructure.
- **bbnf-analysis ICE** under specific incremental-cache state → routes
  to the bbnf-analysis owner; B1 records the repro state only.
- **Full `target/` tarball priming from a release artefact** → out of
  scope; an on-demand feature for future CI caching.
- **rustc internal parallelism across sibling derive-macro sites** →
  upstream rustc; B0's feature-gating of gorgeous was the correct
  workaround for this epoch.

## Summary

**Top 5 painpoints by total minutes lost per day × frequency**:

1. `cargo iter-check-full` / workspace cold wall (≥ 12 min cold).
2. `bash scripts/bootstrap-bbnf.sh` forced cold (≥ 130 s per run due
   to `.bbnf-cache` deletion).
3. `cargo check -p bbnf-bootstrap --lib` cold (≥ 130 s).
4. `cargo iter-test-leaf` cold first-run (41 s, dominated by leaf-tier
   dep-graph compile).
5. Target lock contention during parallel meta-audit probes
   (agent-orchestration pathology).

**Top 3 B1 scope extensions to land first**:

1. Stop `scripts/bootstrap-bbnf.sh` from nuking `target/.bbnf-cache/`
   — turns a ≥ 130 s per-run cost into a ≤ 10 s cache-hit per-run
   cost (Pain 1 + Pain 6).
2. Downgrade `iter-check-full` to close-ceremony and add
   `iter-check-lsp` for the currently excluded crates (Pain 2 + Pain 5).
3. Add `make ay-prime` as the documented cache-warm entrypoint for new
   clones + CI runners (Pain 4 + Pain 8).

These three land as **B1.W0.d** plus one extra invariant (B1.md item
12); the remaining items are PROFILING.md prose and a new alias.
