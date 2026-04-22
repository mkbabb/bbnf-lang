# B1 — Build-Infra Audit + Redress (Prelude Annex Successor to B0)

**Mandate**: comprehensively audit every dev-loop surface this
workspace ships, identify the gap between specification and reality,
and redress until every routine dev operation completes in seconds,
every profiling-prep operation completes in under one minute, and
every final-proof operation runs predictably without surprise cost.

**Predecessor**: B0 (`docs/tranches/B0/FINAL.md`) landed the three-tier
command surface (routine/profiling-prep/final-proof) + the public
alias catalog. B1 validates that surface against reality post-AY-II
tranche churn and extends it with the instrumentation + contracts B0
left implicit.

---

## Context for the agent — read in full before planning

### Instructions layer
1. `docs/instructions/README.md` §Code discipline — the non-negotiables.
2. `docs/instructions/PROFILING.md` §Public fast-path commands — B0's
   canonical alias/target manifest.
3. `docs/instructions/tranche/SPEC.md` — §Prelude annexes, §Bench
   contract, §Diagnostic-loop relinquish, §Three-tier command surface.
4. `docs/instructions/tranche/RESEARCH.md` — the research wave protocol
   (B1 opens with a research wave).
5. `docs/instructions/tranche/WAVE_SPEC.md` — how to author per-wave
   spec docs.
6. Memory at
   `/Users/mkbabb/.claude/projects/-Users-mkbabb-Programming-bbnf-lang/memory/`
   especially:
   - `feedback_build_infra_first.md` — build/test infra lands FIRST in
     any tranche where dev iteration time is a bottleneck.
   - `feedback_test_output_to_file.md` — long cargo runs redirect
     output to file once, grep/tail over the file, never re-invoke.
   - `feedback_bench_sequential.md` — bench sequentially, never in
     separate cmds, always check for regressions.
   - `feedback_no_warm_benches.md` — warm/cached benches are
     disingenuous; use cold per-parse only.
   - `feedback_samply_symbols.md` — samply needs `debug=true` +
     interactive `samply record` for symbol resolution.

### Tranche layer
1. `docs/tranches/B0/B0.md` — the predecessor plan + wave schedule.
2. `docs/tranches/B0/FINAL.md` — what B0 closed on. Quantitative
   baselines (e.g. `cargo iter-check` warm 0.16s, `make iter-test-leaf`
   warm 1.05s) are B0's claims; B1 verifies or refutes each.
3. `docs/tranches/B0/waves/` — per-wave specs; understand how B0
   decomposed its work.
4. `docs/tranches/AY-II/audit/W0p-infra-root-cause.md` — B0's gaps
   surfaced at AY-II execution time.
5. `docs/tranches/AY-II/audit/W0p-infra-fix-plan.md` +
   `W0-iter-surface-verification.md` — the fixes + verification that
   landed under AY-II.W0'.d4–d7. B1 verifies they hold.
6. `docs/tranches/AY-II/PATH-FORWARD.md` — the AY-II close path B1
   must not block.
7. `docs/tranches/AY-II/AY-II.md` §Invariants 10 — `make ay-bench-close
   WAVE=close` runs clean on all five benches at every wave boundary.
   This IS a B1 gate too: if B1 breaks the bench matrix, B1 fixes it.

### Infrastructure layer
1. `Cargo.toml` — workspace root + profile tiers (`ax-iter`,
   `profiling-prep`, `bench`).
2. `.cargo/config.toml` — alias catalog.
3. `Makefile` — iter + expand + samply + close targets (§150-400).
4. `scripts/bootstrap-bbnf.sh` — the regen pipeline.
5. `scripts/prepare-profile-wave.sh` — samply prep (B0.W1.b).
6. `scripts/profile-bench-headless.sh` — headless samply capture.
7. `scripts/test-tier.sh` — the leaf/grammar/workspace test tier
   entry point (B0.W1 flagged it as stale on leaf; verify).

---

## Scope — research wave

B1 opens with a **3-5 parallel research agent sweep** per
`docs/instructions/tranche/RESEARCH.md`. Each agent produces a
deliverable under `docs/tranches/B1/research/`.

### Research agent 1 — Cold + warm surface measurement

Measure every public dev-loop command cold + warm + touch-cascade,
across every alias / Makefile target. Produce a timing matrix:

Commands to measure:
- `cargo iter-check` / `iter-check-full`
- `cargo iter-test-leaf`
- `cargo iter-test-grammar` (via scripts/test-tier.sh)
- `cargo iter-test-ws`
- `make expand-json` / `expand-css` / `expand-bbnf` / `expand-sheets`
- `make ay-expand-json` / `ay-expand-named-type` / `ay-asm-close-compound`
- `make ay-test-value-api` / `ay-test-wire-contract` / `ay-test-named-type`
- `scripts/bootstrap-bbnf.sh` (regen)
- `cargo check -p <each workspace member>` (find the slow ones)
- `cargo test -p tape --tests` / `-p bbnf --test <one>`
- `make ay-prepare-profile-wave`
- `make ay-samply-json-twitter`
- `make ay-bench-close WAVE=close` (only a subset if time-boxed)

Deliverable: `research/01-surface-timing-matrix.md` with cold / warm /
touch-cascade wall-clock per command, CPU usage, and peak memory. Flag
anything that exceeds the target in `research/target-ceilings.md`.

### Research agent 2 — Test discovery + dev-dep dependency graph audit

Enumerate every `#[derive(Parser)]` site across the workspace. Map the
dev-dep graph: what pulls what. Identify the unneeded critical-path
edges (like the gorgeous-as-bbnf-dev-dep that W0'.d5 already cut).

Deliverable: `research/02-derive-sites-depgraph.md` with:
- Every derive site file:line + the grammar it parses.
- Per-crate dev-dep incoming edges (who forces us to build X).
- Top-10 highest-cost expansions + their per-edit invalidation impact.

### Research agent 3 — Profiling prep + samply reality check

Verify samply symbol resolution works end-to-end today. Prior memory
(`feedback_samply_symbols.md`) notes `debug=true` + interactive
`samply record` required. Does our `profiling-prep` profile satisfy
this? Does `make ay-prepare-profile-wave` succeed? Does
`make ay-samply-json-twitter` produce a symbol-resolved profile?

Deliverable: `research/03-profiling-reality-check.md` with one of:
- PASS (all commands work, artefacts land under `.profiles/samply/...`
  with readable symbols), or
- FAIL (which step breaks, exact error, hypothesis for fix).

### Research agent 4 — Bench surface audit

Measure a representative run of `make ay-bench-close WAVE=close`
minimally (just one bench binary). Capture:
- Cold compile time for the bench binary.
- Per-fixture parse time distribution.
- Any regression vs `docs/benchmarks/post-Z.json` or the latest
  post-wave-N.json.
- Whether fat-LTO lands (confirm `--profile bench` inherits LTO).

Deliverable: `research/04-bench-surface-audit.md`.

### Research agent 5 — Incremental cache behavior + ICE repro

The AY-II.W0'.d7 effort hit a reproducible nightly rustc ICE in
`bbnf-analysis` on incremental touch-cascade. Does it still repro?
What edge-triggers it? Is there a narrower workaround than excluding
the whole crate from `iter-check`?

Deliverable: `research/05-incremental-cache-ice.md` with:
- Min repro test case + rustc stack trace.
- Workaround spectrum (from broadest: current exclude, to narrowest:
  per-file override).
- Upstream issue search: does rust-lang/rust-ice already have a match?

---

## Scope — plan wave (opens after research)

Single plan agent reads all five research deliverables. Authors
`docs/tranches/B1/B1.md` (the tranche plan) + `waves/W0.md` +
`waves/W1.md` per SPEC §Plan structure. Wave count: 2-3 max.

Plan gates:
- Every routine operation < 2s warm.
- Every touch-cascade iteration < 5s warm.
- Regen (`scripts/bootstrap-bbnf.sh`) ≤ 3 min cold, byte-identical
  idempotency on double-regen.
- `make ay-bench-close WAVE=close` runs clean on all 5 benches.
- `make ay-samply-<grammar>` produces symbol-resolved profiles.
- No `#[allow(dead_code)]` introduced to hide incomplete work.
- No feature flag shipped in `false` state.

## Scope — implementation waves (W0, W1, …)

Each implementation wave follows `SPEC.md` §Phase structure: numbered
sub-phases with file bounds, hard gates, commit templates. Waves
dispatch 2-5 parallel agents on disjoint file bounds per SPEC
§Parallelism.

Likely implementation directions (the research+plan surfaces the
actual set):
- Narrow additional workspace members out of `iter-check`.
- Feature-gate per-grammar test binaries so routine work only builds
  what the test actually needs.
- Parallelize proc-macro expansion via multi-derive crate splits (one
  sub-crate per grammar under gorgeous/bbnf-bootstrap).
- Cache priming: `make ay-prime` target that pre-populates
  `target/.bbnf-cache` + `target/ax-iter/deps` so new clones skip the
  10-min cold.
- Stale-fingerprint narrowing in `crates/derive/build.rs` (W0'.d6
  started; may need further narrowing per research agent 5).
- Fix or formally annotate the `bbnf-analysis` rustc ICE.
- `scripts/test-tier.sh leaf` stale-arg fix (B0 explicitly flagged).

---

## Close conditions (B1 FINAL.md must satisfy)

- Every plan gate measurably met with a named artefact.
- `docs/benchmarks/post-B1.json` aggregates the timing matrix.
- `cargo test --workspace --no-fail-fast` green.
- No parity-critical runtime behavior changed (B1 is build-infra-only).
- Every `iter-*` / `expand-*` / `ay-*` command documented in
  `docs/instructions/PROFILING.md` or superseded cleanly.
- `docs/tranches/B1/FINAL.md` records hard-gate outcomes with artefact
  citations.
- AY-II tranche was not blocked by any B1 wave; AY-II.W1 executes in
  parallel with B1.W1+ if scheduling allows.

---

## Non-negotiables

- **No quick solutions.** Every fix is an architectural transposition;
  every metric has a runtime-verifiable gate.
- **No workarounds.** If a path is broken, fix the path, not its
  caller.
- **No tolerance for multi-minute routine operations.** The user's
  directive stands: seconds, not minutes, for iter-level work.
- **Hard time caps on all agents**: research 20-30 min, plan 15-20 min,
  redress 30-45 min per agent. Halt + commit partial findings at cap.
- **Read before you act.** The instructions + tranche + memory layers
  contain the precedent. Reinventing when precedent exists is waste.
- **Commit at every milestone**; orchestrator cherry-picks accepted
  commits onto master.

---

## Dispatch order

1. **Research wave**: 5 parallel agents per §Research wave above.
   Each on isolated worktree. 20-min cap. Dispatched simultaneously.
2. **Plan wave**: 1 agent after all 5 research deliverables land.
   15-min cap. Authors `B1.md` + wave specs.
3. **Implementation waves**: per the plan, 2-5 parallel agents per
   wave on disjoint file bounds. Cap per wave TBD by the plan.
4. **FINAL**: orchestrator authors close ceremony + bench diff.

Go.
