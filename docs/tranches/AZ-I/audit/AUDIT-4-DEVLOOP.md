## AUDIT 4 — Dev-loop friction ledger

Audit lens: dev-loop friction. Scope: testing, benching, building.
Reference walls cited inline; estimates labelled `(est)`.

### 1. The actual cycles

Walls below come from B6/B7/AY-II-I FINAL or PROGRESS measurements
on `nightly-2026-04-11` (pinned), `arm64-apple-darwin`,
`CARGO_BUILD_JOBS=4`. Single warm `cargo check -p bbnf-ir --profile
ax-iter` measured this audit at **5.08 s** (target/ shared symlink).

| Cycle | Wall | Source |
|---|---|---|
| `cargo xtask regen --grammar bbnf` cold | **0.46 s** | B6/FINAL §Headline (post-W0 content-equality skip; was 88.26 s pre-B6, 192× delta) |
| `cargo iter-check` cold (4-exclude) | **11.3 s** | AY-II-I/PROGRESS post-d4/d6 fix (was 10+ min stalls pre-fix) |
| `cargo iter-check` warm | **0.14 s** | same |
| `cargo iter-check-full` cold | **600+ s pre-B2 → seconds post-B2** | PROFILING §Routine surface |
| `cargo iter-test-leaf` warm (582 tests) | **1.14 s** | AY-II-I/W0 verification table |
| `cargo nextest run -p bbnf-ir` (375 tests) | **~3-5 s warm (est)** | inferred from leaf-tier 1.14 s for 582 |
| `cargo nextest run --workspace --profile ax-iter` (1546 tests) | **10.832 s warm** | B7/FINAL §Headline (-51.5 % vs 22.353 s pre-B7) |
| `cargo bench -p bbnf --bench json_monolithic` cold | **~30-60 s build + ~20 s divan run (est)** | divan harness; sample_size=1 |
| `cargo xtask regen --check` precommit | **~0.5-1 s for 9 grammars (est)** | derived from 0.46 s × 9 with dedup |

The dev-loop default is `cargo iter-check` (11.3 s cold / 0.14 s warm).
The substantive iteration cycle is therefore the warm 0.14 s — the
post-AY-II-I/post-B6 substrate has *already* eliminated the historic
hour-walls. Friction now lives in the long-tail: workspace nextest,
xtask incremental cost, and fan-out target.local rebuilds.

### 2. Drag points

**D1 — CSS L4 generated.rs at 205 991 LOC** (verified at audit:
`wc -l crates/core/src/grammar/generated/css_l4.rs`). Total generated/
= 301 390 LOC, 15 MB on disk. Recompiling `bbnf` after a CSS L4 regen
costs ~85 s release (B6/FINAL line 34) — the content-equality skip
elides this *only when output is byte-identical*. Any genuine emitter
change still pays the full bbnf rebuild.

Root cause: per-grammar source emission is the architectural target,
not the symptom. Fix: keep the content-equality skip; consider
splitting css_l4.rs into per-rule sub-modules so cargo's fingerprint
invalidation tracks at sub-grammar granularity.

**D2 — Aggregate test binary peak RSS (AX 26 GB OOM).** AX commit
`61053374` split tape_parity into per-grammar binaries (~3 GB RSS,
11-14 s each). Resolved; remaining drag is operational — agents must
remember `CARGO_BUILD_JOBS=4` (README §Memory discipline). No
remaining technical drag.

**D3 — Workspace nextest 10.8 s wall.** Dominated by per-bench-binary
link of all 18 bbnf benches + 1546 test discovery. The `[[bench]]`
table in `crates/core/Cargo.toml` carries 18 entries; each is a
separate binary even at `cargo nextest`'s discovery step. Cut: gate
WASM/TS competitor benches behind a feature flag (they require
wasmtime, tree-sitter, npm) — bench surface remains, but iter-check
discovery skips them. Estimated save: 2-3 s warm.

**D4 — `cargo xtask regen` rebuild on emitter changes.** xtask
path-deps `bbnf` and `bbnf-ir`; any emitter or IR change rebuilds
xtask before regen runs. `xtask` itself is `release` profile via the
`xtask = "run -p xtask --release --"` alias, compounding the cost.
Estimated: 30-60 s (est) cold rebuild penalty when emitter changes,
on top of the regen itself. Cut: switch xtask alias to `--profile
ax-iter` for dev iteration; keep release for CI regen-check.

**D5 — Cache-clear ritual still in README.** §Cache clearing instructs
`find . -name .bbnf-cache -exec rm -rf {} +` before every bench, regen,
or proc-macro expansion. Post-B2 the proc-macro (`crates/derive/`) is
**retired**; xtask/regen is the canonical regen path. The
`.bbnf-cache` directory was the proc-macro's expansion cache. The
ritual is dead code in instructions. Cut: delete §Cache clearing,
delete the `find -name .bbnf-cache` lines from §Testing /
§Benchmarking / §Bootstrap regen. Save: per-iteration mental
overhead, plus the 5-30 s the ritual itself takes when cache is
populated.

**D6 — Per-worktree `target.local` rebuild cost on fan-out.** AZ-I.W1.B
ran 4 parallel agents with `CARGO_TARGET_DIR=$(pwd)/target.local`
(PROGRESS line 134). W2 stage 2 same shape, 4-way parallel B/C/D/E
(W2-EMITTER-REWIRE line 287). Each `target.local` cold-builds the full
dep graph: bbnf-ir (12 deps), bbnf (40+ deps incl wasmtime, lightningcss,
tree-sitter, sonic-rs, etc). Estimated cost: **~3-5 min cold per
worktree, ~15-20 GB disk per worktree (est)** — the main `target/` is
14 GB at audit time; per-worktree target.local approximates the same
once warm. 4 parallel agents = 60-80 GB transient disk + 12-20 min
agent-wall.

**D7 — W2 4-stage cherry-pick consolidation cycle.** W2-EMITTER-REWIRE
§5 sequences A solo (60 min) → B/C/D/E parallel (30-45 min each,
post-A-merge, fresh worktrees) → orchestrator stage 3 (regen +
workspace nextest + parity harness + bench gate). The serialisation
dependency: B/C/D/E can't start until A's commits cherry-pick to
master. Cause: shared `shapes/mod.rs` (328 LOC, owner A) and
`backend/rust/emitter/` paths overlap. Cut: factor `shapes/mod.rs`
into per-shape sub-modules (`shapes/object/mod.rs` etc.) so the
master shape registry has zero overlap with per-shape implementations
— B/C/D/E could then run in true parallel from W2 start.

### 3. Tools that should exist but don't

**T1 — `cargo bbnf-watch` (or bacon).** No `cargo watch` alias in
`.cargo/config.toml`; the Makefile carries one `watch` target locked
to `bbnf-lsp`. A `cargo iter-watch` alias = `watch -x iter-check`
would give zero-friction post-edit feedback. Retires: nothing
existing — the `make watch` target is LSP-specific.

**T2 — `cargo iter-grammar GRAMMAR=json`.** Combined regen → check →
test for one grammar, in one invocation. Today this is three
sequential commands. Retires: the orchestrator's manual sequencing
in W2-EMITTER-REWIRE Stage 3 (steps 2-4 = `xtask regen --grammar
json` + workspace nextest + parity test).

**T3 — `cargo xtask regen --grammar X --diff`.** `regen --check` exits
nonzero on drift but does *not* emit a diff; the orchestrator must
re-run regen for real and `git diff` to see what changed. A
`--diff` mode that runs the tempdir compare and prints `git
diff --no-index <tmp> <checked-in>` would close the loop. Retires:
manual diff-after-regen pattern.

**T4 — Generated-file budget tracker.** `feedback_generated-size-budget`
declares per-tranche LOC budgets. No tooling enforces it. A
`cargo xtask budget` reading `[workspace.metadata.bbnf.budgets]` and
diffing `generated/<ident>.rs` LOC against the budget would make the
feedback enforceable. Retires: ad-hoc inspection — auditors today
manually `wc -l` per audit.

### 4. Tools that exist but are friction

The Makefile is **384 LOC, 46 PHONY targets** — under the 100 hard
gate. The cargo alias surface is **28 aliases** (grep `^[a-z]` in
`.cargo/config.toml`). Both are well-shaped today.

**F1 — Three-tier profile surface (`ax-iter` / `profiling-prep` /
`bench`).** Documented in PROFILING §Bench alias surface and root
`Cargo.toml` lines 31-94. The tier system is principled (cold-iter /
prepared-bench / publish-grade) and pays for itself: AY.W5-W7 gate
contract depends on the tier distinction. Keep.

**F2 — `cargo iter-check-full` vs `iter-check`.** Both exist; full is
the close-gate. AY-II-I PROGRESS shows the four-exclude `iter-check`
was load-bearing (10+ min stall regression resolved at
`iter-check-full` exposure). Keep both; the per-exclude fast-paths
(`iter-check-lsp`, `iter-check-prettify`, `iter-check-bootstrap`)
satisfy invariant 10 truthfully.

**F3 — `make ay-*` gate command surface (12 targets).** The W5-W7
ceremony commands are stable, named, and called verbatim by AY
executors. Keep — these are the "hard gate" surface, not iteration
surface; their cognitive cost is one read of PROFILING §AY W5-W7 gate
commands, paid once per executor.

**F4 — README §Cache clearing ritual.** As noted in D5: dead. Cut.

### 5. The fan-out cost

This session ran 4-parallel (W1.B) and 5-parallel (W2 stages) agent
waves. Each non-orchestrator agent used per-worktree `target.local`
to satisfy `feedback_single-cargo-per-target`. Cost:

- **Disk:** 15-20 GB per `target.local` (est, by analogy to main
  `target/` = 14 GB). 4 parallel = 60-80 GB transient. 5 parallel =
  75-100 GB.
- **Wall:** 3-5 min cold full rebuild per worktree (est). 4 parallel
  agents amortise to wall = max(individual), but each individual pays
  3-5 min before productive work.
- **Memory:** rustc per worker peaks ~3 GB RSS post-AX split. 4
  parallel × `CARGO_BUILD_JOBS=4` per agent = 16 rustc processes
  worst case, ~48 GB RSS combined. Real load lower because worker
  rampup is staggered.

**Proposal — shared sccache.** `RUSTC_WRAPPER=sccache` in
`.cargo/config.toml` `[build]` + shared `~/.cache/sccache/` across
worktrees. Retires per-worktree cold rebuild entirely; cache key is
content-based, survives `target.local` wipes between waves. Does
not violate `feedback_single-cargo-per-target` — that's about
target-dir lock, not rustc-output reuse.

**Alternate — hardlink-clone build-once-then-fork.** Orchestrator
runs one cold `cargo iter-check-full` against main `target/` before
fan-out; agents `cp -al main-target target.local` (near-instant on
APFS, COW-friendly hardlinks diverging on first mutation). Retires
`scripts/seed-worktree.sh --no-target` flag.

### 6. Concrete cuts ledger — top 10 by wall-saved-per-iteration

| # | Cut | Mechanism | Save / iter |
|---|---|---|---|
| 1 | Shared sccache across worktrees | `RUSTC_WRAPPER=sccache` + `~/.cache/sccache/` | **3-5 min cold per fan-out worktree** |
| 2 | Hardlink-clone `target.local` from main `target/` | `cp -al` in `seed-worktree.sh` (drop `--no-target` divergence) | **3-5 min cold per fan-out worktree, 15-20 GB disk** |
| 3 | Switch xtask alias to `--profile ax-iter` for dev | `.cargo/config.toml` alias edit | **30-60 s (est) per emitter change** |
| 4 | Delete README §Cache clearing ritual + every `find -name .bbnf-cache` line | doc edit only | **5-30 s + cognitive cost per regen / bench / test** |
| 5 | Add `cargo iter-grammar GRAMMAR=X` alias (regen + check + test combined) | `.cargo/config.toml` alias | **~10 s + 2 cmd-roundtrips per grammar iter** |
| 6 | Add `cargo iter-watch` (= `watch -x iter-check`) | `.cargo/config.toml` alias + bacon optional | **0.14 s save per save+verify cycle** (warm), eliminates manual cargo invoke |
| 7 | Factor `shapes/mod.rs` so B/C/D/E run true-parallel from W2 start | refactor; rotate stages 1+2 from sequential→parallel | **45-60 min per multi-shape wave** (W2 stage 1 deletion) |
| 8 | Add `cargo xtask regen --grammar X --diff` mode | xtask edit | **~10 s + manual git diff per drift investigation** |
| 9 | Gate WASM/TS competitor benches behind feature flag | `Cargo.toml` `required-features` | **2-3 s per workspace-nextest run** |
| 10 | Add `cargo xtask budget` for generated/ LOC budgets | xtask edit + `[workspace.metadata.bbnf.budgets]` | **per-tranche audit time, est 5 min** |

Top 5 by wall-saved aggregate: **#1 + #2** (fan-out elimination, 3-5
min × N agents per wave) > **#7** (W2 stage 1 deletion) > **#3** (xtask
profile flip) > **#4 + #5** (cache-clear retirement + per-grammar
combo).

### 7. Hand-off — synthesis recommendations for W2-act.W0 prelude

The W2-act execution path opens with:

1. **Land cuts #1 + #2 (fan-out elimination) before any W2-act fan-out
   wave.** This is the highest-leverage prelude — every multi-agent
   wave from W2-act onwards amortises the win. Concrete: edit
   `.cargo/config.toml` to add `[build] rustc-wrapper = "sccache"`,
   document `~/.cache/sccache/` as the shared cache root in
   PROFILING §Shared-target discipline, edit `seed-worktree.sh` to
   `cp -al` the main target into `target.local` instead of the
   current symlink-or-skip dichotomy.

2. **Land cut #4 (cache-clear ritual deletion) in the same prelude
   commit as a doc-coherence sweep.** README §Cache clearing,
   §Testing, §Benchmarking, §Bootstrap regen all reference the dead
   ritual. The mention pollution is non-trivial — first-time agents
   spend cycles understanding why it doesn't matter.

3. **Land cut #3 (xtask profile flip) before W2-act emitter work.**
   Emitter iteration is the dominant W2-act activity; xtask cold
   rebuild on every emitter change is a 30-60 s tax per cycle.

4. **Defer cuts #5, #6, #8, #10 to W2-act.W3 polish wave.** Each is a
   solo 15-30 min refactor, low risk, low blast radius.

5. **Cut #7 (`shapes/mod.rs` factor) is W2-act's core re-shape**, not
   a prelude — it changes wave parallelism shape. Audit synthesis
   should fold it into the W2-act phase plan, not the prelude.

6. **Cut #9 (WASM/TS feature-gate) is bench-Cargo.toml hygiene**, can
   land as a W2-act.W4 close item without affecting the dev cycle.

The thread connecting everything: the historic walls (60+ min cold
workspace check, 88 s xtask regen, 26 GB OOMs) are *gone* —
post-AX/B6/AY-II-I substrate eliminated them. Remaining friction is
**second-order**: fan-out tax, instruction-pollution, and a small
number of missing combinators. The cuts above retire the second-order
overhead; W2-act runs faster than any prior wave.
