# R3 — Infrastructure Speedups

## Summary

Top three iteration-time wins, ranked by estimated wall-time reduction per agent session:

1. **`ax-iter` cargo profile** (T6) — strips debuginfo from compile-gate / test runs that do not need samply attribution. On the aggregate test binaries that triggered the AX.W0a.2.d 26 GB-RSS OOM, peak rustc RSS falls roughly 3× and link time 2×. Orchestrator uses `--profile ax-iter` during emitter-iteration waves; `dev` remains default for samply work. *Estimate: –3 to –6 min per agent session.*
2. **`scripts/test-tier.sh {leaf,grammar,workspace}`** (T4) — formalises the already-implicit three-tier partition. Agents iterating on mining/IR passes run only `leaf` (~1 min cold, <15 s warm); agents iterating on shape emitters run `grammar` (~3–5 min); wave close runs `workspace`. *Estimate: –5 to –10 min per emitter-iteration cycle.*
3. **`seed-worktree.sh --target` owns the target symlink** (T1) — AX.W0a.2.d's memory-mitigation note records that the orchestrator started manually symlinking `target/` per-wave because omitting it cost ~1 hour per agent spin-up on a cold 13 GB target. Seed the symlink by default; agents opt out with `--no-target`. *Estimate: –30 to –60 min per new worktree.*

## Per-axis findings + proposals

### T1. Cargo target + cache strategy

**Diagnosis.** `sccache`/`ccache` not installed. Rebuild profile (from AX.W0a.2.e data) shows rustc — not linker — dominates peak RSS on aggregate test binaries; sccache targets rustc output caching, which would help cross-worktree re-use. `mold`/`lld` not installed either; `.cargo/config.toml` carries a commented block that references wrong paths (`/opt/homebrew/opt/llvm/bin/ld.lld` — Homebrew's `llvm` keg does not ship `ld.lld`; the correct keg is `lld`, installed to `/opt/homebrew/opt/lld/bin/ld64.lld`). Seven existing worktrees plus `.claude/worktrees/agent-*` rely on a 13 GB target; cold re-hydration cost is real.

**Proposals.**

- **(committed) `seed-worktree.sh` owns `target/` symlink** by default. Idempotent: re-running on a worktree with an existing matching symlink is a no-op; existing non-matching symlink produces a warning, not a fail. `--no-target` opt-out for the rare case where an isolated target is wanted.
- **(committed) `.cargo/config.toml` comment rewrite** — correct homebrew paths (`/opt/homebrew/opt/lld/bin/ld64.lld`), state the pre-install check, cite the ~30–50% link speedup. Block stays commented (neither linker is currently installed); the text is now actionable.
- **(deferred) sccache** — evaluate post-AX. Cross-worktree rustc cache hit rate would need measurement; not worth gating AX on.

### T2. Bootstrap regen cycle

**Diagnosis.** `bootstrap-bbnf.sh` rebuilds the bbnf library from scratch on every invocation via `cargo expand -p bbnf-bootstrap`. The orchestrator's idempotency-check convention (regen twice, diff outputs) doubles cost — ~8–16 min × 2 = 16–32 min. The second cycle repeats the full rustc + expand path against the just-emitted `generated.rs`, producing (on success) byte-identical output.

**Proposal (committed indirectly).** `check-bootstrap-clean.sh` already does the right thing for CI (copy-then-regen-then-diff). For local idempotency: propose orchestrator usage — `cp generated.rs /tmp/gen1; bash bootstrap-bbnf.sh; diff /tmp/gen1 generated.rs` — sidesteps the second full regen entirely when the first run is successful. Not a new script; a discipline note (see `docs/instructions/README.md §Bootstrap regen` which already documents this pattern correctly). Not committing new code; existing docs cover it. Cost ~halved when adopted.

**Not proposing.** Delta-regen against `.bbnf-cache` would require proc-macro rewrite; outside AX scope.

### T3. Test binary sharding

**Diagnosis.** `crates/core/tests/` carries 94 files. `tape_parity_*` is already split (6 per-grammar binaries, AX.W0a.2.e commit `61053374`). Remaining multi-grammar aggregates ranked by derive-Parser count (`grep -c '#\[parser\('`):

| File | Derives | Size | Split priority |
|------|---------|------|----------------|
| `serialize_roundtrip.rs` | 9 | 294 LOC | **high** |
| `grammar_profile_wire_contract.rs` | 4 | 498 LOC | medium |
| `gate_predicate_wire_contract.rs` | 3 | 250 LOC | medium |
| `structural.rs` | 1 (via 3 mentions) | 457 LOC | low |
| `aw3_w1_walker_trace.rs` | 2 | 104 LOC | low |

`serialize_roundtrip.rs` is the tallest aggregate — JSON + CSV + BNF + EBNF + math + Sheets + BBNF + CSS-pretty + CSS-L4 all in one compilation unit with `serialize` codegen on top of Parser codegen. Same failure mode as pre-split tape_parity, one grammar less than the OOM-ing version (post-inline-emit).

**Proposal.** Open a W0b follow-on for `serialize_roundtrip.rs` split; mirror the `tape_parity_common/` extraction pattern. Not splitting in this audit — it's an emitter-consuming change and bounds say no emitter touches. *Estimated speedup:* a single derive-Parser site links in ~11 s ±3 per site post-AX.W0a.2.e; 9-in-one likely compiles in 2–5× single-site cost when LTO + codegen-units=1 concatenate.

### T4. Full workspace test cost

**Diagnosis.** README documents a four-step test ladder (`cargo test -p bbnf-tape -p bbnf-ir -p egraph` → roundtrip → payload → workspace) but nothing enforces or captures it. Agents re-derive the ladder in each prompt; drift is inevitable.

**Proposal (committed).** `scripts/test-tier.sh <tier>` encodes the ladder. Three tiers:

- `leaf` — `-p bbnf-tape -p bbnf-ir -p egraph -p csp-solver -p bbnf-ser`. Zero derive-Parser sites; pure data-crate coverage. ~1 min cold, <15 s warm.
- `grammar` — per-grammar binaries (`tape_parity_*`, `*_parity`, etc.). Adjusts automatically as tests are added/split; skips missing entries. ~3–5 min cold.
- `workspace` — full `cargo test --workspace`. Wave close only. ~10–15 min cold.

Output goes to `/tmp/test-tier-<tier>.txt`; script greps for `test result|FAILED|error\[`. Exits non-zero on any failure. Passes extra args through (`--no-run` for compile-gate only).

### T5. `cargo expand` hot path

**Diagnosis.** `prepare-profile-wave.sh` already caches `cargo expand` output at `.profiles/samply/prebuild/expand/<bench>/expand.rs`. That cache is per-wave, not per-commit; each wave re-expands unconditionally.

**Proposal (not committing).** Keying expand cache by `generated.rs` hash + bench source hash would let waves share the cache when codegen didn't change. Orthogonal to AX's correctness work, and the cache key design should live in the profile infra not in a script stub. Defer to post-AX W13 (CPU autotune already touches profiling infra).

### T6. Debug-profile cost

**Diagnosis.** AX.W0a.2.d diag (`post-AX-W0a2d-diag.md:69`): "cargo test -p bbnf --test tape_parity --no-run with all 5 Parser-derive sites consuming the inline helpers peaked at 26 GB RSS for a single rustc process … The tape_parity binary links all five derive expansions into one translation unit with debuginfo=2." Per-grammar split mitigated the aggregate case; debuginfo remains a tax on every iteration that doesn't need samply.

**Proposal (committed).** `[profile.ax-iter]` in workspace `Cargo.toml`:

```toml
[profile.ax-iter]
inherits = "dev"
debug = 0
split-debuginfo = "off"
strip = "debuginfo"
```

Invocation: `cargo test --profile ax-iter -p bbnf --test tape_parity_json`. Orchestrator + agent briefings use `ax-iter` for iteration; `dev` stays the default so `cargo test` without `--profile` preserves samply-ready binaries. Not touching `dev` directly — samply regressions from losing DWARF would be catastrophic.

### T7. Parallel orchestration

**Diagnosis.** `git worktree list` shows 7 active `bbnf-wt-*` worktrees plus 9 `.claude/worktrees/agent-*` plus 2 prunable `/tmp/bbnf-*` that violate README §Worktree isolation (never under `/tmp`). No tooling enumerates HEAD + dirty state + target-symlink wiring across worktrees; no safety net for rogue rustc processes after a session crash.

**Proposals (committed).**

- **`scripts/worktree-status.sh`** — enumerates `../bbnf-wt-*/` siblings. Reports worktree / HEAD / dirty / target-symlink-state / branch. `--tsv` for pipelines; `--dirty` to narrow to worktrees with uncommitted work. First-pass tool for the orchestrator on session resume.
- **`scripts/kill-all-rust.sh`** — TERM-then-KILL rustc / cargo processes outside the caller's own process group. `--dry-run` lists without signaling; `--include-lsp` extends to rust-analyzer when post-crash LSP cleanup is wanted. Exempts the caller shell's PGID so the script doesn't SIGTERM itself.

## Script manifest

| Path | Purpose | Invocation |
|------|---------|------------|
| `scripts/test-tier.sh` | Tiered test runner (leaf / grammar / workspace) | `scripts/test-tier.sh grammar --no-run` |
| `scripts/worktree-status.sh` | Enumerate sibling worktrees with HEAD / dirty / target-symlink state | `scripts/worktree-status.sh --tsv` |
| `scripts/kill-all-rust.sh` | Terminate orphan rustc/cargo processes with grace-then-kill | `scripts/kill-all-rust.sh --dry-run` |
| `scripts/seed-worktree.sh` (edit) | Now owns `target/` symlink by default; `--no-target` opts out | `scripts/seed-worktree.sh ../bbnf-wt-foo` |

## Config edits

| File | Before | After | Rationale |
|------|--------|-------|-----------|
| `Cargo.toml` (workspace) | only `dev` / `release` / `bench` profiles | added `[profile.ax-iter]` inheriting `dev` with `debug = 0`, `strip = "debuginfo"` | AX.W0a.2.d 26 GB-RSS tape_parity OOM was debuginfo-dominated on aggregate binaries. Iteration runs (compile-gate, test-pass) don't need DWARF. `dev` remains default; `ax-iter` is opt-in. |
| `.cargo/config.toml` | commented block with wrong `ld.lld` path (`/opt/homebrew/opt/llvm/bin/ld.lld` — not shipped by that keg) | rewritten comment: correct `lld` keg path (`/opt/homebrew/opt/lld/bin/ld64.lld`), pre-install check, estimated link-time speedup | Current comment is misleading even before speedup realisation. Neither linker is currently installed; block stays commented but text is now actionable when a user runs `brew install lld`. |

## Deferred proposals

Too large / out-of-scope for this wave; orchestrator decides post-restart:

- **sccache rollout.** Cross-worktree rustc caching would help but demands installer + measurement; defer.
- **`serialize_roundtrip.rs` split.** Mirrors tape_parity split. Tempting but touches the emitter-adjacent Parser derive surface; defer to a W0b follow-on where it can be verified against the `serialize` codegen.
- **`bootstrap-bbnf.sh` delta regen.** Requires proc-macro cache-key work; orthogonal to AX's correctness path.
- **`cargo expand` cache by content hash.** Useful once AX routing stabilises; premature now.
- **`grammar_profile_wire_contract.rs` + `gate_predicate_wire_contract.rs` split.** Lower priority than `serialize_roundtrip.rs` (4 and 3 derives vs 9). Bundle with the W0b follow-on.
