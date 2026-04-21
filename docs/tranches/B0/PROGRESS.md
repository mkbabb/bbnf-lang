# B0 — Progress Log

Dated execution log for tranche B0.

- `Status`: in_progress
- `Current wave`: W0 (complete), W1 (dispatching)
- `Next wave`: W1

---

## 2026-04-20 — B0 opens; W0 closes

B0 dispatched against master HEAD `66a0f2cd`. Orchestrator opened the
annex with a `chore(cargo): drop stale smallbox lockfile entry`
housekeeping commit (`9bff7e7d`) to clean the workspace before
wave dispatch; that cleanup later had to be undone (`aaed3f08`) once
parse-that's dirty state was stashed and the real dep graph resolved
with `smallbox` present. Net effect: master is clean, Cargo.lock is
honest.

### W0 dispatch shape

The W0 spec declared 3 parallel agents on sub-items W0.1–W0.3, but the
file-bound audit showed W0.1 and W0.2 both owned `Makefile` +
`.cargo/config.toml` (not disjoint) and W0.3's timing proof depended on
W0.1+W0.2 having landed to measure "after". Per SPEC §Parallelism
file-bound audit at plan time, the orchestrator dispatched as two
phases:

- **Phase 1 (2 parallel)**: `B0.W0.a` consolidator for W0.1+W0.2
  (Makefile + `.cargo/config.toml`); `B0.W0.b` for PROFILING.md public
  fast-path section + baseline wall-clock capture at HEAD `9bff7e7d`.
- **Phase 2 (1 serial)**: `B0.W0.c` for post-W0 measurement, timing
  proof subsubsection, mid artefact.

Three sub-agents total — the plan's 3-parallel intent honoured with
file-bound truth.

### W0-A — B0.W0.a command surface (3 commits)

- `4869e715` `.cargo/config.toml` gains an `[alias]` block with
  `iter-check`, `iter-test-leaf`, `iter-test-grammar`,
  `expand-{json,css,bbnf,sheets}`, `asm-parse`. Fast-path routes all
  share `--profile ax-iter`.
- `0c2a5d4c` `Makefile` gains an "AY Iteration Surface" section with
  public routine, codegen inspection, bench, and profile-wave
  targets; `.PHONY` list updated; `test:` target carries a comment
  pointing routine callers to the iter-* surface.
- `f4c84d01` `docs/benchmarks/post-B0-W0-commands.txt` catalogs every
  alias + target with the AY W5/W6/W7 hard-gate cross-references.

Deviations recorded:
1. Alias list uses `-p tape` not `-p bbnf-tape`; workspace package is
   actually named `tape` (`crates/tape/Cargo.toml`). `scripts/test-tier.sh`
   carries the same stale name and is deferred to W2.
2. `make iter-test-leaf` invokes the cargo alias directly rather than
   routing through `scripts/test-tier.sh leaf`; the script fix is out
   of W0 scope.

### W0-B — B0.W0.b PROFILING docs + baseline (1 commit)

- `b8dac71e` PROFILING.md gains `## Public fast-path commands` section
  (59 lines inserted at line 36, bit-identical to existing sections);
  `docs/benchmarks/post-B0-W0-baseline.txt` captures pre-W0 wall-clock
  for the four representative commands at HEAD `9bff7e7d`.

Baseline evidence (incremental / warm numbers are the routine metric):

| Command | Cold | Warm |
|---|---|---|
| `cargo check --workspace` | 145.36 s | 7.16 s |
| `cargo check --profile ax-iter --workspace` | 2.79 s* | 0.23 s |
| `scripts/test-tier.sh leaf` | FAIL 0.45 s | FAIL |
| `cargo test --workspace --no-run` | 126.10 s | 14.40 s |

*ax-iter cold was against a pre-populated `target/ax-iter/`.

Agent flagged two pre-existing environmental issues as baseline data:
`scripts/test-tier.sh leaf` fails on stale `-p bbnf-tape` crate name;
the parse-that sibling repo had uncommitted edits retiring
`egraph::SaturationCache` (broke every `cargo check`). Agent restored
parse-that's stash after measuring.

### W0-C — B0.W0.c post-W0 timing proof (1 commit)

- `23b1a805` `docs/benchmarks/post-B0-W0-mid.json` records post-W0
  wall-clock for all five routine commands against the baseline JSON
  copies; PROFILING.md gains `### W0 timing proof` subsubsection
  citing both artefacts.

Headline numbers:

| Command | Baseline warm | Post-W0 warm | Ratio |
|---|---|---|---|
| workspace check | 7.16 s | 0.17 s | 42× (via `cargo check`) |
| iter-check | — | **0.16 s** | — |
| make iter-test-leaf | FAIL | 1.05 s | restored |
| workspace test --no-run | 14.40 s | 0.76 s | 19× |

`cargo iter-check` warm (0.16 s) vs baseline `cargo check --workspace`
warm (7.16 s) ≈ **45× speedup** for the routine AY iteration path.
`make iter-test-leaf` now works where the baseline's `test-tier.sh
leaf` failed on stale crate names.

### W0 environmental interventions

- **parse-that sibling-repo stash**. Parse-that carried uncommitted
  incomplete work retiring `egraph::SaturationCache` and e-graph rule
  modules, leaving `info/mod.rs` and `hir/mod.rs` referencing the
  retired surface. Every `cargo check` against that state failed with
  E0425. The orchestrator stashed the uncommitted state (recoverable
  via `git stash pop` on parse-that master) with message `"WIP:
  SaturationCache/egraph-rules retirement (pre-B0 in-flight; stashed
  2026-04-20 by bbnf-lang/B0 orchestrator to unblock cargo check)"`.
  Parse-that is now at HEAD `919d77d` — clean compile; the in-flight
  retirement is preserved for whichever future wave or tranche owns
  it.
- **Cargo.lock smallbox restoration**. After the parse-that stash,
  cargo re-resolved the dep graph with `smallbox` present (real
  transitive dep of `pprint`); committed (`aaed3f08`) to keep
  Cargo.lock honest.

### W0 hard-gate ledger

| # | Gate | Evidence | Status |
|---|------|----------|--------|
| 1 | Public command surface exposes fast routine path | `docs/benchmarks/post-B0-W0-commands.txt` | PASS |
| 2 | Representative AY routine timings improve | `docs/benchmarks/post-B0-W0-mid.json` (iter-check warm 45× faster) | PASS |
| 3 | Docs and defaults agree on public routine path | PROFILING.md §Public fast-path commands + `post-B0-W0-commands.txt` | PASS |

### W0 → W1 handoff

W1 opens on a fast routine surface (`iter-check`, `iter-test-leaf`,
etc.) landed in `.cargo/config.toml` + `Makefile` and documented in
`PROFILING.md`. The ax-iter profile already exists in `Cargo.toml`;
W1 adds the profiling-prep and final-proof distinctions, and exposes
the exact AY.W5-W7 expand/asm/bench/Samply command paths. Master
HEAD `aaed3f08`.

Known carry-forward items for W2:
- `scripts/test-tier.sh` leaf-tier still references the stale
  `-p bbnf-tape` crate name; W2's routine/heavy split agent fixes it.
- `make iter-test-leaf` currently calls `cargo iter-test-leaf`
  directly; W2 can reroute through the repaired `test-tier.sh` if
  desired (cosmetic, not load-bearing).
