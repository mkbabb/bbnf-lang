# Profiling — Samply Workflow and Wave Orchestration

Samply is the canonical profiler. All runtime perf claims cite samply
artefacts. This document governs the profiling workflow, from a single
headless run to a full multi-agent wave.

All directives from `README.md` apply here — file-first expensive
commands, cache clearing, worktree isolation, hardened agent claims,
no trammelling, indefatigability. The rules below layer the samply-
specific contract on top.

## Shared-target discipline

Profiling artefacts live under `.profiles/` in the main repo. All
agents in a profiling wave share **one** absolute `CARGO_TARGET_DIR`
— not per-worktree, not per-agent. Prepare once, then profile many.
Worktrees are **optional** for profiling (they isolate git, not
builds); sibling worktrees are permitted for parallel source-reading,
never `/tmp`.

```bash
export CARGO_TARGET_DIR=/absolute/path/to/shared/profile-target
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null
```

Sub-agents sharing the shared target do not run cargo concurrently
against it (see `README.md` §Concurrent cargo). Parallel per-crate
`cargo check` / `cargo test` probes by sibling agents require a
distinct `CARGO_TARGET_DIR` per agent. `scripts/prepare-profile-wave.sh`
enforces an absolute `CARGO_TARGET_DIR`.

## Public command surfaces — three profiles, three purposes

Routine iteration rides the `ax-iter` profile; profiling stays on
`dev` for DWARF symbols; final-proof work uses `bench` (fat LTO). Do
not cross-wire them. Full alias/target manifest in
`docs/benchmarks/post-B0-W0-commands.txt`.

### Routine — ax-iter profile

`ax-iter` inherits `dev` and strips debuginfo; link time roughly
halves and peak RSS on aggregate test binaries drops ~3×.

- `cargo iter-check` → `cargo check --profile ax-iter --workspace`
  with heavy proc-macro crates excluded per `.cargo/config.toml`.
- `make iter-check` — Makefile wrapper for the routine compile-gate.
- `make iter-test-leaf` — `scripts/test-tier.sh leaf --profile
  ax-iter` (pure-data + substrate crates; fastest correctness tier).
- `make iter-test-grammar` — per-grammar tape-parity + shape emit.
- `make iter-test-ws` — full workspace; only when wider blast radius
  is actually relevant.

### Codegen inspection — cargo expand + cargo asm

Expand and asm artefacts land under `target/expand/` and `target/asm/`
so aggregate expansions do not spill into `/tmp`.

- `make expand-json`, `make expand-css`, `make expand-bbnf`,
  `make expand-sheets` — write `target/expand/<bench>.rs`.
- `make asm-parse BENCH=<name> FN=<symbol>` — writes
  `target/asm/<bench>-<fn>.s`.

### Bench surfaces

Heavy; never in iteration loops. Cold-per-parse by contract
(`README.md` §Benchmarking).

- `make bench-compile BENCH=<name>` — compile-gate without running.
  Appropriate as preflight before a wave's samply prepare.
- `make bench-run BENCH=<name>` — runs the bench; clear `.bbnf-cache`
  first.

### Profiling preparation — prepared-binary reuse

`make profile-wave` wraps `scripts/prepare-profile-wave.sh` with the
shared-target contract. Both `scripts/prebuild-benches.sh` and
`scripts/prepare-profile-wave.sh` reuse cached binaries and `cargo
expand` artefacts when fresh vs the bench source, shape emitters
under `crates/core/src/backend/rust/emitter/shapes/`, and
`crates/core/src/grammar/generated.rs`.

Stale detection: editing a bench source, shape emitter, or regenning
`generated.rs` rebuilds that bench's artefacts; unrelated benches
stay cached. Per-bench stdout reads `reused: <path>` / `rebuilt:
<path>` for the binary step and `expand: reused <path>` / `expand:
regen <path>` for expansion, so the orchestrator can see cache hits
without reopening build logs. `target/release/deps/` and
`target/bench/deps/` remain searched as legacy fallbacks.

Manual invalidation: `rm -rf .profiles/samply/prebuild/{binaries.tsv,wave.tsv,expand}`
and `rm target/profiling-prep/deps/<bench>-*`.

### AY W5-W7 gate commands

`AY.W5-W7` hard gates resolve through public `make ay-*` entrypoints
in the Makefile's `AY W5-W7 Gate Commands` section. AY executors call
them verbatim; they do not re-derive the underlying `cargo expand`,
`cargo asm`, `cargo test`, `cargo bench`, or
`scripts/profile-bench-headless.sh` invocations. Samply targets
require `CARGO_TARGET_DIR` exported and a prepared profile wave.
Prebuilt binaries land under
`$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*` via
`make ay-prepare-profile-wave`; samply targets reuse them without
rebuilding.

| AY hard gate | Makefile target | Artefact |
|---|---|---|
| W5.1 / W7.2 expand JSON | `make ay-expand-json` | `target/expand/ay-json.rs` |
| W6.2 expand named-type | `make ay-expand-named-type` | `target/expand/ay-named-type.rs` |
| W5.3 close-stamp asm | `make ay-asm-close-compound FN=<sym>` | `target/asm/ay-close-<sym>.s` |
| W5.1 value API test | `make ay-test-value-api` | `cargo test` exit status |
| W7.1 wire-contract test | `make ay-test-wire-contract` | `cargo test` exit status |
| W6.1 named-type test | `make ay-test-named-type` | `cargo test` exit status |
| W5.2 samply eager JSON twitter | `make ay-samply-json-twitter WAVE=W5` | `.profiles/samply/AY-W5/json_twitter_eager/` |
| W6.3 samply JSON path lookup | `make ay-samply-json-twitter-lookup WAVE=W6` | `.profiles/samply/AY-W6/json_twitter_lookup/` |
| W5.5 / W6.4 / W7.4 close-matrix bench | `make ay-bench-close WAVE=<label>` | `docs/benchmarks/post-AY-<label>-{json,css,sheets,bbnf,compile}.txt` → aggregate `post-AY-<label>-mid.json` |
| Samply prerequisite | `make ay-prepare-profile-wave` | `$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*` |

Samply ports 3130/3131 route to `ay-samply-json-twitter`; 3132/3133
route to `ay-samply-json-twitter-lookup` so the two gates can run
sequentially against the same prebuilt binary without port collision.
`ay-bench-close` selects `--profile profiling-prep` for mid-wave runs
and `--profile bench` (fat LTO) when `WAVE=close` — use the latter
for publish-grade close-matrix numbers.

### B0 close proof

B0 closes with the command surface split landed, the prepared-binary
workflow idempotent, and the AY W5-W7 gate targets stable. Each close
invariant resolves through a named public command.

| Close invariant | Public command | Artefact |
|---|---|---|
| Routine no longer defaults heavy | `make iter-check`, `make iter-test-leaf`, `make iter-test-grammar` | `docs/benchmarks/post-B0-W0-mid.json` |
| Three distinct profile tiers usable | `cargo iter-check`, `cargo prep-bench`, `cargo final-bench` | `Cargo.toml` profile stanzas + `.cargo/config.toml` aliases |
| AY W5-W7 gate commands exact | §AY W5-W7 gate commands table above | same |
| Prepared-binary reuse | `scripts/prebuild-benches.sh`, `scripts/prepare-profile-wave.sh` (second run `reused:` / `expand: reused`) | §Profiling preparation |
| Samply gate entrypoints | `make ay-samply-json-twitter WAVE=W5`, `make ay-samply-json-twitter-lookup WAVE=W6`, `make ay-prepare-profile-wave` | `.profiles/samply/AY-<WAVE>/<scenario>/` |
| Close-matrix bench entrypoint | `make ay-bench-close WAVE=close` | `docs/benchmarks/post-AY-close-*` |
| Routine/heavy CI separation | `.github/workflows/ci.yml` preflight + heavy close-gate | `.github/workflows/ci.yml` |

### W0 timing proof

- Baseline (pre-W0, HEAD `9bff7e7d`): `docs/benchmarks/post-B0-W0-baseline.txt`.
- Post-W0 (HEAD `b8dac71e`): `docs/benchmarks/post-B0-W0-mid.json`.
- Headline: `cargo iter-check` warm (0.16s) is ~45× faster than
  baseline `cargo check --workspace` warm (7.16s).
- `make iter-test-leaf` warm 1.05s produces real pass/fail output.
- Routine iteration routes via the `iter-*` targets; keep `cargo test
  --workspace` and `cargo bench` for wave-close gates only.

## Prepare a wave

`scripts/prepare-profile-wave.sh` builds every bench binary once,
produces `cargo expand` artefacts per bench, reserves ports,
enumerates every `(bench, entry)` pair, and writes a `wave.tsv`
contract that sub-agents consume verbatim.

```bash
export CARGO_TARGET_DIR=/absolute/path/for/this/wave
find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null

scripts/prepare-profile-wave.sh \
  > .profiles/samply/prebuild/prepare.stdout \
  2> .profiles/samply/prebuild/prepare.stderr
```

Prepare artefacts:

- `.profiles/samply/prebuild/wave.tsv` — one row per `(bench, entry)`
  pair with ports, artefact dir, binary path, expand artefact path,
  bench cwd, target dir.
- `.profiles/samply/prebuild/binaries.tsv` — one prebuilt release
  binary per bench.
- `.profiles/samply/prebuild/expand/<bench>/expand.rs` — cargo expand
  output, read-only after prepare.
- `.profiles/samply/prebuild/expand/<bench>/expand.err` — stderr.
- Per-bench `*-build.txt`.

**Do not rerun `cargo expand` or `cargo bench` inside a wave after
prepare has finished.** Re-runs waste cycles and produce divergent
artefacts. Sub-agents consume prepared artefacts; they do not
regenerate them.

## Profile a single entry

`scripts/profile-bench-headless.sh` runs the prebuilt bench binary
under `samply record`, produces every required artefact, and verifies
named-frame coverage via `syms-proof.txt`.

```bash
scripts/profile-bench-headless.sh \
  --bench json_monolithic \
  --entry canada \
  --record-port 3130 \
  --load-port 3131 \
  --artifact-dir .profiles/samply/json_monolithic/canada \
  --bench-cwd "$(pwd)/crates/core" \
  --bin /absolute/path/to/prebuilt/binary
```

Required artefacts per entry (every one must exist and be non-empty):
`bench.txt`, `build.txt`, `record.txt`, `load.txt`, `profile.json.gz`,
`profile.json.syms.json`, `syms-proof.txt`.

## Orchestration contract — multi-agent waves

When a profiling task covers more than a single entry, the
orchestrator runs one agent per bench and delegates the entry matrix
to that agent. Five agents cover the full bench suite
(json_monolithic, css_l4, google_sheets_monolithic, bbnf_monolithic,
json_value).

**Orchestrator responsibilities.**

- Environment parity: `.cargo/config.toml` path patching, sibling
  patched repos resolve, `cargo expand` and `samply` installed.
- Export the absolute shared target dir; clear caches; run
  `scripts/prepare-profile-wave.sh`.
- Verify `wave.tsv`, `binaries.tsv`, and the expand artefacts.
- Reserve ports before spawning agents — one port pair per bench,
  reused sequentially by that bench's sub-agent across its entries.
- `.profiles/` in the main repo is the only retained artefact root.
  Sub-agents writing elsewhere violate the contract.
- Dispatch sub-agents with their exact `wave.tsv` row as input:
  worktree path (optional), bench, entry list, record port, load
  port, artefact dir, binary path, expand.rs path, expand.err path,
  bench cwd, shared target dir.
- Do not ask sub-agents to rerun `cargo expand` or `cargo bench` if
  prepare already produced the artefacts.
- Verify every agent claim against saved artefacts before accepting.

**Sub-agent responsibilities.**

- Read `docs/instructions/README.md` and this file before starting.
- Operate inside the assigned worktree (if any); otherwise main
  checkout.
- Read-only analysis: no edits to tracked files, no commits (the
  orchestrator cherry-picks from wave artefacts, not sub-agent
  branches).
- Read the provided `expand.rs` and `expand.err`. Never rerun
  `cargo expand`.
- Export the shared `CARGO_TARGET_DIR` before running the profile
  script.
- Run `scripts/profile-bench-headless.sh` with the exact wave.tsv row.
- Verify all seven required artefacts exist after the run. If any is
  missing, report the blocker and stop. Do not proceed on partials.
- Inspect saved artefacts with `grep` / `sed` / `awk`. Do not read
  `profile.json.gz` byte-wise; extract via tools.
- Every claim in the agent's report cites a saved file — bench
  source, provided expand artefact, saved profile/log, or syms-proof.

**Forbidden in sub-agents.** Rerunning `cargo expand`; rerunning
`cargo bench` when a prebuilt `--bin` is provided; ad-hoc profiling
commands outside `scripts/profile-bench-headless.sh`; inference-only
conclusions (every claim grounded in an artefact); writing retained
artefacts outside `.profiles/` in the main repo.

## Samply invocation rules

- Preflight ports before profiling.
- Never write retained profiling artefacts to `/tmp`.
- Use `--unstable-presymbolicate`. Do not use `--save-only`.
- Bench binaries are cwd-sensitive — run from `crates/core`.
- The bencher `--bench <filter>` flag is a substring match. Avoid
  entry names that are prefixes of other entries (`data` matches both
  `data` and `data_xl`). Rename prefixing entries or accept the
  resulting profile contamination — document either choice.

## Orchestrator prompt template

Canonical invocation for a full-matrix profiling wave. Adapt
bench/entry lists as the tranche requires. Performance-claim and
benchmark-set rules live in `README.md`; this template does not
restate them.

```
Analyze tranche {LETTER} with hard evidence only. Read
docs/instructions/README.md and docs/instructions/PROFILING.md
before beginning. Clear all .bbnf-cache directories. Export one
absolute shared CARGO_TARGET_DIR. Reserve ports. Run
scripts/prepare-profile-wave.sh. Verify wave.tsv, binaries.tsv,
and per-bench expand artefacts. Dispatch up to five profiling
sub-agents, each with the exact wave.tsv row for its bench.
Every sub-agent runs scripts/profile-bench-headless.sh against
its prebuilt binary and entry list — no rerun of cargo expand
or cargo bench. Verify every sub-agent claim against saved
artefacts before folding into the tranche document. Refine
the tranche doc with firing / not-firing per dataset, hotspot
union, proposed optimisations cited to artefacts, and
next-tranche seeds. .profiles/ in the main repo is the only
retained artefact root.
```
