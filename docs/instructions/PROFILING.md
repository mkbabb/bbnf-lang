# Profiling — Samply Workflow and Wave Orchestration

Samply is the canonical profiler. All runtime perf claims cite
samply artefacts. This document governs the profiling workflow,
from a single headless profile run to a full multi-agent wave.

All directives from `README.md` apply here — file-first expensive
commands, cache clearing, worktree isolation, hardened agent
claims, no trammelling, indefatigability. The rules below layer
the samply-specific contract on top.

## Shared-target discipline

For bench, profiling, and `cargo expand` analysis:

- Profiling artefacts live under `.profiles/` in the main repo.
- All agents in a profiling wave share **one** absolute
  `CARGO_TARGET_DIR` — not per-worktree, not per-agent.
- Prepare once, then profile many.
- Worktrees are **optional** for profiling (they isolate git, not
  builds). Sibling worktrees are permitted for parallel
  source-reading; never `/tmp` or `/private/tmp`.
- Clear all `.bbnf-cache` directories before any bench, regen, or
  proc-macro expansion:

  ```bash
  find . -name .bbnf-cache -exec rm -rf {} + 2>/dev/null
  ```

Set the shared target first:

```bash
export CARGO_TARGET_DIR=/absolute/path/to/shared/profile-target
```

## Public fast-path commands

Routine iteration rides the `ax-iter` profile via public cargo aliases
and Makefile targets; profiling stays on `dev` for DWARF symbols, and
final-proof work uses `bench`. Three surfaces, three purposes — do not
cross-wire them. See `docs/benchmarks/post-B0-W0-commands.txt` for the
full alias/target manifest.

### Routine — ax-iter profile

The `ax-iter` profile inherits `dev` and strips debuginfo; link time
roughly halves and peak RSS on aggregate test binaries drops ~3×. The
public entrypoints:

- `cargo iter-check` → `cargo check --profile ax-iter --workspace`.
- `make iter-check` — routine compile-gate; same command, Makefile
  wrapper.
- `make iter-test-leaf` → `scripts/test-tier.sh leaf --profile ax-iter`
  (pure-data + substrate crates; fastest correctness tier).
- `make iter-test-grammar` → `scripts/test-tier.sh grammar --profile
  ax-iter` (per-grammar tape-parity + shape emit).
- `make iter-test-ws` → `scripts/test-tier.sh workspace --profile
  ax-iter` (full workspace; use only when wider blast radius is
  actually relevant — this is still a heavy surface).

### Codegen inspection — cargo expand + cargo asm

Expand and asm artefacts land under `target/expand/` and `target/asm/`
respectively so the aggregate expansions do not spill into `/tmp`.
Every AY.W5-W7 expand/asm gate resolves through these entrypoints:

- `make expand-json`, `make expand-css`, `make expand-bbnf`,
  `make expand-sheets` — write `target/expand/<bench>.rs`. AY.W5 hard
  gate 1 cites `cargo expand -p bbnf --bench json_monolithic`;
  `make expand-json` is the public name for that call.
- `make asm-parse BENCH=<name> FN=<symbol>` — writes
  `target/asm/<bench>-<fn>.s`. AY.W5 hard gate 3 (close-stamp asm
  inspection) resolves through this target.

### Bench surfaces

Heavy; never in iteration loops. Benches are cold-per-parse by
contract (`README.md` §Benchmarking) and carry their own sequencing
rules:

- `make bench-compile BENCH=<name>` — compile-gate the bench binary
  without running it. Appropriate as a preflight before a wave's
  samply prepare, not as an iteration command.
- `make bench-run BENCH=<name>` — runs the bench. Gate artefact only;
  clear `.bbnf-cache` first.

### Profiling preparation

`make profile-wave` wraps `scripts/prepare-profile-wave.sh` with the
absolute `CARGO_TARGET_DIR` contract from §Shared-target discipline.
B0.W1 adds prepared-binary reuse so follow-on samply runs skip the
rebuild; until then, prepare once per wave and consume the artefacts
verbatim per §Prepare a wave.

### Prepared binary reuse

- `scripts/prebuild-benches.sh` now reuses cached binaries under
  `target/profiling-prep/deps/` when fresh vs the bench source.
- `scripts/prepare-profile-wave.sh` now reuses cached `cargo expand`
  artefacts when fresh vs the bench source, every shape emitter under
  `crates/core/src/backend/rust/emitter/shapes/`, and the regenerated
  `crates/core/src/grammar/generated.rs`.
- Stale detection: if you edit a bench source, shape emitter, or regen
  `generated.rs`, the next prepare rebuilds that bench's artefacts;
  unrelated benches stay cached.
- Per-bench stdout reads `reused: <path>` / `rebuilt: <path>` for the
  binary step and `expand: reused <path>` / `expand: regen <path>` for
  the expansion step, so the orchestrator can see which artefacts were
  cached without reopening build logs.
- `target/release/deps/` and `target/bench/deps/` remain searched as
  legacy fallbacks until older wave artefacts age out.
- Invalidate manually if needed: `rm -rf .profiles/samply/prebuild/{binaries.tsv,wave.tsv,expand}`
  and `rm target/profiling-prep/deps/<bench>-*`.

### W0 timing proof

- Baseline (pre-W0, HEAD `9bff7e7d`): `docs/benchmarks/post-B0-W0-baseline.txt`.
- Post-W0 (HEAD `b8dac71e`): `docs/benchmarks/post-B0-W0-mid.json`.
- Headline: `cargo iter-check` warm (0.16s) is ~45× faster than baseline
  `cargo check --workspace` warm (7.16s); the ~30× gap the baseline
  flagged is now the public routine path.
- `make iter-test-leaf` warm 1.05s produces real pass/fail output;
  baseline `scripts/test-tier.sh leaf` failed in 0.45s against the
  stale `-p bbnf-tape` crate name (W0.a renamed to `-p tape`).
- Routine iteration routes via the `iter-*` targets; keep
  `cargo test --workspace` and `cargo bench` for wave-close gates only.

### AY W5-W7 gate commands

`AY.W5-W7` hard gates resolve through public `make ay-*` entrypoints in
the Makefile's `AY W5-W7 Gate Commands` section. These are stable
surfaces — AY executors call them verbatim; they do not re-derive the
underlying `cargo expand`, `cargo asm`, `cargo test`, `cargo bench`, or
`scripts/profile-bench-headless.sh` invocations from the wave spec.
Samply targets require `CARGO_TARGET_DIR` exported and a prepared
profile wave; bench targets are cold-per-parse sequential by contract
(`README.md` §Benchmarking). Prebuilt bench binaries land under
`$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*` via
`make ay-prepare-profile-wave`, and the samply targets reuse them
without rebuilding — see §Prepared binary reuse.

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
| Samply prerequisite | `make ay-prepare-profile-wave` | `$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*` (reused by all samply gates) |

Samply ports 3130/3131 route to `ay-samply-json-twitter`; 3132/3133
route to `ay-samply-json-twitter-lookup` so the two gates can run
sequentially against the same prebuilt binary without port collision.
`ay-bench-close` selects `--profile profiling-prep` for mid-wave runs
and `--profile bench` (fat LTO) when `WAVE=close` — use the latter for
publish-grade close-matrix numbers.

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

- `.profiles/samply/prebuild/wave.tsv` — one row per
  `(bench, entry)` pair with ports, artefact dir, binary path,
  expand artefact path, bench cwd, target dir.
- `.profiles/samply/prebuild/binaries.tsv` — one prebuilt release
  binary per bench.
- `.profiles/samply/prebuild/expand/<bench>/expand.rs` — cargo
  expand output, read-only after prepare.
- `.profiles/samply/prebuild/expand/<bench>/expand.err` — cargo
  expand stderr.
- Per-bench `*-build.txt`.

**Do not rerun `cargo expand` or `cargo bench` inside a wave
after prepare has finished.** Re-runs waste cycles and produce
divergent artefacts. Sub-agents consume the prepared artefacts;
they do not regenerate them.

## Profile a single entry

`scripts/profile-bench-headless.sh` runs the prebuilt bench binary
under `samply record`, produces every required artefact, and
verifies named-frame coverage via `syms-proof.txt`.

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

Required artefacts per entry (every one must exist and be
non-empty):

- `bench.txt`
- `build.txt`
- `record.txt`
- `load.txt`
- `profile.json.gz`
- `profile.json.syms.json`
- `syms-proof.txt`

## Orchestration contract — multi-agent waves

When a profiling task covers more than a single entry, the
orchestrator runs one agent per bench and delegates the entry
matrix to that agent. Five agents cover the full bench suite
(json_monolithic, css_l4, google_sheets_monolithic,
bbnf_monolithic, json_value).

**Orchestrator responsibilities.**

- Environment parity: confirm `.cargo/config.toml` path patching,
  sibling patched repos resolve, `cargo expand` installed,
  `samply` installed.
- Export the absolute shared target dir.
- Clear caches.
- Run `scripts/prepare-profile-wave.sh`.
- Verify `wave.tsv`, `binaries.tsv`, and the expand artefacts.
- Reserve ports before spawning agents — one port pair per bench,
  reused sequentially by that bench's sub-agent across its
  entries.
- `.profiles/` in the main repo is the only retained artefact
  root. Sub-agents writing elsewhere violate the contract.
- Dispatch sub-agents with their exact `wave.tsv` row as input:
  worktree path (optional), bench, entry list, record port, load
  port, artefact dir, binary path, expand.rs path, expand.err
  path, bench cwd, shared target dir.
- Do not ask sub-agents to rerun `cargo expand` or `cargo bench`
  if prepare already produced the artefacts.
- Verify every agent claim against saved artefacts before
  accepting it. Untrusted claims are discarded, not integrated.

**Sub-agent responsibilities.**

- Read `docs/instructions/README.md` and this file before starting.
- Operate inside the assigned worktree (if any); otherwise the
  main checkout.
- Read-only analysis: no edits to tracked files, no commits (the
  orchestrator cherry-picks from the wave's artefacts, not from
  sub-agent branches).
- Read the provided `expand.rs` and `expand.err`. Never rerun
  `cargo expand`.
- Export the shared `CARGO_TARGET_DIR` before running the
  profile script.
- Run `scripts/profile-bench-headless.sh` with the exact wave.tsv
  row contents.
- Verify all seven required artefacts exist after the run. If any
  is missing, report the blocker and stop. Do not proceed on
  partial artefacts.
- Inspect saved artefacts with `grep` / `sed` / `awk`. Do not
  read `profile.json.gz` byte-wise; extract via tools.
- Every claim in the agent's report cites a saved file: bench
  source, provided expand artefact, saved profile/log, or
  syms-proof.

**Forbidden in sub-agents.**

- Rerunning `cargo expand`.
- Rerunning `cargo bench` when a prebuilt `--bin` is provided.
- Ad-hoc profiling commands outside
  `scripts/profile-bench-headless.sh`.
- Inference-only conclusions — every claim grounded in an
  artefact.
- Writing retained artefacts outside `.profiles/` in the main
  repo.

## Samply invocation rules

- Preflight ports before profiling.
- Never write retained profiling artefacts to `/tmp`.
- Use `--unstable-presymbolicate`. Do not use `--save-only`.
- Bench binaries are cwd-sensitive — run from `crates/core`.
- The bencher `--bench <filter>` flag is a substring match. Avoid
  entry names that are prefixes of other entries (`data` matches
  both `data` and `data_xl`). Rename prefixing entries or accept
  the resulting profile contamination — document either choice.

## Orchestrator prompt template

The canonical invocation for a full-matrix profiling wave. Adapt
bench/entry lists as the tranche requires. Performance-claims
and benchmark-set rules live in `README.md`; this template does
not restate them.

```
Analyze tranche {LETTER} with hard evidence only. Read
docs/instructions/README.md and docs/instructions/PROFILING.md
before beginning. Clear all .bbnf-cache directories. Export
one absolute shared CARGO_TARGET_DIR. Reserve ports. Run
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
