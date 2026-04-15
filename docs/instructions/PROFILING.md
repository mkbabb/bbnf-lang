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
