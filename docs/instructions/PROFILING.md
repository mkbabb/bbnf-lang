# Profiling — Workflow, Bench Surface, and Wave Orchestration

Samply is the canonical runtime profiler. Divan is the canonical bench
harness. iai-callgrind gates instruction-count regressions on Linux CI.
All runtime perf claims cite a samply or divan artefact; CI-side claims
cite iai-callgrind output. This document governs the development
workflow end-to-end — host setup, ICE recovery, the routine and proof
command surfaces, the bench alias surface, samply's single-run + wave
orchestration contracts, and codegen inspection.

All directives from `README.md` apply here — file-first expensive
commands, cache clearing, worktree isolation, hardened agent claims, no
trammelling, indefatigability. The rules below layer the host /
profiling-specific contract on top.

## Dev-host setup

The committed `rust-toolchain.toml` pins `nightly-2026-04-11`; rustup
honours it automatically the first time any cargo command runs inside
the workspace. No manual `rustup default` invocation is required —
attempting one risks drift across sibling-repo checkouts.

Required tools per host:

- `cargo install cargo-nextest --locked` — nextest is the canonical
  test runner. Every `iter-test*` cargo alias and every CI workflow
  step runs through it; bare `cargo test` is deliberately absent from
  the alias surface (build-infra-first).
- `brew install lld` on macOS as the opt-in fast linker. Distinct from
  `brew install llvm`, which ships only `lldb`. After install, `ld.lld`
  lives at `/opt/homebrew/opt/lld/bin/ld.lld`; the macOS arm64 target
  block in `.cargo/config.toml` carries the link-arg, commented for
  opt-in (~10–20 % rebuild-wall reduction; uncomment after verifying
  the path).
- `apt-get install valgrind` on Linux CI. The `bench-iai.yml` workflow
  installs valgrind in its setup step; iai-callgrind requires it. macOS
  dev-hosts skip iai-callgrind entirely — the dev-dependency is
  Linux-target-gated in `crates/core/Cargo.toml`.

Sibling repos `../parse-that` and `../pprint` carry the same
`rust-toolchain.toml` pin and a minimal `.cargo/config.toml`; rustup
auto-downloads the pinned nightly on the first `cargo check` against
either checkout.

## ICE recovery

The pinned nightly closes the 93-ICE cluster observed under ambient
`1.96.0-nightly (9602bda1d 2026-04-05)`. Residual ICEs from a stale
incremental cache resolve via:

```bash
make clean-incr
# equivalent: rm -rf target/*/incremental
```

`-Zthreads=8` in `.cargo/config.toml` amplifies the ICE count when one
fires — one poisoned query produces one ICE per parallel worker — so
the recovery sequence is part of the routine dev contract on the
pinned nightly until the on-disk-cache `AttrId` fix lands upstream.

## Routine surface

Routine iteration rides the `ax-iter` profile (inherits dev, strips
debuginfo, retains line-tables for samply backtraces). Every alias
appears once, in `.cargo/config.toml`, and carries `--profile ax-iter`
explicitly (iter-profile-always).

| Alias | Surface | Working set |
|---|---|---|
| `cargo iter-check` | workspace minus 4 heavy-link crates | gorgeous, bbnf-bootstrap, bbnf-analysis, bbnf-lsp excluded |
| `cargo iter-check-lsp` | bbnf-analysis + bbnf-lsp fast-path | covers `iter-check`'s exclude |
| `cargo iter-check-prettify` | gorgeous fast-path | covers `iter-check`'s exclude |
| `cargo iter-check-bootstrap` | bbnf-bootstrap fast-path | covers `iter-check`'s exclude |
| `cargo iter-clippy` | clippy under ax-iter, all-targets, deny warnings | matches `iter-check` shape |
| `cargo iter-test` | nextest workspace | full surface |
| `cargo iter-test-core`, `iter-test-ir`, `iter-test-analysis`, `iter-test-prettify`, `iter-test-lsp` | per-package nextest fast-paths | one per heavy crate |

`cargo iter-check` is the dev-loop default — 3.88 s warm,
~11 s cold under the pinned nightly per
`docs/benchmarks/post-B1-W0-routine.txt`. The four `--exclude`d crates
each have a named fast-path alias so the routine surface remains
truthful per B1 invariant 10.

`cargo iter-check-full` is the workspace close-ceremony gate — full
workspace under `ax-iter`. The pre-B2 cold wall (> 600 s, dominated
by `bbnf-bootstrap`'s single-derive proc-macro expansion of the
17-pass IR pipeline) retires with B2.W2: the IR pipeline runs in
`cargo xtask regen` against on-disk source, and consumer compiles
read the per-grammar emitted files via `include!` rather than
re-running expand. Routine iteration still uses `iter-check`; the
full gate runs at close ceremonies only.

The Makefile exposes `make test` (nextest workspace, default profile),
`make test-ci` (`--profile ci`, retries + junit), and `make test-close`
(`--profile close`, full-suite ceremony with per-test wall capture).

`scripts/test-tier.sh leaf --profile ax-iter` (45.89 s for 582 tests
post-B1.W0) routes the smallest correctness tier — tape, bbnf-ir,
egraph, csp-solver, bbnf-ser — for sub-minute pre-dispatch validation.
A bash-3.2-safe empty-array expansion guard (`"${ARR[@]+"${ARR[@]}"}"`)
applies on macOS hosts.

## Grammar regen

`cargo xtask regen` runs the 17-pass IR pipeline + Rust emission for
every grammar enumerated in `[workspace.metadata.bbnf.grammars]`,
writing per-grammar source to
`crates/core/src/grammar/generated/<ident>.rs`. `--grammar <ident>`
narrows to one grammar; `--check` regenerates to a tempdir + diffs
against the checked-in tree (CI / pre-commit gate).

```bash
cargo xtask regen                       # full sweep
cargo xtask regen --grammar bbnf        # single grammar
cargo xtask regen --check               # CI / pre-commit gate
```

The Makefile mirrors the two common entrypoints as `make regen` and
`make regen-check`. Pre-B2 the regen ran through `cargo expand` +
Python post-process under `scripts/bootstrap-bbnf.sh`; that path
retired with the proc-macro at B2.W2, and the wall fell from 80+ min
cold to seconds.

## Bench alias surface

Benches ride the `ay-final` profile (release inheriting fat LTO + debug
1 for samply-resolvable symbols + packed split-debuginfo). Every alias
is one cargo invocation; divan's per-sample regression check runs
inside that single harness pass (bench-single-run,
bench-sequential-regression). Cold-per-parse only —
`no-warm-benches`. Post-B7 (2026-04-27), divan is the only harness
across bbnf-lang, parse-that, and pprint; legacy bencher / libtest /
unstable-harness paths retired entirely. Cross-repo bench-target
counts at B7 close: bbnf-lang ~30 divan bench files (core, ir,
egraph, csp-solver, simd-scan, json-prototype, tape; B7.W0.A1
ported simd-scan from manual `std::time::Instant` to divan);
parse-that 18 (13 parse_that-crate + 1 bootstrap + 4 regex; all
ported from `bencher = "0.1.5"` at B7.W1.A2 + A3); pprint 38
(26 in `pprint.rs` + 12 in `digit_count.rs`; ported from
`#![feature(test)]` + `extern crate test` at B7.W2.A6 + A7).

| Alias | Coverage | Profile |
|---|---|---|
| `cargo bench-all` | every divan bench in the workspace | `ay-final` |
| `cargo bench-json` | json_monolithic, json_parse_that, json_vm, json_competitors, json_stress, json_value | `ay-final` |
| `cargo bench-css` | css_l4, css_vm, css_competitors, css_stress | `ay-final` |
| `cargo bench-bbnf` | bbnf_monolithic | `ay-final` |
| `cargo bench-sheets` | google_sheets_monolithic, google_sheets_vm | `ay-final` |
| `cargo bench-compile` | compile_pipeline | `ay-final` |
| `cargo bench-iai` | iai-callgrind under valgrind, `iai` feature gated | `bench-ci` (Linux CI only) |

Capture divan's structured JSON for any scope by setting the format
env-var and redirecting:

```bash
DIVAN_BENCH_FORMAT=json cargo bench-json > docs/benchmarks/post-B1-json.json
```

`bench-iai` runs only on the `bench-iai.yml` GitHub Actions workflow —
valgrind is a Linux-only dependency. Local invocations on macOS exit
immediately because the dev-dependency entry is gated under
`[target.'cfg(target_os = "linux")'.dev-dependencies]`.

The Makefile mirrors the cargo aliases as `make bench`, `make
bench-json`, `make bench-css`, `make bench-bbnf`, `make bench-sheets`,
`make bench-compile`. Each delegates to the cargo alias without
re-issuing flags.

## Codegen inspection — cargo expand + cargo asm

Expand and asm artefacts land under `target/expand/` and `target/asm/`
so aggregate expansions do not spill into `/tmp`.

- `cargo expand-bootstrap` (alias) → `expand --profile ax-iter -p
  bbnf-bootstrap --lib`. The Makefile target `make expand-bootstrap`
  redirects to `target/expanded-bootstrap.rs`.
- `cargo expand-derive` (alias) → `expand --profile ax-iter -p bbnf
  --lib`. `make expand-derive` writes `target/expanded-derive.rs`.
- `cargo asm-bbnf` (alias) → `asm --profile ay-final -p bbnf`. `make
  asm` delegates.

Per-bench expand for samply correlation runs through the wave-prepare
pipeline (`scripts/prepare-profile-wave.sh`), which writes
`target/expand/<bench>.rs` once per bench and reuses the artefact
across same-wave samply runs.

## Shared-target discipline

Profiling artefacts live under `.profiles/` in the main repo. All
agents in a profiling wave share **one** absolute `CARGO_TARGET_DIR` —
not per-worktree, not per-agent. Prepare once, then profile many.
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

## Profiling preparation — prepared-binary reuse

`make profile` wraps `scripts/prepare-profile-wave.sh` with the
shared-target contract. Both `scripts/prebuild-benches.sh` and
`scripts/prepare-profile-wave.sh` reuse cached binaries and `cargo
expand` artefacts when fresh vs the bench source, shape emitters under
`crates/core/src/backend/rust/emitter/shapes/`, and
`crates/core/src/grammar/generated.rs`.

Stale detection: editing a bench source, shape emitter, or regenning
`generated.rs` rebuilds that bench's artefacts; unrelated benches stay
cached. Per-bench stdout reads `reused: <path>` / `rebuilt: <path>` for
the binary step and `expand: reused <path>` / `expand: regen <path>`
for expansion, so the orchestrator can see cache hits without
reopening build logs. `target/release/deps/` and
`target/bench/deps/` remain searched as legacy fallbacks.

Manual invalidation: `rm -rf
.profiles/samply/prebuild/{binaries.tsv,wave.tsv,expand}` and
`rm target/profiling-prep/deps/<bench>-*`.

## AY W5-W7 gate commands

`AY.W5-W7` hard gates resolve through public `make ay-*` entrypoints
in the Makefile's `AY W5-W7 Gate Commands` section. AY executors call
them verbatim; they do not re-derive the underlying `cargo expand`,
`cargo asm`, `cargo nextest run`, `cargo bench`, or
`scripts/profile-bench-headless.sh` invocations. Samply targets require
`CARGO_TARGET_DIR` exported and a prepared profile wave. Prebuilt
binaries land under `$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*`
via `make ay-prepare-profile-wave`; samply targets reuse them without
rebuilding.

| AY hard gate | Makefile target | Artefact |
|---|---|---|
| W5.1 / W7.2 expand JSON | `make ay-expand-json` | `target/expand/ay-json.rs` |
| W6.2 expand named-type | `make ay-expand-named-type` | `target/expand/ay-named-type.rs` |
| W5.3 close-stamp asm | `make ay-asm-close-compound FN=<sym>` | `target/asm/ay-close-<sym>.s` |
| W5.1 value API test | `make ay-test-value-api` | nextest exit status |
| W7.1 wire-contract test | `make ay-test-wire-contract` | nextest exit status |
| W6.1 named-type test | `make ay-test-named-type` | nextest exit status |
| W5.2 samply eager JSON twitter | `make ay-samply-json-twitter WAVE=W5` | `.profiles/samply/AY-W5/json_twitter_eager/` |
| W6.3 samply JSON path lookup | `make ay-samply-json-twitter-lookup WAVE=W6` | `.profiles/samply/AY-W6/json_twitter_lookup/` |
| W5.5 / W6.4 / W7.4 close-matrix bench | `make ay-bench-close WAVE=<label>` | `docs/benchmarks/post-AY-<label>-{json,css,sheets,bbnf,compile}.txt` → aggregate `post-AY-<label>-mid.json` |
| Samply prerequisite | `make ay-prepare-profile-wave` | `$(CARGO_TARGET_DIR)/profiling-prep/deps/<bench>-*` |

Samply ports 3130/3131 route to `ay-samply-json-twitter`; 3132/3133
route to `ay-samply-json-twitter-lookup` so the two gates can run
sequentially against the same prebuilt binary without port collision.
`ay-bench-close` selects `--profile profiling-prep` for mid-wave runs
and `--profile ay-final` (fat LTO) when `WAVE=close` — use the latter
for publish-grade close-matrix numbers.

## Prepare a wave

`scripts/prepare-profile-wave.sh` builds every bench binary once,
produces `cargo expand` artefacts per bench, reserves ports, enumerates
every `(bench, entry)` pair, and writes a `wave.tsv` contract that
sub-agents consume verbatim.

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

When a profiling task covers more than a single entry, the orchestrator
runs one agent per bench and delegates the entry matrix to that agent.
Five agents cover the full bench suite (json_monolithic, css_l4,
google_sheets_monolithic, bbnf_monolithic, json_value).

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
  worktree path (optional), bench, entry list, record port, load port,
  artefact dir, binary path, expand.rs path, expand.err path, bench
  cwd, shared target dir.
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
- Read the provided `expand.rs` and `expand.err`. Never rerun `cargo
  expand`.
- Export the shared `CARGO_TARGET_DIR` before running the profile
  script.
- Run `scripts/profile-bench-headless.sh` with the exact wave.tsv row.
- Verify all seven required artefacts exist after the run. If any is
  missing, report the blocker and stop. Do not proceed on partials.
- Inspect saved artefacts with `grep` / `sed` / `awk`. Do not read
  `profile.json.gz` byte-wise; extract via tools.
- Every claim in the agent's report cites a saved file — bench source,
  provided expand artefact, saved profile/log, or syms-proof.

**Forbidden in sub-agents.** Rerunning `cargo expand`; rerunning `cargo
bench` when a prebuilt `--bin` is provided; ad-hoc profiling commands
outside `scripts/profile-bench-headless.sh`; inference-only conclusions
(every claim grounded in an artefact); writing retained artefacts
outside `.profiles/` in the main repo.

## Samply invocation rules

- Preflight ports before profiling.
- Never write retained profiling artefacts to `/tmp`.
- Use `--unstable-presymbolicate`. Do not use `--save-only`.
- Bench binaries are cwd-sensitive — run from `crates/core`.
- Divan's `--bench <filter>` is a substring match. Avoid entry names
  that are prefixes of other entries (`data` matches both `data` and
  `data_xl`). Rename prefixing entries or accept the resulting profile
  contamination — document either choice.

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
