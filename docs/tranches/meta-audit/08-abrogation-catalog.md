# Ad-hoc Abrogation Catalog

Every script, Makefile target, CI workflow, and inline hack in the bbnf-lang
workspace classified under one of five verdicts: **KEEP**, **KEEP-MODERNIZE**,
**REPLACE**, **ABROGATE**, **FOLD-INTO-TOOLING**.

The principle is the user's direction: *eschew ad hoc scripts for benching,
testing, profiling, and tool chaining — unless genuinely valuable and not
subsumed by a more modern and idiomatic approach*. Every REPLACE candidate
names a concrete upstream tool and version. Every ABROGATE candidate names
the follow-on cleanup.

Scope:

- `scripts/` — 19 entries (18 shell, 1 Python)
- `Makefile` — 37 targets across four surface groups
- `.github/workflows/` — 2 workflows (ci, release)
- `.cargo/config.toml` — 1 patch table, 7 aliases
- `.config/nextest.toml` — 1 profile set (already modern)
- `.vscode/` — tasks.json, launch.json
- Inline hacks across `crates/`

Verdict counts (anticipatory; derived in the summary section):

| Verdict              | Scripts | Makefile | CI | Configs | Inline | Total |
|----------------------|--------:|---------:|---:|--------:|-------:|------:|
| KEEP                 |       4 |        9 |  1 |       3 |      0 |    17 |
| KEEP-MODERNIZE       |       4 |        5 |  1 |       0 |      0 |    10 |
| REPLACE              |       4 |        2 |  0 |       0 |      1 |     7 |
| ABROGATE             |       5 |       13 |  0 |       0 |      1 |    19 |
| FOLD-INTO-TOOLING    |       2 |        8 |  0 |       0 |      0 |    10 |
| **Total**            |  **19** |   **37** | **2** |    **3** |    **2** | **63** |

---

## Part 1 — `scripts/` catalog

Line-count total for `scripts/`: 2,696 (18 shell + 1 Python). Expected LOC
delta after executing this catalog: **−1,480 lines removed, +60 added** (net
−1,420).

### `scripts/bench_regression.sh`

- **Purpose**: runs five key benches, parses `cargo bench` text output with
  regex, and compares `ns/iter` against a JSON baseline with a ±THRESHOLD%
  gate.
- **Lines**: 89 (actually a Python script, `#!/usr/bin/env python3`, despite
  the `.sh` extension — a smell in itself).
- **Last modified**: 2026-03-30 (tranche AO profiling harness).
- **Verdict**: REPLACE.
- **Rationale**: text-scraping `cargo bench` is brittle; `divan` emits
  structured JSON baselines via `--save-baseline` / `--baseline` natively
  without regex. Filename extension lies about the language.
- **Replacement**: **divan ≥0.1.17** (`https://github.com/nvzqz/divan`). Wire
  divan baselines into `.config/divan/` directory with per-bench JSON. The
  threshold gate becomes a 20-line Rust bin or a `cargo xtask` subcommand.
- **Migration cost**: M — requires porting the five benchmark targets from
  criterion (if still used) to divan; already in motion per Tranche AO
  bench-results memory. Once divan, this script collapses to the alias
  `cargo bench -- --save-baseline current && cargo bench -- --baseline main`.
- **Blocks**: divan migration of `crates/core/benches/` must land first.

### `scripts/bisect-fastpath.sh`

- **Purpose**: wraps `git bisect run` with per-commit log capture to
  `/tmp/bisect-<sha>.log`, pre-flight checks for dirty tree + existing bisect.
- **Lines**: 162.
- **Last modified**: 2026-04-11 (tranche AG.0 build/test infra).
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: `git bisect run` alone does not persist per-step output —
  the wrapper's log-capture value is real. However, 162 lines to do this is
  heavy; the core is a 20-line runner stub.
- **Modernization**: shrink to a 30-line wrapper. Delete the argparse-style
  `--help` (git-style CLIs use man pages or `-h`), delete the pre-flight
  redundancies that `git bisect` itself enforces (dirty-tree detection is
  already in `git bisect start`), keep only the per-commit log-file
  invariant.
- **Migration cost**: S — pure rewrite, no workflow change.

### `scripts/bootstrap-bbnf.sh`

- **Purpose**: expand proc-macro output from `crates/bootstrap/` into
  `crates/core/src/grammar/generated.rs`; post-process to strip unstable
  features and collapse auto-derived impls into stable `#[derive]`s.
- **Lines**: 338.
- **Last modified**: 2026-04-10 (tranche AC.2 tape-first integration).
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: this is the self-hosting grammar regeneration step —
  load-bearing, non-optional, no upstream subsumes it (cargo expand alone
  does not do the post-processing for stable derives). But 338 lines of
  sed-style stream mangling is the wrong tool; the post-processor belongs
  in a Rust bin that manipulates a `syn::File` AST, not string regex.
- **Modernization**: refactor the post-processor into
  `crates/bootstrap/src/bin/post_expand.rs` using `syn` + `quote`. Shell
  wrapper drops to ~40 lines (invoke cargo-expand, pipe into the bin, write
  the output).
- **Migration cost**: M — the sed substitutions have ~10 specific cases that
  map 1:1 to `syn` visitors.
- **Blocks**: `clean-regen-discipline` memory requires the bootstrap cache
  flush (`rm -rf target/.bbnf-cache/`) to remain in the wrapper. Preserve.

### `scripts/check-bootstrap-clean.sh`

- **Purpose**: CI gate that fails when committed `generated.rs` drifts from
  a fresh bootstrap — copies committed file, re-runs bootstrap, diffs.
- **Lines**: 43.
- **Last modified**: 2026-04-16 (tranche AW.0.7).
- **Verdict**: KEEP.
- **Rationale**: enforces `clean-regen-discipline` memory (generated files
  are always output of fresh regen). No modern tool subsumes this — it is
  the structural CI check corresponding to `bootstrap-bbnf.sh`'s output
  contract. 43 lines is reasonable.

### `scripts/check-cst-invariants.sh`

- **Purpose**: greps the tree for four wave-specific anti-patterns
  (`extract_span_text` helpers, `substitute_and_lower`, `ir_visitor.rs`
  presence, `host::extract_*` callers).
- **Lines**: 85.
- **Last modified**: 2026-04-08 (Phase E, tranche AE-era).
- **Verdict**: ABROGATE.
- **Rationale**: all four invariants are from a closed tranche (CstSchema
  refactor Phases A–E). The greps now only match nothing — they consume CI
  budget proving an already-proven negative. This is wave-specific cruft,
  the exact abrogation candidate the user names.
- **Migration cost**: S — delete the file + remove the CI step.
- **Blocks**: none.

### `scripts/cost-grid-sweep.sh`

- **Purpose**: parametric sweep of `BBNF_COST_*` env vars over a grid,
  runs benches per-configuration, aggregates to
  `docs/benchmarks/cost-weights-sweep.json`.
- **Lines**: 390.
- **Last modified**: 2026-04-17 (AW-IV.W5.3).
- **Verdict**: REPLACE.
- **Rationale**: 390 lines of shell to sweep a numeric grid is an ad-hoc
  re-implementation of what divan's `#[divan::bench(args = ...)]` parametric
  benchmarks do natively, with structured output and baseline persistence.
  The `median-of-3` randomised pass is also a divan feature (`sample_count`
  + `sample_size`).
- **Replacement**: **divan parametric benches** +
  `crates/egraph/benches/cost_grid.rs`. The sweep becomes a single
  `#[divan::bench(args = COST_GRID)]` function reading `BBNF_COST_*` from
  the `args` tuple. JSON output via divan's `--output-format json`.
- **Migration cost**: M — port grid definition from shell to a Rust
  `const COST_GRID: &[(...)] = &[...]` slice; rewrite the pipeline wall-clock
  capture as a divan bench body.
- **Blocks**: divan migration (same as `bench_regression.sh`).

### `scripts/deploy.sh`

- **Purpose**: builds WASM, builds playground, rsyncs
  `playground/dist/` to `mbabb@mbabb.fridayinstitute.net:/var/www/grammar`.
- **Lines**: 22.
- **Last modified**: 2026-03-16.
- **Verdict**: KEEP.
- **Rationale**: deploy target, not dev iteration. rsync over ssh is the
  idiomatic approach; the script is thin, no abstraction is warranted. The
  Makefile `deploy` target already calls it — the pair is correct.

### `scripts/extract_hotspots.py`

- **Purpose**: parse samply JSON output (Firefox Profiler schema),
  aggregate self-time by user-code markers, print top-N table. Also
  supports `--compare before.json after.json`.
- **Lines**: 302.
- **Last modified**: 2026-03-30 (tranche AO profiling harness).
- **Verdict**: REPLACE.
- **Rationale**: Python-in-a-Rust-repo is a smell; more importantly, this
  re-implements what samply itself + Firefox Profiler's UI display
  natively, and what `jq` can extract as a one-liner for CLI use. The
  `--compare` mode is subsumed by Firefox Profiler's diff feature
  (`profiler.firefox.com/?profileUrl=...&comparison=...`).
- **Replacement**: a 15-line `jq` snippet committed under `scripts/` (or
  promoted to a `docs/instructions/PROFILING.md` recipe) that extracts
  top-N self-time from samply JSON:
  ```
  gunzip -c profile.json | jq -r '.threads[0] |
    ([.frameTable.func, .funcTable.name] as [$ft, $nt] |
     .stackTable as $st | ...)'
  ```
  plus the Firefox Profiler `&comparison=` URL recipe for diffs.
- **Migration cost**: M — the jq one-liner is non-trivial; a thin Rust bin
  under `crates/core/src/bin/hotspots.rs` may be cleaner and still deletes
  the Python. Per `build-infra-first`, land the bin before removing
  `extract_hotspots.py`.
- **Blocks**: none — both paths can coexist during migration.

### `scripts/kill-all-rust.sh`

- **Purpose**: SIGTERM→SIGKILL orphan rustc/cargo/rust-analyzer/samply
  processes left over from crashed agents.
- **Lines**: 86.
- **Last modified**: 2026-04-19 (tranche AX R3 infra).
- **Verdict**: KEEP.
- **Rationale**: real pain point in the multi-agent worktree workflow
  (20+ worktrees seen in `/Users/mkbabb/Programming/`). No modern tool
  subsumes this; nextest's `leak-timeout` catches test-time leaks only,
  not orchestrator-crash residue. The `--dry-run` + caller-PID-tree
  exemption are correct instincts.

### `scripts/prebuild-benches.sh`

- **Purpose**: build the five monolithic bench binaries under the
  `profiling-prep` profile, stash them under
  `.profiles/samply/prebuild/<bench>/` for reuse by samply captures.
  Tracks bench-source mtimes for idempotency.
- **Lines**: 135.
- **Last modified**: 2026-04-20 (B0.W1.b).
- **Verdict**: KEEP.
- **Rationale**: the `build once / samply many` invariant is genuine
  value — samply runs invalidate cargo's incremental cache if the binary
  rebuilds between captures. No upstream tool provides this. 135 lines is
  reasonable for a content-keyed build cache.

### `scripts/prepare-profile-wave.sh`

- **Purpose**: orchestrates `prebuild-benches.sh` + `cargo expand` + `nm`
  inspection + port assignment for a parallel samply-wave; writes a TSV
  manifest (`.profiles/samply/prebuild/wave.tsv`) consumed by agent
  dispatch.
- **Lines**: 180.
- **Last modified**: 2026-04-20 (B0.W1.b).
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: the samply-wave orchestration is user-specific and has no
  upstream. However, the port-pair table is inline constants; moving it to
  `docs/instructions/PROFILING.md` §ports + a TOML config under
  `.config/profile-wave.toml` read by the script makes the add-a-bench
  workflow cleaner.
- **Modernization**: extract port map + bench list to TOML; shell reads via
  `yq` (already in common dev envs) or a small Rust bin replaces the shell
  outright.

### `scripts/profile-bench-headless.sh`

- **Purpose**: runs samply against a prebuilt bench binary in headless
  mode with configurable record/load ports + artifact directory; consumed
  by AY hard-gate `ay-samply-*` make targets.
- **Lines**: 229.
- **Last modified**: 2026-04-20.
- **Verdict**: KEEP.
- **Rationale**: **explicitly user-endorsed**: *"our profiling script for
  samply is likely good"*. samply-symbol-resolution memory confirms
  interactive `samply record` is the canonical capture mode; this script
  wraps that interaction for CI-accessible port-pair discipline. No
  upstream provides the port-pair headless convention.

### `scripts/profile.sh`

- **Purpose**: single-bench samply capture convenience wrapper — build bench
  with debug info, run `samply record`, optionally open in Firefox
  Profiler.
- **Lines**: 40.
- **Last modified**: 2026-03-30 (tranche AO).
- **Verdict**: ABROGATE.
- **Rationale**: *superseded* by `profile-bench-headless.sh` +
  `prepare-profile-wave.sh`, which handle the same task with proper
  binary-reuse discipline. `profile.sh` rebuilds every invocation — the
  exact pathology the prebuild-benches pipeline eliminated.
- **Migration cost**: S — delete file; no callers remain outside the
  original tranche AO profiling docs (migrate the one README mention to
  `profile-bench-headless.sh`).
- **Blocks**: none.

### `scripts/seed-worktree.sh`

- **Purpose**: symlink `target/` + `data/` into a freshly-created git
  worktree so agents inherit shared cache + large corpora without
  duplicating 13 GB + corpus storage.
- **Lines**: 69.
- **Last modified**: 2026-04-19.
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: the symlink-shared-target convention is load-bearing for
  multi-agent parallelism (confirmed by 20+ sibling worktrees on disk).
  `git worktree add` alone does not seed gitignored resources. However, the
  one-shot `cargo metadata` warm could fold into `.cargo/config.toml`
  `[env]` or a `rustc-wrapper` sccache warm.
- **Modernization**: keep the script, but extract the symlink targets
  (`target/`, `data/`) to a comment in `CONTRIBUTING.md` or
  `docs/instructions/WORKTREES.md` and drop the `--no-target` flag (either
  the symlink is correct or it isn't — user-configurable variants breed
  drift).

### `scripts/sync-external-docs.sh`

- **Purpose**: rsync sibling-repo playground docs
  (`parse-that/docs/playground/`, `pprint/docs/playground/`,
  `gorgeous/docs/playground/`) into this repo's `docs/<sibling>/`.
- **Lines**: 30.
- **Last modified**: 2026-04-20.
- **Verdict**: KEEP.
- **Rationale**: the "synced docs committed to git — builds need no sibling
  repos" discipline is correct for worktree isolation. No upstream
  subsumes cross-repo doc sync (git-submodules would work but are a
  heavier tax than a 30-line rsync).

### `scripts/test-tier.sh`

- **Purpose**: routes `leaf` / `grammar` / `workspace` tiers to the right
  `cargo test` invocation, writes `/tmp/test-tier-<tier>.txt` for the
  orchestrator's file-first output discipline.
- **Lines**: 85.
- **Last modified**: 2026-04-20 (B0.W2.a).
- **Verdict**: FOLD-INTO-TOOLING.
- **Rationale**: three named test tiers is exactly what `nextest`'s
  `[profile.<name>]` sections are for. The tier definitions (which crates
  + which tests) belong in `.config/nextest.toml` as `default-filter` +
  crate scope, not a shell wrapper.
- **Migration target**: `.config/nextest.toml` adds:
  ```
  [profile.leaf]
  default-filter = 'package(tape) | package(bbnf-ir) | package(egraph) | package(csp-solver) | package(bbnf-ser)'

  [profile.grammar]
  default-filter = 'test(/_parity$/) | test(/tape_parity_/) | test(grammar_roundtrip)'

  [profile.workspace]
  # default everything
  ```
  Then `make iter-test-leaf` becomes `cargo nextest run --profile leaf`.
  The `/tmp/test-tier-<tier>.txt` redirect folds into the caller (shell
  redirect at the Makefile target).
- **Migration cost**: S — 40-line nextest.toml edit, delete 85-line shell.

### `scripts/verify-w2-asm.sh`

- **Purpose**: disassembles bench binaries, asserts no `bl`/`call`
  instructions target hot-path helpers — AW-IV.W2.2 close-gate evidence.
- **Lines**: 161.
- **Last modified**: 2026-04-17.
- **Verdict**: ABROGATE.
- **Rationale**: wave-specific (AW-IV.W2 tranche closed). `cargo-show-asm`
  (`cargo install cargo-show-asm`) subsumes the disassembly step; the
  hot-path helper assertion should become a per-wave test in
  `crates/core/tests/` with `#[test]` gating, not a permanent shell script.
  When the next tranche wants similar proof, author a fresh per-wave test,
  per the `new-tranche-new-doc` discipline.
- **Migration cost**: S — delete + drop any CI reference. The tranche it
  served is already closed.

### `scripts/verify-w2-symbols.sh`

- **Purpose**: `nm`-scans bench binaries for hot-path helper symbol
  presence/absence — AW-IV.W2.2 close gate.
- **Lines**: 178.
- **Last modified**: 2026-04-17.
- **Verdict**: ABROGATE.
- **Rationale**: same as `verify-w2-asm.sh`. Wave-specific; `cargo-show-asm`
  + per-wave `#[test]` subsume it.

### `scripts/worktree-status.sh`

- **Purpose**: enumerate sibling worktrees, report HEAD + dirty + target
  symlink status as human table or TSV.
- **Lines**: 72.
- **Last modified**: 2026-04-19.
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: `git worktree list --porcelain` alone doesn't report
  dirty state or symlink health; the wrapper's value is real. But 72 lines
  is heavy; the core is 25 lines.
- **Modernization**: shrink to a minimal `git worktree list --porcelain |
  while read ...` loop. Drop the `--tsv` mode (TSV and table formats both
  exist because no caller needs both — pick one, or delegate via `column
  -t`).

---

## Part 2 — `Makefile` target catalog

Makefile total: 470 lines, 37 targets. Four surface groups documented in
the header comment:

- **Build** (9 targets): `all`, `build`, `build-lsp`, `build-lsp-debug`,
  `build-ext`, `build-wasm`, `dev`, `install`, `package`.
- **Routine iteration** (4 targets + 4 expand): `iter-check`,
  `iter-test-leaf`, `iter-test-grammar`, `iter-test-ws`, `expand-{json,css,bbnf,sheets}`.
- **Heavy** (7 targets): `test-close`, `test-heavy-rust`, `test-ci`,
  `bench`, `asm-parse`, `bench-compile`, `bench-run`, `profile-wave`.
- **AY W5-W7 gates** (8 targets): `ay-expand-json`, `ay-expand-named-type`,
  `ay-asm-close-compound`, `ay-test-value-api`, `ay-test-wire-contract`,
  `ay-test-named-type`, `ay-samply-json-twitter`,
  `ay-samply-json-twitter-lookup`, `ay-bench-close`,
  `ay-prepare-profile-wave`.
- **Release** (4): `bump-patch`, `bump-minor`, `bump-major`, `release`.
- **Clean / deploy / watch** (4): `clean`, `clean-vsix`, `deploy`, `watch`.

### Per-group verdicts

**Build group** — all KEEP. These are the canonical build surfaces; they
mirror `cargo build` / `wasm-pack` / `npm run build` with repo-specific
conventions (copy binary to `server/`). No modernization candidate.

**Routine iteration group** — KEEP-MODERNIZE as a whole.

- `iter-check`, `iter-test-leaf`, `iter-test-grammar`, `iter-test-ws`:
  shell wrappers over `cargo iter-*` aliases. The aliases themselves
  (in `.cargo/config.toml`) are correct. The Makefile wrappers are
  thin passthroughs — arguable whether they add value over naming the
  alias directly. **FOLD-INTO-TOOLING**: delete the Makefile wrappers;
  callers learn `cargo iter-check` directly (one more character than
  `make iter-check`). Keep only if the orchestrator's dispatch surface
  genuinely couples to `make` rather than `cargo`.
- `expand-{json,css,bbnf,sheets}`: also thin aliases over
  `cargo expand-<grammar>`. Same verdict — **FOLD-INTO-TOOLING**.

**Heavy group** — mixed.

- `test-close`, `test-heavy-rust`, `test-ci`: **KEEP**. Real logic
  (nextest vs timeout fallback) that cannot live in an alias.
- `bench`: **ABROGATE**. It runs `cargo test -p bbnf-lsp --test bench_lsp`
  (which is a test, not a bench — mis-named). A real `cargo bench` exists
  on the AY gate path.
- `asm-parse`, `bench-compile`, `bench-run`: **KEEP**. Parametric targets
  (`BENCH=` / `FN=` variables) that `make` models naturally; `cargo
  asm` / `cargo bench` invocations are one-liners but the Makefile adds
  the `target/asm/` directory discipline.
- `profile-wave`: **KEEP**. Thin wrapper over
  `scripts/prepare-profile-wave.sh` with the `CARGO_TARGET_DIR` precondition
  check.

**AY W5-W7 gate group** — mixed.

- `ay-expand-json`, `ay-expand-named-type`, `ay-asm-close-compound`:
  **KEEP** for now (wave is live). After tranche AY closes: **ABROGATE**.
  These are the exact wave-specific entries the user flagged as cruft
  ("wave-specific scripts abrogate after tranche close" per the
  orchestrator instruction). Per the `new-tranche-new-doc` discipline,
  the next tranche opens new gate targets.
- `ay-test-value-api`, `ay-test-wire-contract`, `ay-test-named-type`:
  **KEEP** during AY; **ABROGATE** post-close. Same rationale.
- `ay-samply-json-twitter`, `ay-samply-json-twitter-lookup`: **KEEP**
  during AY; **ABROGATE** post-close. These wrap
  `profile-bench-headless.sh` with a hard-coded port pair + artifact
  dir — the exact kind of generated-from-wave-spec boilerplate that
  should not ossify.
- `ay-bench-close`: **KEEP** during AY; **REPLACE** post-close with a
  `cargo xtask bench-close` binary or a divan baseline script.
  `ay-prepare-profile-wave`: **KEEP** (shared prerequisite).

**Release group** — **KEEP** all four (`bump-patch`, `bump-minor`,
`bump-major`, `release`). Idiomatic for extension-publish workflows.
`cargo-release` exists but doesn't cover the VS Code extension's
`npm version` + `git tag` + marketplace publish; the current targets are
correct.

**Clean / deploy / watch**:

- `clean`, `clean-vsix`, `deploy`: **KEEP**.
- `watch`: **REPLACE**. `cargo watch` is unmaintained (last release 2023);
  **bacon** (`https://github.com/Canop/bacon`, v3.14+) is the modern
  successor — better UX, supports `.bacon.toml` per-project config,
  integrates with clippy / test / doc jobs out of the box. Replace the
  target with a `bacon.toml` sitting at repo root + a `make watch` that
  invokes `bacon`.

### Per-target quick-reference table

| Target | Verdict | Replacement / note |
|---|---|---|
| `all`, `build`, `build-lsp`, `build-lsp-debug`, `build-ext`, `build-wasm`, `dev` | KEEP | — |
| `install`, `package` | KEEP | — |
| `iter-check`, `iter-test-leaf`, `iter-test-grammar`, `iter-test-ws` | FOLD-INTO-TOOLING | Already in `.cargo/config.toml` aliases; delete shell wrappers |
| `expand-json`, `expand-css`, `expand-bbnf`, `expand-sheets` | FOLD-INTO-TOOLING | Already in `.cargo/config.toml` aliases |
| `test-close`, `test-heavy-rust`, `test-ci` | KEEP | nextest/timeout dispatch logic non-trivial |
| `bench` | ABROGATE | Mis-named; not a real bench |
| `asm-parse`, `bench-compile`, `bench-run` | KEEP | Parametric |
| `profile-wave` | KEEP | Real precondition checking |
| `ay-*` (all 10) | KEEP-during-AY → ABROGATE-post-close | Wave-specific; `new-tranche-new-doc` |
| `bump-patch`, `bump-minor`, `bump-major`, `release` | KEEP | — |
| `clean`, `clean-vsix`, `deploy` | KEEP | — |
| `watch` | REPLACE | bacon v3.14+ |

---

## Part 3 — CI workflow catalog

### `.github/workflows/ci.yml`

- **Purpose**: two jobs — (1) Rust toolchain + preflight
  (bootstrap-regen clean, clippy, iter-check) + heavy workspace tests +
  sonic-rs/lightningcss parity; (2) extension lint + build.
- **Lines**: ~60.
- **Verdict**: KEEP-MODERNIZE.
- **Rationale**: the two-tier preflight/heavy structure is correct.
  Modernizations:
  1. Replace `cargo test --workspace` in the Heavy step with
     `cargo nextest run --workspace --profile ci` (nextest is already
     installed + configured per `.config/nextest.toml`); nextest's
     `fail-fast = false` on the `ci` profile unmasks multi-failure runs.
  2. Add **cargo-deny** as a preflight step (license + advisory check);
     small crate (`https://github.com/EmbarkStudios/cargo-deny`), adds
     ~30 s.
  3. Pin `dtolnay/rust-toolchain@nightly` to a specific nightly
     (`@nightly-2026-04-15` or similar) to prevent silent upstream
     drift. Nightly-chasing is a known source of CI flake.
- **Migration cost**: S.

### `.github/workflows/release.yml`

- **Purpose**: cross-platform build (5 targets: linux-x64, linux-arm64,
  macos-x64, macos-arm64, win-x64), package `.vsix` per target, publish
  to VS Code Marketplace on tag push.
- **Lines**: ~100.
- **Verdict**: KEEP.
- **Rationale**: standard release pipeline; idiomatic. The `cross`
  installation for linux-arm64 is correct. No modernization candidate.

---

## Part 4 — Inline hacks

Grep results for `eprintln!` / `Instant::now()` outside of `benches/`:
**278 hits across 57 files**. Concentrations (>5 per file):

| File | Hits | Verdict |
|---|---:|---|
| `crates/lsp/tests/integration.rs` | 46 | KEEP (test) |
| `crates/core/tests/named_pipeline_probe.rs` | 36 | KEEP (test) |
| `crates/gorgeous/src/main.rs` | 25 | KEEP (CLI stdout/stderr) |
| `crates/core/tests/typed_accessor_surface.rs` | 19 | KEEP (test) |
| `crates/core/tests/payload_layouts.rs` | 17 | KEEP (test) |
| `crates/core/tests/pipeline.rs` | 16 | KEEP (test) |
| `crates/core/tests/dfa_fidelity.rs` | 14 | KEEP (test) |
| `crates/lsp/tests/analyze.rs` | 13 | KEEP (test) |
| `crates/core/tests/regex_audit.rs` | 13 | KEEP (test) |
| `crates/gorgeous/src/jit.rs` | 7 | **FLAG** — production code |
| **`crates/core/src/pipeline/compile.rs`** | **7** | **REPLACE** |

### `crates/core/src/pipeline/compile.rs` inline instrumentation

- **Verdict**: REPLACE.
- **Rationale**: this is production code, not tests. The `clean-instrumentation`
  memory is explicit: *"Timing/profiling must be architecturally clean; no
  eprintln macros or inline hacks"*. Seven hits here likely carry
  `Instant::now()` diff prints for pipeline-phase timing.
- **Replacement**: **tracing** (`https://github.com/tokio-rs/tracing`,
  v0.1.40+) with `#[instrument]` attributes on phase fns + a
  `tracing-subscriber` `fmt` layer gated by a `BBNF_LOG` env var. Per-phase
  wall-clock falls out of `#[instrument(level = "info")]` automatically.
- **Migration cost**: S — 7 hits is a morning's work.

### `crates/gorgeous/src/jit.rs`

- **Verdict**: FLAG (investigate individually — may be legitimate JIT
  debug output, may not).

### `TODO(perf)` / `HACK` / `FIXME:profiling`

Not surveyed in depth (scope cap). Recommendation: one-off `rg -n
'TODO\(perf\)|HACK|FIXME.*profiling' crates/` pass by the B1 cleanup
agent; fold findings into the next tranche plan.

---

## Part 5 — `.vscode/` and `.claude/`

### `.vscode/tasks.json`

- **Purpose**: six tasks — `Build Extension`, `Build LSP (Release)`,
  `Build LSP (Debug)`, `Build All (Debug)`, `Test LSP`, `Test All`.
- **Verdict**: KEEP.
- **Rationale**: each task is a trivial `cargo` / `npm` invocation.
  Thin, idiomatic, no abstraction warranted.

### `.vscode/launch.json`

- **Purpose**: two launch configs — `Launch Extension` (release LSP) and
  `Launch Extension (Debug LSP)` (debug LSP, relies on `cargo build` debug
  output).
- **Verdict**: KEEP.

### `.claude/settings.local.json`

Not inspected (likely contains agent-specific prefs — out of scope for a
public-facing abrogation catalog).

### `.cargo/config.toml`

- **Verdict**: KEEP. Modern, idiomatic. Contains:
  - `[patch.crates-io]`: path-patching for sibling repos (pprint,
    parse-that, bbnf-regex, gorgeous, bbnf-ir, csp-solver, egraph).
    Correct for the multi-repo dev pattern.
  - `[alias]`: seven aliases (`iter-check`, `iter-check-full`,
    `iter-test-leaf`, `iter-test-grammar`, `expand-*`, `asm-parse`,
    `prep-bench`, `final-bench`). All thin; all correctly scoped to
    `--profile ax-iter` / `--profile profiling-prep` / `--profile bench`.
  - Commented-out lld/mold linker blocks: KEEP as comments (platform-
    optional fast-linker opt-in).

### `.config/nextest.toml`

- **Verdict**: KEEP. Already modern — defines `default` + `ci` profiles
  with `slow-timeout`, `leak-timeout`, `retries`, plus per-test overrides
  for `compile` and `bench_*` pattern-named tests. No modernization
  candidate; this IS the modern idiom.

---

## Cross-cutting analysis

### 1. Abrogation summary table

| Verdict | Count | LOC affected |
|---|---:|---:|
| KEEP                      | 17 | — |
| KEEP-MODERNIZE            | 10 | ~500 lines reduced in place |
| REPLACE                   |  7 | ~900 lines removed, ~120 added (divan + bacon + tracing + jq one-liners) |
| ABROGATE                  | 19 | ~800 lines removed outright |
| FOLD-INTO-TOOLING         | 10 | ~200 lines of Makefile wrappers removed; ~40 added to `.config/nextest.toml` |
| **Total LOC delta**       | —  | **−1,960 removed, +160 added, net −1,800** |

The `.cargo/config.toml` aliases already cover the cargo-side FOLD
target — most of this work is *deletion* (wave-specific scripts, Makefile
passthroughs), not new authoring.

### 2. Total LOC delta — details

- `scripts/`: 2,696 → ~1,200 (−1,496).
  - Deleted: `check-cst-invariants.sh` (85), `verify-w2-asm.sh` (161),
    `verify-w2-symbols.sh` (178), `profile.sh` (40),
    `extract_hotspots.py` (302) → subtotal 766.
  - Replaced: `bench_regression.sh` (89 → ~20 via divan), `cost-grid-sweep.sh`
    (390 → ~40 via divan parametric), `bisect-fastpath.sh` (162 → ~30),
    `bootstrap-bbnf.sh` (338 → ~120 with Rust post-processor),
    `prepare-profile-wave.sh` (180 → ~80 with TOML config),
    `worktree-status.sh` (72 → ~25) → subtotal 1,231 → ~315 (−916). Offset
    by Rust bin authoring: ~100 lines in `crates/bootstrap/src/bin/post_expand.rs`.
- `Makefile`: 470 → ~300 (−170).
  - Wrappers deleted: `iter-*` (4), `expand-*` (4), `bench`, `watch` → ~80
    lines. Post-AY-close: all `ay-*` targets (10, ~180 lines) deleted.
- `.github/workflows/ci.yml`: minor (+5 for cargo-deny step).
- Inline: ~50 lines in `pipeline/compile.rs` replaced with `#[instrument]`.

### 3. Dependency ordering

```
divan migration of crates/core/benches/
  ↓
bench_regression.sh REPLACE   (depends on divan baselines)
cost-grid-sweep.sh REPLACE    (depends on divan parametric)
ay-bench-close post-AY REPLACE (depends on divan baselines)
                │
                ↓
        [B1 bench-gate reliability]

tracing adoption in crates/core
  ↓
pipeline/compile.rs inline REPLACE
  ↓
[B1 production-clean discipline]

tranche AY close
  ↓
ay-* Makefile targets ABROGATE
verify-w2-asm.sh ABROGATE (already due)
verify-w2-symbols.sh ABROGATE (already due)
                │
                ↓
        [BA clean-up wave]

bacon adoption
  ↓
`make watch` REPLACE  (independent — can land B1 immediately)

Rust post_expand bin
  ↓
bootstrap-bbnf.sh modernize (MEDIUM — self-hosting load-bearing; defer to BA)
```

Independent threads that can land in parallel in B1:

- `make watch` → bacon (30 min).
- `check-cst-invariants.sh` delete (15 min).
- `profile.sh` delete (15 min).
- `bench` target delete (5 min).
- Makefile FOLD-INTO-TOOLING (iter-* + expand-* wrappers) (30 min).
- cargo-deny CI step (30 min).

### 4. Risks

Items load-bearing for current workflows, where premature abrogation would
break user paths:

1. **`bootstrap-bbnf.sh`**: LOAD-BEARING. Every grammar edit regens
   `generated.rs` through this script. The `Rust post_expand` bin must
   ship + be CI-verified before the shell regex is retired. `check-bootstrap-clean.sh`
   would catch any drift, but re-authoring the post-processor inline is
   the larger risk — don't rush.
2. **`profile-bench-headless.sh` + `prepare-profile-wave.sh`**:
   LOAD-BEARING for the AY samply gates. Keep as KEEP / KEEP-MODERNIZE;
   the port-pair convention is tranche-active.
3. **`ay-*` Makefile targets**: load-bearing DURING AY. Only ABROGATE
   after the tranche-close commit. The `new-tranche-new-doc` discipline
   means the follow-on tranche (AZ? BA?) opens its own fresh gate
   targets, not a rename of AY's.
4. **`kill-all-rust.sh`**: load-bearing for multi-agent workflows. Don't
   touch.
5. **`seed-worktree.sh`**: same — multi-agent load-bearing.

### 5. Sequencing recommendation

**B1 (immediate — ~2 hours dev-friction-reduction)**:

1. Delete `check-cst-invariants.sh` + its CI step.
2. Delete `profile.sh`.
3. Delete `Makefile` `bench` target (mis-named).
4. FOLD `iter-check`, `iter-test-leaf`, `iter-test-grammar`,
   `iter-test-ws`, `expand-{json,css,bbnf,sheets}` Makefile targets —
   callers use `cargo iter-check` / `cargo expand-json` directly.
5. Replace `make watch` with `bacon` + `bacon.toml`.
6. Add `cargo-deny` preflight step to `ci.yml`.
7. Pin `dtolnay/rust-toolchain@nightly-<date>`.

**BA (post-AY-close, 1-week wave)**:

1. ABROGATE all `ay-*` Makefile targets.
2. ABROGATE `verify-w2-asm.sh`, `verify-w2-symbols.sh`.
3. Modernize `bisect-fastpath.sh` (162 → 30).
4. Modernize `worktree-status.sh` (72 → 25).
5. Modernize `prepare-profile-wave.sh` (180 → 80 + TOML).
6. Replace `pipeline/compile.rs` inline instrumentation with `tracing`.
7. FOLD `test-tier.sh` into `.config/nextest.toml` profiles.

**BB (larger — divan migration gated)**:

1. Port `crates/core/benches/` to **divan**.
2. Replace `bench_regression.sh` → divan baselines.
3. Replace `cost-grid-sweep.sh` → divan parametric bench.
4. Replace `extract_hotspots.py` → Rust `hotspots` bin + jq recipe.
5. Modernize `bootstrap-bbnf.sh` with Rust `post_expand` bin.

---

## Highest-leverage abrogation

**Single highest-leverage abrogation**: the **`ay-*` Makefile target
family + `verify-w2-*.sh` pair**, totaling ~520 lines, is the cleanest
structural win — wave-specific cruft that already has a stale-ness
smell in the working tree, and whose continued presence actively
misleads executors about which gates are current.

**Single highest-leverage modernization**: replace **`cargo watch` with
bacon**. Every dev iteration goes through `make watch`; bacon's better
ergonomics (clippy + test + run in one panel, live error folding, crash
detection) is felt on every edit. 30-minute migration, immediate daily
benefit.

**Single highest-leverage REPLACE with architectural payoff**: the
**divan migration** unlocks REPLACE of three scripts
(`bench_regression.sh`, `cost-grid-sweep.sh`, `ay-bench-close` post-close)
and aligns with the user's *"bencher and criterion fully abrogated"*
direction. Divan is the modern idiom — structured JSON output,
parametric benches, baseline persistence — that these three ad-hoc
shell sweeps reinvented badly.

---

## Top 5 highest-value REPLACE items ranked by dev-friction-reduction

1. **`cargo watch` → bacon**. Daily edit loop; immediate payoff.
2. **`bench_regression.sh` → divan baselines**. Removes 89 lines of
   regex-over-text; unblocks two further replacements
   (`cost-grid-sweep.sh`, `ay-bench-close`).
3. **`pipeline/compile.rs` inline `eprintln!`/`Instant::now()` → tracing +
   `#[instrument]`**. Architectural-hygiene win per `clean-instrumentation`.
4. **`test-tier.sh` → `.config/nextest.toml` profiles**. Removes 85 lines
   of shell; aligns with nextest's native profile surface; the
   `/tmp/test-tier-<tier>.txt` redirect folds into a trivial shell
   redirect at the Makefile target.
5. **`extract_hotspots.py` → `jq` recipe + Rust `hotspots` bin**.
   Removes Python from a Rust repo; aligns with samply's structured
   output contract.

## Genuinely-valuable KEEP items

Per the user's explicit endorsement:

- **`scripts/profile-bench-headless.sh`** + **samply** tooling: *"our
  profiling script for samply is likely good."* Confirmed by
  `samply-symbol-resolution` memory (samply requires `debug=true` +
  interactive `samply record`, not `--save-only`); no replacement is
  proposed.
- **`scripts/prebuild-benches.sh`** — the build-once/samply-many
  invariant is real and upstream-unsubsumed.
- **`scripts/bootstrap-bbnf.sh`** — self-hosting load-bearing;
  modernization is internal refactor, not replacement.
- **`scripts/seed-worktree.sh`** + **`scripts/kill-all-rust.sh`** —
  multi-agent workflow load-bearing; no upstream subsumes either.
- **`.config/nextest.toml`** — already the modern idiom.
- **`.cargo/config.toml` aliases** — idiomatic; the Makefile wrappers
  over them are redundant, not the aliases themselves.

---

## Appendix — abrogation checklist for B1

Ordered for a single PR landing ~3 hours of work:

- [ ] `rm scripts/check-cst-invariants.sh` + remove CI step referencing it.
- [ ] `rm scripts/profile.sh` + grep for callers in `docs/`, `README.md`.
- [ ] Delete `Makefile` `bench:` target.
- [ ] Delete `Makefile` `iter-check:`, `iter-test-leaf:`, `iter-test-grammar:`, `iter-test-ws:` targets.
- [ ] Delete `Makefile` `expand-json:`, `expand-css:`, `expand-bbnf:`, `expand-sheets:` targets.
- [ ] Update any instructions doc that references `make iter-check` → `cargo iter-check`.
- [ ] Replace `Makefile` `watch:` target with `bacon` invocation; add `bacon.toml`.
- [ ] Add `cargo-deny` preflight step to `.github/workflows/ci.yml`.
- [ ] Pin nightly in `.github/workflows/ci.yml` to specific date.
- [ ] Commit with message: `build: B1 abrogation wave — 5 scripts + 9 Makefile targets + CI hardening`.

LOC delta for the B1 PR: −420 net.
