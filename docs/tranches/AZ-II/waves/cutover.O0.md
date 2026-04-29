# AZ-II.cutover.O0 — Tooling Preflight
**Opens after**: cutover.N halt record
**Agents**: up to 10 parallel
**Hard gate**: close-evidence command surfaces compile, resolve, or are explicitly de-canonicalized before any terminal benchmark claim.
**Status**: complete

## Scope

1. Repair stale bench aliases and feature tiers before they are used as
   AZ-II close evidence.
2. Repair IAI CI so it invokes a tracked comparison helper and a bench
   binary that is compiled with the right feature.
3. Repair profiling scripts so profile preparation targets the current
   bench matrix and invalidates stale generated-code artifacts.
4. Pin release workflow Rust setup to the repository toolchain instead
   of a floating nightly.
5. Delete obsolete benchmark wrappers that would create a second
   authority beside the canonical O6 matrix.
6. Record the command-surface disposition in AZ-II progress docs
   without collecting a performance baseline in O0.

## File bounds

| File | Access |
|---|---|
| `.cargo/config.toml` | modify |
| `.github/workflows/bench-iai.yml` | modify |
| `.github/workflows/release.yml` | modify |
| `Makefile` | modify |
| `scripts/prebuild-benches.sh` | modify |
| `scripts/prepare-profile-wave.sh` | modify |
| `scripts/profile-bench-headless.sh` | modify |
| `scripts/iai-compare.sh` | create |
| `scripts/bench_regression.sh` | delete |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.md` | modify |

**Do NOT touch**: emitter source, runtime builders, generated parser
files, parity tests, benchmark result JSON, or `docs/benchmarks/post-AZ-II.json`.
Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`, with
explicit allow/forbidden lists; the orchestrator owns the main worktree
and any final documentation consolidation.

## Phase sub-items

### AZ-II.cutover.O0.1 Bench Alias Feature Audit

Mechanism: reconcile bench aliases in `.cargo/config.toml` with the
features actually required by JSON parse-that, JSON value, competitor,
stress, VM, and workspace bench targets.

Files touched: `.cargo/config.toml`.

Sub-gate: `cargo metadata --no-deps --format-version 1` resolves after
the alias edits.

### AZ-II.cutover.O0.2 Makefile Close Targets

Mechanism: update close-facing make targets so they invoke canonical
bench/profile scripts and do not call deleted wrappers.

Files touched: `Makefile`.

Sub-gate: `make -n profile`, `make -n bench-json`, and
`make -n bench-sheets` print commands that exist in the tree.

### AZ-II.cutover.O0.3 IAI Workflow Repair

Mechanism: make `.github/workflows/bench-iai.yml` invoke
`json_callgrind` with the `callgrind` feature and consume a tracked
comparison script.

Files touched: `.github/workflows/bench-iai.yml`,
`scripts/iai-compare.sh`.

Sub-gate: `scripts/iai-compare.sh` handles a missing baseline and a
skipped job without failing spuriously.

### AZ-II.cutover.O0.4 Release Toolchain Pin

Mechanism: update the release workflow to install the repository
`rust-toolchain.toml` channel rather than floating
`dtolnay/rust-toolchain@nightly`.

Files touched: `.github/workflows/release.yml`.

Sub-gate: workflow YAML references `rust-toolchain.toml`.

### AZ-II.cutover.O0.5 Prebuild Script Repair

Mechanism: align `scripts/prebuild-benches.sh` with the current bench
feature matrix and reject missing bench binaries loudly.

Files touched: `scripts/prebuild-benches.sh`.

Sub-gate: `bash -n scripts/prebuild-benches.sh` passes.

### AZ-II.cutover.O0.6 Profile Preparation Repair

Mechanism: make profile preparation target `json_value`, carry required
competitor features, and invalidate expand artifacts when generated
grammar sources change.

Files touched: `scripts/prepare-profile-wave.sh`.

Sub-gate: `bash -n scripts/prepare-profile-wave.sh` passes.

### AZ-II.cutover.O0.7 Headless Profile Script Repair

Mechanism: align headless profiling with the repaired profile
preparation surfaces and current bench target names.

Files touched: `scripts/profile-bench-headless.sh`.

Sub-gate: `bash -n scripts/profile-bench-headless.sh` passes.

### AZ-II.cutover.O0.8 Obsolete Wrapper Deletion

Mechanism: delete `scripts/bench_regression.sh` instead of retaining a
stale benchmark authority beside O6's close matrix.

Files touched: `scripts/bench_regression.sh`.

Sub-gate: `rg 'bench_regression\\.sh' .github Makefile scripts docs`
returns no live command surface outside historical prose.

### AZ-II.cutover.O0.9 Command Surface Scan

Mechanism: archive command-resolution output for the repaired bench,
profile, IAI, and release surfaces.

Files touched: documentation/progress records only.

Sub-gate: all O0 shell syntax and dry-run checks are captured in the
progress entry.

### AZ-II.cutover.O0.10 Progress Boundary

Mechanism: record O0 as a tooling preflight, explicitly not a semantic
or performance baseline, and route measurement truth to O6.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/waves/cutover.md`.

Sub-gate: progress docs agree that O1 is the next substrate gate and
O6 owns the post-AZ-II performance baseline.

## Hard gate

1. `bash -n scripts/prebuild-benches.sh scripts/prepare-profile-wave.sh scripts/profile-bench-headless.sh scripts/iai-compare.sh`
   passes.
2. `make -n profile`, `make -n bench-json`, and `make -n bench-sheets`
   resolve existing scripts and bench aliases.
3. `cargo metadata --no-deps --format-version 1` resolves the package
   graph after alias/workflow edits.
4. `rg 'bench_regression\\.sh' .github Makefile scripts docs` finds no
   live command invocation.
5. AZ-II docs say O0 repaired proof surfaces and did not collect the
   terminal performance baseline.

## Verification artefacts

- `/tmp/az-ii-o0-bash-n.txt`
- `/tmp/az-ii-o0-make-dry-runs.txt`
- `/tmp/az-ii-o0-cargo-metadata.txt`
- `/tmp/az-ii-o0-iai-compare.txt`
- O0 close commit hashes recorded in `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: cutover.N halt record
- **Blocks**: AZ-II.cutover.O1

## Archaeology

B0-B7 modernized the toolchain, but later bench and profiling surfaces
drifted from the actual feature matrix. O0 fixes the evidence path
before O6 can publish throughput truth.
