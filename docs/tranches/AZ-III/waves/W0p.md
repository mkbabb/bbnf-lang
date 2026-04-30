# AZ-III.W0p - Throughput Substrate

**Name**: W0p - Throughput Substrate
**Opens after**: W0 - Quarantine and Dispatch Repair.
**Agents**: up to 5 parallel.
**Hard gate**: bench-iter profile lands and is measured; profile redundancy resolved; regen `--staged` plumbed; nextest partition wired; `make doctor` host-readiness probe added; a 5-harness sweep is measured under the new bench-iter profile.
**Status**: planned

## Scope

1. Add `[profile.bench-iter]` (`lto = "off"`, `codegen-units = 16`,
   `incremental = true`, `debug = "line-tables-only"`) and `bench-iter*`
   aliases to `Cargo.toml` and `.cargo/config.toml`.
2. Resolve the duplicate `[profile.ax-iter]` definition between
   `Cargo.toml:125-129` and `.cargo/config.toml:65-70` so a single source
   of truth governs the iteration profile.
3. Add `xtask regen --check --staged` flag plumbing in `xtask/src/main.rs`
   and `xtask/src/regen.rs`; pre-commit hook consumes the flag.
4. Add `make doctor` host-readiness probe (`sccache`, `cargo-nextest`,
   `samply`, `lld` availability) under `Makefile` and a non-cargo runner
   under `scripts/`.
5. Wire `cargo nextest --partition` for CI sharding under
   `.github/workflows/ci.yml` and `.config/nextest.toml`.
6. Measure cold and warm wall for `iter-check-full`,
   `bench-iter-{json,css,sheets,bbnf,compile}` compile, regen `--staged`
   wall, and the 5-harness sweep under the new profile; archive under
   `docs/benchmarks/AZ-III/W0p-*.txt`.

## File Bounds

| File | Access |
|---|---|
| `Cargo.toml` (`[profile.*]` only) | modify-carve |
| `.cargo/config.toml` (`[profile.*]` and `[alias]`) | modify-carve |
| `Makefile` (new top-level targets only) | modify-carve |
| `xtask/src/main.rs` | modify-carve (CLI flag) |
| `xtask/src/regen.rs` | modify-carve (`--staged` behaviour) |
| `scripts/**` | modify-carve (host probe, partition helper) |
| `.config/nextest.toml` (per-profile only) | modify-carve |
| `.github/workflows/ci.yml` (sharding only) | modify-carve |
| `docs/benchmarks/AZ-III/W0p-*.txt` | create |
| `docs/tranches/AZ-III/**` | modify |

Do NOT touch: source code, generated grammar, parity tests, bench harness
sources beyond `[[bench]]` registration in `crates/core/Cargo.toml`,
benchmark artefacts outside the W0p evidence directory.

## Agent Units

### AZ-III.W0p.1 Bench-Iter Profile

- Mechanism: append `[profile.bench-iter]` and the matching `bench-iter`
  aliases; verify the 5-harness sweep compiles in <60 s warm.
- Files: `Cargo.toml`, `.cargo/config.toml`.
- Sub-gate: `cargo bench-iter-json --no-run` cold wall <60 s archived;
  warm wall <5 s archived.

### AZ-III.W0p.2 Profile Redundancy Cleanup

- Mechanism: pick `.cargo/config.toml` as the single home for `ax-iter`
  and drop the root `[profile.ax-iter]`; verify alias inheritance still
  resolves; archive a `cargo build --profile ax-iter -v` capture that
  shows the consolidated settings.
- Files: `Cargo.toml`, `.cargo/config.toml`.
- Sub-gate: only one `[profile.ax-iter]` definition exists in the
  workspace; settings match the alias-side intent.

### AZ-III.W0p.3 Regen Staged Mode

- Mechanism: extend the CLI to accept `--staged`; restrict the regen loop
  to grammars whose source path overlaps the staged set; pre-commit hook
  picks the flag up automatically.
- Files: `xtask/src/main.rs`, `xtask/src/regen.rs`,
  `scripts/hooks/pre-commit`.
- Sub-gate: `cargo xtask regen --check --staged` returns 0 in <1 s when
  no grammar source is staged; archived wall measurement.

### AZ-III.W0p.4 Make Doctor Host Probe

- Mechanism: add `make doctor` target that probes `sccache`,
  `cargo-nextest`, `samply`, and `lld` availability; emit actionable
  install hints; <1 s wall.
- Files: `Makefile`, optional helper under `scripts/`.
- Sub-gate: `make doctor` exits 0 on a green host; exits non-zero with a
  named missing component otherwise.

### AZ-III.W0p.5 Nextest Partition Wiring

- Mechanism: add `count:N/M` partition support to the workspace test
  step in `.github/workflows/ci.yml`; document the sharding policy in
  `.config/nextest.toml` if a profile-level override is required.
- Files: `.config/nextest.toml`, `.github/workflows/ci.yml`.
- Sub-gate: CI matrix runs three workspace shards; archived workflow
  output in W0p evidence directory.

## Triumvirate Dispatch

If any substrate change measures cold-vs-cold worse than the prior
median, or reveals a host-portability gap that cannot land inside the
declared file bounds, pause that lane and dispatch research, plan
augment/synthesis, and redress/redeployment agents. The synthesis must
either fold the change back into W0p or open a same-tranche replacement
wave before the affected substrate ships. HARD CAP for any redress
dispatch under W0p: 30 min.

## Hard Gate

1. `cargo bench-iter-json --no-run` cold wall <60 s and warm wall <5 s
   archived in `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt`.
2. `[profile.ax-iter]` is defined exactly once across `Cargo.toml` and
   `.cargo/config.toml`; archived `cargo build --profile ax-iter -v`
   capture confirms.
3. `cargo xtask regen --check --staged` cold-no-grammar wall <1 s
   archived; pre-commit hook routes through the flag.
4. `make doctor` exit-zero capture on the dispatch host archived.
5. CI nextest partition matrix runs at least three shards; capture from
   one workflow run archived.
6. `make ay-bench-close WAVE=close` analogue under
   `bench-iter` profile (5-harness sweep wall) measured and archived;
   total wall is below the fat-LTO baseline by a measured ratio.

## Format And Lint Cadence

Run `cargo fmt --all -- --check` after each accepted manifest or source
edit; run `git diff --check` after each integration batch. Run
`cargo iter-check` after `xtask` and profile changes. Before W0p closes,
rerun `cargo fmt --all -- --check`, `cargo iter-check`, and
`git diff --check`.

## Verification Artefacts

- `docs/benchmarks/AZ-III/W0p-bench-iter-walls.txt`
- `docs/benchmarks/AZ-III/W0p-profile-source-of-truth.txt`
- `docs/benchmarks/AZ-III/W0p-regen-staged-wall.txt`
- `docs/benchmarks/AZ-III/W0p-doctor.txt`
- `docs/benchmarks/AZ-III/W0p-ci-partition.txt`
- `docs/benchmarks/AZ-III/W0p-bench-iter-sweep.txt`

## Commit Plan

Expected scopes, each with an evidence-bearing body:

- `chore(profile/bench-iter): add bench-iter profile and aliases`
- `chore(profile/ax-iter): consolidate ax-iter to a single source of truth`
- `feat(xtask/regen-staged): add --staged flag for incremental regen check`
- `chore(make/doctor): add host-readiness probe`
- `ci(nextest/partition): add three-shard workspace matrix`
- `docs(az-iii.W0p): record throughput substrate evidence`

Each implementation commit names the measured wall delta in its body.
The docs commit cites the evidence files.

## Dependencies

- **Depends on**: W0 - Quarantine and Dispatch Repair.
- **Blocks**: W1 - O5 Reclose, W2 - Semantic Parity and Bootstrap
  Canonicalization, W3a - Fact and Type Authority, W3b - CSP Strategy
  Globalization, W3c - Projection Consumption and Registry Authority,
  W4 - Benchmark, Profile, and Workspace Truth, and W5 - Terminal Close
  and Handoff.

## Archaeology

The 2026-04-30 REAUDIT lane 6
(`docs/tranches/AZ-III/audit/REAUDIT-2026-04-30/06-throughput-commit.md`)
identified bench compile as the W4 throughput blocker: fat-LTO produces
>10 min per harness, and the 5-harness 17-entry sweep is >50 min. The
AZ-III W4 hard gate (refreshed `post-AZ-III.json` matrix) cannot be
satisfied honestly without bench-iter relief. Per the user's
`feedback_build_infra_first` precept, build/test infrastructure
improvements land FIRST in any tranche where dev iteration time is a
bottleneck. W0 was authored as a doc-only quarantine wave (no source
touched per its file bounds); W0p exists so that source-bearing
infrastructure work runs after dispatch repair but before W1-W5
implementation, preserving W0's clean-source posture and respecting the
infra-first precept.
