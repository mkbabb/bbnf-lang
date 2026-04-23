# gorgeous (sibling mirror) — Modernization Plan

## Role in the fleet
Parallel copy of `crates/gorgeous`. Sibling repo at
`/Users/mkbabb/Programming/gorgeous`. Originally intended to allow downstream
`gorg` releases independent of the bbnf-lang version bump cadence. Currently
effectively dead: its `.cargo/config.toml` `[patch.crates-io]` block points at
paths that ceased to exist post-arch-consolidation (`../bbnf-lang/rust/bbnf`,
`../bbnf-lang/rust/bbnf-derive`, `../bbnf-lang/rust/bbnf-ir`), and its CI
performs the matching sed-rewrite against the same dead paths.

## Current posture (from Wave 1-B assay)
- HEAD `df45aca4` on `master`; 72 commits; 1 uncommitted `src/bbnf.rs.bak`
  (.bak artefact suggests hand-patched generated output —
  `feedback_generated_files_clean_regen` violation risk).
- Single-crate. bin (`gorg`) + lib (`gorgeous`). 2 features (`default`, `vm`).
- **2 `[[bench]]` entries**: `gorgeous.rs`, `competitors.rs`, both
  `harness = false`, `bencher = "0.1"`.
- `rust-toolchain.toml` **absent**; edition 2024; `rust-version = "1.85"`
  declared (matches pprint's claim).
- `.cargo/config.toml` at repo root; `[patch.crates-io]` references 6
  **STALE** paths (`../bbnf-lang/rust/bbnf`, etc.). `cargo check` from this
  repo will fail patch resolution and silently fall through to crates.io
  versions.
- Biome dev-deps pinned at `=0.4.0` — contrasts with `crates/gorgeous` which
  uses `cli/v1.9.4` tag to dodge the 0.5.7/0.5.8 rowan skew. This sibling is
  therefore vulnerable to the same uncompilable combination on upstream
  update; the `=0.4.0` pin is effectively broken by upstream drift.
- CI: `.github/workflows/ci.yml` checks out 3 sibling repos then
  sed-rewrites `rust/bbnf`, `rust/bbnf-derive`, `rust/bbnf` paths — also
  targeting dead paths. **CI must be broken on HEAD** unless pinned to an
  older bbnf-lang SHA.
- Consumes `bbnf_derive` at 6+1 `#[derive(Parser)]` sites (including the
  `bbnf.rs.bak` orphan). Aggregate expansion is the single heaviest in the
  workspace.
- `[profile.bench]`: fat LTO, `codegen-units = 1`, `opt-level = 3`.
- No scripts, no justfile, no Makefile.

## Target posture
**Recommended disposition: RETIRE.** The workspace copy at
`crates/gorgeous` is already authoritative (path-patched into bbnf-lang
workspace; uses the working biome pin; carries the 6 grammar features;
participates in every iter-check profile). The sibling has been dead since
the April 2026 architectural consolidation. Modernising a dead mirror is
wasted work.

**Retirement path**:
- Keep the git repo in place (history is valuable).
- Mark `master` as end-of-life with a tombstone commit.
- Update README.md to redirect to `bbnf-lang/crates/gorgeous` as the
  authoritative source.
- Delete the broken `.cargo/config.toml` (or mark it with a "see bbnf-lang"
  banner).
- Disable CI workflow (move to `.disabled` suffix — same pattern as
  csc411).

**Alternative (NOT recommended): RE-TRACK.** If the sibling must live, all
of: path-patch realignment (`../bbnf-lang/rust/bbnf` → `crates/core`, etc.),
CI sed rewrite against new paths, biome pin bump to `cli/v1.9.4`, bencher
→ divan migration, nextest install, rust-toolchain pin, `.bbnf.rs.bak`
audit. ~10–15 hours work on a codebase that is never exercised in the fleet
iteration loop.

## Gap — what must change (retirement path)
1. Delete `src/bbnf.rs.bak` (untracked artefact); confirm `src/bbnf.rs` is
   a fresh regen from `crates/gorgeous`'s tooling (10 min).
2. Replace README.md with tombstone redirect to `bbnf-lang/crates/gorgeous`
   (15 min).
3. Move `.github/workflows/ci.yml` → `.disabled` suffix (5 min).
4. Delete `.cargo/config.toml` (or leave with banner explaining why it is
   stale) (5 min).
5. Bump `Cargo.toml` version to `x.y.z-tombstone` or similar (5 min).

**Total (retirement)**: ~40 min.

## Gap — what must change (re-track path, for completeness)
If user rejects retirement:
1. Install `rust-toolchain.toml` (3 min).
2. Rewrite `.cargo/config.toml` `[patch.crates-io]` to new paths (`crates/core`,
   `crates/derive`, `crates/ir`) (30 min).
3. Rewrite CI sed expressions to match (15 min).
4. Bump biome pins from `=0.4.0` to `cli/v1.9.4` git tag (30 min; requires
   testing that the monorepo tag compiles clean on the pinned nightly).
5. Port 2 benches: `bencher` → `divan` (1.5 hours).
6. Install `.config/nextest.toml`; rewrite CI to use nextest (45 min).
7. Drop `rust-version = "1.85"` MSRV claim (5 min).
8. Audit `bbnf.rs.bak` orphan; delete (10 min).
9. Establish bench parity vs workspace copy (unlikely — workspace copy has no
   benches; siblings' benches become the authoritative bench source, but then
   `crates/gorgeous` becomes the bench-less version). Governance question
   deferred to retirement-vs-retrack decision.

**Total (re-track)**: ~4–5 hours + indeterminate governance cost.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: retirement. Item 3 (disable CI) is
  mandatory — fleet-wide CI consistency demands this repo stop emitting
  failure noise.
- **Phase B / Phase C**: nothing if retired. If re-tracked, retirement
  Phase A expands to full modernization (~5 hours).

## Dependencies
- **Upstream blockers**: none. Retirement can happen independently.
- **Downstream blocks**: none. Nothing in the fleet depends on this repo —
  all consumers use `crates/gorgeous`.
- **B1 coupling**: none on retirement path. Re-track path couples to Step 11.

## Risks
- **Retirement risk**: a future user attempting to check out the sibling for
  a standalone `gorg` release discovers it is dead. Mitigated by clear
  tombstone README.
- **Re-track risk**: paths drift again on the next arch consolidation; the
  sibling becomes broken-by-default, and the whole exercise repeats.
- **Bak-file risk**: `bbnf.rs.bak` must be audited before retirement; if it
  contains divergence from the current `bbnf.rs`, the user may want to
  extract changes. Probably just stale.

## Verification (retirement path)
```bash
cd /Users/mkbabb/Programming/gorgeous
cat README.md | grep -q "tombstone\|crates/gorgeous"  # redirect present
[ -f .github/workflows/ci.yml.disabled ]              # CI disabled
[ ! -f src/bbnf.rs.bak ]                              # bak removed
```

## Verification (re-track path)
```bash
cd /Users/mkbabb/Programming/gorgeous
rustc --version               # pinned nightly
cargo iter-check              # builds with corrected patch paths
cargo nextest run             # tests pass
cargo bench                   # divan output on 2 benches
```

## Specific changes (retirement — patch-ready)
- `README.md`:
  ```markdown
  # gorgeous (tombstone)
  This repo is retired. Authoritative source moved to
  `bbnf-lang/crates/gorgeous` during the April 2026 architectural
  consolidation. See /Users/mkbabb/Programming/bbnf-lang.
  ```
- `.github/workflows/ci.yml` → `.github/workflows/ci.yml.disabled`.
- `Cargo.toml`: optional version bump to `-tombstone`.
- Delete `src/bbnf.rs.bak`.
