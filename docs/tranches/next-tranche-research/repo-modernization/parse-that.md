# parse-that — Modernization Plan

## Role in the fleet
Parser-combinator substrate + bespoke regex engine. Sibling repo at
`/Users/mkbabb/Programming/parse-that`. Hosts `parse_that`, `bbnf-regex`, and
`regex-bootstrap`. Every bbnf grammar site lowers through `parse_that`
combinators; `bbnf-regex` carries the HIR→NFA→DFA pipeline that `bbnf-ir`
consumes. Path-patched into bbnf-lang via the workspace `.cargo/config.toml`
(`parse_that`, `bbnf-regex` entries). Also hosts a TypeScript sibling at
`typescript/`; isomorphic API per `feedback_isomorphic_api`.

## Current posture (from Wave 1-B assay)
- HEAD `919d77d1` on `master`; 479 commits; 1 uncommitted `rustc-ice-*.txt` at
  `rust/parse_that/` — LTO bitcode write-after-target-deletion class.
- `rust-toolchain.toml` **absent**; edition 2024; MSRV not declared; CI uses
  ambient `dtolnay/rust-toolchain@nightly`.
- Workspace root at `rust/Cargo.toml` with 4 members: `src`, `parse_that`,
  `bootstrap`, `regex`. Declares `[profile.release-lto]` and `[profile.bench]`.
- **18 `[[bench]]` entries** across 3 crates: `parse_that` 13, `bbnf-regex` 4,
  `regex-bootstrap` 1. All `harness = false`. All `bencher = "0.1.5"`.
- `.cargo/config.toml` at `rust/.cargo/` is **gitignored** (per header) but
  carries 8-entry `[patch.crates-io]` reaching back to bbnf-lang and csp-solver.
  No aliases, no profile overrides, no rustflags.
- `justfile` at repo root with wrappers: `ts-build`, `rs-test`, `rs-build`,
  `all`. Thin over cargo/npm; no divergence from CI.
- `rust/scripts/bootstrap-regex.sh` mirrors bbnf-lang's `bootstrap-bbnf.sh`.
- CI single workflow (tsc + npm + `cargo clippy --workspace -- -D warnings`
  + `cargo test --workspace`, with second pass `--features diagnostics`). No
  matrix, no bench invocation, no release-profile parity.
- `bbnf-regex` compliant with `regex-crate-isomorphic` (uses egraph +
  egraph-derive + csp-solver through path-patch).

## Target posture
- Pinned nightly at `nightly-2026-04-11` (per B1 §1.4), identical to bbnf-lang.
- `.cargo/config.toml` **tracked** (stop gitignoring it — B1 rule via §B.6 of
  assay; governance cost of the `.gitignore paradox`); carries `ax-iter` profile
  + subset alias surface.
- Divan on all 18 benches. `test::Bencher` never introduced.
- `.config/nextest.toml` mirror of bbnf-lang's 4-profile layout.
- CI uses `cargo nextest run --workspace`; adds release-profile parity gate on
  `bbnf-regex` against the upstream `regex` crate (HIR-level equivalence).
- `justfile` retained but thinned to `cargo` aliases, not direct `cargo`
  invocations. No duplication with the alias surface.
- TypeScript API kept isomorphic — Rust modernization MUST NOT change public
  combinator signatures (per `feedback_isomorphic_api`).

## Gap — what must change
1. Install `rust-toolchain.toml` with pinned nightly (3 min; mirror of B1 §1.4).
2. Track `rust/.cargo/config.toml`; remove from `.gitignore`; add `[build]`
   rustflags + `[profile.ax-iter]` + minimal alias set (30 min).
3. Delete `bencher = "0.1.5"` from `parse_that`, `bbnf-regex`, `bootstrap`
   `[dev-dependencies]`; add `divan = "0.1"`; port 18 benches (6–8 hours).
4. Rewrite `rust/scripts/bootstrap-regex.sh` to drop `rm -rf` anti-patterns
   (mirror of bbnf-lang's rewrite; 30 min).
5. Install `.config/nextest.toml`; add freezing guards; wire to CI (45 min).
6. Rewrite CI workflow: `taiki-e/install-action@nextest`; replace
   `cargo test --workspace` with `cargo nextest run --workspace --profile ci`
   (15 min).
7. Resolve the 1 uncommitted ICE file — gitignore the `.txt` pattern; delete
   the stray (10 min).
8. `justfile` rewrite delegating to cargo aliases (20 min).
9. Add release-profile parity gate for `bbnf-regex` vs `regex` crate HIR
   canonicalisation (1.5 hours; secondary win).

**Total**: ~10–12 hours.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 5, 6 (~2 hours). This is B1
  Step 11 cross-propagation. Blocks nothing but is required for the path-patch
  to remain coherent against the pinned nightly.
- **Phase B (post-B1, before BA)**: items 3, 4, 8 (~8 hours). Divan migration
  is disjoint from bbnf-lang; an agent can own the 18-bench port in parallel.
- **Phase C (during BA)**: item 9 (parity gate); tied to BA's cost-model work
  on the regex egraph.

## Dependencies
- **Upstream blockers**: bbnf-lang B1 Step 1 (pinned-nightly draft) must exist
  so the sibling can mirror.
- **Downstream blocks**: bbnf-lang's path-patch resolution. Any proc-macro
  expansion of `parse_that` types via `bbnf_derive` is deterministic only when
  the pinned nightly matches. Until parse-that pins, every bbnf-lang
  incremental build carries silent nightly drift.
- **B1 coupling**: Steps 1 (pin), 2 (.cargo/config), 3 (nextest), 9 (CI), 11
  (propagation).

## Risks
- `.cargo/config.toml` currently gitignored; tracking it exposes whatever
  untracked path-patch state the developer last committed to disk. Audit
  before tracking; reconcile against bbnf-lang's 11-entry table.
- LTO bitcode ICE class is different pathology from bbnf-lang's `on_disk_cache`
  cluster. Pinning to the same nightly may or may not dodge it; verify on the
  exemplar bench port.
- `feedback_regex_crate_isomorphic`: `bbnf-regex`'s internal optimization
  architecture (egraph + CSP) must not regress during the dev-dep shuffle.
  Divan migration is `[dev-dependencies]` only — no library-surface impact.
- TypeScript sibling has no Rust toolchain concern, but any signature change
  during refactor breaks isomorphism (blocker, not risk).

## Verification
```bash
cd /Users/mkbabb/Programming/parse-that
rustc --version                 # shows pinned nightly
cargo iter-check                # resolves alias; builds workspace
cargo nextest run --workspace   # passes
cargo bench -p parse_that       # divan JSON output on all 13 benches
cargo bench -p bbnf-regex       # 4 benches
cargo bench -p regex-bootstrap  # 1 bench
cd ../bbnf-lang && cargo iter-check  # path-patch still resolves under pin
```

## Specific changes (patch-ready)
- `rust-toolchain.toml` — identical to bbnf-lang's `patches/rust-toolchain.toml.draft`.
- `rust/.cargo/config.toml` — remove from `.gitignore`; add:
  ```toml
  [build]
  rustflags = ["-Zthreads=8", "-Zshare-generics=y"]
  [profile.ax-iter]
  inherits = "dev"; opt-level = 0; debug = "line-tables-only"
  incremental = true; codegen-units = 256
  [alias]
  iter-check = "check --profile ax-iter"
  iter-test  = "nextest run --cargo-profile ax-iter"
  ```
- `rust/parse_that/Cargo.toml` `[dev-dependencies]`: drop `bencher`, add `divan`.
- `rust/regex/Cargo.toml` `[dev-dependencies]`: drop `bencher`, add `divan`.
- `rust/bootstrap/Cargo.toml` `[dev-dependencies]`: drop `bencher`, add `divan`.
- `.github/workflows/ci.yml`: add nextest install action; replace test invocation.
