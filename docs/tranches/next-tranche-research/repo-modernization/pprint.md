# pprint — Modernization Plan

## Role in the fleet
Pretty-printing substrate. Sibling repo at `/Users/mkbabb/Programming/pprint`.
Provides `pprint` + `pprint_derive` (`#[derive(PrettyPrint)]`). Every
BBNF-consuming crate depends on it (core, gorgeous, parse_that, csp-solver via
derive). Path-patched into bbnf-lang as `pprint` and `pprint_derive`. The only
appurtenant repo in the fleet that publishes an MSRV claim — `rust-version =
"1.85"` — which conflicts with every other repo's implicit ambient nightly.

## Current posture (from Wave 1-B assay)
- HEAD `928c17a7` on `master`; 78 commits; no uncommitted changes. Smallest
  footprint in the fleet.
- Not a workspace. `rust/Cargo.toml` is a plain `[package]`; `rust/derive/` is
  a sibling package.
- `pprint`: lib with 3 features (`default = []`, `regex`, `ser`). **2
  `[[bench]]` entries**: `digit_count.rs`, `pprint.rs`, both depending on
  unstable `test::Bencher` (requires nightly `#![feature(test)]`). No bencher,
  no criterion in `[dev-dependencies]`.
- `pprint_derive`: proc-macro lib.
- `rust-toolchain.toml` **absent**; edition 2024; MSRV `1.85` declared (both
  packages); CI uses nightly anyway.
- `.cargo/config.toml` present at `rust/.cargo/`; `[patch.crates-io]` has
  **exactly one entry** (`bbnf-ser` → `../../bbnf-lang/crates/ser`). Narrowest
  patch table in the fleet. No aliases, no profile overrides.
- `[dev-dependencies]`: `rand`, `pretty`. Benches depend on unstable
  `test::Bencher` — nothing else wires them.
- Integration tests at `rust/tests/`: `builder_tests.rs`, `derive_tests.rs`,
  `digit_count.rs`, `pretty_tests.rs`.
- `.config/nextest.toml`: absent.
- CI: single-job `.github/workflows/ci.yml` with `cargo clippy --workspace`
  + `cargo test --workspace`. Working-directory `rust`. No bench invocation.
- No `scripts/`, no `justfile`, no `Makefile`. Zero ad-hoc surface.

## Target posture
- Pinned nightly identical to bbnf-lang (`nightly-2026-04-11`).
- **Drop the `rust-version = "1.85"` MSRV claim** — conflicts with ambient
  nightly the rest of the fleet requires. Either raise to ambient or delete.
- Divan on the 2 benches. `test::Bencher` eliminated.
- `.config/nextest.toml` installed.
- CI uses nextest.
- `.cargo/config.toml` tracked (already present, per assay; confirm tracking
  status during Phase A).

## Gap — what must change
1. Install `rust-toolchain.toml` with pinned nightly (3 min).
2. Drop `rust-version = "1.85"` from `rust/Cargo.toml` (pprint) and
   `rust/derive/Cargo.toml` (pprint_derive). Alternatively raise to match
   ambient. Recommend drop + documented "MSRV intent is nightly-only for
   fleet coherence" (5 min).
3. Rewrite both benches to divan; remove `#![feature(test)]`; add
   `divan = "0.1"` to `[dev-dependencies]` (1.5 hours).
4. Remove the stale `[profile.bench]` block from `rust/Cargo.toml` (it is
   empty with a commented `# opt-level = 0` left over from an earlier session;
   B1 profile posture comes from `.cargo/config.toml`) (5 min).
5. Install `.config/nextest.toml` mirror of bbnf-lang's (30 min).
6. Update CI workflow to use `cargo nextest run` (15 min).
7. Add alias subset to `rust/.cargo/config.toml` (`iter-check`, `iter-test`)
   (15 min).

**Total**: ~3 hours.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 5, 6, 7 (~1.5 hours). B1
  Step 11 cross-propagation. The MSRV drop is a hard-blocker for fleet nightly
  coherence and must land simultaneously with the pin.
- **Phase B (post-B1, before BA)**: items 3, 4 (~1.5 hours). Divan migration
  on 2 benches; disjoint from bbnf-lang.
- **Phase C**: nothing deferred.

## Dependencies
- **Upstream blockers**: bbnf-lang B1 Step 1 (pin draft).
- **Downstream blocks**: every pprint consumer's proc-macro expansion
  determinism. `pprint_derive` is lower expansion-cost than `bbnf_derive` but
  its consumer count across the fleet is higher (every crate that wants to
  pretty-print).
- **B1 coupling**: Steps 1, 2, 3, 9, 11.

## Risks
- MSRV drop is a minor contract break for any external pprint consumer
  outside the fleet. Check crates.io metadata for current public downloads;
  if non-zero, coordinate the version bump.
- `test::Bencher` removal requires rewriting 2 benches from scratch — the API
  shape differs from divan. Low risk because the benches don't drive CI
  gates anyway (no bench in CI; they are documentation-quality).
- `pprint_derive` is proc-macro — expansion-site count must not regress during
  the dev-dep shuffle (no surface change planned).

## Verification
```bash
cd /Users/mkbabb/Programming/pprint
rustc --version             # pinned nightly
cd rust && cargo iter-check # builds with pprint + pprint_derive
cargo nextest run           # all integration tests pass
cargo bench                 # divan JSON on both benches
cd ../../bbnf-lang && cargo iter-check  # path-patch resolves
```

## Specific changes (patch-ready)
- `rust-toolchain.toml` at repo root — identical to bbnf-lang's draft.
- `rust/Cargo.toml`:
  - Delete `rust-version = "1.85"`.
  - Delete empty `[profile.bench]` block.
  - `[dev-dependencies]`: drop any `test::Bencher` references; add
    `divan = "0.1"`.
- `rust/derive/Cargo.toml`: delete `rust-version = "1.85"`.
- `rust/benches/digit_count.rs` — rewrite to divan.
- `rust/benches/pprint.rs` — rewrite to divan.
- `rust/.cargo/config.toml` — add alias block.
- `rust/.config/nextest.toml` — install.
- `.github/workflows/ci.yml` — nextest-install + nextest-run.
