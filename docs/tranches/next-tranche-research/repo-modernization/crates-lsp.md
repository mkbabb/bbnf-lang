# crates/lsp (bbnf-lsp) — Modernization Plan

## Role in the fleet
Language-server binary for BBNF grammar files. Shipped as the VSCode
extension's server backend. Depends on `bbnf`, `bbnf-ir`, `bbnf-analysis`.
Hosts the most infrastructure-heavy CI job in the fleet — a 5-target release
matrix (Linux x86_64/aarch64, macOS x86_64/aarch64, Windows x86_64).

## Current posture (from Wave 1-B assay)
- Workspace member. lib + bin (`bbnf-lsp`). No features. No benches.
- `[dependencies]`: `tower-lsp-server = "0.23"`, `tokio` (full), `serde`,
  `serde_json`, `bbnf`, `bbnf-ir`, `bbnf-analysis`.
- `[dev-dependencies]`: `tempfile`. Integration tests in `tests/` (not
  enumerated).
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- EXCLUDED from `iter-check` (alongside analysis, gorgeous, bootstrap).
  Compile-gate coverage happens only at workspace-test time.
- `bbnf-lang/.github/workflows/release.yml` is primarily a `cargo build
  --release -p bbnf-lsp` matrix — most CI infra in the fleet.
- `Makefile` at workspace root has `build-lsp` / `dev` targets that copy
  `target/release/bbnf-lsp` to `server/`.
- No proc-macro defined; indirect `bbnf_derive` consumption via `bbnf`.
- No direct ICE liability.

## Target posture
- Inherits fleet pin.
- `release.yml` matrix adopts the pinned nightly on **every** target (Linux,
  macOS, Windows) so release builds do not drift from dev builds. Currently
  release uses `@nightly` ambient.
- Remains EXCLUDED from `iter-check` (derive-Parser sites inside `bbnf`
  transitively make this expensive; exclusion stays).
- Benches: none added (LSP is I/O-bound; bench gate would be noisy and
  uninformative).

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Update `.github/workflows/release.yml` to use the pinned nightly instead
   of `@nightly` ambient. 5-target matrix edit (30 min).
3. Rewrite `Makefile` targets `build-lsp` + `dev` to delegate to the new
   cargo alias surface (after B1 Step 4 lands) (15 min).
4. Install `tempfile`-friendly nextest test-grouping (already inherited;
   verify during B1 Step 3) (5 min).

**Total**: ~1 hour.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 3, 4. The release.yml pin
  update MUST land with the pin or release builds drift.
- **Phase B**: nothing.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: B1 Step 1 (pin); Step 4 (Makefile rewrite).
- **Downstream blocks**: VSCode extension release cadence. If the pin
  behaves differently on Windows x86_64, extension releases stall until
  the pin is validated on all 5 targets.
- **B1 coupling**: Steps 1, 4, 9 (CI workflow update).

## Risks
- Pinned nightly's Windows x86_64 support is not yet verified. B1 Step 1
  defaults to macOS arm64 + Linux x86_64. Before release.yml adopts the pin,
  dry-run the matrix on all 5 targets.
- `tokio` full-features pulls a large transitive graph; any subtle
  nightly-feature shift in tokio's async codegen has surfaced as Windows
  compile regressions historically. Detect early.
- LSP being excluded from `iter-check` means compile regressions surface
  at workspace-test time — increase release-gate vigilance.

## Verification
```bash
cd bbnf-lang
cargo build --release -p bbnf-lsp   # Linux
cargo test -p bbnf-lsp              # tempfile-driven tests
make build-lsp                      # copies to server/
# CI matrix: all 5 targets green on the pinned nightly.
```

## Specific changes (patch-ready)
- `.github/workflows/release.yml`:
  ```yaml
  # replace:
  - uses: dtolnay/rust-toolchain@nightly
  # with:
  - uses: dtolnay/rust-toolchain@master
    with:
      toolchain: nightly-2026-04-11
      components: rust-src, clippy, rustfmt
  ```
- `Makefile`: `build-lsp` target becomes `cargo build --release -p bbnf-lsp`
  routed through the new alias scheme (per B1 `Makefile.draft`).
- Inherit nextest.
