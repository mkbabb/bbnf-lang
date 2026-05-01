# crates/simd-scan — Modernization Plan

## Role in the fleet
Architecture-neutral SIMD structural-bitmap kernel for the DTA driver.
Builds a StructuralIndex (positions + kinds) once per parse via per-arch
kernels (NEON / AVX2 / AVX-512 / WASM SIMD / portable scalar) with runtime
feature detection.

## Current posture (from Wave 1-B assay)
- Workspace member. lib.
- 2 features: `default = []`, `avx512` (opt-in; requires
  `RUSTFLAGS="-C target-feature=+avx512vbmi2"` per crate comment — additional
  toolchain knob not captured by `rust-toolchain.toml`).
- **1 `[[bench]]`**: `stage1_throughput`. Uses unstable `test::Bencher` API.
- `[dependencies]`: `tape` (default-features=false), `proc-macro2`, `syn`,
  `quote` — **`proc-macro2`/`syn`/`quote` as runtime deps look wrong** for a
  plain lib; W1-B flags this as a probable stale leftover from an earlier
  codegen approach.
- `[dev-dependencies]`: `proptest`.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- `tests/` + proptest-based fuzz.
- No proc-macro defined.

## Target posture
- Inherits fleet pin.
- Migrate the 1 bench from `test::Bencher` to divan.
- **Dependency audit**: drop `proc-macro2`/`syn`/`quote` runtime deps unless
  load-bearing. If they are load-bearing (e.g. compile-time SIMD kernel
  codegen via `build.rs`), they should be `[build-dependencies]` instead of
  `[dependencies]`.
- `avx512` feature documented in `rust-toolchain.toml` via a rustflag hint
  — or separate CI job with the target-feature flag.

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Audit `proc-macro2`/`syn`/`quote` deps (30 min). If unused: drop (5 min).
   If used via `build.rs`: relocate to `[build-dependencies]` (15 min).
3. Add `[dev-dependencies]` with `divan`; add `divan = "0.1"` (5 min).
4. Rewrite `benches/stage1_throughput.rs` from `test::Bencher` to divan
   (1.5 hours).
5. Document `avx512` feature activation in `README.md` / crate top-level
   doc comment: `cargo +pinned build --features avx512 --target ...` with
   required `RUSTFLAGS` (15 min).
6. Add a dedicated CI job for `avx512` feature (deferred to Phase C; the
   main CI runner may not have AVX-512).

**Total**: ~2.5 hours (B1 scope); additional CI work Phase C.

## Sequencing — when this repo lands
- **Phase A**: item 1 (automatic).
- **Phase B (post-B1, before BA)**: items 2, 3, 4, 5.
- **Phase C**: item 6 (dedicated AVX-512 CI job).

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar.
- **Downstream blocks**: DTA driver in `crates/core` relies on this crate;
  regressions here propagate to JSON + CSS throughput benches.
- **B1 coupling**: Step 1 indirectly.

## Risks
- **Runtime `proc-macro2`/`syn`/`quote` audit**: if these deps are actually
  referenced at runtime (not build-time), removing them breaks the crate.
  Audit carefully; `cargo tree` + `rg` in `src/`.
- AVX-512 gating: the target-feature flag is a secondary toolchain concern
  not covered by B1's pin. Document inline; do not force every developer to
  pass the flag.
- `test::Bencher` → divan port must preserve the stage1 throughput measurement
  shape (MB/s). Divan's custom unit support handles this.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                                    # simd-scan included
cargo bench -p simd-scan --bench stage1_throughput      # divan JSON (MB/s)
# AVX-512 path:
RUSTFLAGS="-C target-feature=+avx512vbmi2" cargo +pinned bench -p simd-scan \
    --features avx512 --bench stage1_throughput
```

## Specific changes (patch-ready)
- `crates/simd-scan/Cargo.toml`:
  - Audit `[dependencies]`: drop `proc-macro2`/`syn`/`quote` if unused.
  - If build-time: move to `[build-dependencies]`.
  - Add `divan = "0.1"` to `[dev-dependencies]`.
- `crates/simd-scan/benches/stage1_throughput.rs` — divan port.
- `crates/simd-scan/README.md` or crate doc — AVX-512 activation hint.
