# crates/tape — Modernization Plan

## Role in the fleet
Tape representation for bbnf-lang parser output — the eager-AST replacement.
Columnar (Struct-of-Arrays) record substrate. Per `feedback_preserve_rich_ast`
its presence is the ONE sanctioned speed optimisation that doesn't sacrifice
AST richness.

## Current posture (from Wave 1-B assay)
- Workspace member. lib.
- 2 features: `default = ["rayon"]`, `dta-replay` (AW.1.7 — decision log +
  resumable snapshot; off by default so LLVM has no hot-path branch to hoist).
- **1 `[[bench]]`**: `reduce_column` (AW-IV.W5.1 — "≥ 6× SIMD speedup over
  scalar left-fold" hard gate). Uses unstable `test::Bencher` API —
  `harness = false` set but no framework wired in; bench must provide its
  own main.
- `[dependencies]`: `serde` (optional), `rayon` (optional).
- `[dev-dependencies]`: empty.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Included in `iter-test-leaf` alias.
- No proc-macro, no ICE liability.

## Target posture
- Inherits fleet pin.
- Migrate the 1 bench from unstable `test::Bencher` to divan.
- Preserve the "≥ 6× SIMD speedup over scalar left-fold" AW-IV.W5.1 hard gate
  — encoded as a divan `#[divan::bench]` with a gate check that fails if
  the ratio drops below 6×.
- Tests stay inherited; no new test surface.

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Add `[dev-dependencies]` with `divan`; add `divan = "0.1"` (5 min).
3. Rewrite `benches/reduce_column.rs` from `test::Bencher` to divan; remove
   `#![feature(test)]` (1.5 hours).
4. Encode the 6× SIMD gate as a divan post-run assertion (45 min).

**Total**: ~2.5 hours.

## Sequencing — when this repo lands
- **Phase A**: item 1 (automatic).
- **Phase B (post-B1, before BA)**: items 2, 3, 4.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar (B1 Step 5).
- **Downstream blocks**: AW-IV.W5.1 hard gate discipline. If the 6× SIMD
  gate goes uncovered, the SIMD path can silently regress.
- **B1 coupling**: Step 1 indirectly.

## Risks
- `test::Bencher` removal + `#![feature(test)]` removal eliminates the
  nightly-only surface for this crate. Low risk because tape has no
  downstream MSRV consumers.
- Divan's sample-body split must faithfully preserve the scalar-vs-SIMD
  two-variant measurement. Post-port, compare against the pre-port wall
  within ±5% (B1 §5.4 risk register applies).
- The 6× gate must fire on regression; encode it in divan's JSON post-processing
  or as an inline `assert!` inside the bench function body.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                                      # tape included
cargo bench -p tape --bench reduce_column                 # divan JSON
# Gate check:
cargo bench -p tape --bench reduce_column -- --simd-gate  # asserts ≥6× ratio
```

## Specific changes (patch-ready)
- `crates/tape/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan = "0.1"
  ```
- `crates/tape/benches/reduce_column.rs`:
  - Remove `#![feature(test)]` + `extern crate test`.
  - Replace `#[bench] fn (b: &mut test::Bencher)` with divan attributes.
  - Emit scalar + SIMD as two divan-registered fns; post-run assertion
    enforces SIMD/scalar ratio ≥ 6×.
