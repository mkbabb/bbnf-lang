# crates/ser (bbnf-ser) — Modernization Plan

## Role in the fleet
Grammar-guided Serializer/Deserializer traits. Smallest lib in the workspace.
Consumed optionally by pprint (via its `ser` feature). Zero modernization
surface.

## Current posture (from Wave 1-B assay)
- Workspace member. lib. No features. **No benches. No dev-deps. No tests.**
- `[dependencies]`: `ryu = "1"`, `itoa = "1"` — numeric formatting only.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Included in `iter-test-leaf` alias (tagged leaf-tier crate).
- No proc-macro, no ICE liability, no ad-hoc scripts.
- **Possibly the only crate in the fleet with zero modernization surface.**

## Target posture
- Inherits fleet pin (automatic; zero action).
- Remains test-less and bench-less. The surface is tiny enough that
  unit-level coverage from consumers (`pprint`, `bbnf-lang/tests/*`) is
  adequate. Adding tests here for its own sake is wasted.

## Gap — what must change
1. Inherit workspace `rust-toolchain.toml` (0 min).

**Total**: 0 hours of active work.

## Sequencing — when this repo lands
- **Phase A**: item 1 (inheritance). No explicit action.
- **Phase B**: nothing.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: none.
- **Downstream blocks**: none. pprint's `ser` feature consumes this
  passively.
- **B1 coupling**: none direct.

## Risks
- `ryu` and `itoa` are stable crates.io dependencies; no known nightly
  interaction. No risk surface.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf   # includes bbnf-ser
cargo check -p bbnf-ser
```

## Specific changes (patch-ready)
None. The crate is already in a terminal state of modernization.
