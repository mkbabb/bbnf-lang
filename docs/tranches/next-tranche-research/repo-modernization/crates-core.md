# crates/core (bbnf) — Modernization Plan

## Role in the fleet
The BBNF compiler crate. Depends on parse_that, pprint, bbnf-ir, bbnf-ser,
tape, simd-scan, egraph, csp-solver. Consumes `bbnf_derive`. Hosts every
grammar bench (JSON, CSS L4, Google Sheets, BBNF self-host, WASM, TypeScript
comparators). Densest bench surface in the fleet and primary ICE victim.

## Current posture (from Wave 1-B assay)
- Workspace member of bbnf-lang; inherits workspace toolchain posture
  (no `rust-toolchain.toml`; edition 2024; no MSRV).
- lib (`bbnf`). 1 feature: `dhat-heap = ["dep:dhat"]`. No proc-macro here.
- **19 `[[bench]]` entries**, all `harness = false`, `bencher = "0.1"`.
  The densest bench surface in the fleet.
- `[dev-dependencies]`: `bencher`, `mimalloc`, `sonic-rs`, `serde_json`,
  `serde`, `simd-json`, `jiter`, `serde_json_borrow`, `nom`, `winnow`, `pest`,
  `pest_grammars`, `tree-sitter`, `tree-sitter-json`, `cssparser`,
  `lightningcss = "1.0.0-alpha.71"`, `wasmtime`, `fast-float2`.
- Inherits workspace `.cargo/config.toml`; inherits `.config/nextest.toml`.
- Integration tests at `tests/` — 25+ parity harnesses (sonic_rs_parity,
  lightningcss_parity, css_l4_parity, value_api_apples_to_apples, etc.).
- CI exercises `cargo test -p bbnf --test sonic_rs_parity --release` and
  `--test lightningcss_parity --release` as heavy close gates.
- Consumes `bbnf_derive` at ~30+ sites across `tests/`, `benches/`,
  `examples/`, `src/runtime/mod.rs`, `src/runtime/parsed.rs`. Primary
  contributor to the 93-ICE cluster (`on_disk_cache.rs:663`).
- Benches drive through workspace `prep-bench` / `final-bench` aliases.

## Target posture
- Inherits fleet toolchain pin.
- All 19 benches on divan; `bencher` removed.
- `sonic-rs` version reconciled across core + json-prototype (currently `0.5`
  in core, `0.3` in json-prototype — see json-prototype plan).
- `iter-check` alias continues to include core (it already does).
- iai-callgrind secondary surface adopted for the JSON + CSS headline benches
  (per B1 §1.2 CI-only mode).

## Gap — what must change
1. Inherit workspace `rust-toolchain.toml` (0 min — handled by B1 Step 1).
2. Drop `bencher = "0.1"` from `Cargo.toml:40`; add `divan = "0.1"` +
   `iai-callgrind = { version = "0.14", optional = true }` (5 min).
3. Port 19 benches to divan (B1 Step 5 + Step 6 — ~1 agent-day).
4. Add `iai = ["iai-callgrind"]` feature and `[[bench]] name = "json_callgrind"`
   entry (per B1 §1.2; ~2 hours).
5. Reconcile `sonic-rs` version with json-prototype (5 min — likely bump
   json-prototype to `0.5`).
6. After ICE cluster clears (B1 Step 1 + toolchain pin), re-run one full
   `cargo iter-check-full` cold to establish the post-B1 baseline ceiling
   (~10 min wall).

**Total**: ~1 agent-day + half-day for iai-callgrind integration.

## Sequencing — when this repo lands
- **Phase A (during bbnf-lang B1)**: items 1, 2, 3, 4, 5. **This IS B1 Step
  5 + 6 + 8**; the bulk of B1's divan migration lives in this crate.
- **Phase B**: item 6 (ceiling capture); happens at B1 Step 12 close.
- **Phase C**: bench-architecture split (deferred to BA per B1 §6).

## Dependencies
- **Upstream blockers**: none — core owns this work.
- **Downstream blocks**: the divan migration exemplar (B1 Step 5) is a
  core bench (`compile_pipeline.rs`). Every other repo's divan port follows
  that pattern.
- **B1 coupling**: Steps 5, 6, 7, 8, 9.

## Risks
- ICE cluster compounding: as long as `on_disk_cache.rs:663` triggers,
  incremental re-compilation of 19 re-shaped bench files will likely re-fire
  the panic. **Pin MUST land before the 19-bench port starts.**
- Divan parity within ±5% vs bencher baseline — B1 §5.4 risk register.
  Measured on exemplar first; port remaining 18 only if parity holds.
- 30+ derive sites are re-expanded on every bench edit. The cumulative
  cost of 19 bench ports with re-expansion is the dominant B1.W0 wall.
- `feedback_bench_single_run`: every divan invocation must be one command.
  Makefile target `bench-json` delegates; do not loop.

## Verification
```bash
# From bbnf-lang root:
cargo iter-check                                       # compiles
cargo nextest run -p bbnf                              # tests pass
cargo bench -p bbnf --bench compile_pipeline           # divan exemplar
cargo bench-json > docs/benchmarks/post-B1-W0-divan-json.json
DIVAN_BENCH_FORMAT=json cargo bench -p bbnf --bench json_value
# iai-callgrind CI:
cargo bench -p bbnf --features iai --bench json_callgrind
```

## Specific changes (patch-ready)
- `crates/core/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan         = "0.1"
  iai-callgrind = { version = "0.14", optional = true }
  # drop: bencher = "0.1"
  sonic-rs      = "0.5"  # reconcile with json-prototype

  [features]
  iai = ["iai-callgrind"]

  [[bench]]
  name    = "json_callgrind"
  harness = false
  required-features = ["iai"]
  ```
- 19 bench files under `crates/core/benches/`: mechanical divan port per
  `docs/tranches/B1/patches/divan-migration.md §Migration order`.
- `crates/core/benches/common/timeout.rs`: update shim signature for divan.
