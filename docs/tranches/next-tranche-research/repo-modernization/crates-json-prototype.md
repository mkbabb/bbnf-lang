# crates/json-prototype — Modernization Plan

## Role in the fleet
JSON-only hand-tuned per-shape inline parser prototype. AW-V.W2 speed-ceiling
validation against sonic-rs via twin-pair benches. Establishes the upper
bound on JSON throughput that the general bbnf runtime should approach.

## Current posture (from Wave 1-B assay)
- Workspace member. lib. No features. **1 `[[bench]]`**: `json_value`.
- `[dependencies]`: `tape`, `simd-scan`, `parse_that` (inlines Eisel-Lemire
  `compute_f64` via workspace LTO).
- `[dev-dependencies]`: **`bencher = "0.1"`**, `mimalloc`, **`sonic-rs = "0.3"`**,
  `serde_json`.
- **Version drift**: `sonic-rs = "0.3"` here vs. `sonic-rs = "0.5"` in
  `crates/core`. Two JSON benches compete against each other against
  different sonic-rs versions — the comparison is invalid.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- No `tests/` (prototype-only).
- No proc-macro.

## Target posture
- Inherits fleet pin.
- Migrate 1 bench from `bencher` to `divan`.
- **`sonic-rs` version pinned to match `crates/core`** (`0.5`) so the twin-pair
  comparison is valid. Workspace `[workspace.dependencies]` entry recommended
  for shared version governance.
- No tests added (prototype discipline).

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Bump `sonic-rs` from `0.3` to `0.5` to match core (5 min).
3. Drop `bencher = "0.1"` from `[dev-dependencies]`; add `divan = "0.1"`
   (5 min).
4. Rewrite `benches/json_value.rs` from bencher to divan (1 hour).
5. Centralise `sonic-rs` version in workspace `[workspace.dependencies]`
   (15 min; governance win — prevents future drift).

**Total**: ~1.5 hours.

## Sequencing — when this repo lands
- **Phase A**: item 1 (automatic).
- **Phase B (post-B1, before BA)**: items 2, 3, 4, 5.
- **Phase C**: nothing.

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar; core's sonic-rs pin
  (currently `0.5`).
- **Downstream blocks**: JSON speed-ceiling validity.
- **B1 coupling**: Step 1 indirectly; Step 6 (bench port).

## Risks
- `sonic-rs 0.3 → 0.5` API shift; may require small adaptor changes. Audit
  during the version bump.
- `bencher → divan` port must preserve the AW-V.W2 comparator semantics
  (throughput MB/s vs sonic-rs baseline).
- Prototype status means no CI gate — regressions here land silently. Acceptable
  because this crate IS a research artefact; the headline JSON bench lives
  in `crates/core`.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                                   # json-prototype included
cargo bench -p json-prototype --bench json_value       # divan JSON
# Compare against core's JSON bench: both on sonic-rs 0.5, apples-to-apples.
cargo bench -p bbnf --bench json_value
```

## Specific changes (patch-ready)
- Workspace `Cargo.toml`:
  ```toml
  [workspace.dependencies]
  sonic-rs = "0.5"
  ```
- `crates/json-prototype/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan    = "0.1"
  sonic-rs = { workspace = true }
  # drop: bencher, sonic-rs = "0.3"
  ```
- `crates/core/Cargo.toml`:
  ```toml
  [dev-dependencies]
  sonic-rs = { workspace = true }
  ```
- `crates/json-prototype/benches/json_value.rs` — divan port.
