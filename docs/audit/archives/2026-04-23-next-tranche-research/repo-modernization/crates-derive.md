# crates/derive (bbnf_derive) — Modernization Plan

## Role in the fleet
Proc-macro emitter for `#[derive(Parser)]`. Emits ~30 k-LOC TokenStream per
site. Consumed by every grammar consumer in the fleet (~87 sites total across
workspace + siblings). Single largest compile-time driver in the ecosystem
and primary source of the `on_disk_cache.rs:663 — cannot decode AttrId`
incremental cache panic.

## Current posture (from Wave 1-B assay)
- Workspace member. `proc-macro = true`. No features. No benches.
- `[dependencies]`: `bbnf`, `bbnf-ir`, `pprint`, `parse_that`, `syn 2`,
  `quote 1`, `proc-macro2 1`, `indexmap 2`.
- `build.rs` present (rare in the workspace).
- No dev-deps, no `tests/` — exercised transitively via every consumer.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Content-keyed cache at `target/.bbnf-cache/` — the BA lift target
  (per `project_at_tranche` and B1 §6 Deferred).
- Directly implicated in the 93-ICE cluster: every edit to a consumer
  grammar re-emits a differently-shaped TokenStream, shifts AttrId space,
  occasionally the decoder panics mid-load.

## Target posture
- Inherits fleet pin (eliminates nightly-drift component of ICE firing).
- Carries a **divan-based expansion-cost regression gate** — the single
  new bench surface introduced to the fleet. Measures derive-expansion wall
  on a canonical grammar corpus and fires a regression on a ±10% delta.
- Content-keyed cache relocates to `$XDG_CACHE_HOME` (deferred to BA per
  B1 §6).
- Optional: stable symbol emission reduces incremental-cache churn
  (architectural ask; deferred to BA).

## Gap — what must change
1. Inherit workspace pin (0 min).
2. Add `[dev-dependencies]` with `divan`; new `benches/expansion_cost.rs`
   that drives `bbnf_derive` against a canonical grammar corpus (~4–6
   hours; new work, no precedent).
3. Add `[[bench]]` entry for `expansion_cost` (5 min).
4. Deferred (BA): content-keyed cache lift to `$XDG_CACHE_HOME`; watt
   WASM precompilation (2–4 weeks — BA/BB).
5. Deferred (BA): stable AttrId-friendly symbol emission.

**Total (B1 scope)**: ~half-day for the expansion-cost gate.

## Sequencing — when this repo lands
- **Phase A**: item 1 (pin inheritance); zero work in this crate.
- **Phase B (post-B1, before BA)**: items 2, 3 — the expansion-cost gate.
  Agent 2's tranche AT budget permits this.
- **Phase C (BA)**: items 4, 5 — structural changes with multi-week scope.

## Dependencies
- **Upstream blockers**: bbnf-lang B1 Steps 1, 2 (pin + profile).
- **Downstream blocks**: every grammar consumer's build determinism. Fleet
  ICE elimination depends on this crate and the pin jointly.
- **B1 coupling**: indirectly Step 1 (pin); not a direct step.

## Risks
- Expansion-cost gate's corpus must be stable and representative. Starting
  corpus: JSON grammar (medium), CSS L4 (heavy), BBNF self-host (light). If
  the gate's corpus is too narrow, regressions escape it.
- Deferred items (4, 5) are BA-tranche-scoped; attempting them in B1 breaks
  the bounded-annex remit.
- `feedback_no_deferrals` applies to **in-scope** optimizations; the BA-routed
  items are structural changes outside B1 scope and are correctly deferred
  per B1 §7.

## Verification
```bash
cd bbnf-lang
cargo bench -p bbnf_derive --bench expansion_cost
# Regression gate lives in divan's JSON output; ci compares to baseline.
cargo iter-check  # after pin lands, ICE count stable at zero over a
                  # 30-minute dev session
```

## Specific changes (patch-ready)
- `crates/derive/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan = "0.1"

  [[bench]]
  name    = "expansion_cost"
  harness = false
  ```
- `crates/derive/benches/expansion_cost.rs` — new file; drives derive
  expansion against a 3-grammar corpus (JSON / CSS L4 / BBNF self-host).
- Deferred design captured in `docs/tranches/B1/patches/derive-cache-design.md`
  (B1 §9 patch-file index).
