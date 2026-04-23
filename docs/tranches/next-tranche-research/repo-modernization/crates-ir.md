# crates/ir (bbnf-ir) — Modernization Plan

## Role in the fleet
Canonical Grammar IR for the BBNF compiler pipeline. Hosts the grammar
e-graph rewrite rules and CSP-scheduled pass manager. Depends on `bbnf-regex`
(leaf classification e-graph), `egraph`, `egraph-derive`, `csp-solver`.
Paired with `parse-that/rust/regex/` per `feedback_regex_crate_isomorphic` —
the same substrate at both ends of the HIR→DFA pipeline.

## Current posture (from Wave 1-B assay)
- Workspace member. lib (`bbnf-ir`). No features. **No benches.**
- `[dependencies]`: `serde` (`rc` feature), `serde_json`, `rmp-serde`,
  `parse_that`, `bbnf-regex` (serde), `rustc-hash`, `rayon`, `smallvec`,
  `csp-solver`, `egraph`, `egraph-derive`.
- No `[dev-dependencies]`; relies on `cargo test --workspace`.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Workspace-level `[profile.dev]` gives `bbnf-ir` `opt-level = 1` explicitly
  (elevated because IR passes are hot in dev iteration).
- Consumes `egraph-derive` (the `#[derive(Language)]` per
  `feedback_derive_language`). No proc-macro sites defined here.
- Included in `iter-check`; indirectly implicated in the ICE cluster via
  Language-derive sites (small — ~3 sites only; minor contributor).

## Target posture
- Inherits fleet pin.
- Adds **divan e-graph saturation benches** (`add`, `congruence`,
  `apply_rules`, `extract`) as a local bench surface. Currently this work
  is buried inside `crates/core/benches/compile_pipeline.rs` — divided
  attention means the e-graph's own perf is invisible at fine grain.
  `feedback_general_infra_crates` demands a stand-alone bench surface.
- Continues to inherit `.config/nextest.toml` and workspace aliases.

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Add `[dev-dependencies]` with `divan`; add `benches/ir_egraph.rs` with
   per-pass fine-grain benches (saturation, rewrite-fire-rate, extract
   cost) (3–4 hours; new work).
3. Add `[[bench]]` entries (5 min).
4. Decide on iai-callgrind coverage for e-graph hot loops (secondary;
   instruction-count gate is the cleanest signal here) (1 hour).

**Total**: ~half-day.

## Sequencing — when this repo lands
- **Phase A**: item 1 (inheritance, automatic).
- **Phase B (post-B1, before BA)**: items 2, 3, 4. The e-graph bench surface
  is a B2-scope addition.
- **Phase C**: nothing deferred.

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar from core (Step 5).
- **Downstream blocks**: none directly; fleet benefits from finer IR perf
  signal.
- **B1 coupling**: Step 1 (pin). New bench surface is post-B1.

## Risks
- `feedback_regex_crate_isomorphic`: `bbnf-regex`'s internal
  egraph+csp architecture must not be perturbed by IR-side benching. Keep
  the new benches scoped to IR passes only — do not cross-import bbnf-regex
  bench fixtures.
- `feedback_general_infra_crates`: egraph + csp-solver are general-infra
  crates; their bench surfaces should live there, not inherit from IR.
  IR benches must be about the *grammar* Language impl, not the general
  substrate.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                    # includes bbnf-ir
cargo bench -p bbnf-ir --bench ir_egraph  # divan JSON emitted
```

## Specific changes (patch-ready)
- `crates/ir/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan = "0.1"

  [[bench]]
  name    = "ir_egraph"
  harness = false
  ```
- `crates/ir/benches/ir_egraph.rs` — new file; saturation / rewrite / extract
  sub-benches driven by divan, one-command invocation per
  `feedback_bench_single_run`.
