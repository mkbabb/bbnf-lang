# crates/gorgeous (workspace copy) — Modernization Plan

## Role in the fleet
Authoritative `gorgeous` source path-patched into the workspace. Mirrors the
sibling `/Users/mkbabb/Programming/gorgeous` but diverges in biome pins
(uses monorepo `cli/v1.9.4` tag vs. sibling's `=0.4.0`). Lives in-tree
because the derive-Parser aggregate here is the heaviest compile-gate site
in the fleet; pulling it into the workspace makes `cargo expand` +
`prep-bench` flows coherent. Excluded from `iter-check` due to the 6
aggregated `#[derive(Parser)]` sites that dominate compile wall.

## Current posture (from Wave 1-B assay)
- Workspace member. lib + bin (`gorg`). Same SHA as bbnf-lang.
- **8 features**: `default` (re-exports all 6 grammar features + `bbnf-grammar`),
  `bbnf-grammar`, `json-grammar`, `css-grammar`, `ebnf-grammar`, `bnf-grammar`,
  `sheets-grammar`, `bin-full`, `vm`. Per-grammar feature gating IS the
  compile-cost knob.
- **No benches** in this copy (sibling has 2; benches live at
  `/Users/mkbabb/Programming/gorgeous/benches/`).
- `[dev-dependencies]`: `biome_css_parser`, `biome_css_formatter`,
  `biome_formatter`, `biome_css_syntax` — all pinned via git tag
  `cli/v1.9.4` (dodges the 0.5.7/0.5.8 rowan skew that breaks crates.io
  published builds on modern nightly).
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- EXCLUDED from `iter-check` alias per the alias rationale ("≥1
  `#[derive(Parser)]` site that triggers the full bbnf-derive proc-macro
  pipeline"). Covered only at workspace-test time.
- Consumes `bbnf_derive` at 6 sites (one per grammar feature). Each site
  gated by its feature flag — enabling only `json-grammar` pays only that
  site's ~30 k-LOC expansion.
- Primary contributor to the 93-ICE cluster (6 aggregated Parser sites =
  highest AttrId churn in the workspace).

## Target posture
- Inherits fleet pin.
- **Benches continue to live in the sibling** IF the sibling is re-tracked;
  if the sibling is retired (recommended per `gorgeous-sibling.md`), the
  2 benches MIGRATE INTO THIS WORKSPACE COPY at `crates/gorgeous/benches/`.
- `iter-check` EXCLUSION remains (6-site expansion wall is structural).
- Biome pin stays on `cli/v1.9.4` monorepo tag until biome publishes a
  rowan-compatible release.

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. **If sibling retires** (recommended disposition): copy the 2 sibling
   benches (`gorgeous.rs`, `competitors.rs`) into `crates/gorgeous/benches/`;
   migrate from `bencher` to `divan`; add `divan` to `[dev-dependencies]`;
   add 2 `[[bench]]` entries. (2 hours).
3. Evaluate whether the 6-grammar-feature gate admits a bench harness that
   selectively compiles features for per-grammar cost measurement (deferred;
   bench-architecture refactor is BA-scope).
4. iai-callgrind coverage deferred (this crate's compile-wall dominates;
   the runtime perf is less critical than the compile cost).

**Total (B1 scope)**: ~2 hours if benches migrate; 0 otherwise.

## Sequencing — when this repo lands
- **Phase A**: item 1 (inheritance). No active work.
- **Phase B (post-B1, before BA)**: item 2 — bench migration, conditional on
  sibling disposition.
- **Phase C**: item 3 (bench-architecture refactor).

## Dependencies
- **Upstream blockers**: sibling disposition decision (retire vs. re-track).
- **Downstream blocks**: the 6-site expansion wall is load-bearing for every
  workspace clean-build. B1 pin MUST be validated against this crate before
  B1 Step 12 declares close.
- **B1 coupling**: Step 1 indirect; Step 12 close-gate.

## Risks
- Biome `cli/v1.9.4` tag is dead-upstream; if the tag disappears or the
  upstream repo is archived, pin breaks. Mirror the subrepo locally as
  insurance (deferred governance issue).
- The 6-site expansion wall is the highest AttrId churn; any pin bump or
  nightly change must smoke-test this crate specifically.
- Sibling-vs-workspace divergence: if the sibling is re-tracked, biome pin
  discipline diverges; workspace stays on `cli/v1.9.4`, sibling on `=0.4.0`.
  Recommend retirement precisely to eliminate the divergence.

## Verification
```bash
cd bbnf-lang
cargo build -p gorgeous --features json-grammar   # minimum expansion
cargo build -p gorgeous --features default        # full 6-site expansion
cargo nextest run -p gorgeous                     # integration tests
# If benches migrate:
cargo bench -p gorgeous --bench gorgeous
cargo bench -p gorgeous --bench competitors
```

## Specific changes (patch-ready)
- `crates/gorgeous/Cargo.toml` (conditional on sibling retirement):
  ```toml
  [dev-dependencies]
  divan = "0.1"

  [[bench]]
  name    = "gorgeous"
  harness = false

  [[bench]]
  name    = "competitors"
  harness = false
  ```
- `crates/gorgeous/benches/gorgeous.rs` — migrated from sibling, divan-port.
- `crates/gorgeous/benches/competitors.rs` — migrated from sibling, divan-port.
