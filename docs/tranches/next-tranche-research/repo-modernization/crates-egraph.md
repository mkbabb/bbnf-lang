# crates/egraph — Modernization Plan

## Role in the fleet
General-purpose e-graph substrate: equality saturation, rewrite rules,
cost-model extraction. Per `feedback_general_infra_crates` this is
deliberately general-purpose, not a bbnf-specific module. Consumed by
`bbnf-ir` (grammar e-graph) and `bbnf-regex` (HIR e-graph, via path-patch
to parse-that). Pairs with `egraph-derive` for the `#[derive(Language)]`
proc-macro per `feedback_derive_language`.

## Current posture (from Wave 1-B assay)
- Workspace member. lib. No features. **No benches.**
- `[dependencies]`: `smallvec`, `rustc-hash`, `csp-solver`.
- `[dev-dependencies]`: `egraph-derive` (path) — exercises the Language
  derive in tests.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Included in `iter-test-leaf` alias.
- `tests/` present.
- No proc-macro defined; consumes `egraph-derive` in dev-deps only.
- No ICE liability directly.

## Target posture
- Inherits fleet pin.
- **Gains its own divan bench surface** — per `feedback_general_infra_crates`
  + `feedback_csp_always_optimize`, foundational general infrastructure needs
  stand-alone bench gates, not implicit coverage via downstream `core`.
- Adds benches for: `add` (hashcons + deduplication), `congruence` (union-find
  + class-merge propagation), `apply_rules` (pattern-match firing rate),
  `extract` (cost-model DP extraction).
- Added surfaces do not cross-pollute with domain-specific fixtures from
  bbnf-ir or bbnf-regex. The bench corpus uses synthetic Language impls.

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. Add `[dev-dependencies]` with `divan`; add `benches/egraph.rs` with 4
   sub-benches (saturate, add, congruence-close, extract) (3–4 hours; new
   work).
3. Add `[[bench]]` entry (5 min).
4. Evaluate iai-callgrind coverage for the congruence-close hot loop —
   canonical `uf.union(a, b) + rebuild()` kernel; instruction-count delta is
   a cleaner signal than wall on this substrate. Deferred (half-day).

**Total**: ~half-day (B1 scope) + half-day iai-callgrind (Phase C).

## Sequencing — when this repo lands
- **Phase A**: item 1 (inheritance; automatic).
- **Phase B (post-B1, before BA)**: items 2, 3. Divan bench surface lands.
- **Phase C (BA or later)**: item 4 — iai-callgrind.

## Dependencies
- **Upstream blockers**: B1 pin; divan exemplar from core (B1 Step 5).
- **Downstream blocks**: `feedback_csp_always_optimize` invariant — without
  a bench gate here, foundational CSP-driven saturation regressions escape.
- **B1 coupling**: Step 1 indirectly; new bench surface is post-B1.

## Risks
- Bench corpus must be stable and representative of the downstream
  consumers' Language shapes. A corpus biased toward `bbnf-ir`'s grammar
  Language would bias saturation metrics. Use synthetic Language
  (e.g. arithmetic-expression trees) per `feedback_general_infra_crates`.
- `smallvec` + `rustc-hash` are stable. No known nightly interaction.
- Decoupling the bench surface from downstream consumers means the numbers
  may not match end-to-end perf; document that explicitly in the bench
  file headers.

## Verification
```bash
cd bbnf-lang
cargo iter-test-leaf                    # egraph included
cargo bench -p egraph --bench egraph    # divan JSON
```

## Specific changes (patch-ready)
- `crates/egraph/Cargo.toml`:
  ```toml
  [dev-dependencies]
  divan          = "0.1"
  egraph-derive  = { path = "../egraph-derive" }   # already present

  [[bench]]
  name    = "egraph"
  harness = false
  ```
- `crates/egraph/benches/egraph.rs` — new file; synthetic Language impl;
  4 sub-benches via divan attributes; one-command invocation.
