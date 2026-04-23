# crates/egraph-derive — Modernization Plan

## Role in the fleet
`#[derive(Language)]` proc-macro for the egraph crate. Per
`feedback_derive_language` it projects recursive enum fields into `Id`
references trivially. Consumer count is small (~3–5 sites total across
`bbnf-ir`, `bbnf-regex`, `egraph` tests) — orders of magnitude lighter than
`bbnf_derive`.

## Current posture (from Wave 1-B assay)
- Workspace member. proc-macro lib.
- No features. **No benches. No dev-deps. No tests.**
- `[dependencies]`: `syn`, `quote`, `proc-macro2`.
- Inherits workspace toolchain + `.cargo/config.toml` + `.config/nextest.toml`.
- Consumed by `bbnf-ir` (grammar Language enum), `bbnf-regex` (HIR Language
  enum), `egraph` tests (via dev-dep).
- Minor ICE-cluster contribution — small consumer count, light emission.

## Target posture
- Inherits fleet pin.
- Carries no bench surface of its own (expansion-cost gate for this derive
  is not worth the new bench infrastructure; small consumer count means
  regressions have bounded impact).
- Stays aligned with `bbnf_derive` on stable-symbol-emission discipline (BA
  scope).

## Gap — what must change
1. Inherit workspace pin (0 min; automatic).
2. No active work.

**Total**: 0 hours.

## Sequencing — when this repo lands
- **Phase A**: item 1 (automatic).
- **Phase B**: nothing.
- **Phase C (BA)**: stable-symbol discipline alignment with `bbnf_derive` —
  if the BA-scope symbol-emission work for `bbnf_derive` applies patterns
  that also help `egraph-derive`, propagate.

## Dependencies
- **Upstream blockers**: none.
- **Downstream blocks**: 3 consumer sites (`bbnf-ir`, `bbnf-regex`, `egraph`
  tests). Small surface.
- **B1 coupling**: none direct.

## Risks
- `syn` 2.x / `quote` 1.x / `proc-macro2` 1.x are stable. No known
  nightly-feature interaction.
- ICE contribution is minor but non-zero; pin adoption clears it.

## Verification
```bash
cd bbnf-lang
cargo check -p egraph-derive           # builds
cargo test -p egraph                   # exercises the derive (via dev-dep)
```

## Specific changes (patch-ready)
None for B1. BA may emit a patch if stable-symbol discipline propagates from
`bbnf_derive`.
