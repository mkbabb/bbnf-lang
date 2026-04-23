# crates/analysis (bbnf-analysis) — Modernization Plan

## Role in the fleet
Pure analysis logic for BBNF grammars; shared by LSP + WASM. On the
deprecation glide-path per `project_analysis_consolidation` — AST analysis
is consolidating out of this crate into IR passes as the single source of
truth. Continuing to modernize deprecated code wastes effort.

## Current posture (from Wave 1-B assay)
- Workspace member. lib (`bbnf-analysis`). No features. No benches.
- `[dependencies]`: `ls-types`, `bbnf`, `bbnf-ir`, `pprint`, `indexmap`,
  `self_cell = "1.2.2"`.
- No `[dev-dependencies]`; relies on workspace-level test harness only.
- Workspace `iter-check` alias EXCLUDES `bbnf-analysis` (alongside gorgeous,
  bootstrap, lsp) — so compile-gate iteration does not cover this crate.
- Included in `cargo clippy --all-targets` CI sweep.
- No proc-macro consumption or definition.
- No ad-hoc scripts, no custom profile overrides.

## Target posture
**Recommended: HOLD.** Do not modernize until `project_analysis_consolidation`
completes. Post-consolidation state is deletion of this crate entirely; the
analysis logic moves to IR passes. Inheriting the workspace pin is free (it
happens automatically); active migration work is wasted.

If consolidation slips past tranche BA, revisit.

## Gap — what must change
1. Inherit workspace `rust-toolchain.toml` (0 min; automatic).
2. Re-include in `iter-check` alias ONLY IF consolidation completes and this
   crate becomes part of the active compile-gate surface; otherwise let it
   remain excluded (0 min — decision deferred).
3. When consolidation completes, DELETE the crate entirely (part of a
   separate tranche, not B1 scope).

**Total (B1 scope)**: 0 hours.

## Sequencing — when this repo lands
- **Phase A**: nothing.
- **Phase B**: nothing.
- **Phase C (during analysis-consolidation tranche, post-BA)**: crate
  deletion. Out of scope for fleet modernization.

## Dependencies
- **Upstream blockers**: none.
- **Downstream blocks**: LSP + WASM consumers; will migrate when the
  consolidation ships.
- **B1 coupling**: none direct. Inherits passively.

## Risks
- If the consolidation fails to ship in the BA window, this crate remains
  a silent blind spot in `iter-check` coverage. Revisit at BA close.
- `feedback_analysis-consolidation`: the deletion is architectural truth
  already agreed. Patching cosmetic modernization delays that deletion.

## Verification
```bash
cd bbnf-lang
cargo clippy -p bbnf-analysis --all-targets -- -D warnings  # baseline green
# After B1: the pin inheritance does not change anything observable here.
```

## Specific changes (patch-ready)
None. This crate is a deprecation target; modernization work is tracked
elsewhere (analysis-consolidation tranche). The fleet-modernization plan
explicitly excludes it from every Phase.
