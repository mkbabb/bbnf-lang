# AZ-II.cutover.M - Non-BBNF Resolver Fleet Activation
**Opens after**: AZ-II.cutover.L close
**Agents**: up to 10 parallel
**Hard gate**: CSV, Math, BNF, and CSS Pretty flip to StructDirect; EBNF blocker is named and routed to O2.
**Status**: complete

## Scope

1. Activate StructDirect resolver arms for CSV.
2. Activate StructDirect resolver arms for Math.
3. Activate StructDirect resolver arms for BNF.
4. Activate StructDirect resolver arms for CSS Pretty.
5. Expand AltDispatch StructDirect emission for literal, regex, and
   structural `Seq` branch families.
6. Regenerate all nine grammars and prove idempotence.
7. Leave EBNF deferred on the high-branch / structural branch routing
   blocker and name the follow-on.
8. Update FINAL, PROGRESS, and post-AZ-II placeholder docs to reflect
   8/9 active fleet state.

## File bounds

| File | Access |
|---|---|
| `crates/ir/src/registry/strategy.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs` | modify |
| `crates/core/src/grammar/generated/*.rs` | modify |
| `docs/tranches/AZ-II/FINAL.md` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/benchmarks/post-AZ-II.json` | modify |

**Do NOT touch**: EBNF resolver activation, `Parsed<R>` deletion,
`crates/tape/` deletion, terminal bench truth. Deployment invariant:
resolver and emitter agents use fully-contained worktrees; orchestrator
owns final full-fleet regen.

## Phase sub-items

### AZ-II.cutover.M.1 Resolver Arm Activation

Mechanism: flip CSV, Math, BNF, and CSS Pretty resolver arms to their
StructDirect builders/documents.

Files touched: `crates/ir/src/registry/strategy.rs`.

Sub-gate: resolver tests show the four grammars no longer use
TapeDirect.

### AZ-II.cutover.M.2 AltDispatch Literal and Regex Branches

Mechanism: emit byte comparisons plus `push_leaf_with_unit()` and
branch tags for literal/regex AltDispatch branches.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`.

Sub-gate: BBNF type-name and CSS pseudo-class branches do not collapse
to no-op loops.

### AZ-II.cutover.M.3 AltDispatch Structural Seq Branches

Mechanism: add initial structural `Seq` support sufficient for the
activated non-EBNF fleet.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/alt_dispatch/branches.rs`.

Sub-gate: all activated grammars compile after regen.

### AZ-II.cutover.M.4 Full Fleet Regen

Mechanism: run canonical regen across all nine grammars.

Files touched: `crates/core/src/grammar/generated/*.rs`.

Sub-gate: `cargo xtask regen --check` is clean.

### AZ-II.cutover.M.5 EBNF Deferral

Mechanism: document EBNF's high-branch literal and structural branch
layout-routing gap as the only remaining resolver-arm blocker.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: EBNF has a named follow-on route to cutover.O.

### AZ-II.cutover.M.6 Close Docs

Mechanism: refresh FINAL/PROGRESS and post-AZ-II placeholder state for
the 8/9 fleet.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`,
`docs/benchmarks/post-AZ-II.json`.

Sub-gate: docs do not claim tape deletion or terminal benchmarks.

## Hard gate

1. CSV, Math, BNF, and CSS Pretty resolve StructDirect.
2. AltDispatch literal/regex/seq branches emit non-placeholder code.
3. Full generated fleet is idempotent.
4. EBNF blocker is explicitly documented and routed.
5. FINAL remains partial and names terminal blockers.

## Verification artefacts

- Commits `a29a1265`, `43f0795b`.
- `docs/tranches/AZ-II/FINAL.md`.
- `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.L
- **Blocks**: AZ-II.cutover.N, AZ-II.cutover.O

## Archaeology

cutover.E authored the non-BBNF substrates but held activation on a
shared emitter blocker. cutover.M consumes the K/L repairs and flips
the fleet except EBNF, whose remaining gap becomes O2.
