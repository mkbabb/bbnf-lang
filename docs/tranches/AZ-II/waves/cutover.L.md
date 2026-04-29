# AZ-II.cutover.L - Keyword Alt-of-Ref StructDirect Branches
**Opens after**: AZ-II.cutover.K close
**Agents**: up to 10 parallel
**Hard gate**: keyword-shape StructDirect emission handles Alt-of-Ref branches without falling back to tape-shaped dispatch.
**Status**: complete_with_misses

## Scope

1. Add keyword-shape StructDirect support for Alt-of-Ref branches.
2. Prefix-check Ref-led keyword branches and delegate to target shape
   functions.
3. Admit Wrap and Keyword transparent rules under StructDirect in the
   per-rule emission gate.
4. Verify CSS L4 pseudoClass / pseudoElement branch emission.
5. Preserve remaining resolver fleet activation for cutover.M.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | modify |
| `crates/core/src/grammar/generated/css_l4.rs` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: non-BBNF resolver arm activation, AltDispatch
literal/regex/seq branches, `Parsed<R>` deletion, `crates/tape/`
deletion. Deployment invariant: keyword-shape agents use
fully-contained worktrees; orchestrator owns generated-source review.

## Phase sub-items

### AZ-II.cutover.L.1 Keyword Alt-of-Ref Emitter

Mechanism: emit StructDirect Ref-branch prefix checks and target shape
delegation in keyword rules.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/keyword/struct_direct.rs`.

Sub-gate: pseudoClass / pseudoElement Ref branches generate code.

### AZ-II.cutover.L.2 Transparent Rule Admission

Mechanism: allow Wrap- and Keyword-classified transparent rules to emit
under StructDirect.

Files touched: `crates/core/src/backend/rust/emitter/shapes/mod.rs`.

Sub-gate: generated transparent keyword call sites resolve.

### AZ-II.cutover.L.3 CSS L4 Generated Check

Mechanism: regenerate and inspect CSS L4 pseudo-class/pseudo-element
emission.

Files touched: `crates/core/src/grammar/generated/css_l4.rs`.

Sub-gate: focused CSS L4 pseudo selector tests compile.

### AZ-II.cutover.L.4 Progress Boundary

Mechanism: record L as Phase 3a and route the rest of Phase 3 to M.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: progress docs do not claim full fleet activation yet.

## Hard gate

1. Keyword Alt-of-Ref StructDirect branch emission exists.
2. Transparent Wrap/Keyword classified rules emit under StructDirect.
3. CSS L4 pseudo selector generated code compiles.
4. Remaining resolver fleet activation is routed to cutover.M.

## Verification artefacts

- Commit `b770fae7`.
- `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.K
- **Blocks**: AZ-II.cutover.M

## Archaeology

cutover.K removed lower and Err-frame blockers; cutover.L closed the
next exposed keyword shape blocker but did not widen its scope to the
entire resolver fleet.
