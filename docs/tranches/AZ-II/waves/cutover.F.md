# AZ-II.cutover.F - StructDirect Emitter Bug Classes
**Opens after**: AZ-II.cutover.E Discovery 1
**Agents**: up to 10 parallel
**Hard gate**: the array Wrap-vs-Repeat and flat inline-position StructDirect emitter bugs are fixed, with remaining bootstrap activation routed to cutover.G.
**Status**: complete_with_misses

## Scope

1. Re-diagnose Discovery 1 as emitter-side, not lower-side.
2. Fix array StructDirect dispatch for Wrap-vs-Repeat shape calls.
3. Fix flat StructDirect inline-position emission for Alt, Repeat,
   Regex, Negate, and Minus.
4. Document remaining TokenDispatch / Pratt / bootstrap blockers.
5. Keep resolver activation deferred until a working BBNF bootstrap
   parser can exercise the fixed emitters.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/backend/rust/emitter/shapes/array/mod.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs` | modify |
| `docs/tranches/AZ-II/audit/cutover.F-PARTIAL.md` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: BBNF handwritten bootstrap parser, resolver-arm
activation, non-BBNF runtime substrates, `Parsed<R>` deletion,
`crates/tape/` deletion. Deployment invariant: diagnosis and fixes run
in fully-contained worktrees; resolver activation waits for cutover.G.

## Phase sub-items

### AZ-II.cutover.F.1 Discovery 1 Reproduction

Mechanism: reproduce BBNF offset-0 rejection and inspect generated
emitter shape calls.

Files touched: `docs/tranches/AZ-II/audit/cutover.F-PARTIAL.md`.

Sub-gate: report supersedes the cutover.E lower-side attribution.

### AZ-II.cutover.F.2 Array Wrap-vs-Repeat Dispatch

Mechanism: update array StructDirect emission so Wrap and Repeat
branches dispatch to the correct shape functions.

Files touched: `crates/core/src/backend/rust/emitter/shapes/array/mod.rs`.

Sub-gate: generated array branches no longer call the wrong wrapper.

### AZ-II.cutover.F.3 Flat Inline-Position Emission

Mechanism: emit inline-position code for Alt, Repeat, Regex, Negate,
and Minus under the flat StructDirect path.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/flat/struct_direct.rs`.

Sub-gate: generated flat branches no longer collapse to unreachable or
empty bodies.

### AZ-II.cutover.F.4 Remaining Blocker Routing

Mechanism: document TokenDispatch, Pratt, and chicken-and-egg bootstrap
blockers and assign them to cutover.G or later waves.

Files touched: `docs/tranches/AZ-II/audit/cutover.F-PARTIAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: no document claims full resolver activation after F.

## Hard gate

1. Array Wrap-vs-Repeat StructDirect dispatch is fixed.
2. Flat inline-position StructDirect emission is fixed.
3. Discovery 1 report names remaining blocker classes.
4. Resolver activation remains deferred with a named destination.

## Verification artefacts

- Commits `b813eb64`, `246efda7`, `6056baee`.
- `docs/tranches/AZ-II/audit/cutover.F-PARTIAL.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.E
- **Blocks**: AZ-II.cutover.G, AZ-II.cutover.K

## Archaeology

cutover.F corrected cutover.E's attribution. It landed shared emitter
substrate but refused to claim activation before a bootstrap parser
could exercise the repaired generated path.
