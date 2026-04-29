# AZ-II.cutover.H - BBNF Resolver Reflip and Partial Close
**Opens after**: AZ-II.cutover.G close
**Agents**: up to 10 parallel
**Hard gate**: BBNF resolver re-flips to StructDirect with transparent-rule emission repaired, and unresolved close gates are recorded in a partial FINAL.
**Status**: complete_with_misses

## Scope

1. Skip value-expression subtrees during nonterminal dependency walks.
2. Re-flip BBNF resolver arm to StructDirect.
3. Repair transparent-rule emission for Wrap-classified rules under
   StructDirect.
4. Fix BBNF pretty-hint span handling needed by compact source
   emission.
5. Document the `bbnf_rule` serializer deferral.
6. Author AZ-II `FINAL.md` as PARTIAL CLOSE and update progress.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/graph/deps.rs` | modify |
| `crates/ir/src/registry/strategy.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/mod.rs` | modify |
| `crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs` | modify |
| `crates/core/src/grammar/bootstrap_parser.rs` | modify |
| `crates/core/tests/serialize_roundtrip.rs` | modify |
| `crates/core/src/grammar/generated/bbnf.rs` | modify |
| `docs/tranches/AZ-II/FINAL.md` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md` | modify |

**Do NOT touch**: non-BBNF resolver fleet activation, `Parsed<R>`
deletion, `crates/tape/` deletion, O6 benchmark truth. Deployment
invariant: agents use fully-contained worktrees; the partial close must
name every miss rather than hiding it as future work.

## Phase sub-items

### AZ-II.cutover.H.1 Dependency Walk Fix

Mechanism: make `collect_refs_from_compound` skip value-expression
subtrees so host-function identifiers are not misclassified as
nonterminal refs.

Files touched: `crates/core/src/graph/deps.rs`.

Sub-gate: JSON regen-check no longer fails on value-expression host
idents.

### AZ-II.cutover.H.2 BBNF Resolver Reflip

Mechanism: restore the BBNF StructDirect resolver arm after the bridge
and transparent-rule fix path are available.

Files touched: `crates/ir/src/registry/strategy.rs`.

Sub-gate: BBNF resolver reports StructDirect.

### AZ-II.cutover.H.3 Transparent Wrap Emission

Mechanism: emit Wrap-classified transparent rules under StructDirect
instead of skipping their functions.

Files touched: `crates/core/src/backend/rust/emitter/shapes/mod.rs`,
`crates/core/src/backend/rust/emitter/shapes/wrap/struct_direct.rs`.

Sub-gate: generated call sites for transparent wrap rules resolve.

### AZ-II.cutover.H.4 Pretty-Hint Span Fix

Mechanism: push parenthesized pretty-hint argument spans as children so
compact source can recover literals.

Files touched: `crates/core/src/grammar/bootstrap_parser.rs`.

Sub-gate: pretty-hint roundtrip no longer loses required text.

### AZ-II.cutover.H.5 Serializer Deferral

Mechanism: document why `bbnf_rule` remains ignored until a typed
`BbnfDocument` compact-source walker exists.

Files touched: `crates/core/tests/serialize_roundtrip.rs`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: deferral has a named follow-on phase.

### AZ-II.cutover.H.6 Partial Final

Mechanism: author `FINAL.md` as PARTIAL CLOSE with explicit misses and
progress state.

Files touched: `docs/tranches/AZ-II/FINAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`,
`docs/tranches/AZ-II/audit/cutover.G-PARTIAL.md`.

Sub-gate: docs do not claim terminal tape deletion or benchmark truth.

## Hard gate

1. BBNF resolver arm is StructDirect.
2. Transparent Wrap rules emit callable functions under StructDirect.
3. Bootstrap reproducibility remains green.
4. `FINAL.md` exists and is clearly PARTIAL CLOSE.
5. Remaining misses route to named follow-on cutover substages.

## Verification artefacts

- Commits `42e0906b`, `3e8a0ed7`, `a61507eb`, `ee568213`.
- `docs/tranches/AZ-II/FINAL.md`.
- `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.G
- **Blocks**: AZ-II.cutover.I, AZ-II.cutover.K

## Archaeology

cutover.H converted the productive BBNF substrate into a documented
partial close. It did not treat partial close as completion; its misses
became cutover.I and later O-wave work.
