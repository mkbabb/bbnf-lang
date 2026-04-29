# AZ-II.cutover.K - Value-Expression Recovery and Err-Frame Cleanup
**Opens after**: AZ-II.cutover.I blocker report
**Agents**: up to 10 parallel
**Hard gate**: mapped-factor discrimination, typed-leaf source recovery, and per-shape Err cleanup unblock non-BBNF resolver activation attempts.
**Status**: complete

## Scope

1. Wrap `mapped_factor` mapping targets in anonymous compounds for
   lower-side discrimination.
2. Recover typed-leaf source text through structural compound kinds and
   a descendant walker.
3. Extend bootstrap reproducibility to JSON regen idempotence.
4. Close open compound frames on Err paths in relevant StructDirect
   shape emitters.
5. Refresh generated fleet after substrate fixes.
6. Update AZ-II progress with K phase results.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/grammar/bootstrap_parser.rs` | modify |
| `crates/core/src/lower/value_expr/**` | modify |
| `crates/core/src/runtime/bbnf/**` | modify |
| `crates/core/src/backend/rust/emitter/shapes/{flat,wrap,pratt,arglist}/struct_direct.rs` | modify |
| `crates/core/tests/bbnf_bootstrap_reproducibility.rs` | modify |
| `crates/core/src/grammar/generated/*.rs` | modify |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: keyword Alt-of-Ref branches, resolver-arm fleet
activation, `Parsed<R>` deletion, `crates/tape/` deletion. Deployment
invariant: each phase uses fully-contained worktrees; orchestrator owns
final regen.

## Phase sub-items

### AZ-II.cutover.K.1 Mapped-Factor Wrapper

Mechanism: make the bootstrap parser wrap mapped-factor mappings in an
anonymous compound so lower-side discrimination sees the right shape.

Files touched: `crates/core/src/grammar/bootstrap_parser.rs`.

Sub-gate: `lower_mapped_factor` discrimination works on the new shape.

### AZ-II.cutover.K.2 Typed-Leaf Source Recovery

Mechanism: add structural compound-kind walking so value-expression
typed leaves recover source text without span-only hacks.

Files touched: `crates/core/src/lower/value_expr/**`,
`crates/core/src/runtime/bbnf/**`.

Sub-gate: typed-leaf bool/null source recovery is correct.

### AZ-II.cutover.K.3 JSON Reproducibility Extension

Mechanism: extend the bootstrap reproducibility test suite to include
JSON regen idempotence.

Files touched: `crates/core/tests/bbnf_bootstrap_reproducibility.rs`.

Sub-gate: BBNF and JSON regen idempotence both pass.

### AZ-II.cutover.K.4 Err-Frame Cleanup

Mechanism: wrap StructDirect shape bodies so Err paths close any open
compound frames before returning.

Files touched:
`crates/core/src/backend/rust/emitter/shapes/{flat,wrap,pratt,arglist}/struct_direct.rs`.

Sub-gate: failed parse paths no longer leave open frames in those
shapes.

### AZ-II.cutover.K.5 Orchestrator Regen

Mechanism: regenerate the affected parser fleet after substrate fixes.

Files touched: `crates/core/src/grammar/generated/*.rs`.

Sub-gate: `cargo xtask regen --check` is clean.

## Hard gate

1. Mapped-factor wrapper fix lands.
2. Typed-leaf source recovery lands.
3. Per-shape Err paths close open frames.
4. BBNF and JSON reproducibility tests pass.
5. Full regen is idempotent.

## Verification artefacts

- Commits `a09173dc`, `cbf77e06`, `7d283a8f`.
- `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.I
- **Blocks**: AZ-II.cutover.L, AZ-II.cutover.M

## Archaeology

cutover.I named typed-leaf recovery and Err-frame leaks as activation
blockers. cutover.K closes both root causes before resolver fleet
activation resumes.
