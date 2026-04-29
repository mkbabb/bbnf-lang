# AZ-II.cutover.E - Non-BBNF Runtime Substrates and Discovery 1
**Opens after**: AZ-II.cutover.D close
**Agents**: up to 10 parallel
**Hard gate**: CSV, Math, BNF, EBNF, and CSS Pretty StructDirect runtime substrates land, while resolver activation is deferred on a named emitter blocker.
**Status**: complete_with_misses

## Scope

1. Author StructDirect runtime substrates for CSV.
2. Author StructDirect runtime substrates for Math.
3. Author StructDirect runtime substrates for BNF.
4. Author StructDirect runtime substrates for EBNF.
5. Author StructDirect runtime substrates for CSS Pretty.
6. Attempt resolver activation and diagnose the BBNF generated parser
   rejecting every input at offset 0.
7. Defer non-BBNF resolver arms until the shared emitter blocker is
   repaired.
8. Write the cutover.E partial-close report.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/runtime/csv/**` | create |
| `crates/core/src/runtime/math/**` | create |
| `crates/core/src/runtime/bnf/**` | create |
| `crates/core/src/runtime/ebnf/**` | create |
| `crates/core/src/runtime/css_pretty/**` | create |
| `crates/core/src/runtime/mod.rs` | modify |
| `crates/ir/src/registry/strategy.rs` | modify |
| `docs/tranches/AZ-II/audit/cutover.E-PARTIAL.md` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: BBNF consumer migration, generated BBNF parser
surgery, `Parsed<R>` deletion, `crates/tape/` deletion. Deployment
invariant: substrate agents run in fully-contained worktrees; resolver
activation halts when a shared emitter blocker is found.

## Phase sub-items

### AZ-II.cutover.E.1 CSV Runtime

Mechanism: author CSV value/arena/builder/document runtime modules and
module exports.

Files touched: `crates/core/src/runtime/csv/**`,
`crates/core/src/runtime/mod.rs`.

Sub-gate: CSV runtime compiles and builder finalises.

### AZ-II.cutover.E.2 Math Runtime

Mechanism: author Math StructDirect runtime modules and strategy
binding.

Files touched: `crates/core/src/runtime/math/**`,
`crates/ir/src/registry/strategy.rs`.

Sub-gate: Math runtime compiles.

### AZ-II.cutover.E.3 BNF / EBNF / CSS Pretty Runtimes

Mechanism: author remaining non-BBNF runtime substrates.

Files touched: `crates/core/src/runtime/{bnf,ebnf,css_pretty}/**`,
`crates/core/src/runtime/mod.rs`.

Sub-gate: each runtime has builder/document modules and exports.

### AZ-II.cutover.E.4 Resolver Activation Probe

Mechanism: attempt resolver arm activation and regen to identify the
shared emitter failure.

Files touched: `crates/ir/src/registry/strategy.rs`.

Sub-gate: probe failure is reduced to a named emitter blocker.

### AZ-II.cutover.E.5 Deferral Report

Mechanism: document Discovery 1 and route activation to follow-on
emitter repair waves.

Files touched: `docs/tranches/AZ-II/audit/cutover.E-PARTIAL.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: no resolver arm is left half-activated without a doc route.

## Hard gate

1. Five non-BBNF runtime substrates exist and compile.
2. Resolver activation is either green or explicitly reverted/deferred.
3. Discovery 1 is documented with reproduction and follow-on ownership.
4. No source claims non-BBNF resolver arms are active at close.

## Verification artefacts

- Commits `57e017de`, `6b2f3ca7`, `911ee70f`, `cb36c997`,
  `9f40f17c`.
- `docs/tranches/AZ-II/audit/cutover.E-PARTIAL.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.D
- **Blocks**: AZ-II.cutover.F, AZ-II.cutover.M

## Archaeology

cutover.E surfaced that runtime substrate was not the only blocker:
StructDirect emitter shape handling was still insufficient for BBNF and
therefore unsafe for resolver fleet activation.
