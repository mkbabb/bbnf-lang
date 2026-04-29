# AZ-II.cutover.A - Substrate Hoist and BBNF Runtime
**Opens after**: AZ-II.cutover open
**Agents**: up to 10 parallel
**Hard gate**: BBNF owns StructDirect runtime substrate, `tape::dta` moves to IR, and dead tape visitor/helper substrate is removed.
**Status**: complete

## Scope

1. Hoist `tape::dta` IDs and precedence types to `bbnf-ir`.
2. Delete the unused tape visitor trait family and driver helper
   functions with zero non-doc consumers.
3. Close `project_types` / `StructRegistry` on BBNF named rules.
4. Author `crates/core/src/runtime/bbnf/` value, arena, builder,
   document, view, and module surfaces.
5. Add the BBNF StructDirect resolver arm.
6. Amputate dead DTA recognizer summary/profile surfaces while
   preserving `collect_pattern_set` for DFA codegen.

## File bounds

| File | Access |
|---|---|
| `crates/ir/src/dta/mod.rs` | create |
| `crates/ir/src/dta/types.rs` | create |
| `crates/ir/src/lib.rs` | modify |
| `crates/tape/src/dta.rs` | delete |
| `crates/tape/src/visitor.rs` | delete |
| `crates/tape/src/driver.rs` | modify-carve |
| `crates/tape/src/lib.rs` | modify-carve |
| `crates/tape/Cargo.toml` | modify |
| `grammar/bbnf/bbnf.bbnf` | modify |
| `crates/core/src/runtime/bbnf/**` | create |
| `crates/core/src/runtime/mod.rs` | modify |
| `crates/ir/src/registry/strategy.rs` | modify |
| `crates/ir/src/passes/audit/payload_coverage.rs` | modify |
| `crates/ir/src/passes/types/mod.rs` | modify |
| `crates/ir/src/passes/recognizers/dta.rs` | modify-carve |
| `crates/ir/src/passes/recognizers/pattern_alphabet.rs` | modify-carve |

**Do NOT touch**: generated parser files, BBNF consumer migrations,
`Parsed<R>` deletion, `crates/tape/` directory deletion, benchmark
result JSON. Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`; the
orchestrator owns final integration on master.

## Phase sub-items

### AZ-II.cutover.A.1 DTA Type Hoist

Mechanism: move `DtaStateId`, `DtaRuleId`, `DtaAssociativity`, and
`DtaPrecedenceEntry` from tape to `bbnf_ir::dta`.

Files touched: `crates/ir/src/dta/mod.rs`,
`crates/ir/src/dta/types.rs`, `crates/ir/src/lib.rs`,
`crates/tape/src/dta.rs`, `crates/tape/Cargo.toml`.

Sub-gate: `rg 'pub.*DtaPrecedenceEntry' crates/` has one production
owner under `crates/ir/src/dta`.

### AZ-II.cutover.A.2 Tape Visitor and Driver Helper Deletion

Mechanism: delete visitor traits and driver helpers that no live
generated parser consumes.

Files touched: `crates/tape/src/visitor.rs`,
`crates/tape/src/driver.rs`, `crates/tape/src/lib.rs`.

Sub-gate: `rg 'GrammarVisitor|ObjectVisitor|emit_leaf|trim_ascii_ws' crates/tape`
returns no production hits.

### AZ-II.cutover.A.3 BBNF Type Closure

Mechanism: add BBNF typed-leaf annotations and extend audit/type passes
until BBNF named rules produce layouts.

Files touched: `grammar/bbnf/bbnf.bbnf`,
`crates/ir/src/passes/audit/payload_coverage.rs`,
`crates/ir/src/passes/types/mod.rs`.

Sub-gate: the BBNF struct-registry regression test sees non-empty
layouts for named BBNF rules.

### AZ-II.cutover.A.4 BBNF Runtime Substrate

Mechanism: author the BBNF value, arena, builder, document, and runtime
view modules following the existing StructDirect runtime pattern.

Files touched: `crates/core/src/runtime/bbnf/**`,
`crates/core/src/runtime/mod.rs`.

Sub-gate: `BbnfStructBuilder` implements the shared builder trait and
can finalise a `BbnfDocument`.

### AZ-II.cutover.A.5 BBNF Resolver Arm

Mechanism: route `BbnfBootstrap` / `BbnfParser` through StructDirect.

Files touched: `crates/ir/src/registry/strategy.rs`.

Sub-gate: resolver tests show BBNF binds to `BbnfStructBuilder` and
`BbnfDocument`.

### AZ-II.cutover.A.6 Recognizer Decay Sweep

Mechanism: remove dead DTA summary/profile/builder surfaces while
keeping the live pattern collection function.

Files touched: `crates/ir/src/passes/recognizers/dta.rs`,
`crates/ir/src/passes/recognizers/pattern_alphabet.rs`.

Sub-gate: recognizer source shrinks materially and downstream DFA
codegen still compiles.

## Hard gate

1. `bbnf_ir::dta::*` is the single production owner for DTA shared
   types.
2. Tape visitor traits and dead driver helpers are absent.
3. `StructRegistry` closes on BBNF named rules.
4. `crates/core/src/runtime/bbnf/` exists and builds.
5. BBNF resolver arm is StructDirect.
6. `cargo check -p bbnf-ir -p bbnf --profile ax-iter` passes.

## Verification artefacts

- Commits `63cacbe2`, `d3977825`, `19a2669a`, `82a88696`,
  `ec7a0fa1`.
- `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`.
- `docs/tranches/AZ-II/waves/cutover.md`.

## Dependencies

- **Depends on**: AZ-I.W2-act close
- **Blocks**: AZ-II.cutover.B, AZ-II.cutover.D

## Archaeology

AZ-I proved StructDirect on the data grammars while BBNF remained the
last tape consumer. cutover.A moved shared ownership to IR and authored
the BBNF runtime before attempting bootstrap regeneration.
