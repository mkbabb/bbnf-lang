# AZ-II.cutover.N - Halted Terminal Deletion Attempt
**Opens after**: AZ-II.cutover.M close
**Agents**: up to 10 parallel
**Hard gate**: EBNF activation, `Parsed<R>` deletion, tape deletion, bench refresh, and FINAL conversion either close or reroute with no uncommitted code loss.
**Status**: blocked

## Scope

1. Diagnose and attempt EBNF StructDirect activation.
2. Delete `Parsed<R>` as production parser result after all grammars
   return concrete documents.
3. Delete `TapeDirect` fallback semantics from the strategy surface.
4. Delete or relocate remaining tape runtime/crate consumers.
5. Refresh semantic parity and close-matrix benchmarks.
6. Convert AZ-II FINAL from partial to terminal close.
7. Halt cleanly at organizational usage limit with worktree state
   recorded and no code commits landed.

## File bounds

| File | Access |
|---|---|
| `crates/ir/src/registry/strategy.rs` | modify |
| `crates/core/src/runtime/parsed.rs` | delete |
| `crates/core/src/runtime/mod.rs` | modify |
| `crates/core/src/backend/rust/emitter/**` | modify |
| `crates/core/src/grammar/generated/*.rs` | modify |
| `crates/tape/**` | delete |
| `Cargo.toml` | modify |
| `crates/core/Cargo.toml` | modify |
| `crates/core/tests/**` | modify |
| `docs/benchmarks/post-AZ-II.json` | modify |
| `docs/tranches/AZ-II/FINAL.md` | modify |
| `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |

**Do NOT touch**: source code on master after halt; uncommitted
worktree state must be recorded instead of force-landed. Deployment
invariant: every agent uses fully-contained worktrees; a halt requires
a snapshot before any further implementation resumes.

## Phase sub-items

### AZ-II.cutover.N.1 EBNF Activation Probe

Mechanism: attempt EBNF resolver flip and inspect high-branch literal
and structural branch failures.

Files touched: `crates/ir/src/registry/strategy.rs`,
`crates/core/src/backend/rust/emitter/**`,
`crates/core/src/grammar/generated/ebnf.rs`.

Sub-gate: either EBNF returns `EbnfDocument` or the blocker is named.

### AZ-II.cutover.N.2 Parsed and TapeDirect Deletion Probe

Mechanism: identify production `Parsed<R>` and `TapeDirect` call sites.

Files touched: `crates/core/src/runtime/parsed.rs`,
`crates/ir/src/registry/strategy.rs`,
`crates/core/src/backend/rust/emitter/**`.

Sub-gate: deletion plan names every call-site owner.

### AZ-II.cutover.N.3 Tape Deletion Probe

Mechanism: scan tape crate and runtime symbol consumers after M.

Files touched: `crates/tape/**`, `Cargo.toml`,
`crates/core/Cargo.toml`.

Sub-gate: deletion remains blocked until generated view and Parsed
surfaces are gone.

### AZ-II.cutover.N.4 Semantic and Bench Probe

Mechanism: attempt parity/bench refresh and record stale surfaces.

Files touched: `docs/benchmarks/post-AZ-II.json`,
`crates/core/tests/**`.

Sub-gate: stale performance truth is not promoted as terminal.

### AZ-II.cutover.N.5 Halt Snapshot

Mechanism: record worktree state, no-code landing, and next route to
cutover.O.

Files touched: `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`,
`docs/tranches/AZ-II/PROGRESS.md`.

Sub-gate: snapshot says cutover.N landed no code commits.

## Hard gate

1. No cutover.N code commits land on master.
2. Worktree state and intended deletion/bench plan are recorded.
3. EBNF blocker is routed to cutover.O1/O2 prerequisites.
4. Parsed/tape/bench/FINAL work routes to cutover.O3-O7.

## Verification artefacts

- `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`.
- Worktree record `/private/tmp/bbnf-worktrees/cutover-N` cited in the
  snapshot.
- Commits `1d9a80bb`, `53d3e6b2`, `77d8cdf7` as post-halt doc
  alignment before O0/O1/O2.

## Dependencies

- **Depends on**: AZ-II.cutover.M
- **Blocks**: AZ-II.cutover.O

## Archaeology

cutover.N is the scope-reveal that required the O-wave decomposition.
Its halted state is historical evidence that no implementation should
resume without fully written O0-O7 specs.
