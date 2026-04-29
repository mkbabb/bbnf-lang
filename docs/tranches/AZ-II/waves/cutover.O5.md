# AZ-II.cutover.O5 — Tape Crate Deletion
**Opens after**: AZ-II.cutover.O4 close
**Agents**: up to 10 parallel
**Hard gate**: `crates/tape` is deleted from the workspace and `cargo build -p bbnf --no-default-features` succeeds without it.
**Status**: planned

## Scope

1. Delete the standalone `crates/tape` crate after O4 removes production `Parsed<R>` / `TapeDirect` consumers.
2. Relocate only genuinely non-tape scan/index primitives to their natural owner; no public tape runtime may survive under another name.
3. Remove workspace and package manifest edges to tape.
4. Recode or delete tests, benches, and examples that exist only to validate tape.
5. Consume O3a A1 disposition for `json-prototype` and analysis/LSP
   failures if they are tape-only or archive-only surfaces.
6. Archive deletion scans and update AZ-II terminal-progress docs.

## File bounds

| File | Access |
|---|---|
| `Cargo.toml` | modify |
| `crates/core/Cargo.toml` | modify |
| `crates/core/src/runtime/mod.rs` | modify |
| `crates/core/src/backend/rust/emitter/**` | modify-carve |
| `crates/core/src/backend/rust/trace.rs` | modify-carve |
| `crates/core/src/backend/rust/precedence.rs` | modify |
| `crates/core/src/backend/rust/profile.rs` | modify |
| `crates/core/benches/json/value.rs` | modify-carve |
| `crates/core/benches/common/validate.rs` | modify |
| `crates/core/examples/json_check.rs` | modify |
| `crates/core/tests/{tape_walker_allocs,visitor_reduce,keyword_ref_branch_wire_contract,shape_dispatch_emission}.rs` | modify-carve |
| `crates/tape/**` | delete |
| `crates/core/src/grammar/generated/*.rs` | modify |
| `docs/benchmarks/AZ-II/cutover/O5-tape-delete-scan.txt` | create |
| `docs/tranches/AZ-II/PROGRESS.md` | modify |
| `docs/tranches/AZ-II/waves/cutover.md` | modify |

**Do NOT touch**: `../parse-that/**`, `../pprint/**`, `docs/benchmarks/post-AZ-II.json`, sonic-rs/lightningcss parity fixtures. O6 owns close-matrix measurement.
Deployment invariant: every sub-agent runs in a sibling
fully-contained worktree seeded with `scripts/seed-worktree.sh`, with
explicit allow/forbidden lists; only the orchestrator updates the main
workspace manifest after accepted deletion/recode commits compose.

## Phase sub-items

### AZ-II.cutover.O5.1 Workspace Severance

Mechanism: remove `crates/tape` from the workspace and remove package dependencies on tape.

Files touched: `Cargo.toml`, `crates/core/Cargo.toml`.

Sub-gate: `cargo metadata --no-deps --format-version 1` contains no `crates/tape` package.

### AZ-II.cutover.O5.2 Runtime Re-Export Removal

Mechanism: delete tape re-exports from core runtime and ensure all public runtime modules are document/arena/builder/path surfaces.

Files touched: `crates/core/src/runtime/mod.rs`.

Sub-gate: `rg 'runtime::tape|pub use .*tape' crates/core/src/runtime --type rust` returns zero.

### AZ-II.cutover.O5.3 Emitter Tape Symbol Cleanup

Mechanism: remove residual tape type names from emitter helper modules after O4 changes the production return model.

Files touched: `crates/core/src/backend/rust/emitter/**`.

Sub-gate: emitter source no longer imports `Tape`, `TapeCursor`, `TapeRec`, or `TapeOffset`.

### AZ-II.cutover.O5.4 Trace/Profile/Precedence Cleanup

Mechanism: move non-tape diagnostics that remain useful into backend-owned structs and delete tape-specific trace/profile naming.

Files touched: `crates/core/src/backend/rust/trace.rs`, `crates/core/src/backend/rust/precedence.rs`, `crates/core/src/backend/rust/profile.rs`.

Sub-gate: trace/profile tests compile without tape crate symbols.

### AZ-II.cutover.O5.5 Bench and Example Recode

Mechanism: recode JSON value benches and examples from tape walking to document/value API walking, or delete tape-only lanes from the canonical bench surface.

Files touched: `crates/core/benches/json/value.rs`, `crates/core/benches/common/validate.rs`, `crates/core/examples/json_check.rs`.

Sub-gate: `cargo check -p bbnf --benches --profile ax-iter` compiles after O5 integration.

### AZ-II.cutover.O5.6 Tape Test Disposition

Mechanism: delete tape-only tests and recode any still-load-bearing tests to document/builder contracts.

Files touched: `crates/core/tests/{tape_walker_allocs,visitor_reduce,keyword_ref_branch_wire_contract,shape_dispatch_emission}.rs`.

Sub-gate: no test imports `bbnf::runtime::tape`.

### AZ-II.cutover.O5.7 Crate Deletion

Mechanism: delete `crates/tape/**` entirely after upstream call sites are gone.

Files touched: `crates/tape/**`.

Sub-gate: `test ! -e crates/tape` succeeds.

### AZ-II.cutover.O5.8 Orchestrator Regen

Mechanism: run canonical regen after manifest/runtime/emitter edits; review generated diffs for zero tape imports.

Files touched: `crates/core/src/grammar/generated/*.rs`.

Sub-gate: `cargo xtask regen --check` passes with no generated tape imports.

### AZ-II.cutover.O5.9 Close Scans

Mechanism: archive workspace scans proving no production tape crate or runtime surface remains.

Files touched: `docs/benchmarks/AZ-II/cutover/O5-tape-delete-scan.txt`.

Sub-gate: scan artifact includes `rg '^crates/tape/'`, `cargo metadata`, and production tape symbol scans.

### AZ-II.cutover.O5.10 Progress Boundary

Mechanism: update AZ-II docs with tape deletion evidence and O6 as the active close-matrix wave.

Files touched: `docs/tranches/AZ-II/PROGRESS.md`, `docs/tranches/AZ-II/waves/cutover.md`.

Sub-gate: status lines agree across the wave spec and progress docs.

### AZ-II.cutover.O5.11 O3a A1 Archive/Deletion Integration

Mechanism: consume the O3a A1 triad output. If `json-prototype`,
analysis, or LSP failures exist only to exercise tape-era fixtures or
archived prototype paths, O5 owns deletion/archive. If they remain live
development surfaces, O5 records the child-wave owner before removing
tape.

Files touched: `docs/tranches/AZ-II/audit/O3a-A1-*.md`,
`crates/analysis/**`, `crates/lsp/**`,
`crates/core/benches/json-prototype/**`, workspace manifests if
archive/deletion is chosen.

Sub-gate: A1 has a no-shim disposition before `crates/tape` is deleted.

## Hard gate

1. `test ! -e crates/tape` succeeds and `Cargo.toml` no longer lists the crate.
2. `cargo metadata --no-deps --format-version 1` contains no tape package.
3. `cargo build -p bbnf --no-default-features --profile ax-iter` passes.
4. `cargo check -p bbnf --benches --profile ax-iter` passes.
5. `docs/benchmarks/AZ-II/cutover/O5-tape-delete-scan.txt` records zero production `runtime::tape`, `TapeCursor`, `TapeRec`, `TapeOffset`, or `crates/tape` hits outside historical docs.
6. O3a A1 has a committed archive/delete/repair disposition with no
   compatibility shim.

## Verification artefacts

- `/tmp/az-ii-o5-metadata.txt`
- `/tmp/az-ii-o5-build-no-default-features.txt`
- `/tmp/az-ii-o5-bench-check.txt`
- `/tmp/az-ii-o5-regen-check.txt`
- `docs/tranches/AZ-II/audit/O3a-A1-*.md`
- `docs/benchmarks/AZ-II/cutover/O5-tape-delete-scan.txt`
- O5 close commit hashes recorded in `docs/tranches/AZ-II/PROGRESS.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.O4, O3a A1 disposition
- **Blocks**: AZ-II.cutover.O6

## Archaeology

cutover.C revealed that immediate tape deletion exposed thousands of live generated and consumer references. O5 reopens deletion only after O2/O3/O4 remove the grammar, generated-view, and return-model blockers.
