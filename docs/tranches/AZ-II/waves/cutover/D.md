# AZ-II.cutover.D - BBNF Consumer Migration
**Opens after**: AZ-II.cutover.C scope reveal
**Agents**: up to 10 parallel
**Hard gate**: BBNF consumers read `BbnfDocument` / `BbnfView` instead of generated node views or tape cursor surfaces.
**Status**: complete

## Scope

1. Migrate type extraction and rule-entry access to `BbnfView`.
2. Migrate graph dependency and metadata consumers to runtime views.
3. Migrate pipeline and directive parsing onto `BbnfDocument`.
4. Migrate lower/value-expression consumers away from
   `BbnfBootstrapNodeView`.
5. Extend runtime BBNF view/accessor surfaces needed by consumers.
6. Recode BBNF parity harnesses to struct-vs-reference comparisons.
7. Harmonize `span_text` / `span_range` API use across analysis and
   LSP-facing crates.

## File bounds

| File | Access |
|---|---|
| `crates/core/src/host.rs` | modify |
| `crates/core/src/types.rs` | modify |
| `crates/core/src/graph/**` | modify |
| `crates/core/src/pipeline/**` | modify |
| `crates/core/src/lower/**` | modify |
| `crates/core/src/runtime/bbnf/**` | modify |
| `crates/core/tests/bbnf_*_parity.rs` | modify |
| `crates/core/tests/serialize_roundtrip.rs` | modify |
| `crates/analysis/src/**` | modify |
| `crates/lsp/src/**` | modify |
| `crates/gorgeous/src/**` | modify |

**Do NOT touch**: emitter strategy, non-BBNF runtime substrates,
`crates/tape/` deletion, benchmark close JSON. Deployment invariant:
parallel agents use sibling fully-contained worktrees with disjoint
file bounds; the orchestrator consolidates shared runtime view edits.

## Phase sub-items

### AZ-II.cutover.D.1 Types and Rule Entries

Mechanism: migrate `RuleEntry::rhs` and type-surface readers from
generated node views to `BbnfView`.

Files touched: `crates/core/src/types.rs`.

Sub-gate: type tests compile without `BbnfBootstrapNodeView`.

### AZ-II.cutover.D.2 Graph Consumers

Mechanism: recode dependency and metadata walkers over `BbnfView`.

Files touched: `crates/core/src/graph/**`.

Sub-gate: graph dependency tests pass.

### AZ-II.cutover.D.3 Pipeline Consumers

Mechanism: parse pipeline/directive input from `BbnfDocument` and
runtime views.

Files touched: `crates/core/src/pipeline/**`, `crates/core/src/host.rs`.

Sub-gate: pipeline compile tests pass without generated node views.

### AZ-II.cutover.D.4 Lowering Consumers

Mechanism: migrate expression hierarchy and value-expression lowering
to `BbnfView`, adding value-expression runtime arms as needed.

Files touched: `crates/core/src/lower/**`,
`crates/core/src/runtime/bbnf/**`.

Sub-gate: lower/value-expression tests pass.

### AZ-II.cutover.D.5 Runtime View Accessors

Mechanism: add `span_text`, `span_text_opt`, `span_range`, compound
kind, branch, and child traversal accessors required by migrated
consumers.

Files touched: `crates/core/src/runtime/bbnf/**`.

Sub-gate: runtime view substrate tests compile and pass.

### AZ-II.cutover.D.6 Analysis and LSP Harmonization

Mechanism: update analysis and LSP features to consume `BbnfView` and
the runtime span API.

Files touched: `crates/analysis/src/**`, `crates/lsp/src/**`.

Sub-gate: analysis and LSP crates compile.

### AZ-II.cutover.D.7 Parity Harness Recode

Mechanism: recode BBNF parity tests to compare struct view output
against the maintained `bbnf::ast` reference.

Files touched: `crates/core/tests/bbnf_*_parity.rs`,
`crates/core/tests/serialize_roundtrip.rs`.

Sub-gate: BBNF parity tests no longer import tape or generated node
views.

## Hard gate

1. Production BBNF consumers do not import `BbnfBootstrapNodeView`.
2. BBNF parity harnesses are struct-vs-reference.
3. Analysis and LSP crates compile against runtime view APIs.
4. `cargo check -p bbnf -p bbnf-analysis -p bbnf-lsp --profile ax-iter`
   passes.

## Verification artefacts

- Commits `113a1d23`, `dba623b8`, `34280d2a`, `7648b723`,
  `bcdf25ed`, `073aa703`, `fa3026e8`, `3396f472`, `a7a9f771`,
  `24b19281`, `43526778`, `825e8a06`, `685bad2f`, `7a320ce4`,
  `b1d0576a`, `8428d4fc`, `2aa6822e`.
- `docs/tranches/AZ-II/PROGRESS-SNAPSHOT-2026-04-29.md`.

## Dependencies

- **Depends on**: AZ-II.cutover.C
- **Blocks**: AZ-II.cutover.F, AZ-II.cutover.G

## Archaeology

cutover.C measured the consumer surface. cutover.D consumed that scope
directly instead of preserving compatibility shims around generated node
views.
