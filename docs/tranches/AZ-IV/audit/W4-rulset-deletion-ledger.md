# W4 RuleSet Deletion Ledger

**Tranche**: AZ-IV
**Wave**: W4 (Optimization Substrate Activation)
**Sub-unit**: AZ-IV.W4.1 Rewrite Ruler Chain (DELETION)
**Decision**: D2 ADOPTED — DELETE the unconsumed RuleSet field +
egraph::ruler::* family. Recycled BA recreates clean against
surviving storage primitives.
**Reference**: `docs/tranches/AZ-IV/audit/SYNTHESIS-2026-05-02.md` §6,
`docs/tranches/AZ-IV/audit/AUDIT-E-pathforward-2026-05-02.md` §8.
**Date**: 2026-05-02

## Per-Test Justification Table

Tests/examples that exclusively exercised the deleted
`egraph::ruler::{enumerate,oracle,residue}` substrate. Each row names
the file, the substrate(s) it consumed, and the disposition under
the W1.9-style per-test justification rule.

| File (deleted) | Substrate consumed | Disposition | Justification |
|---|---|---|---|
| `crates/egraph/tests/ruler_enumerate.rs` (113 LOC) | `egraph::ruler::enumerate` (CVC enumeration) | **DELETE** | Tests the CVC enumeration entry-point which is itself deleted; recycled BA reauthors enumeration tests against the rebuilt substrate. No production runtime path was ever consumed; the gate was test-only. |
| `crates/egraph/tests/ruler_oracle.rs` (72 LOC) | `egraph::ruler::oracle` (VM-based equivalence checker) | **DELETE** | Tests the oracle's per-input equivalence contract, parameterised by the toy boolean DSL fixture. Oracle module is deleted; recycled BA reauthors equivalence-checker tests on the rebuilt VM oracle. |
| `crates/egraph/tests/ruler_residue.rs` (49 LOC) | `egraph::ruler::residue::ResidueFilter` | **DELETE** | Tests the residue-filter fast path (e-graph-pre-filter) for the oracle. Filter module is deleted with the oracle it serves; recycled BA recreates fresh against the rebuilt filter discipline. |
| `crates/egraph/tests/common/mod.rs` (136 LOC) | Boolean DSL fixture for ruler tests above | **DELETE** | Pure test fixture. Defines the toy `Bool` language that satisfies `Alphabet` / `LangNode` / `Language` / `Interpreter` traits — every trait is consumed only by the three deleted ruler integration tests. No production reach. |
| `crates/egraph/examples/ruler_smoke.rs` (183 LOC) | `egraph::ruler::*` (all three modules) | **DELETE** | End-to-end smoke example showing the full ruler pipeline on the boolean DSL. Example-only; never built by production. Deletion-coupled with the substrate. |

**Total tests/example removed**: 5 files, 553 LOC.

## Production Source Deletions (paired ledger)

| File (deleted/carved) | Operation | Justification |
|---|---|---|
| `crates/egraph/src/ruler/enumerate.rs` (301 LOC) | DELETE | Wired-not-consumed substrate per AUDIT-E §8 D2. |
| `crates/egraph/src/ruler/oracle.rs` (140 LOC) | DELETE | As above. |
| `crates/egraph/src/ruler/residue.rs` (126 LOC) | DELETE | As above. |
| `crates/egraph/src/ruler/mod.rs` (33 LOC) | DELETE | Module declaration; no surviving sub-modules. |
| `crates/egraph/src/lib.rs` | CARVE | Removed `pub mod ruler;` declaration. |
| `crates/core/src/pipeline.rs` | CARVE | Removed `PipelineOptions::rewrites` field and the `bbnf_ir::rewrites::RuleSet` payload it carried. |
| `crates/core/src/pipeline/compile/pipeline.rs` | CARVE | Removed lines 43-60 (the `BBNF_PIPELINE_REPORT` eprintln-only sink for `options.rewrites`). |
| `crates/ir/src/rewrites/mod.rs` | CARVE | Removed `RuleSet::load_from_dir` (zero-caller after xtask carve); rewrote module docstring to reflect surviving-API surface. |
| `crates/ir/src/rewrites/base.rs` | CARVE | Removed three doc-comment paragraphs that referenced the deleted `egraph::ruler::Pattern<N>` adapter. |
| `xtask/src/regen.rs` | CARVE | Removed `GrammarEntry::rewrites_dir` method, the `RuleSet::load_from_dir` call site + `rewrites_for_pipeline` plumbing, and the `bbnf_ir::rewrites::RuleSet` import. |

**Total production lines deleted (rough)**: 600 LOC across 4 deleted
files plus ~80 LOC across 6 carved files.

## Preserved (per plan §PRESERVE)

- `crates/ir/src/rewrites/path_seed.rs` — W3.0 product (3
  hand-authored Class-1 path-shape rewrites).
- `crates/ir/src/rewrites/mod.rs::RuleSet::merge_path_seed` — W3.0
  loader (deterministic seed registration).
- `crates/ir/src/rewrites/{base,rank,schema,tiering}.rs` — storage
  primitives (`Pattern`, `Atom`, `Witness`, `Rule`, `RuleClass`, etc.).
- `crates/ir/src/rewrites/mod.rs::RuleSet::{load_from_ron, save_to_ron, from_file, to_file, merge_path_seed}` — surviving public API.
- All non-ruler `egraph` infrastructure (`Analysis`, `EGraph`,
  `Extractor`, `Language`, `Rewrite`, `Scheduler`, `UnionFind`,
  `CostConfig`, `CostWeights`, `CspScheduler`).

## T3 Transposition (folded in)

Per AUDIT-F transposition T3 (mid-tranche §R2 close), the inline-trace
recording channel was lifted from sibling `_with_trace` wrappers into
the canonical pass form via `&mut dyn TraceSink`.

| Surface | Before | After |
|---|---|---|
| `inline_acyclic` | `(ir: &mut GrammarIR)` + sibling `inline_acyclic_with_trace(ir, trace: &mut InlineTrace)` | `(ir: &mut GrammarIR, trace: &mut dyn TraceSink)` |
| `fuse_single_use` | `(ir: &mut GrammarIR)` + sibling `fuse_single_use_with_trace(ir, trace: &mut InlineTrace)` | `(ir: &mut GrammarIR, trace: &mut dyn TraceSink)` |
| `_inner` private helpers | `(ir, mut trace: Option<&mut InlineTrace>)` private indirection | (deleted; canonical pass operates on the trait directly) |
| `crates/ir/src/passes/inline_trace.rs` | `InlineTrace::record` + manual `Option<&mut InlineTrace>` plumbing in callers | New `TraceSink` trait + `NoopTraceSink` zero-overhead unit type. `InlineTrace` impls `TraceSink`. |

The W2.2 production caller in `crates/core/src/pipeline/compile/pipeline.rs` threads an `InlineTrace` directly through the canonical signature; tests that drive a single pass without recording pass `NoopTraceSink`. The recording channel and the canonical pass form are now the same surface.

## Hard Gate Evidence

1. **Build**: `cargo build -p bbnf -p bbnf-ir -p egraph --profile ax-iter` — clean (no errors; pre-existing 187 warnings in generated grammar code unrelated to the carve).
2. **Production grep**: `rg "egraph::ruler|RuleSet::load_from_dir|CompileOptions::rewrites" crates/ src/ xtask/` — zero production matches; the only remaining match is one doc-comment line in `crates/ir/src/rewrites/mod.rs` that narrates what was deleted.
3. **Workspace nextest**: results recorded inline in this ledger after the post-deletion run completes; deleted ruler tests do not count as regressions because they are explicitly removed with the per-test justification above.
4. **Format**: `cargo fmt --check` on modified files — clean.

## Commit

- **Scope**: `egraph-ruler/delete` + `ir-passes/inline-trace-sink` (single combined commit per W4.1 unit; commit body names both transpositions).
- **Hash**: recorded in the commit body and FINAL.md after landing.
