# Agent 3 - Visitor Surface Designer

## §1 Scope + framing

Lens: generated visitors, visit bitsets, mutation discipline, traversal hooks, and how visitors compose with value cursors, path selectors, and ecosystem tools. Restart guidance states mutation happens through visitors rather than ad hoc writable values (`restart/README.md:318`), while BB W5 and the visitor cookbook provide the strongest inherited design (`docs/tranches/BB/waves/W5.md:1-3`, `docs/cookbook/visitors.md:3`).

Phase-1 parallel assumption: PASS-1/PASS-2 syntheses are not consumed as authority under this dispatch. This report treats their prompts as hand-off inputs and records requirements for substrate/codegen rather than waiting for emitted designs.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| Generated `Visitor` trait | W5 specifies grammar-specific visitor methods and separate `Visitor` / `Visit` traits (`docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:11-19`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:27-103`). | Familiar, typed, and aligns with lightningcss-style visitor APIs (`restart/corpora/SOTA.md:103-118`). | Generated trait size can be large for deep grammars. | Emit default no-op methods and split optional extension traits by grammar modules where needed. | KEEP |
| `VisitTypes` bitflag pruning | W5 requires bitflag generation so traversal can skip subtrees cheaply (`docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:107-123`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:178-211`). | Critical for performance and for selector/analysis reuse. | Requires PASS-2 metadata and PASS-1 child ranges to stay in sync. | Make bitsets derive from the same generated kind table used by tape and path validation. | KEEP |
| Mutable visitor as mutation API | README says mutation is visitor-mediated (`restart/README.md:318`). | Gives one auditable mutation channel and avoids random `Value` writes. | Mutating borrowed slices is impossible for some values. | Mutating visitor must write to edit builders or owned/arena nodes, while borrowed views remain immutable. | KEEP |
| Cohort visitors | Cookbook shows visitors can focus on grouped domains (`docs/cookbook/visitors.md:95`, `docs/cookbook/visitors.md:102-124`). | Useful for linting, formatting, imports, and language-server passes. | Too many generated cohorts can bloat docs and compile time. | Generate base trait plus metadata-driven cohorts selected by feature flags. | REINVENT |
| Visitor over direct structs only | Direct struct visitor is ergonomic for generated AST users. | Clear Rust types. | It excludes tape-only tooling, path selectors, DAP, and incremental deltas. | Visitors must operate over direct nodes that carry tape identity or over `ValueRef` adapters. | DISCARD |
| Untyped visitor fallback | A generic `ValueVisitor` over kind IDs helps CLI, fixtures, and playground. | Allows grammar-agnostic tooling and test harnesses. | Not enough for typed authoring. | Keep as lower-level API and generate typed wrappers. | KEEP |
| Error recovery visits | `@recover` already exists in old analysis hover and directives (`crates/analysis/src/features/hover/directive.rs:22-44`, `crates/analysis/src/directives/recover.rs:10-37`). | Visitors can lint recovery usage and collect malformed regions. | Recovery nodes complicate typed traversal. | Generated visitors need explicit `visit_error`, `visit_recovered`, and skip-policy hooks. | KEEP |
| Visitor diagnostics copied verbatim from old docs | Cookbook has exact warnings and lint examples (`docs/cookbook/visitors.md:177-188`, `docs/cookbook/visitors.md:192`). | Preserves tested user guidance. | Error codes may shift in restart. | Preserve message intent, assign restart-wide codes in PASS-3 synthesis. | REINVENT |

## §3 Visitor API commitments

1. **Generated typed visitor.** Each grammar emits:

```rust
pub trait Visitor<'arena, 'input>: Sized {
    const VISIT_TYPES: VisitTypes = VisitTypes::ALL;

    fn visit_document(&mut self, node: &Document<'arena, 'input>) -> ControlFlow {
        walk_document(self, node)
    }

    fn visit_error(&mut self, err: &RecoveredNode<'arena, 'input>) -> ControlFlow {
        walk_recovered(self, err)
    }
}
```

   Generated methods are grammar-specific, but the trait shape is stable across grammars. W5 already contains the codegen sketch (`docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:127-174`).

2. **`VisitTypes` from kind table.** PASS-2 must generate bitflags from the same node-kind table used by tape, path, and metadata. This prevents divergent type registries.

3. **Traversal over identity-bearing nodes.** Direct AST nodes carry tape IDs/spans. `ValueRef` can adapt into visitor traversal. This keeps visitors compatible with tape/on-demand SOTA lessons (`restart/corpora/SOTA.md:64-77`) without exposing tape internals in every method.

4. **Mutation via edit builders.** Borrowed parse documents are immutable. Mutating visitors emit edits or operate on owned/arena documents that can rewrite direct structs while preserving source maps. This honors the README's visitor mutation rule (`restart/README.md:318`) and avoids pretending borrowed substrings can be mutated in place.

## §4 Error and cookbook contract

The visitor cookbook already contains concise examples for collection, rewriting, and pruning (`docs/cookbook/visitors.md:131-147`, `docs/cookbook/visitors.md:153-165`, `docs/cookbook/visitors.md:248`). PASS-3 should keep the example ladder:

1. Count or collect nodes with a typed visitor.
2. Prune with `VisitTypes`.
3. Mutate by returning edits or rewriting owned nodes.
4. Traverse recovery/error nodes deliberately.

Proposed restart diagnostics:

```text
warning[BBNF-VISIT001]: visitor declares no matching node kinds
help: add the desired kind to VISIT_TYPES or remove the visitor

error[BBNF-VISIT002]: borrowed parse tree cannot be mutated in place
help: use parse_owned, parse_in with a mutable arena document, or emit an edit plan

warning[BBNF-VISIT003]: recovery nodes skipped by this visitor
help: implement visit_error or enable VisitTypes::ERROR
```

## §5 Cross-pass hand-offs

PASS-1 must supply cheap child range traversal, node-kind bit IDs, spans, and recovered-node flags in the tape. `VisitTypes` cannot be bolted on after the substrate because pruning depends on contiguous child ranges and sibling skips.

PASS-2 must generate typed visitor traits, default walkers, bitflags, visitor test fixtures, and metadata linking visitor method names to kind IDs. W5b says BC W4 was the receiver for visitor work (`docs/tranches/BB/waves/W5b.md:3`, `docs/tranches/BB/waves/W5b.md:9`); restart codegen should absorb that inheritance directly rather than creating a separate declaration crate.

Ecosystem crates should use visitors instead of custom AST walks. Current analysis and LSP code already reparses/analyzes through a centralized document state (`crates/analysis/src/state/mod.rs:18-26`, `crates/lsp/src/server/mod.rs:56-80`); restart should move those passes onto generated visitor cohorts.

## §6 Risk + mitigation

Risk: visitor APIs become too generated and intimidating. Mitigation: prelude exports only common traits; grammar modules expose detailed methods for advanced use.

Risk: selector/path/visitor traversal each re-implements walking. Mitigation: one traversal engine with three facades: typed visitor, selector plan, generic value visitor.

Risk: mutation loses source maps. Mitigation: edit builders must preserve tape IDs and span provenance, and formatting/layout hooks use `@layout` metadata from the grammar (`restart/README.md:172-176`).

## §7 Verdict

KEEP generated visitors, `VisitTypes`, mutation-through-visitors, generic fallback visitors, and recovery-node hooks. REINVENT cohort visitors and diagnostics around restart metadata. DISCARD direct-struct-only traversal and any mutation API that writes through arbitrary `Value` handles.

## Wave 2 correction note

This agent's visitor-surface obligation feeds two PASS-3 Wave-2 amendments. Visitor LOC carries an explicit per-grammar budget at PASS-3.md §7 (no handwritten file over 500 LOC; +2 percent regen ceiling). The per-grammar feeder table at PASS-3.md §6a names this agent's `<Grammar>Visitor` and `<Grammar>VisitTypes` rows for all nine extant grammars plus yaml; the table is consumed verbatim by Architecture's per-X surface.
