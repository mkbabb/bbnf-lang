# Agent 2 - Path + Select DSL Designer

## §1 Scope + framing

Lens: typed pointer syntax, selector DSL, cross-language projections, validation metadata, and how `path!`/`select!` should sit on top of the tape/direct-to-struct union. PASS-3 inherits Value API, path, and visitor work as a linked surface (`restart/inheritance/INDEX.md:37`) and is explicitly responsible for path crates in the restart top layer (`restart/README.md:100`).

Phase-1 parallel assumption: PASS-1/PASS-2 syntheses are not consumed as authority under this dispatch. This report reads the prompts, README, locks, BB W5 materials, current `bbnf-path`, and current TypeScript mirror, then hands ABI/codegen details to SYNTHESIS.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| `path!` typed pointer macro | BB W5 settled on typed path surfaces and a pointer syntax decision that accepts both explicit type suffixes and implicit terminal inference (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:20-27`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:148-156`). | Compile-time validation gives strong diagnostics and lets generated code avoid stringly runtime lookups. | Current `bbnf-path` is fixture-backed and grammar-marker bound (`crates/bbnf-path/src/registry.rs:80-98`). | Rebuild validation on grammar metadata descriptors, not hardcoded fixture registries. | REINVENT |
| `select!` query macro | `select!` builds multi-result traversal over the same typed segments, complementing single-pointer `path!` and visitor pruning. | Needed for CLI extraction, playground inspection, LSP features, and bulk visitors. | Could duplicate visitor traversal and fragment performance work. | Implement selectors as compiled traversal plans over the common cursor/tape identity. | KEEP |
| Explicit pointer suffix | The W5 decision keeps `#Name` type suffixes for disambiguation (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:31-87`). | Stable, readable, and gives deterministic errors for ambiguous grammar fields. | More syntax for simple paths. | Keep suffix optional when metadata proves a unique terminal. | KEEP |
| Implicit terminal inference | W5 recommends supporting both explicit and implicit syntax (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:20-27`). | Keeps common examples compact. | Error quality is harder when many fields share names. | Error messages must include candidate grammar paths and suggested suffixes. | KEEP |
| Current `TypedPath::<..., ()>` terminal placeholder | Current proc-macro code still emits `()` for terminal type in paths (`crates/bbnf-path/src/path_macro.rs:198-199`). | It let older tests progress before metadata was ready. | It is not a real typed contract and blocks downstream type safety. | PASS-2 must emit terminal marker types or metadata IDs that the macro can resolve. | DISCARD |
| Hardcoded grammar registry | Current `registry.rs` maps only `Json`, `CssL4`, `Sheets`, and `Bbnf` marker names (`crates/bbnf-path/src/registry.rs:80-98`). | Simple for fixtures. | Violates Lock 14 generality and no-overfit guidance (`restart/locks/14-LOCKS.md:60`, `restart/README.md:11-25`). | Replace with metadata discovery generated for every grammar. | DISCARD |
| TypeScript duplicated compiler | Current `bbnf-path-ts` duplicates path parsing because the proc-macro crate cannot be consumed in TS (`crates/bbnf-path-ts/src/compile.rs:1-12`, `crates/bbnf-path-ts/src/compile.rs:41-65`). | Demonstrates TS playground/extension demand. | A duplicated compiler will drift from Rust. | Move shared parsing/lowering to `bbnf-path-core`, expose Rust proc-macro and TS bindings as shells. | REINVENT |
| Runtime string paths only | Avoiding macros and validating at runtime would simplify crate boundaries. | Useful as fallback for dynamic UI input. | Loses compile-time diagnostics, one of the W5 wins. | Support `Path::parse_with_metadata` for dynamic tools, but macros remain the authored Rust API. | KEEP |

## §3 Path/select surface commitments

1. **Crate split.** `bbnf-path-core` owns lexical parsing, AST, lowering, metadata validation, diagnostics, and runtime traversal plans. `bbnf-path` owns Rust proc macros. `bbnf-path-ts` owns TS template tags and schema consumption. Lock 7 already requires path consolidation (`restart/locks/14-LOCKS.md:46`), and current code proves duplication exists (`crates/bbnf-path-ts/src/compile.rs:1-12`).

2. **Metadata-first validation.** Generated grammars emit a grammar metadata descriptor containing node kinds, field names, terminal markers, multiplicity, optionality, and type IDs. This replaces fixture-derived descriptors in current `bbnf-path` (`crates/bbnf-path/src/path_macro.rs:146-210`, `crates/bbnf-path/src/registry.rs:185-200`) and aligns with Amendment 01's generic workspace metadata route (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:36-56`).

3. **Two authored forms.**

```rust
let title = path!(CssStylesheet => "/rules/0#Rule/selector#Selector");
let decls = select!(CssStylesheet => "/rules/*#Rule/declarations/*#Declaration");
```

   The exact grammar names are illustrative; generated metadata supplies the real type IDs. Single pointer returns `TypedPath<Root, Terminal>`. Selector returns `SelectorPlan<Root, Item>` with iteration/projection.

4. **Runtime projection API.** Both macros lower to `PathPlan`/`SelectorPlan` over `ValueRef`, not over direct structs alone. This is required so CLI, DAP, playground, and LSP can operate on the same tape identity as runtime users.

## §4 Diagnostics contract

BB W5 already drafted pointer diagnostics and cookbook flows (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:92-128`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:169-176`). PASS-3 should preserve that shape and add three restart-wide diagnostics:

```text
error[BBNF-PATH001]: unknown grammar path segment `foo`
help: known fields on `CssRule` are `selector`, `block`, `span`

error[BBNF-PATH002]: ambiguous terminal `name`
help: add a type suffix such as `#Identifier` or `#PropertyName`

error[BBNF-PATH003]: selector result type is not statically uniform
help: use `select_value!` for heterogeneous results or split the selector
```

The cookbook's current `path-macro.md` already teaches typed paths and examples (`docs/cookbook/path-macro.md:3-31`, `docs/cookbook/path-macro.md:140-181`). Restart docs should preserve the examples but rewrite crate names and generated metadata assumptions.

## §5 Cross-pass hand-offs

PASS-1 must guarantee stable tape identity, node-kind IDs, sibling/child traversal, spans, and payload access needed by `PathPlan`. The old `ParseStream` word in PASS-1 is stale and must not leak into path APIs (`restart/prompts/PASS-1-SUBSTRATE.md:3`).

PASS-2 must emit metadata descriptors and typed marker modules for every generated grammar, including field multiplicity and terminal IDs. It must not emit per-grammar declaration crates; Amendment 01 explicitly routes declarations through generated runtime subdirs and metadata (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:30-32`, `restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:123-130`).

SYNTHESIS must reconcile path crate names with the final workspace, because old docs use `bbnf-path` while restart asks PASS-3 to design `path`, `path-core`, and related user crates (`restart/README.md:100`).

## §6 Risk + mitigation

Risk: selector traversal duplicates visitor pruning. Mitigation: selector lowering produces a visitor-like traversal plan with the same `VisitTypes` bitsets and tape child ranges.

Risk: TS path support drifts. Mitigation: ship the grammar path schema as generated JSON/TS and keep the path parser/lowerer in `bbnf-path-core`; TS bindings consume the same schema.

Risk: metadata descriptors become a hidden declaration crate. Mitigation: descriptor emission is generated into the grammar runtime module and serialized workspace metadata; no separate per-grammar crate is allowed by default (`restart/inheritance/INDEX.md:62`).

## §7 Verdict

KEEP typed `path!`, `select!`, explicit suffixes, implicit inference, dynamic runtime parsing, and cookbook examples. REINVENT path implementation around `bbnf-path-core` plus generated metadata. DISCARD fixture-bound registries, `()` terminal placeholders, Rust/TS duplicated compilers, and runtime-string-only designs.

## Wave 2 correction note

The authored Rust macro is `pointer!` (this agent's `path!` references survive only as legacy citation). The crate set is `path`, `path-core`, `path-ts`, `test-fixtures` (no `bbnf-` prefix). Hardcoded grammar marker registries are not a deferral — they are a deletion item bound to the close gate at PASS-3.md §3. The diagnostic codes `BBNF-POINTER001` (unknown segment), `BBNF-POINTER002` (grammar-inference failure), and `BBNF-POINTER003` (stale schema) appear verbatim at PASS-3.md §6b and must round-trip into the cookbook receivers.
