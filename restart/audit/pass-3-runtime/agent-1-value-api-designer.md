# Agent 1 - Value API Designer

## §1 Scope + framing

Lens: public runtime value surfaces, ownership/lifetime ergonomics, host-function values, and cookbook-facing APIs for generated grammars. PASS-3 owns the top user layer for the restart (`restart/README.md:100`, `restart/README.md:410`), but this report resolves stale prompt vocabulary toward the settled authority: tape remains the substrate and is unioned with direct-to-struct, while `ParseStream` is only a stale name in the prompt stack (`restart/prompts/PASS-3-RUNTIME.md:3`, `restart/inheritance/INDEX.md:65-66`).

Phase-1 parallel assumption: PASS-1 and PASS-2 syntheses are not consumed as authority under this dispatch. This report therefore reads their prompts and the shared restart README, then leaves substrate/codegen reconciliation to SYNTHESIS rather than blocking on sister PASS syntheses.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| Slice-borrow default parse surface | Generated grammars expose `parse(&str)` as the default borrow-preserving API, matching the Lock 9 requirement for slice-borrow first, `parse_in`, and `parse_owned` (`restart/locks/14-LOCKS.md:50`). | Keeps fast paths close to sonic-style borrowed values and avoids forcing allocation on JSON/CSS/simple grammars. | Error messages around arenas and source lifetimes are a known friction point in BB W4 (`docs/tranches/BB/waves/W4.md:60`, `docs/cookbook/lifetime-surfaces.md:69-89`). | The API must make the common path lifetime-free at call sites; named lifetime forms belong in advanced constructors and docs, not in every example. | KEEP |
| `parse_in` arena surface | `parse_in(source, arena)` exists for high-throughput, batch, and tree-retention workflows, following W4a's arena naming and examples (`docs/tranches/BB/waves/W4a.md:16-19`, `docs/cookbook/lifetime-surfaces.md:27-41`). | Enables bulk allocation without per-node churn and creates a clear home for direct-to-struct ASTs. | Arena mismatch diagnostics are a support burden and must be precise (`docs/cookbook/lifetime-surfaces.md:93-105`). | PASS-2 must generate one canonical arena trait/interface rather than each grammar inventing its own allocator story. | KEEP |
| `parse_owned` retained-source surface | Owned documents deep-copy or otherwise retain source bytes so value references can outlive caller buffers, as mandated by Lock 9 (`restart/locks/14-LOCKS.md:50`). | Gives CLI, LSP, playground, and cache users a safe durable object without teaching lifetimes first. | Owned mode can hide avoidable copies if it becomes the default. | Bench output must show owned overhead against borrowed/arena modes so users can choose intentionally. | KEEP |
| Direct-to-struct + tape union | The runtime exposes typed structs for normal use and a tape-backed value cursor for reflective/query/debug paths; restart guidance explicitly requires a proper union of tape and direct-to-struct (`restart/README.md:285-314`, `restart/locks/14-LOCKS.md:34`). | Maintains ergonomic generated ASTs while preserving simdjson/sonic-style on-demand traversal options (`restart/corpora/SOTA.md:64-77`). | If the API treats tape as a second-class debug artifact, path, visitor, and LSP features will fork. | Value cursors must be first-class, typed, and able to project into direct structs without reparse. | REINVENT |
| Untyped catch-all `Value` | A universal `Value` exists for tools, generic fixtures, and unknown grammars, informed by the old Value API inheritance (`restart/inheritance/INDEX.md:37`). | Useful for playground, CLI `--json`, path projections, and metadata-driven tests. | A single untyped value can become the whole API and erase grammar-specific types. | Keep it behind `Document::root_value()` / `Cursor` access and make generated typed roots the normal entrypoint. | KEEP |
| Per-grammar declaration crate | Old plans had per-grammar declaration crates, but Amendment 01 deletes that route and replaces it with generic primitives, workspace metadata, or `@host fn` (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-22`, `restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:36-56`). | Avoids workspace sprawl and stops every grammar from requiring Rust support code. | Some host domains may still need bespoke adapters. | Any escape valve must be explicitly rare, named in metadata, and reviewed as an exception. | DISCARD |
| Grammar-level Unicode algebra values | Restart README settles Unicode class algebra as deferred to parse-that/regex rather than grammar-level BBNF (`restart/README.md:123`, `restart/README.md:133-143`). | Keeps Value API about parsed grammar data, not regex set algebra. | Users may ask for named classes in grammar examples. | Docs should route Unicode examples to lexer/regex facilities and avoid grammar-level APIs. | DISCARD |
| Rewrite-mode value hooks | Dispatch authority says rewrite-mode is out; README line `restart/README.md:473` is stale where it includes rewrite-mode in an extension list. | Removes semantic ambiguity from runtime values. | Existing prompt text may still refer to rewrites. | PASS-3 should call this conflict out and ensure generated APIs do not reserve rewrite hook names. | DISCARD |

## §3 Architectural commitments ratified

1. **Root type shape.** Every grammar emits a root `Document<'arena, 'input>` (or grammar-specific alias) with typed accessors, a `source()` view, diagnostics, and `root_value()` for generic tooling. Simple borrowed parse examples use elided lifetimes, while generated type definitions retain the two-axis model from BB W4: source lifetime and allocation lifetime (`docs/tranches/BB/waves/W4.md:66-74`, `docs/cookbook/lifetime-surfaces.md:13-23`).

2. **Three constructors.**
   - `Grammar::parse(source: &str) -> Result<Document<'_, '_>, ParseError>`
   - `Grammar::parse_in(source: &str, arena: &impl Arena) -> Result<Document<'_, '_>, ParseError>`
   - `Grammar::parse_owned(source: impl Into<String>) -> Result<OwnedDocument, ParseError>`

   The public cookbook should lead with `parse`, then show `parse_in` for batch parsing and `parse_owned` for storage, matching the cookbook's decision flow (`docs/cookbook/lifetime-surfaces.md:45-65`).

3. **Value cursor.** `ValueRef<'doc, 'input, K>` points at a typed tape slot and can borrow direct struct fields. It is the shared substrate for `path!`, visitors, debug stepping, playground inspection, and LSP feature extraction. This follows the README union mandate (`restart/README.md:285-314`) and overrides the stale `ParseStream` naming in PASS-3 (`restart/prompts/PASS-3-RUNTIME.md:79-81`).

4. **Host values.** `@host fn`, multi-function chaining, generics, `@error`, and `@layout` remain in. The runtime models host outputs as typed values with metadata-declared purity and borrowing constraints, not as per-grammar crates. README anchors these features at `restart/README.md:155`, `restart/README.md:161`, and `restart/README.md:172-176`; Amendment 01 gives the non-crate decomposition path (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:36-56`).

## §4 Proposed Value API skeleton

```rust
pub trait Grammar {
    type Root<'arena, 'input>;
    type OwnedRoot;

    fn parse(input: &str) -> Result<Self::Root<'_, '_>, ErrorBundle>;
    fn parse_in<'arena, 'input>(
        input: &'input str,
        arena: &'arena Arena,
    ) -> Result<Self::Root<'arena, 'input>, ErrorBundle>;
    fn parse_owned(input: impl Into<String>) -> Result<Self::OwnedRoot, ErrorBundle>;
}

pub trait DocumentView<'input> {
    fn source(&self) -> &'input str;
    fn root_value(&self) -> ValueRef<'_, 'input, RootKind>;
    fn diagnostics(&self) -> &[Diagnostic];
}
```

The generated surface should avoid exposing tape internals in first examples, but it must not hide tape from advanced consumers. The old `bbnf-path` API already exposes a typed-path idea (`crates/bbnf-path/src/lib.rs:1-22`, `crates/bbnf-path/src/lib.rs:28-37`), but its grammar markers and registries are fixture-bound rather than metadata-driven (`crates/bbnf-path/src/registry.rs:80-98`, `crates/bbnf-path/src/registry.rs:125-138`). PASS-3 should preserve typed access while replacing fixture registries with generated metadata.

## §5 Cross-pass hand-offs

PASS-1 must provide a stable tape token ABI, span encoding, payload encoding, and projection hook so the Value API can be zero-copy over tape and direct structs. PASS-1 prompts still contain `ParseStream` and rewrite-mode clauses (`restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:31`, `restart/prompts/PASS-1-SUBSTRATE.md:66`); SYNTHESIS must normalize those to tape and the settled extension set.

PASS-2 must emit the three constructors, the `DocumentView` implementation, typed value kinds, host-function ABI shims, and metadata descriptors for path/visitor/LSP. PASS-2 prompt references to `ParseStream` and stale extension rails should likewise be treated as stale (`restart/prompts/PASS-2-CODEGEN.md:33`, `restart/prompts/PASS-2-CODEGEN.md:81`).

## §6 Risk + mitigation

Risk: lifetime-heavy generated signatures could recreate the W4 friction. Mitigation: lead with simple `parse(&str)` and provide exact lifetime diagnostics from the cookbook (`docs/cookbook/lifetime-surfaces.md:69-89`, `docs/cookbook/lifetime-surfaces.md:109-139`).

Risk: tape/direct union could become two runtimes. Mitigation: make `ValueRef` the common cursor and require typed structs to carry tape identity.

Risk: host function decomposition could leak into per-grammar crates. Mitigation: metadata-driven host registries only, with rare escape valves explicitly named and reviewed, preserving Amendment 01's zero-crate invariant (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:156-161`).

## §7 Verdict

KEEP the three public constructors, typed generated roots, untyped tool value, and host-function extension set. REINVENT the value substrate as a first-class tape/direct-to-struct union. DISCARD per-grammar declaration crates, grammar-level Unicode algebra, rewrite-mode APIs, and all public `ParseStream` naming.
