# find_child audit — `crates/core/src/types.rs` (AW-II.W3.3)

## Scope

Owner file (exclusive write for this sub-wave): `crates/core/src/types.rs`.

Wave W3.3 is the narrowest of the three W3 sub-waves. The plan
(§W3.3) flags the file as responsible for "type-annotation decoding"
and describes the scope as "narrower; likely few sites". This audit
discharges the enumeration.

## Method

Grep over the file for every call pattern the plan's DTA migration
classification enumerates:

- `find_child_by_kind` — the legacy direct-child kind scan.
- `children().find(|c| c.rule_kind() == …)` — the inlined form.
- `child(N).rule_kind()` — indexed direct-child dispatch.
- direct `rule_kind`-based match arms dispatching over a view.
- `descendants` walks, `.find(` iterator-based dispatch on a view.

```text
grep -nE 'find_child_by_kind|rule_kind|children\(\)'  crates/core/src/types.rs
grep -nE 'child\(|descendants|\.find\('               crates/core/src/types.rs
grep -nE '\.rhs\.|\.sync_expr\.|view\.|cursor\.'      crates/core/src/types.rs
```

All three greps return **zero matches**.

## Audit table

| File:line | Function | Target rule_kind | Classification | Action |
|-----------|----------|------------------|----------------|--------|
| _(no rows)_ | — | — | — | — |

## Why the file surfaces zero rows

`crates/core/src/types.rs` (131 lines) declares the data-structure
surface of the grammar AST layer, not its walk/dispatch logic:

- `RuleEntry<'a>` — a `{name_span, rhs: BbnfBootstrapNodeView<'a>}`
  record. Holds a view; does not traverse one.
- `ImportDirective`, `RecoverDirective`, `PrettyDirective`,
  `HostFnDecl`, `ImportedName` — directive payload structs. Each is
  a plain data carrier (`Cow<'a, str>`, `Span<'a>`, `Vec<_>`, and in
  `RecoverDirective` a `BbnfBootstrapNodeView<'a>` stored for later
  consumer-side walks).
- `AST<'a>` — a type alias for `IndexMap<&'a str, RuleEntry<'a>>`.
- `GrammarExtract<'a>` — an observational aggregate. Its only `impl`
  is `GrammarExtract::empty()`, which constructs an empty record
  via `IndexMap::new()` / `Vec::new()` / `None`. No view is
  inspected.

The file's sole doc-commented reference to tape traversal is the
`RuleEntry::rhs` comment (lines 9–26) noting that under AC.2 the
RHS is a cursor-backed `BbnfBootstrapNodeView`, not a borrowed
enum. That comment describes consumer-side shape; the consumers
themselves live in `crates/core/src/lower/**` (W3.1) and
`crates/core/src/graph/**` (W3.2).

Type-annotation decoding — the activity the plan gestured at —
runs in `crates/core/src/lower/value_expr.rs` (W4 owner) and in
`crates/core/src/lower/expression.rs` / `call_arg.rs` consumers
(W3.1 owners). `types.rs` holds the downstream payload shapes that
those sites populate, not the decoders.

## Classification vocabulary (referenced for completeness)

Per `AW-II.md` §W3.1 + §W3.2, each call site would fall into one of:

- **DESCENDANT** — target rule_kind is a semantic nested rule; the
  DTA tape wraps its body in a Seq compound one level deeper than
  the fn-per-rule layout. Migration: swap
  `find_child_by_kind(view, X)` for
  `find_descendant_by_kind(view, X)` (substrate helper landed in
  `lower/tape_walk.rs` via W1).
- **LEAF-DIRECT** — target is an identifier / literal / regex
  leaf still emitted as a direct child under DTA. Migration:
  preserve `find_child_by_kind`; audit-only confirmation.
- **SENTINEL** — target is a structural sentinel (delimiter
  literal, terminator, brace alphabet); evaluate case-by-case.
- **DEAD** — call site unreachable post-DTA; delete.

No call site in this file requires any of these migrations.

## Conclusion

`types.rs` call-site audit: **0 rows**. All helpers are
shape-agnostic; the file is a data-structure surface, not a
tape walker. Step 2 (migration) is vacuous for this sub-wave;
step 3 (verification) re-runs the workspace test suite to
confirm no ambient regression introduced by the sibling W3.1 /
W3.2 worktrees (verified separately in the W3.3 report).
