# find_child_by_kind audit — `crates/core/src/lower/**` (AW-II.W3.1)

Wave: AW-II.W3.1
Owner files: `crates/core/src/lower/expression.rs`, `lower/mod.rs`,
`lower/tape_walk.rs`, `lower/metadata.rs`, `lower/string_interner.rs`,
`lower/fn_table.rs`
Excluded from migration: `lower/value_expr.rs` (W4 scope).

## Method

`grep -rn 'find_child_by_kind\|children()\.find\|child([0-9]).*rule_kind\|peel_transparent'`
against the owner files, complemented with a top-to-bottom read of
`expression.rs` for the `for c in node.children()` + `if c.rule_kind() ==`
idiom and direct `child(N)` positional reads.

Classifications:

- **DESCENDANT** — target is a nested-rule output that DTA wraps one
  Seq deeper AND whose depth-unbounded descent is bounded by an
  external gate (e.g. span-text predicate). Migrate to
  `find_descendant_by_kind`.
- **DESCENDANT-SIBLING** — target is a sibling-level body component
  separated from the caller's view by one or more anonymous
  structural wrappers (Seq / Alt / Repeat with `rule_kind` ∈
  {`Unknown`, `int_lit` sentinel}). A blind descendant walk would
  cross into a sibling's own grammar subtree; the correct primitive
  is `find_sibling_by_kind` / `collect_siblings_by_kind` which stop
  at any semantic-rule compound that isn't the target. Derived during
  W3.1's contact with the lowering pipeline (see "Substrate addition"
  below).
- **LEAF-DIRECT** — target is a span-only terminal (identifier /
  literal / regex) that the grammar still surfaces as a direct child
  under DTA. Keep the direct scan (cheaper; the rule is a
  single-token leaf).
- **SENTINEL** — call site reads a structural wrapper or uses
  `rule_kind()` comparison for sub-dispatch (not a descent search).
  No call-site rewrite — leave the read in place.
- **DEAD** — call site unreachable under DTA. Delete.

## Audit

| File:line | Function | Target rule_kind | Classification | Action |
|-----------|----------|------------------|----------------|--------|
| `expression.rs:34` | (import) | `find_child_by_kind`, `find_descendant_by_kind`, `iter_rep_children`, `peel_transparent` | SENTINEL | Helper re-exports; no change. |
| `expression.rs:55` | `lower_rhs` | `peel_transparent(node)` | SENTINEL | Whitelist peel through `rhs` / `grammar_item` / `directive` / `lhs` — not a rule-kind search, keep. |
| `expression.rs:76` | `dispatch_expression` | `peel_transparent(node)` | SENTINEL | Same as above. |
| `expression.rs:155` | `dispatch_expression` | `match node.rule_kind()` | SENTINEL | Dispatch on view's own kind — not a child scan. |
| `expression.rs:453` | `recognize_binary_operator` | `child.rule_kind() == binary_operators` | SENTINEL | Matches on the scanning cursor's own kind (caller already walks flattened operand list via `collect_binary_operands`); belt-and-braces rule-id check, keep. |
| `expression.rs:525-526` | `is_iteration_pair_wrapper` | `view.rule_kind() == mapped_factor` / `binary_operators` | SENTINEL | Wrapper classifier; reads own kind. |
| `expression.rs:557` | `iter_pair_children` | `child.rule_kind() == int_lit` | SENTINEL | Classifier on own child — not a descent. |
| `expression.rs:561-562` | `iter_pair_children` | `c.rule_kind() == binary_operators` (via `child.children().find`) | DESCENDANT | **Migrate**. Peels one level into an anonymous Seq wrapper to surface the `binary_operators` Alt. Under DTA the Alt may sit two levels deep. |
| `expression.rs:734` | `lower_mapped_factor` → `find_value_expr_child` | value_expr layer kinds | SENTINEL | Already uses `find_descendant_by_kind` internally. |
| `expression.rs:806` | `find_type_annotation_child` | `type_annotation` | SENTINEL | Already uses `find_descendant_by_kind`. |
| `expression.rs:835` | `lower_factor` | `term` | DESCENDANT-SIBLING | **Migrate** via direct-child classifier + `find_sibling_by_kind` fallback (W3.1.6). A blind descendant walk over-reaches: the term may contain grouped sub-expressions whose own factors carry their own terms. The direct-child loop classifies each slot by trimmed span text; sibling-scoped descent is the correct primitive for a modifier lookup if the upfront classifier misses. |
| `expression.rs:850` | `lower_factor` | `modifier` | DESCENDANT-SIBLING | **Migrate** via direct-child classifier (W3.1.6). Same reasoning — `modifier`'s span is one of `?w` / `?` / `*` / `+`, disambiguating it at the direct-child level without descending into the term's subtree. |
| `expression.rs:1213` | doc-comment | "`peel_transparent` — routes through here" | SENTINEL | Prose, no code. |
| `expression.rs:1357-1370` | `find_inner_expression` | expression-layer kinds | SENTINEL | Already uses `find_descendant_by_kind` with fallback descent. |
| `expression.rs:1406` | `lower_identifier_with_optional_call` | `identifier` | DESCENDANT-SIBLING | **Migrate** via `find_sibling_by_kind` (W3.1.4 + W3.1.5 correction). The term rule's identifier branch is `identifier , ( "(" , call_arg ?w , (...) * , ")" )?` — under DTA the identifier record sits inside one or more anonymous Seq / Alt wrappers alongside the call-arg list. Sibling-scoped descent finds the identifier without crossing into a nested expression's own identifier. Fallback arm (first-substantive child search) remains for the optimizer-inlined shape. |
| `expression.rs:1408-1411` | `lower_identifier_with_optional_call` | (fallback) `children().find(span>0)` | LEAF-DIRECT | **Keep**. This is the fallback when the identifier sibling descent already produced no result — it picks the first non-empty-span child, useful under the optimizer-inlined shape where the identifier surfaces as a bare leaf. Leaving as direct scan — the descendant fallback would re-enter the same tree. |
| `expression.rs:1424-1427` | `lower_identifier_with_optional_call` | `call_arg` (filter over children) | DESCENDANT-SIBLING | **Migrate** via `collect_siblings_by_kind` (W3.1.4 + W3.1.5 correction). Multiple `call_arg` compounds surface as positional siblings — sibling-scoped collection gathers them without flattening the nested call_args inside any individual argument's body. |
| `expression.rs:1533` | `lower_map_arrow` | `ann.rule_kind() == type_annotation` | SENTINEL | Guards on the already-resolved `type_annotation` view's own kind. |
| `expression.rs:1535-1540` | `lower_map_arrow` | `type_node.rule_kind()` match | SENTINEL | Classifier on own kind. |
| `mod.rs:242-332` | `extract_closure_def` | positional `node.child(N)` reads | SENTINEL | Closure / alternation / concatenation / binary_factor / mapped_factor / factor positional-child reads that peek at specific slots by grammar position. These are NOT rule-kind scans — they peel specific grammar positions (`child(1)` = first param, `child(2)` = rest params, etc.) inside a well-known compound structure. Closures are extracted from pre-lowered closure bodies where the fn-per-rule tape shape was preserved (W1 closure wrapping is upstream of DTA's tape-shape changes); the calls receive views over already-stable shapes. Leave as-is for this wave. |
| `metadata.rs:44-52` | `build_rule_meta` | `ctx.recovers.and_then(|r| r.get(name))` | SENTINEL | Map lookup against recovered directive table; no rule-kind search. |
| `tape_walk.rs:82-87` | `find_child_by_kind` | definition | SENTINEL | Helper definition. Keep. |
| `tape_walk.rs:106-122` | `find_descendant_by_kind` | definition | SENTINEL | Helper definition. Keep. |
| `tape_walk.rs:141-158` | `peel_transparent` | definition | SENTINEL | Helper definition. Keep. |

## Row counts

- **DESCENDANT** (genuine depth-unbounded descent — span-text gate
  bounds the search): 1
  - `expression.rs:561-562` — binary_operators descent inside
    iteration-pair wrapper (operator-span gate bounds)
- **DESCENDANT-SIBLING** (sibling-scoped — descend only through
  anonymous structural wrappers, stop at semantic-rule compounds): 4
  - `expression.rs:835` — `term` child of `factor` (resolved via
    upfront direct-child classifier, find_sibling_by_kind as
    fallback)
  - `expression.rs:850` — `modifier` child of `factor` (resolved via
    upfront direct-child classifier)
  - `expression.rs:1406` — `identifier` child of `term` (sibling
    descent via find_sibling_by_kind)
  - `expression.rs:1424-1427` — `call_arg` filter (sibling collection
    via collect_siblings_by_kind)
- **LEAF-DIRECT** (keep as direct scan): 1
  - `expression.rs:1408-1411` — fallback first-substantive-child
    after the identifier sibling descent already returned `None`.
- **SENTINEL** (classifier / own-kind read / helper definition /
  positional closure peel): 18
- **DEAD** (unreachable): 0

Total distinct call sites migrated: 5.

## Substrate addition (tape_walk.rs)

### Initial design (W3.1.1) — revised in W3.1.5

Initially added `collect_descendants_by_kind` mirroring
`grammar::host::collect_pretty_hint_descendants`:

```rust
pub(crate) fn collect_descendants_by_kind<'tape>(...);
```

Contact revealed that blind depth-first descent over-reaches when the
targets are "body-component siblings" rather than "occurrences at any
depth". A `factor`'s `modifier` must NOT descend into the sibling
`term`'s subtree (which may contain nested factors with their own
modifiers); a term compound's `call_arg` list must NOT flatten the
call_args nested inside any individual argument's own body.

### Final design (W3.1.5)

Two primitives, both scoped to "sibling-level body components
separated from the caller's view by one or more anonymous structural
wrappers":

```rust
pub(crate) fn find_sibling_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
) -> Option<BbnfBootstrapNodeView<'tape>>;

pub(crate) fn collect_siblings_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
    out: &mut Vec<BbnfBootstrapNodeView<'tape>>,
);
```

Descent gate: `is_anonymous_wrapper(child)` — true iff `child` is a
Rule / Seq / Alt / Repeat compound whose `rule_kind` is `Unknown` or
the `int_lit` sentinel (walker's variant_idx=0 convention for
compounds emitted without a `DtaState::Ref` dispatch). Genuine
int_lit value-expr leaves carry `TapeKind::Span` / `Literal` /
`Regex` and are thereby excluded from the descent.

`collect_descendants_by_kind` is replaced by `collect_siblings_by_kind`;
`find_descendant_by_kind` retains its purpose (descent into a
single-hit target that genuinely may sit arbitrarily deep — e.g. the
`binary_operators` Alt inside an operator-spanned Seq wrapper whose
span-text gate already bounds the search).

## Producer-side escalation

None. Every row resolves in-wave via consumer-side migration or the
one-new-helper substrate addition. W1 and W2's producer surface
(walker/lifter/emitter) is held stable as the invariant requires.
