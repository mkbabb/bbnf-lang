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

- **DESCENDANT** — target is a nested-rule output that DTA wraps one Seq
  deeper. Migrate to `find_descendant_by_kind`.
- **LEAF-DIRECT** — target is a span-only terminal (identifier / literal /
  regex) that the grammar still surfaces as a direct child under DTA.
  Keep the direct scan (cheaper; the rule is a single-token leaf).
- **SENTINEL** — call site reads a structural wrapper or uses
  `rule_kind()` comparison for sub-dispatch (not a descent search). No
  call-site rewrite — leave the read in place.
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
| `expression.rs:835` | `lower_factor` | `term` | DESCENDANT | **Migrate**. `factor = big_comment? , term ?w , modifier? , big_comment?` under DTA emits these as a Seq body of factor; `term` sits inside that Seq, not as a direct child. |
| `expression.rs:850` | `lower_factor` | `modifier` | DESCENDANT | **Migrate**. Same reasoning — `modifier` is a semantic nested rule inside the factor Seq body. |
| `expression.rs:1213` | doc-comment | "`peel_transparent` — routes through here" | SENTINEL | Prose, no code. |
| `expression.rs:1357-1370` | `find_inner_expression` | expression-layer kinds | SENTINEL | Already uses `find_descendant_by_kind` with fallback descent. |
| `expression.rs:1406` | `lower_identifier_with_optional_call` | `identifier` | DESCENDANT | **Migrate**. The term rule's identifier branch is `identifier , ( "(" , call_arg ?w , (...) * , ")" )?` — under DTA the whole body is a Seq compound and the `identifier` record sits inside that Seq alongside the call-arg Repeat. Direct-child lookup misses the identifier behind the Seq wrapper. Fallback arm (first-substantive child search) remains as the identifier-inlined path. |
| `expression.rs:1408-1411` | `lower_identifier_with_optional_call` | (fallback) `children().find(span>0)` | LEAF-DIRECT | **Keep**. This is the fallback when the identifier rule-kind descent already produced no result — it picks the first non-empty-span child, useful under the optimizer-inlined shape where the identifier surfaces as a bare leaf. Leaving as direct scan — the descendant fallback would re-enter the same tree. |
| `expression.rs:1424-1427` | `lower_identifier_with_optional_call` | `call_arg` (filter over children) | DESCENDANT | **Migrate**. Multiple `call_arg` compounds surface — a descendant collector (not a single-hit descent) is required. Introduce `collect_descendants_by_kind` in `tape_walk.rs` to mirror `host.rs::collect_pretty_hint_descendants`. |
| `expression.rs:1533` | `lower_map_arrow` | `ann.rule_kind() == type_annotation` | SENTINEL | Guards on the already-resolved `type_annotation` view's own kind. |
| `expression.rs:1535-1540` | `lower_map_arrow` | `type_node.rule_kind()` match | SENTINEL | Classifier on own kind. |
| `mod.rs:242-332` | `extract_closure_def` | positional `node.child(N)` reads | SENTINEL | Closure / alternation / concatenation / binary_factor / mapped_factor / factor positional-child reads that peek at specific slots by grammar position. These are NOT rule-kind scans — they peel specific grammar positions (`child(1)` = first param, `child(2)` = rest params, etc.) inside a well-known compound structure. Closures are extracted from pre-lowered closure bodies where the fn-per-rule tape shape was preserved (W1 closure wrapping is upstream of DTA's tape-shape changes); the calls receive views over already-stable shapes. Leave as-is for this wave. |
| `metadata.rs:44-52` | `build_rule_meta` | `ctx.recovers.and_then(|r| r.get(name))` | SENTINEL | Map lookup against recovered directive table; no rule-kind search. |
| `tape_walk.rs:82-87` | `find_child_by_kind` | definition | SENTINEL | Helper definition. Keep. |
| `tape_walk.rs:106-122` | `find_descendant_by_kind` | definition | SENTINEL | Helper definition. Keep. |
| `tape_walk.rs:141-158` | `peel_transparent` | definition | SENTINEL | Helper definition. Keep. |

## Row counts

- **DESCENDANT** (migrate to `find_descendant_by_kind` / descendant
  collector): 4
  - `expression.rs:561-562` — binary_operators descent inside
    iteration-pair wrapper
  - `expression.rs:835` — `term` child of `factor`
  - `expression.rs:850` — `modifier` child of `factor`
  - `expression.rs:1406` — `identifier` child of `term` (call-arg
    dispatcher)
  - `expression.rs:1424-1427` — `call_arg` filter (needs a descendant
    collector helper)
- **LEAF-DIRECT** (keep as direct scan): 1
  - `expression.rs:1408-1411` — fallback first-substantive-child after
    the identifier rule-kind descent already returned `None`.
- **SENTINEL** (classifier / own-kind read / helper definition /
  positional closure peel): 18
- **DEAD** (unreachable): 0

Total distinct call sites: 5 DESCENDANT migrations (the 4 bullets above
count `expression.rs:1424` separately because it becomes a new helper
call site).

## Substrate addition (tape_walk.rs)

The `lower_identifier_with_optional_call` call-arg filter needs a
many-hit descendant collector (not the single-hit
`find_descendant_by_kind`). Mirror `host.rs::collect_pretty_hint_descendants`
with a generic reusable helper:

```rust
pub(crate) fn collect_descendants_by_kind<'tape>(
    view: BbnfBootstrapNodeView<'tape>,
    target: BbnfBootstrapRuleKind,
    out: &mut Vec<BbnfBootstrapNodeView<'tape>>,
);
```

The helper stops descent at the first target hit along each branch —
nested targets within a target's own subtree are typically the same
rule (e.g. `call_arg` inside another `call_arg` would be a grammar-level
composition, not a sibling), so stop-at-hit semantics preserve the
caller's intent.

## Producer-side escalation

None. Every row resolves in-wave via consumer-side migration or the
one-new-helper substrate addition. W1 and W2's producer surface
(walker/lifter/emitter) is held stable as the invariant requires.
