# W3 Consolidated — find_child_by_kind Migration Audit

AW-II.W3 wave produced three parallel audits, one per file-bounds partition.
This file indexes the per-partition audit, summarises counts, and records
the W3-close workspace state.

## Audit partitions

| Scope | File | Owner | Commits |
|-------|------|-------|---------|
| `crates/core/src/lower/**` (except `value_expr.rs`) | [find-child-audit-lower.md](find-child-audit-lower.md) | W3.1 | `02a57978` → `efcb6b44` (8 commits) |
| `crates/core/src/graph/**` | [find-child-audit-graph.md](find-child-audit-graph.md) | W3.2 | `b66002d2`, `1dab1bd5`, `12c9690f` |
| `crates/core/src/types.rs` | [find-child-audit-types.md](find-child-audit-types.md) | W3.3 | `c39412d2` |

## Aggregate row counts

| Partition | DESCENDANT | LEAF-DIRECT | SENTINEL | DEAD | Total |
|-----------|:---:|:---:|:---:|:---:|:---:|
| lower/** | 5 (migrated) | 1 | 18 | 0 | 24 |
| graph/** | 4 (migrated) | 18 | 2 (W4 deferred) | 0 | 24 |
| types.rs | 0 | 0 | 0 | 0 | 0 |

`types.rs` is a pure data-structure module (`RuleEntry`, `ImportDirective`,
`RecoverDirective`, `PrettyDirective`, `HostFnDecl`, `GrammarExtract`) that
holds `BbnfBootstrapNodeView<'a>` as a field value but never invokes view
methods; the "type-annotation decoding" scope is in `lower/expression.rs`
(W3.1) and `lower/value_expr.rs` (W4).

## Substrate additions

W3.1 landed two new `lower/tape_walk.rs` primitives alongside W1's
`find_descendant_by_kind`:

- `collect_descendants_by_kind` — pre-order recursive gather of every
  descendant with matching `rule_kind()`. Mirrors the existing
  `find_descendant_by_kind`/`collect_identifier_descendants` pair.
- `find_sibling_by_kind` / `collect_siblings_by_kind` — sibling-scoped
  descent that peels anonymous structural wrappers (Rule/Seq/Alt/Repeat
  with `rule_kind ∈ {Unknown, int_lit}`) and stops at any semantic-rule
  compound that isn't the target. Closes the correctness trap where
  `find_descendant_by_kind(factor, modifier)` returns a nested expression's
  own modifier instead of the factor's sibling modifier.

## W4 deferrals

Two `graph/metadata.rs` sites were classified SENTINEL and left for W4's
`value_expr` wave:

- `metadata.rs:126-131` — `mapped_factor` mapping/inner positional-child
  pattern. Closely coupled to the `->` map-expression lowering; landing
  in W3 would have forced W4 to unwind and re-migrate.
- `metadata.rs:136` — the same pattern at a sibling scope.

Both sites currently function correctly because the mapping-arm children
under DTA surface as direct children at HEAD's committed tape shape.
W4 will re-audit after the `value_expr` migration lands.

## Workspace state at W3 close

`cargo check --workspace` exit 0. `cargo test --workspace --no-fail-fast`:
**1035 passed / 62 failed / 67 ignored** — identical to W2 close. Zero
new regressions from W3 migrations.

The 62 residual failures distribute across W4 and W5:

- **~35 W4 (value_expr / `->` map-expression / payload materialization)** —
  CSS hex/named color, JSON string decoding, sheets arithmetic / boolean
  payload, ebnf prettify round-trip.
- **~17 W5 (tape_parity golden regeneration)** — bbnf/css/ebnf/json/sheets
  golden fixture updates under the DTA tape shape.
- **~10 misc** — integration/smoke tests tied to either value_expr payload
  or fixture regen, to be resolved at W4 or W5 close.

## Close-time classification notes

The W3 migrations under the current tape shape are preventive: they
preserve correct sibling-scoped lookups when the lifter/walker emits
Seq-wrapped semantic children, a shape that HEAD's committed `generated.rs`
does not universally produce. W5's one-shot regen under the post-W4
emitter/walker/lifter pipeline will exercise these migration paths in
anger. The invariant W3 establishes — "every `find_child_by_kind` call
whose target is a nested rule uses a sibling-scoped descent primitive" —
is load-bearing for the DTA self-host round-trip even when its test-count
delta at W3 close is zero.
