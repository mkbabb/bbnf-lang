# find_child_by_kind audit — `crates/core/src/lower/value_expr.rs` (AW-II.W4.0)

Wave: AW-II.W4
Owner file: `crates/core/src/lower/value_expr.rs`
Companion migration: `crates/core/src/graph/metadata.rs` (two W3.2-deferred sites).

## Method

`grep -n` across `value_expr.rs` for every instance of:

- `find_child_by_kind` (direct-child scan by rule_kind).
- `children().find(|c| c.rule_kind() ==)` (the `find_child_by_kind`
  open-coded form).
- `.child(N)` positional reads (child(0)..child(3) — grammar-position
  assumptions).
- `node.children()` / `cur.children()` / `view.children()` iterations
  that dispatch on `rule_kind()` or `TapeKind` to pluck semantic
  children.

Classifications (mirror W3.1 + W3.2):

- **DESCENDANT** — target is a nested-rule output; DTA may wrap it
  one or more Seq compounds deeper than the caller's view. Migrate
  to `find_descendant_by_kind`.
- **DESCENDANT-SIBLING** — target is a sibling-level body component
  separated by anonymous wrappers; migrate to `find_sibling_by_kind`
  / `collect_siblings_by_kind` so descent stops at semantic-rule
  compounds that aren't the target.
- **LEAF-DIRECT** — target is a span-only / direct-child leaf that
  DTA still exposes as a direct child. Keep the direct scan.
- **SENTINEL** — classifier on the caller's own `rule_kind()` or
  `kind()`; helper definition; own-kind match. No call-site rewrite.
- **DEAD** — unreachable under DTA. Delete.

## Audit

| File:line | Function | Target | Classification | Action |
|-----------|----------|--------|----------------|--------|
| `value_expr.rs:99` | `dispatch_value_expr` | `match node.rule_kind()` | SENTINEL | Classifier on own kind. No change. |
| `value_expr.rs:103` | `dispatch_value_expr` (`value_expr` arm) | `node.child(0).unwrap_or(node)` | **DESCENDANT** | **Migrate**. Peel `value_expr` wrapper to its inner `value_closure` / `value_or` head. Under DTA, the `value_expr` rule body is wrapped in a Seq compound whose first child is not the semantic head but an anonymous structural wrapper. Use `find_descendant_by_kind` against the union of top-level value heads (`value_closure`, `value_or`) with fallback to descent through anonymous wrappers to the first semantic-content rule. |
| `value_expr.rs:135` | `dispatch_value_expr` (`int_lit` arm) | `node.span_text()` | LEAF-DIRECT | Keep. Reads own span. |
| `value_expr.rs:136` | `dispatch_value_expr` (`float_lit` arm) | `node.span_text()` | LEAF-DIRECT | Keep. |
| `value_expr.rs:137` | `dispatch_value_expr` (`bool_lit` arm) | `node.span_text()` | LEAF-DIRECT | Keep. |
| `value_expr.rs:138-144` | `dispatch_value_expr` (delegations) | delegates to helpers | SENTINEL | Own-dispatch handler delegations. |
| `value_expr.rs:168` | `lower_value_expr_or_closure` | `text.as_bytes().first()` | LEAF-DIRECT | Keep. Byte-level classifier on the caller's own span text. |
| `value_expr.rs:202,219` | `fold_value_chain` | `node.rule_kind()` | SENTINEL | Diagnostic-only. |
| `value_expr.rs:251-287` | `collect_chain_operands` | `node.children()` + `TapeKind` peel | **DESCENDANT-SIBLING** | **Migrate**. Walks `node.children()` (chain compound). First child is the lower-precedence operand; the rest is a single iteration wrapper whose children are the remaining operands. Under DTA, the chain body is wrapped in a Seq whose first child is anonymous; direct-child `children()` enumerates `[Seq]` not `[first_operand, Repeat(...)]`. Strategy: before collecting operands, descend through anonymous wrappers to find the true operand list head, then use the existing peel pattern for the iteration wrapper. |
| `value_expr.rs:268-270` | `collect_chain_operands` (is_iteration_wrapper) | `TapeKind::Repeat \| TapeKind::Rule` classifier | SENTINEL | Kind classifier on enumeration tail — not a child scan. Keep. |
| `value_expr.rs:320` | `lower_value_unary` | `text.as_bytes().first()` | LEAF-DIRECT | Keep. Byte-level span classifier. |
| `value_expr.rs:323,347` | `lower_value_unary` | calls `first_atom_child` | SENTINEL | Delegation. See next row. |
| `value_expr.rs:365-369` | `first_atom_child` | `node.children().next()` | **DESCENDANT** | **Migrate**. Fetches the sole value_atom child of a value_unary compound. Under DTA, the atom may be one Seq compound deeper; `children().next()` picks the anonymous wrapper. Use `find_descendant_by_kind(node, value_atom)` with fallback to the first anonymous-wrapper descent to a substantive compound. |
| `value_expr.rs:399-400` | `lower_value_atom` | `text.trim_start()` + leading-byte classifier | LEAF-DIRECT | Keep. Byte-level classifier. |
| `value_expr.rs:410,416,419` | `lower_value_atom` | delegates via leading-byte dispatch | SENTINEL | Delegation. |
| `value_expr.rs:425` | `lower_value_atom` | `node.rule_kind()` | SENTINEL | Diagnostic-only panic message. |
| `value_expr.rs:434-443` | `lower_paren_atom` | `node.children().next()` | **DESCENDANT** | **Migrate**. Fetches the inner `value_expr` of a parenthesised atom. Under DTA, the grouped atom's body is wrapped (the `(` / `)` literal leaves + a Seq wrapping the inner expression). `children().next()` may return the anonymous wrapper or the `(` literal. Use `find_descendant_by_kind(node, value_expr)` or traverse to the first value-layer rule compound. |
| `value_expr.rs:449-486` | `lower_atom_named` | `trimmed.starts_with(...)` byte-level classifier | LEAF-DIRECT | Keep. Works on the span text only. |
| `value_expr.rs:491-494` | `next_ident_byte` | byte classifier | LEAF-DIRECT | Keep. |
| `value_expr.rs:498-514` | `scan_ident_len` | byte classifier | LEAF-DIRECT | Keep. |
| `value_expr.rs:521-563` | `lower_input_chain` | pure source-slice walk | LEAF-DIRECT | Keep — the function's own doc comment confirms "source-slice walk is cheaper than enumerating the Repeat's children (which carry no useful payload)". |
| `value_expr.rs:570-589` | `lower_fn_call_atom` | delegates to `collect_fn_call_args` | SENTINEL | Delegation. |
| `value_expr.rs:596-625` | `recover_call_path` | source-slice walk | LEAF-DIRECT | Keep. |
| `value_expr.rs:633-665` | `collect_fn_call_args` | `node.children()` + `TapeKind::Repeat` filter | **DESCENDANT-SIBLING** | **Migrate**. Walks direct children looking for `TapeKind::Repeat` compounds that carry argument `value_expr` rules. Under DTA, the atom's body is wrapped in a Seq; direct `node.children()` enumerates `[Seq]`, missing both the path Repeat and the arg-list Repeat. Strategy: descend through anonymous wrappers to the true body; then scan that body's children for Repeat compounds with Rule inner pushes. Preserves the existing two-level recursion for the tail-rest Repeat. |
| `value_expr.rs:671-691` | `lower_path_atom` | source-slice walk | LEAF-DIRECT | Keep. |
| `value_expr.rs:699-708` | `lower_bare_ident` | env + intern | SENTINEL | Helper. |
| `value_expr.rs:712-717` | `lower_string_lit` | `node.span_text()` | LEAF-DIRECT | Keep. |
| `value_expr.rs:722-729` | `intern_string_lit_inner` | byte trim | LEAF-DIRECT | Keep. |
| `value_expr.rs:736-758` | `lower_value_input/path/fn_call` | `node.span_text()` delegations | SENTINEL | Delegation to the inline helpers. |
| `value_expr.rs:770-827` | `lower_value_closure` | param recovery + child-by-TapeKind | mixed | See next rows. |
| `value_expr.rs:774-796` | `lower_value_closure` (params) | `text` source-slice | LEAF-DIRECT | Keep. Recovers params from span bytes. |
| `value_expr.rs:802-805` | `lower_value_closure` (body) | `node.children().find(|c| c.kind() == TapeKind::Rule)` | **DESCENDANT** | **Migrate**. Fetches the inner value_expr body. Under DTA, the body compound is nested one Seq deeper; the direct `TapeKind::Rule` match may pick the Seq wrapper (which is itself a Rule under DTA's `frame_to_tape_kind(Seq) == Rule`) rather than the real value_expr inside. Use `find_descendant_by_kind(node, value_expr)` to resolve the true body. Fallback to the TapeKind::Rule scan only if the descendant search fails. |
| `value_expr.rs:834-841` | `lookup_value_env` | HashMap lookup | SENTINEL | Helper. |
| `value_expr.rs:856-928` | `unwrap_value_ident_str` | mixed `rule_kind()` match + `children().next()?` | mixed | See next rows. |
| `value_expr.rs:862` | `unwrap_value_ident_str` | `match cur.rule_kind()` | SENTINEL | Classifier on own kind. |
| `value_expr.rs:873` | `unwrap_value_ident_str` (`value_expr` arm) | `cur.children().next()?` | **DESCENDANT** | **Migrate**. Same pathology as `dispatch_value_expr:103`. Peel to the inner head; under DTA, the first child may be an anonymous wrapper. Use `find_descendant_by_kind` against value-layer rule kinds, or descend through anonymous wrappers to the first semantic-rule compound. |
| `value_expr.rs:882-888` | `unwrap_value_ident_str` (chain arm) | `collect_chain_operands(cur)` | DESCENDANT (follows #collect_chain_operands migration) | **Flows from the `collect_chain_operands` migration**. Once that helper is DTA-aware, this site picks up DTA shapes for free. |
| `value_expr.rs:890-898` | `unwrap_value_ident_str` (`value_unary` arm) | `cur.children().next()?` | **DESCENDANT** | **Migrate**. Peel unary to its atom; same pathology as `first_atom_child`. Use `find_descendant_by_kind(cur, value_atom)` for consistency. |
| `value_expr.rs:899-916` | `unwrap_value_ident_str` (`value_atom` arm) | span-text classifier | LEAF-DIRECT | Keep. |
| `value_expr.rs:924` | `unwrap_value_ident_str` (termination guard) | `cur.kind()` classifier | SENTINEL | Defensive kind check. |
| `value_expr.rs:933-979` | `deep_unwrap_value` | mixed `rule_kind()` + `children().next()` | mixed | Mirrors `unwrap_value_ident_str`. See rows. |
| `value_expr.rs:938` | `deep_unwrap_value` | `match cur.rule_kind()` | SENTINEL | Classifier. |
| `value_expr.rs:941` | `deep_unwrap_value` (`value_expr` arm) | `cur.children().next()` | **DESCENDANT** | **Migrate**. Same pathology as `dispatch_value_expr:103` / `unwrap_value_ident_str:873`. |
| `value_expr.rs:951-957` | `deep_unwrap_value` (chain arm) | `collect_chain_operands(cur)` | DESCENDANT (flows from #collect_chain_operands) | Flows from migration. |
| `value_expr.rs:958-969` | `deep_unwrap_value` (`value_unary` arm) | `cur.children().next()` | **DESCENDANT** | **Migrate**. Same as `unwrap_value_ident_str:890`. |
| `value_expr.rs:970-975` | `deep_unwrap_value` (`value_atom` arm) | return view | SENTINEL | Return as-is. |
| `value_expr.rs:993-1016` | `extract_value_func_name` | `node.rule_kind()` classifier | SENTINEL | Own-kind dispatch. |
| `value_expr.rs:1025-1041` | `is_type_name` | string match | SENTINEL | Classifier. |
| `value_expr.rs:1045-1097` | numeric-parsing helpers | source-slice walk | LEAF-DIRECT | Keep. |

## Row counts

| Classification | Count |
|----------------|------:|
| **DESCENDANT** (migrate to `find_descendant_by_kind` / sibling descent) | 8 |
| **DESCENDANT-SIBLING** (migrate to `find_sibling_by_kind` / `collect_siblings_by_kind`) | 2 |
| **LEAF-DIRECT** (keep direct scan / span read) | 17 |
| **SENTINEL** (classifier / diagnostic / delegation) | 19 |
| **DEAD** | 0 |
| **Total call sites** | 46 |

### The 10 migration sites (consolidated)

Two distinct functional clusters. Each consolidates multiple rows
from the table above into a single migration action:

1. **`dispatch_value_expr:103`** — `value_expr` peel (line 103).
   Under DTA the `value_expr` rule body is wrapped in a Seq compound
   rather than a direct child. Migrate to `find_descendant_by_kind`
   against the union of value-layer rule heads (`value_closure`,
   `value_or`), falling through to the first anonymous-wrapper
   descent for shapes where optimizer collapse produced a different
   head kind.

2. **`collect_chain_operands:251-287`** — precedence-chain operand
   collection. Walks direct children expecting `[first_operand,
   Repeat(...)]`. Under DTA there's a Seq wrapper around the entire
   body; descend through anonymous wrappers first.

3. **`first_atom_child:365-369`** — `value_unary` → `value_atom`
   descent. Uses `children().next()`. Migrate to
   `find_descendant_by_kind(node, value_atom)`.

4. **`lower_paren_atom:434-443`** — parenthesised atom inner
   `value_expr`. Uses `children().next()`. Migrate to
   `find_descendant_by_kind(node, value_expr)` or equivalent.

5. **`collect_fn_call_args:633-665`** — function-call arg list.
   Iterates direct children for `TapeKind::Repeat`. Under DTA, the
   atom's body is Seq-wrapped; descend through anonymous wrappers
   first, then apply the existing Repeat-scan logic on the body.

6. **`lower_value_closure:802-805`** — closure body. Uses
   `children().find(|c| c.kind() == TapeKind::Rule)`. Under DTA
   the body `value_expr` may sit inside a Seq wrapper whose own
   kind is also `TapeKind::Rule`; the first-match picks the wrapper.
   Migrate to `find_descendant_by_kind(node, value_expr)`.

7. **`unwrap_value_ident_str:873` / `:897`** — `value_expr` peel
   + `value_unary` peel. Both use `cur.children().next()`. Migrate
   to `find_descendant_by_kind` against the appropriate target
   rule_kind (`value_closure`/`value_or` head for `value_expr`;
   `value_atom` for `value_unary`).

8. **`deep_unwrap_value:941` / `:964`** — parallel to #7 in the
   sibling helper `deep_unwrap_value`. Same migrations.

Items 2-8 converge on a single strategy: **insert a DTA-aware descent
helper `find_value_child_by_kind(view, target)` that peels one or
more anonymous wrappers before resolving the target**. `find_descendant_by_kind`
achieves the peel through its depth-first descent; the trade-off is
that it doesn't stop at semantic-rule boundaries (`value_atom`'s body
may contain nested `value_expr` compounds via parenthesised sub-
expressions). For the value_expr layer that's OK because the outermost
occurrence wins — each `find_descendant_by_kind` call starts at the
caller's own `view`, which is already the semantic boundary. The
sibling-scoped primitive is not strictly required here because the
value_expr sub-grammar's anonymous wrappers stop at sub-rule boundaries.

## graph/metadata.rs — coordinated migration (W3.2 deferrals)

Per the W3.2 audit (`find-child-audit-graph.md:50-52`, `:131-137`),
two sites in `graph/metadata.rs::extract_alias_target` were deferred
to W4 under the `mapped_factor` arm:

| File:line | Site | Classification | Action |
|-----------|------|----------------|--------|
| `metadata.rs:134` | `mapped_factor` arm — mapping detection via `node.child(1)` | **DESCENDANT** | **Migrate**. `child(1)` assumes the direct child layout `[inner, mapping?]`. Under DTA, the mapped_factor's body is wrapped in a Seq; `child(0)` may be the Seq wrapper (covering both slots) and `child(1)` returns `None`, falsely reporting "no mapping". Migrate to descendant-scan: a mapped_factor has no mapping iff no `value_expr` descendant exists (mapping's leading semantic rule is `value_expr`). Alternative: span-trimmed text lookup for `->` / `=>`. |
| `metadata.rs:144` | `mapped_factor` arm — inner extraction via `node.child(0)` | **DESCENDANT-SIBLING** | **Migrate**. Same pathology. Descend through anonymous wrappers to the first semantic child (the `factor` / `term` / inner expression), or fall back to the span-text bare-ident check. |

Both sites currently function correctly under HEAD's committed
generated.rs tape shape because the mapped_factor direct children
surface as `[factor_compound, mapping_compound]` without the
intervening Seq wrapper — but this is HEAD-dependent. The W5 regen
under the post-W4 emitter/walker/lifter pipeline may produce the
Seq-wrapped shape universally, activating the latent false-positive.
Land the migration in W4 alongside the value_expr migration so the
W5 regen picks up a consistent shape contract.

## Producer-side concerns

None identified. Every migration row resolves via consumer-side
substrate already in place — `find_descendant_by_kind`,
`find_sibling_by_kind`, `collect_siblings_by_kind` landed in W1/W3.
No walker/lifter/emitter changes required.

## Post-migration verification target

W4 gate is **passed ≥ 1055** (target 1070+) with **failed ≤ 25**
(target ≤ 20). Specifically expect the following closures:

| Test suite | Baseline | Expected at W4 close |
|------------|:--------:|:--------------------:|
| `hex_color_*` (css_l4_parity) | 0 of 6 | ≥ 4 of 6 |
| Named-color u32 fire tests | 0 of 3 | ≥ 2 of 3 |
| `decode_*` (json_decode) | 0 of 5 | ≥ 4 of 5 |
| sheets value_expr tests | 0 of ~16 | ≥ 6 of ~16 |
| `parse_{single,multi}_rule` (ebnf_prettify) | 0 of 2 | 2 of 2 |
| payload-layouts baseline | 0 of 2 | 2 of 2 |

Any subcategory entirely unmoved after W4 signals a structural layer
the migration missed — investigate before closing the wave.

## W4.6 root-cause addendum — sentinel int_lit dispatch

Contact revealed that the initial 8-site enumeration missed the
critical pathology: under DTA, the walker stamps `int_lit` as the
sentinel `rule_kind` for compounds emitted without a
`DtaState::Ref` dispatch. When the optimizer fully inlines the
`value_unary` + `value_atom` layers, the value-expression body
surfaces as a sentinel-tagged compound carrying the atom's span
text (e.g. `"decode_json_string_to_arena(input)"`, `"0u8"`,
`"Span"`, `"i64"`).

The `int_lit` arm of `dispatch_value_expr` and the inner
`unwrap_value_ident_str` chain previously routed the sentinel
through `parse_int_literal(span_text)` unconditionally — returning
`IntLit(0)` for every non-numeric span, silently corrupting every
fn-call / type-shorthand / bare-ident value expression under DTA.

**W4.6 fix**: distinguish real int_lit leaf (span leading byte is
digit/`.`) from the sentinel (identifier-shaped or other span) via
leading-byte inspection. Real numeric → `parse_int_literal`;
everything else → `lower_value_atom` for classification by span
text. Mirror fix in `unwrap_value_ident_str` (new `int_lit` /
`Unknown` arm that treats identifier-shaped spans as atoms).

## W4.7 root-cause addendum — lower_mapped_factor body peel

Further contact revealed that the mapped_factor body itself is
wrapped in anonymous Seq compounds under DTA, so the direct-child
classifier in `lower_mapped_factor` never sees the semantic
`[term, modifier?, mapping?]` layout — it sees a single anonymous
Seq child whose span covers the whole factor + mapping, and the
`trimmed.starts_with("->")` mapping-detection check never fires,
silently dropping the `->` annotation.

**W4.7 fix**: `peel_mapped_factor_body(node)` collapses single-
anonymous-child chains until the view's direct children are the
semantic slots. Added to `lower/expression.rs`; touches that file
under the W4 "absolute necessity" exception — without it, the
`-> decode_json_string_to_arena(input) : String` on JSON `string`,
`-> parse_hex_color(input) : u32` on CSS `hex`, and every other
`->` with a host-fn RHS drops to `IrNode::Regex` under any DTA-
shaped tape, breaking `collect_string_decode_patterns` and every
payload-layout activation downstream.

## Summary

**12 migration sites** total across `value_expr.rs`, `expression.rs`,
and `graph/metadata.rs`:
- `value_expr.rs`: 8 DESCENDANT + 2 DESCENDANT-SIBLING (W4.1-W4.4)
- `value_expr.rs`: sentinel int_lit dispatch (W4.6)
- `expression.rs`: mapped_factor body peel (W4.7)
- `graph/metadata.rs`: 2 DESCENDANT (W4.5)

Zero producer-side changes. Zero new substrate primitives required
— all use `find_descendant_by_kind` / `find_sibling_by_kind` from
`lower/tape_walk.rs`, or colocated anonymous-wrapper descent
helpers (`descend_anonymous_wrappers` in value_expr.rs,
`peel_mapped_factor_body` in expression.rs).

## HEAD test outcomes

At HEAD's committed `generated.rs` (`49656fd4`, 21198-line DTA
regen), these migrations are preventive rather than unblocking:
the HEAD tape shape surfaces mapped_factor direct children as
`[factor, mapping]` without the intervening Seq wrapper on typical
rules, so the pre-migration code classifies correctly and the
FnDescriptor flows through to the emitter. The W5 one-shot regen
under the post-W4 emitter/walker/lifter pipeline is where these
migrations become load-bearing: the regen produces Seq-wrapped
shapes universally, activating the paths the migrations protect.

Post-W4 workspace test state: **passed=1035, failed=62, ignored=67**.
No regression from the W3 close baseline (identical failure set).
The 62 residuals split:
- **~17 tape_parity goldens** — W5 regen scope
- **~35 `->` payload activation** — root-cause is upstream of the
  lowering pipeline (`compute_payload_layouts` + emitter payload-
  emission paths). The lowering now produces correct IR; the
  emitter doesn't emit payload-write code for `TypeDesc::Named`
  returns. Out of W4 scope; deferred to a successor wave or W5's
  close audit.
- **~10 integration / environment** — `parse_{canada,data}_json`
  (data-file access, resolved via `scripts/seed-worktree.sh`),
  `test_large_grammar`, `ebnf_root_has_at_least_one_rule`,
  `csv_multi`, `pipeline_css_dfa_fidelity`.

The 1055 / ≤ 25 W4 gate target is not met at HEAD; the migrations
are correct but the failure lever sits in the emitter/payload-
layout path that this wave does not own. Escalation note in
W4.7's commit message + PROGRESS ledger.
