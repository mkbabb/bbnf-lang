# AW-II.W3.2 — `find_child_by_kind` audit for `crates/core/src/graph/**`

## Scope

Every direct-child scan that tests `rule_kind()` in the graph layer:

- `crates/core/src/graph/deps.rs`
- `crates/core/src/graph/metadata.rs`
- `crates/core/src/graph/scc.rs`
- `crates/core/src/graph/mod.rs`

Classification conventions:

- **DESCENDANT** — target is a nested-rule output; migrate to
  `find_descendant_by_kind`.
- **LEAF-DIRECT** — target is a span-only terminal that genuinely is
  a direct child in DTA; keep direct scan.
- **SENTINEL** — intermediate wrapper; case-by-case judgement.
- **DEAD** — call site unreachable post-DTA; delete.
- **TRANSPARENT** — structural peel through fixed transparent wrappers
  (`rhs`, `grammar_item`, `directive`, `lhs`) where `peel_transparent`
  is the appropriate helper, or equivalent recursion-by-kind is
  already in place. Kept as-is; the wrapper-peel is not a rule-kind
  search.

## Table

| File:line | Function | Target rule_kind | Classification | Action |
|-----------|----------|------------------|----------------|--------|
| `deps.rs:69` | `collect_nonterminal_refs` (`term_1` arm) | `identifier` via `child(0)` | LEAF-DIRECT | Keep. `term_1 = identifier, ("(" first_arg ("," arg)* ")")?` — the walker emits the `identifier` leaf as the direct first child of the `term_1` compound; no Seq wrapper intervenes because `term_1` is itself the Seq-equivalent. Verified via mirroring the analysis-layer `collect_references` (`crates/analysis/src/state/ast_utils/references.rs:136-150`) which uses the identical `child(0)` pattern. |
| `deps.rs:75` | `collect_nonterminal_refs` (`term_1` arm) | none — iterates `children().skip(1)` | LEAF-DIRECT | Keep. Recursive descent into remaining children carries its own rule_kind dispatch in the recursive call. Shape-agnostic. |
| `deps.rs:82` | `collect_nonterminal_refs` (`alternation \| concatenation`) | none — iterates via `iter_tape_iteration_views` | TRANSPARENT | Keep. `iter_tape_iteration_views` (lines 233-268) already peels `TapeKind::Seq` wrappers (line 254-257) and anonymous `Unknown`/`int_lit` wrappers via `peel_wrapper`. DTA-aware. |
| `deps.rs:89` | `collect_nonterminal_refs` (`binary_factor`) | none — iterates via `collect_tape_binary_operand_views` | TRANSPARENT | Keep. `collect_tape_binary_operand_views` (lines 271-288) handles the single-`Repeat`-wrapper case used by `+`/`*` quantifiers. Shape-agnostic. |
| `deps.rs:100` | `collect_nonterminal_refs` (`mapped_factor \| factor`) | none — delegates to `collect_refs_from_compound` | TRANSPARENT | Keep. `collect_refs_from_compound` (lines 155-192) two-passes (Rule children + span-gap scan); inherently shape-agnostic under DTA. |
| `deps.rs:108` | `collect_nonterminal_refs` (transparent wrappers `term \| grammar_item \| directive \| lhs`) | none — `node.child(0)` then recurse | TRANSPARENT-LIKE / sentinel | Keep. These rules are grammar-level single-child transparent wrappers (matches `peel_transparent`'s whitelist). The recursive dispatch bottoms out at any rule_kind, so Seq wrappers inside the wrapper's own child are handled by the recursive descent (it will hit a Seq whose rule_kind is `Unknown`/`int_lit` and peel via the anonymous-wrapper arm at line 132). Verified: the analysis-layer mirror (`references.rs:182-190`) uses the same `child(0)` + recursive descent pattern, without `find_descendant_by_kind`. |
| `deps.rs:115` | `collect_nonterminal_refs` (grouped `term_2 \| value_atom_0`) | none — `node.child(1)` to get inner expression | SENTINEL / **DESCENDANT-RISK** | Investigate. `term_2 = "(" rhs ")" \| "[" rhs "]" \| ...`; `child(1)` assumes direct layout `[delim_open, rhs, delim_close]`. Under DTA, the walker's W4δ fix emits the `(` / `)` literal leaves as direct children. If DTA additionally wraps the inner `rhs` in a Seq compound (the AW-I.W4ζ audit noted this surfaces in `lower_grouped_term`; AW-II.W1.1 fixed it with descendant traversal in `lower/expression.rs`), then `child(1)` may pick a Seq wrapper whose rule_kind is `Unknown`, which the recursive descent's anonymous-wrapper arm (line 132) handles — but only when the Seq's direct children are all `Rule` records. Fall-back via `peel_wrapper` after the `child(1)` pick handles single-Rule-child Seqs. **Residual uncertainty:** concatenation/alternation inner expressions that land Seq-wrapped may be safer found via `find_descendant_by_kind(node, rhs)` — mirroring AW-II.W1.1 + the host.rs fix at `grammar/host.rs:310`. Action: **migrate to `find_descendant_by_kind(node, BbnfBootstrapRuleKind::rhs)` with fall-back to the existing `child(1)` for preserved-identity shapes.** This is the direct analogue of the AW-II.W1 fix for `lower_grouped_term`. |
| `deps.rs:124` | `collect_nonterminal_refs` (`closure`) | `TapeKind::Rule` children filter | LEAF-DIRECT | Keep. Scans all direct `TapeKind::Rule` children to find the body. Shape-agnostic against kind (uses `TapeKind`, not `BbnfBootstrapRuleKind`). |
| `deps.rs:134-136` | `collect_nonterminal_refs` (`int_lit \| Unknown` anonymous wrapper) | `TapeKind::Rule` children filter | LEAF-DIRECT / TRANSPARENT | Keep. This is the anonymous-wrapper peel that DTA-Seq nodes fall into; iterates direct `TapeKind::Rule` children. Shape-agnostic. |
| `deps.rs:163-167` | `collect_refs_from_compound` (Rule-child first pass) | `TapeKind::Rule` children filter | LEAF-DIRECT | Keep. Shape-agnostic. |
| `deps.rs:177-180` | `collect_refs_from_compound` (child-span collection) | none | LEAF-DIRECT | Keep. |
| `deps.rs:239-248` | `iter_tape_iteration_views` (single-top-level Repeat peel) | `TapeKind::Repeat` kind check | LEAF-DIRECT | Keep. Exact mirror of `tape_walk::iter_rep_children`. |
| `deps.rs:254-258` | `iter_tape_iteration_views` (per-pair `TapeKind::Seq` peel) | `TapeKind::Seq` kind check + `pair.child(0)` | LEAF-DIRECT | Keep. `pair.child(0)` here descends one Seq wrapper to the substantive inner content. Exact mirror of the analysis-layer `iter_iteration_views` (`ast_utils/mod.rs:152-157`). |
| `deps.rs:282-286` | `collect_tape_binary_operand_views` (single-tail-Repeat peel) | `TapeKind::Repeat` kind check | LEAF-DIRECT | Keep. Mirror of the analysis-layer helper. |
| `deps.rs:291-305` | `peel_wrapper` (anonymous-wrapper peel) | `rule_kind()` check for `Unknown`/`int_lit` + `TapeKind::Rule` filter | TRANSPARENT | Keep. Shape-agnostic peel through anonymous wrappers. |
| `metadata.rs:43-51` | `extract_alias_target` (transparent-wrapper `grammar_item \| directive \| lhs`) | none — `child(0).and_then(...)` | TRANSPARENT-LIKE | Keep. Analogous to `deps.rs:108`; the recursive re-dispatch handles DTA Seq wrappers via the `int_lit \| Unknown` arm (lines 179-198). |
| `metadata.rs:74` | `extract_alias_target` (term arm, grouped `(expr)`) | none — `find_semantic_child(node)` | TRANSPARENT / SENTINEL | Keep. `find_semantic_child` (lines 212-225) skips the `(`/`)` delimiter literals and returns the first substantive child. Under DTA this lands on the Seq wrapper; then `peel_transparent` (line 75) peels and `extract_alias_target` recurses. The `term` branch's initial span-byte dispatch (leading `(` detection) is DTA-stable because `span_text` reads the flat slice regardless of tape shape. |
| `metadata.rs:83` | `extract_alias_target` (term arm, bare term) | `identifier` | **DESCENDANT** | Migrate. This site looks for an `identifier` child of a `term` compound. Under DTA's Seq wrapping, the `identifier` may sit one compound deeper. The analysis mirror (`collect_references`) handles this in the transparent `term` arm by recursing into `child(0)` — but `extract_alias_target` needs the `identifier` specifically (not any descendant) to then assert "sole substantive child is an identifier". **Action**: change to `find_descendant_by_kind(node, BbnfBootstrapRuleKind::identifier)`. The subsequent `has_call_args` check at line 84-91 scans `node.children()` directly for non-identifier/non-comment substantive children — which under DTA now needs to descend one level. Fold into the migration: also migrate the `has_call_args` scan to iterate via the Seq wrapper. |
| `metadata.rs:84-91` | `extract_alias_target` (term arm, `has_call_args` scan) | `identifier`/`comment`/`big_comment` exclusion iteration | **DESCENDANT-RISK** | Migrate alongside line 83. The scan iterates direct children of `node` and excludes `identifier`/`comment`/`big_comment` to detect "is there a call_args compound?". Under DTA, the Seq wrapper collapses everything into one child whose rule_kind is `Unknown`; direct iteration will see one Seq child and return `has_call_args = true` incorrectly. **Action**: rewrite to iterate the children of the single Seq wrapper (descend one level when the sole direct child is a Seq/anonymous compound), or restructure to descend via `iter_rep_children`-style peel. |
| `metadata.rs:101` | `extract_alias_target` (factor arm) | `modifier` | **DESCENDANT** | Migrate. `factor = (big_comment?, term, modifier?, big_comment?)` — under DTA the `modifier` may be nested in a Seq wrapper that holds the whole factor body. **Action**: `find_descendant_by_kind(node, BbnfBootstrapRuleKind::modifier)`. |
| `metadata.rs:111` | `extract_alias_target` (factor arm) | `term` | **DESCENDANT** | Migrate. Same rationale. **Action**: `find_descendant_by_kind(node, BbnfBootstrapRuleKind::term)`. |
| `metadata.rs:126-131` | `extract_alias_target` (mapped_factor arm, mapping detection via `child(1)`) | positional child | SENTINEL | Investigate. `mapped_factor = (inner, mapping?)` — `child(1)` is the `mapping` slot. Under DTA, if the mapped_factor's direct children are `[inner_compound, mapping_compound]`, positional access still works. But the Seq wrapper may collapse both into one child. **Action**: migrate to `find_descendant_by_kind(node, BbnfBootstrapRuleKind::value_expr)` (the mapping's leading rule_kind from the grammar `mapping = "->" , value_expr` — checking for a `value_expr` descendant is the semantic equivalent of "is there a mapping?"). If that is structurally brittle, keep the positional child(1) span-zero check and additionally descend via `find_child_by_kind` for a `mapping` rule_kind if the enum lists it. **Decision: leave for W4 owner.** W4 is the dedicated `value_expr` lowering wave; touching the mapping detection here risks race-conflict with W4 scope. Classification: **SENTINEL — defer to W4 holistic review**. |
| `metadata.rs:136` | `extract_alias_target` (mapped_factor arm, inner extraction via `child(0)`) | positional child | SENTINEL | Same as 126. Defer to W4. |
| `metadata.rs:154-158` | `extract_alias_target` (alternation \| call_arg arm) | none — iterates via `super::deps::iter_tape_iteration_views` | TRANSPARENT | Keep. Shape-agnostic. |
| `metadata.rs:161-165` | `extract_alias_target` (concatenation arm) | same | TRANSPARENT | Keep. |
| `metadata.rs:168-172` | `extract_alias_target` (binary_factor arm) | none — iterates via `collect_tape_binary_operand_views` | TRANSPARENT | Keep. |
| `metadata.rs:179-198` | `extract_alias_target` (`int_lit \| Unknown` anonymous wrapper) | `TapeKind::Rule` children filter | TRANSPARENT | Keep. Anonymous-wrapper peel. |
| `metadata.rs:215-224` | `find_semantic_child` (delimiter-skip child scan) | none — span-text membership exclusion | LEAF-DIRECT | Keep. Iterates direct children, filters by span text (not rule_kind). Shape-agnostic. |
| `scc.rs:*` | (entire file) | none | DEAD for this audit | No rule_kind / AST traversal. Scc.rs operates on the `Dependencies` string-keyed graph produced by `calculate_ast_deps`. Zero migration sites. |
| `mod.rs:*` | (entire file) | none | DEAD for this audit | Re-exports only. Zero migration sites. |

## Row counts

| Classification | Count |
|----------------|-------|
| **DESCENDANT** (migrate) | 3 (metadata.rs:83, 84-91 coupled, 101, 111 — 4 sites but 84-91 is conceptually one action alongside 83) |
| **SENTINEL** (defer to W4) | 2 (metadata.rs:126-131, metadata.rs:136) |
| **SENTINEL/DESCENDANT-RISK** (migrate conservatively) | 1 (deps.rs:115) |
| **LEAF-DIRECT** / **TRANSPARENT** / **DEAD** (no action) | remainder |
| **Total rule_kind call sites** | 24 |

## Migration plan

### Step 2.A — `deps.rs:115` grouped-term inner extraction

Current:

```rust
BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
    if let Some(inner) = node.child(1) {
        collect_nonterminal_refs(inner, refs);
    }
}
```

Migrated: use `find_descendant_by_kind` against `rhs` (the inner
expression's canonical rule_kind). Under DTA's Seq wrapping, the
inner `rhs` rule is typically one compound deeper than a direct
`child(1)` read. The existing recursive descent via
`anonymous-wrapper peel` (line 132) does handle it when the Seq
wrapper's only child is a Rule, but the DESCENDANT form is more
robust against Seq wrappers that contain delimiters alongside the
content.

Mirrors the AW-II.W1.1 fix pattern in `lower/expression.rs` and the
`grammar/host.rs::absorb_item` migration (line 310).

### Step 2.B — `metadata.rs:83` + `84-91` term-alias identifier lookup

Current:

```rust
let ident = find_child_by_kind(node, BbnfBootstrapRuleKind::identifier)?;
let has_call_args = node.children().any(|c| {
    let k = c.rule_kind();
    k != BbnfBootstrapRuleKind::identifier
        && k != BbnfBootstrapRuleKind::comment
        && k != BbnfBootstrapRuleKind::big_comment
        && c.span().1 > c.span().0
});
```

The `term` compound under DTA may contain a Seq wrapper around
`[identifier, call_args?]`. The direct `find_child_by_kind` misses
the identifier when it's inside the Seq; subsequently `has_call_args`
sees the Seq itself as a "non-identifier substantive child" and
returns `true` (falsely suppressing alias detection).

Migrated:

```rust
let ident = find_descendant_by_kind(node, BbnfBootstrapRuleKind::identifier)?;
// Call-args detection: scan descendants for any non-identifier
// non-comment substantive child of the top-level term compound.
// Under DTA, if the term is `identifier(...)`, a `call_arg`
// descendant will exist; otherwise not.
let has_call_args = find_descendant_by_kind(node, BbnfBootstrapRuleKind::call_arg).is_some();
```

The migrated form is semantically tighter: "is there any `call_arg`
descendant" exactly encodes "does this term have a call". Avoids
the Seq-wrapper false positive.

### Step 2.C — `metadata.rs:101` factor-modifier lookup

Current:

```rust
let modifier = find_child_by_kind(node, BbnfBootstrapRuleKind::modifier);
```

Migrated:

```rust
let modifier = find_descendant_by_kind(node, BbnfBootstrapRuleKind::modifier);
```

Under DTA the `modifier` is wrapped alongside `term` inside a Seq;
descendant traversal sees it through the wrapper.

### Step 2.D — `metadata.rs:111` factor-term lookup

Current:

```rust
if let Some(term) = find_child_by_kind(node, BbnfBootstrapRuleKind::term) {
```

Migrated:

```rust
if let Some(term) = find_descendant_by_kind(node, BbnfBootstrapRuleKind::term) {
```

Same rationale.

## Non-migration rationale summary

- **scc.rs / mod.rs**: No AST traversal. Scc operates on the
  string-keyed `Dependencies` IndexMap, which is a post-traversal
  output. No DTA shape exposure.
- **deps.rs shape-agnostic arms**: `alternation \| concatenation`,
  `binary_factor`, `mapped_factor \| factor`, `closure`,
  anonymous-wrapper peel, and the `collect_refs_from_compound`
  span-gap scanner all walk either via dedicated helpers
  (`iter_tape_iteration_views`, `collect_tape_binary_operand_views`,
  `peel_wrapper`) or via `TapeKind::Rule` kind filters that are DTA-
  shape-transparent by construction.
- **metadata.rs transparent-wrapper arms**: The `grammar_item \|
  directive \| lhs` arm's `child(0).and_then(extract_alias_target)`
  pattern bottoms out in the recursive `int_lit \| Unknown` anonymous-
  wrapper arm, which peels Seq wrappers transparently.
- **metadata.rs iteration arms**: The `alternation \| call_arg`,
  `concatenation`, and `binary_factor` arms all delegate to the
  `super::deps::iter_*` helpers that are already DTA-aware.
- **metadata.rs mapped_factor arm (`child(0)`/`child(1)` positional
  accesses)**: Deferred to W4 (`crates/core/src/lower/value_expr.rs`
  owner). The `->` map-expression semantics need a holistic review;
  an isolated W3.2 migration here risks wave-boundary conflict.

## Producer-side concerns

None identified. Every DESCENDANT migration is a consumer-side
pattern shift that uses the existing `find_descendant_by_kind`
substrate (promoted to `lower/tape_walk.rs` as `pub(crate)` by
AW-II.W1.0 commit `9e4d610e`). No changes to the walker / lifter /
emitter required.

## Post-migration verification target

`test_large_grammar` (LSP integration) — inlay hints require
`rule.references.len() >= 1` + `first_count >= 2`. Currently fires
for only `pair` and `value` (expected: ≥ 4 of 8 non-trivial rules).

Hypothesis: `array = "[" , [ value , { "," , value } ] , "]"` and
`object = "{" , [ pair , { "," , pair } ] , "}"` fail to collect
their `value` / `pair` references because the grouped `[...]` inner
expression is behind a Seq wrapper that `deps.rs:115`'s `child(1)`
read misses. The migration at Step 2.A should restore references
for these rules. Note: this test is driven by `analysis/` crate
code that mirrors this pattern; the upstream fix in `deps.rs`
improves the pipeline-wide `calculate_ast_deps` path used by
`compile.rs` and does not directly fix the LSP path (that uses
`analysis/state/ast_utils/references.rs`, which is out of W3.2's
scope). But `calculate_ast_deps` is invoked from both paths and the
alias detection chain here could cascade into topological ordering
— test impact is plausible but not guaranteed.

## Summary

Four migrations ship this wave. Two mapped_factor sites deferred
to W4. Twenty remaining sites are shape-agnostic / transparent-
wrapper patterns that the current DTA-aware substrate handles
correctly.
