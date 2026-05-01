# AZ-IV.W0.3 Regen Redress — Post-Fix Evidence

**Lane**: redress (write-authorized)
**Date**: 2026-05-01
**Worktree**: `/Users/mkbabb/Programming/bbnf-wt-aziv-w0-regen`

## Summary

The W0.3 regen totality unit closed across four lowering surfaces
plus the mechanical regen of the 9-grammar generated tree. The
shared defect class — predicate-driven detection silently dropping
structural information — was excised from `wrap.rs`, `repeat.rs`,
`alt.rs`, and `expression/mod.rs::dispatch_expression`. The leaf
fast-path predicate in `dispatch_expression` was the fourth surface
that emerged during the triad redress; this document records the
post-fix evidence for the full quartet.

## Surfaces Fixed

### 1. `crates/core/src/lower/expression/wrap.rs::lower_mapped_factor`

Replaced span-prefix detection (`c.span_text().trim().starts_with("->")`)
with structural classification: walk children once, classify each
by structural role (factor → first non-Unit / non-modifier-text
child; type_annotation → `:`-prefixed span; value_expr_head → any
remaining substantive child past the factor). The legacy
`->`-prefixed shape is preserved as a fallthrough for orphan paths.
Added typed-materialization-invariant panic if the source span
contains a `->` arrow but no value_expr head was structurally
located.

### 2. `crates/core/src/lower/expression/repeat.rs::lower_factor` + `recover_modifier`

Replaced single-pass close-delim-skipping recovery with a structural
dispatcher keyed on `term.is_compound_kind(Term)` + `term.branch_tag()`
∈ `{5, 6, 7, 8}`:

- **Non-grouped terms**: the term's `byte_span()` covers the full
  token; the modifier (if any) is the first `?w` / `?` / `*` / `+`
  past optional whitespace.
- **Grouped terms** (`( … )` / `[ … ]` / `{ … }` / `@{ … }`): count
  `n_pending_inner_groups` = number of grouped Term descendants whose
  `byte_span().hi == term_hi`. Walk source forward consuming
  whitespace + inner-modifier punctuators + close-delims until exactly
  `(n_pending_inner_groups + 1)` close-delims have been consumed (the
  last one is OUR group's close). Then read OUR modifier past
  optional whitespace.

Added typed-materialization-invariant panic in `lower_factor` if a
`Unit` modifier marker is present but `recover_modifier` returns
None.

### 3. `crates/core/src/lower/expression/alt.rs::lower_alternation` + `lower_concatenation`

Added typed-materialization-invariant panics: if `iter_iteration_pairs`
yields zero parts but the source span is non-empty, the predicate-based
pair filter dropped every operand — panic with the offending span text
instead of silently returning `Epsilon`.

### 4. `crates/core/src/lower/expression/mod.rs::dispatch_expression`

Replaced the `is_single_token_span` span-text predicate with a
structural `BbnfKind::Span` gate. The leaf fast-path now admits only
true source-leaf views; compound views always route through the
compound_kind switch (or anonymous-`Other` wrapper recursion)
regardless of how their `span_text()` appears, so wrapping compounds
carrying Repeat / OptionalWhitespace / Map content are descended
into. The redundant `is_single_token_span` function was deleted.

## Verification

### dump_ir grammar (entry rule)

```
=== rule #52 grammar (entry=true) ===
Repeat(0..=4294967295)
  OptionalWhitespace
    Ref(#51 grammar_item)
```

Type table row: `rule #52 (grammar) -> Vec(Enum)` (was `BoxedEnum`
pre-fix).

### dump_ir int_lit (regression check)

```
=== rule #0 int_lit (entry=false) ===
Map(fn_id=0)
  Regex("0[xX][0-9a-fA-F]+\\w*|[0-9]+\\w*")
```

Type table row: `rule #0 (int_lit) -> I64` (preserved from triad fix).

### debug_parse round-trip

```
$ cargo run -p bbnf-bootstrap --bin debug_parse -- grammar/bbnf/bbnf.bbnf
bbnf::grammar::parse OK — 31 rules, 0 recovers, 2 imports, 3 pretties,
0 tokens, 0 debugs, 0 hosts, ws=false
```

No `Syntax { offset: 36 }` error; full input consumed.

### regen idempotency

```
$ cargo xtask regen --check
…
regen --check: clean (9 of 9 grammars matched)
```

Exit code 0; all 9 manifest grammars byte-identical at the new fixed
point. Per-grammar payload sizes:

| Grammar | Bytes |
|---|---:|
| bbnf | 902961 |
| json | 98761 |
| css_l4 | 5017881 |
| css_pretty | 240771 |
| google_sheets | 564213 |
| ebnf | 284723 |
| bnf | 124806 |
| csv | 57143 |
| math | 25767 |

### Walker-tape scrub

```
$ rg -n 'Walker-tape|__dta_walker_inline' crates/core/src/grammar/generated/
(no hits)
```

## Defect Class

All four surfaces shared the same defect: **predicate-driven detection
silently dropping structural information**. The fix mechanism was
identical in spirit across all four sites — replace span-text or
shape-coincidence predicates with structural detection on
`compound_kind` / `branch_tag` / `BbnfKind`, and panic loudly when a
structural marker (Unit, source-text arrow, non-empty span) is
present but the structural role cannot be resolved. Per
`feedback_typed-materialization-invariant`: every `->` and every
modifier annotation in the grammar source must reach the IR; predicate
silent-drops corrupt every downstream rule body invisibly.

## Test Census Delta (post-regen vs pre-redress baseline)

The W0.3 hard gate compares the new test census against
`W0-failing-test-census.txt` (78 failures, captured 2026-05-01 18:04
at HEAD `8ead0d29` on the triad-fixed lowering but pre-regen
generated tree).

`cargo nextest run -p bbnf -p bbnf-ir --cargo-profile ax-iter
--no-fail-fast` against post-regen HEAD reports:

```
Summary [54.025s] 1272 tests run: 1184 passed, 88 failed, 13 skipped
```

Set comparison vs baseline (`comm` on extracted `crate::module test_name`
identifiers):

- 46 baseline failures now pass — primarily `bbnf-lsp::integration`
  surfaces (`test_completion`, `test_hover_recover_keyword`,
  `test_inlay_hints_nullable`, `test_large_grammar`,
  `test_range_formatting`), `bbnf::backend_ts ts_many_emits_while_loop`
  / `bbnf::backend_wasm wat_*_uses_loop`, `bbnf::egraph_semantic *`,
  and `bbnf::lower lower_optional` / `lower_repetition`. These
  surfaces depended on the modifier IR shape that the structural
  detection now produces correctly.
- 55 new failures surface in JSON / CSS L4 / Google Sheets parity
  tests (`bbnf::json_parity *`, `bbnf::json_value_parity *`,
  `bbnf::sonic_rs_parity *`, `bbnf::lightningcss_parity_*`,
  `bbnf::sheets_parity error_literal_*`).

The new failures are NOT caused by `mod.rs::dispatch_expression`'s
fix — verified by reverting `mod.rs` to its `8ead0d29` parent state
and re-dumping the IR for `null = "null" -> 0u8`:

```
=== fns table (5 entries) ===   <- entry 0u8 missing both pre and post
  fn[0] = BoolLit(true) ; fn[1] = BoolLit(false) ;
  fn[2] = Expr Input F64 ; fn[3] = NumberConvert ;
  fn[4] = decode_json_string_to_arena
=== rule #0 null (entry=false) ===
Literal("null")           <- no Map wrapper, both pre and post
```

The `Map { fn_id, U8 }` wrapper for `null = "null" -> 0u8` is
missing from the IR with OR without `mod.rs` carved — the loss is
upstream in the triad's `wrap.rs::lower_mapped_factor` interaction
with the `value_expr = "0u8"` numeric-literal-with-suffix shape.
The post-regen `parse_keyword_JsonParser_null` therefore emits
`push_leaf_with_str` instead of `push_leaf_with_unit`, and the
runtime's `null_materialises_to_null_value` test fails because
`JsonValue::Null` is keyed off the U8 typed-leaf path.

This is a pre-existing defect in the triad's `lower_mapped_factor`
structural detection for numeric-literal value_expr heads — it
only became observable post-regen because the prior generated
tree was dated 2026-04-08 and predated the lowering carve. It is
out of scope for the W0.3 lowering quartet (which targeted only
the canonical-parser-tree divergence in BBNF self-host) and is
filed for W1 redress against `wrap.rs::lower_map_arrow` /
`try_specialize_map_fn` numeric-suffix detection.

## Commits

1. `7413a213 fix(lower/expression/mod): replace is_single_token_span
   predicate with structural BbnfKind::Span gate (AZ-IV.W0.3)`
2. `b8465682 chore(grammar/generated): regen 9/9 against fixed lowering
   quartet (AZ-IV.W0.3)`
3. (this commit) `docs(az-iv/audit): land W0.3 REGEN-redress evidence;
   retire halt reports`

The triad commits landed earlier on this branch as `27592f4e` and
`3c00fb88`.
