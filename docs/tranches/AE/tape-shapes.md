# AE.0 — Tape compound shapes under structural mode

Reference for the tape records each bbnf grammar construct produces
through the tape-first Rust emitter. Drives the shape-agnostic
lowering rewrite in AE.1.

## Push rules (what emits a tape record and what doesn't)

| Construct | Pushes? | Record kind | variant_idx |
|---|---|---|---|
| `Literal("...")` | no | — | — |
| `Regex("...")` | no | — | — |
| `Epsilon` | no | — | — |
| `Seq(children)` | no | — | — |
| `Alt([...])` | no | — | — |
| `Optional(x)` *(`x?`)* | yes | `Repeat` | 0 |
| `Repeat(x, 0..)` *(`x*`)* | yes | `Repeat` | 0 |
| `Repeat(x, 1..)` *(`x+`)* | yes | `Repeat` | 0 |
| `sep_by(elem, sep)` | yes | `Repeat` | 0 |
| `Rule` *(MustTape class)* | yes | `Rule` | `rule.id as u8` |
| `Rule` *(TapeSpanOnly class)* | yes | `Span` | `rule.id as u8` |
| `Rule` *(TransparentElide class)* | n/a | — | — *(function elided; inlined at call sites)* |
| `Ref(target)` | inherits | whatever `target` pushes |
| `Skip(l,r)`, `Next(l,r)`, `Minus(l,r)` | inherits |
| `Map { inner, fn_id }` | inherits from `inner` |
| `OptionalWhitespace(inner)` | inherits from `inner` |

**Key insight:** Only `Repeat` / `Optional` / `Rule` push records.
Every other construct is a side-effect chain that returns
`Option<()>`. The tape layout is therefore: rule compounds at the
top, containing the flattened stream of sub-rule calls and repeat
wrappers that ran during the rule's body.

**Side effect:** `state.offset` advances as leaves / regexes match.
All children reserve their run via `TapeBuilder::mark_children(tape)`
in the rule/Repeat prelude and commit via `push_compound` / `push_leaf`
in the epilogue.

## Shape implications

### 1. Rule compound children are flat (no Seq wrappers)

`Seq` doesn't push. So a rule like
`pair = key , ":" , value` under MustTape produces
`Rule(pair, children=[Rule(key), Rule(value)])` — the `":"` literal
pushes nothing, `Seq` pushes nothing, only the two sub-rule calls
reach the tape. The lowering sees `pair.children()` as the direct
list of non-elided parts.

### 2. Repeat wrapping is explicit

`alternation = ( concatenation ?w , "|" ? ) +` under MustTape
produces:

```
Rule(alternation)
├── Repeat (variant_idx=0)   ← from `+`
│   ├── Rule(concatenation)   ← iteration 1 body
│   ├── Repeat (empty)        ← iteration 1 optional pipe
│   ├── Rule(concatenation)   ← iteration 2 body
│   ├── Repeat (1 child)      ← iteration 2 optional pipe
│   └── ...
```

The `+` compound wraps every iteration's side-effect chain in a
single `TapeKind::Repeat` compound. Iterations are flattened into
the Repeat's child run — there's no per-iteration wrapper
compound, just the linearized sub-parse effects from each
iteration.

Since the `|` literal doesn't push and `"|"?` is Optional (which
DOES push an empty-or-single-child Repeat), the iteration pattern
leaves alternating `(concatenation_compound, optional_repeat)`
pairs. The lowering must consume them as **pairs**, not as flat
branches.

### 3. Optional compounds are always pushed (even when empty)

`"|" ?` under MustTape pushes `Repeat(variant_idx=0)` with zero
children on a miss, one child on a hit. Span collapses to `(lo,
lo)` when empty. The lowering uses a span-emptiness check to
distinguish present / absent, not child count.

### 4. `?w` (OptionalWhitespace) pushes nothing

`?w` is a trailing modifier that consumes whitespace after a term.
It's side-effect only; no record lands on the tape. Lowering can
ignore `?w` — it doesn't affect the CST shape at all.

### 5. `mapped_factor = factor , ( "->" , ( value_expr , type? ) )?`

The outer `( ... )?` is Optional → `Repeat` compound.
- If the mapping arrow is absent: `Repeat(variant_idx=0, children=[])`
- If present: `Repeat(variant_idx=0, children=[Rule(value_expr), Repeat(type?)])`

The lowering must:
- Check the outer Optional's span-emptiness to decide whether to
  emit a `Map` IR node.
- If present, walk its children (unwrapping the nested
  `(value_expr, type_ann?)` Seq).

### 6. Top-level `grammar = ( grammar_item ?w ) *`

- `Rule(grammar)` at the root.
- Its single child is the outer `Repeat` from `*`.
- `Repeat`'s children are one `Rule(grammar_item)` per top-level
  item.
- Each `grammar_item` is a transparent single-branch alternation
  over `comment | big_comment | directive | rule`. Under
  preserve_identity, `grammar_item` and `directive` are emitted
  as rule compounds with a single child (the actual item).

`host.rs::extract_grammar` already handles the top-level
`TapeKind::Repeat` unwrap (line 47). AE must mirror this pattern
for every layer.

## Canonical lowering discipline

From these shapes, AE.1's lowering design follows:

1. **`iter_rep_children(view)`** — if `view.children()` yields a
   single `TapeKind::Repeat` compound, return that Repeat's
   iter; else return `view.children()`. Mirrors
   `host.rs:44-54`.

2. **`find_child_by_kind(view, K)`** — iterate `view.children()`
   and return the first child with matching `rule_kind()`.
   Replaces positional `child(1) / child(2)` reads that break
   under preserved Optional wrappers.

3. **`peel_transparent(view)`** — whitelisted single-child
   wrapper rules (`rhs`, `lhs`, `grammar_item`, `directive`,
   etc.) descend through `child(0)` until reaching a
   semantic-content rule_kind.

4. **`iter_pairs_rep(view)`** — for repeat-of-Seq patterns like
   `( concat , "|"? ) +` and `( bf , "," ? ) +`, iterate the
   unwrapped Repeat's children, consuming them as
   `(content, optional)` pairs. The optional child is ignored;
   the content child is lowered.

5. **Catch-all is a panic.** `lower_node`'s `_ =>
   panic!("unhandled bbnf.bbnf rule_kind: {:?}", k)`. Silent
   Epsilon is forbidden.

## Reference: where in the code

| Construct | File | Function |
|---|---|---|
| Rule compound push | `crates/core/src/backend/rust/emitter/tape_prelude.rs` | `emit_must_tape_epilogue` |
| Rule span leaf push | `crates/core/src/backend/rust/emitter/tape_prelude.rs` | `emit_tape_span_only_epilogue` |
| Repeat push | `crates/core/src/backend/rust/emitter/repeat.rs` | `emit_repeat_many_impl` |
| Optional push | `crates/core/src/backend/rust/emitter/repeat.rs` | `emit_repeat_optional_impl` |
| Sep_by push | `crates/core/src/backend/rust/emitter/repeat.rs` | `emit_sep_by_impl` |
| Seq composition | `crates/core/src/backend/rust/emitter/seq.rs` | `emit_seq_grouped_impl` |
| Alt composition | `crates/core/src/backend/rust/emitter/alt.rs` | `emit_alt_*_impl` |
| Literal match | `crates/core/src/backend/rust/emitter/leaves.rs` | `emit_literal_match_impl` |
| Regex match | `crates/core/src/backend/rust/emitter/leaves.rs` | `emit_regex_match_impl` |
| TransparentElide skip | `crates/core/src/backend/rust/emitter/grammar.rs` | `emit_rule_function_impl` (early-returns empty) |
| Cursor children walk | `crates/bbnf-tape/src/cursor.rs` | `TapeCursor::children` (backward post-order walk) |
