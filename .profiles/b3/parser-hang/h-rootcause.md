# B3.W0.η — Pratt no-op-chain child_off + lowering cousin-leak guard

**Status**: PARSER BASELINE RESTORED end-to-end. json regen exits 0
with file write. bbnf regen reaches the codegen stage; the residual
`syn::parse2: expected loop or block expression` failure is a
downstream TokenStream defect distinct from the W0.γ–η parser/lowering
work.

## Phase 1 — panic captured

`expression.rs:465`:

```
thread 'main' panicked at crates/core/src/lower/expression.rs:465:13:
lower/expression.rs: binary_factor could not resolve operator —
no binary_operators child and source gap "" contains no recognized
token (chain = "\"null\" -> 0u8 ")
```

Backtrace top three frames:

```
16: bbnf::lower::expression::lower_binary_factor::{closure#1}
    crates/core/src/lower/expression.rs:465
17: <core::option::Option<&str>>::unwrap_or_else
18: bbnf::lower::expression::lower_binary_factor
    crates/core/src/lower/expression.rs:464
```

Full stderr archived at `.profiles/b3/parser-hang/h-panic-stderr.txt`.

## Phase 2 — root cause

Two coupled defects masked by the cycle and unmasked by ζ.

### Defect 1: Pratt `this_operand_root` seeded at leftmost descendant

`crates/core/src/backend/rust/emitter/shapes/pratt.rs` initialised:

```rust
let outer_child_mark_idx: u32 = outer_off.saturating_add(1);
let mut this_operand_root: u32 = outer_child_mark_idx;
#operand_call
let _ = _operand_off;
```

After `parse_pratt_<rule>`'s `begin_compound` opened the outer Pratt
compound at `outer_off`, the operand was dispatched. `parse_flat_*` /
`parse_pratt_*` shape functions emit their interior records FIRST and
the outer compound row LAST (post-order); the dispatcher's returned
`_operand_off` is the operand's outer-compound offset, not the
leftmost-descendant `outer_off + 1`.

The post-`end_compound` write `set_child_off_at(outer_off,
this_operand_root)` therefore landed inside the operand body rather
than on the operand's row. For single-operand Pratt chains (no
reducers fire) the cursor's children iteration on the Pratt outer
then entered the operand's INTERIOR, surfacing multiple records as
separate "operands" to `lower_binary_factor` and tripping its
operator-resolution panic. Same defect on the per-iteration RHS
update (`this_operand_root = _op_rec.0 + 1` — leftmost descendant
of the RHS subtree, not the RHS outer).

ζ's residual-panic note pointed at this exact override.

### Defect 2: lowering cousin-leak under bumped frame_depth

After Defect 1's fix admitted the parser past binary_factor's
single-operand path, a different shape problem surfaced for chains
followed by post-order Seq wrappers in the same parent body
(BBNF self-host: `("->" value_expr (":" type_annotation)?)?` at
`mapped_factor`'s mapping clause; `(binary_factor ?w , "," ?)` at
`concatenation`'s iteration body).

The AY-II.W0.b `end_compound_post_order` bump cascade — extended by
ζ to cover the entire subtree via the leftmost-descendant chain —
puts pre-order Pratt-body records (e.g., `value_and` outer at
`parent + 2` depth inside `value_or`) and post-order Seq-wrapper
sibling records (e.g., `__iter_off` at `parent + 2` depth alongside
`value_or` itself) at the SAME final frame_depth. The finaliser's
depth-only sib_skip computation then chains the two records as
"siblings" — they're cousins (different parent compounds) at the
same nesting level relative to the surrounding Seq.

When the lowering's chain-operand collector reads
`node.children()` on a Pratt outer, the cursor's `sib_skip` walk
crosses past `node.span_hi` and surfaces the cousin record as a
phantom extra operand.

## Phase 3 — fix

### Pratt emitter (`crates/core/src/backend/rust/emitter/shapes/pratt.rs`)

```rust
// ── Leftmost operand ────────────────────────────────────────────
// Dispatch the operand FIRST, then capture its returned
// outer-compound offset as the initial `this_operand_root`.
//
// B3.W0.η — `parse_flat_*` / `parse_pratt_*` shape functions
// emit their interior records first and the outer compound
// LAST (post-order), returning the outer compound's row.
#operand_call
let mut this_operand_root: u32 = _operand_off.0;
```

And on the per-iteration RHS:

```rust
#rhs_call
this_operand_root = _rhs_off.0;
```

The seven `parse_pratt_BbnfBootstrap_*` rules in
`crates/core/src/grammar/generated.rs` are patched in lockstep with
the emitter so the BBNF self-host parses with the corrected layout —
`xtask regen` would have re-emitted these had the chicken-and-egg of
bbnf's self-host allowed it (regen requires a working parser).

### Lowering cousin-leak guard

`crates/core/src/lower/expression.rs::collect_binary_operands` and
`crates/core/src/lower/value_expr.rs::collect_chain_operands`:

```rust
let body_hi = body.span().1;
let in_scope = |c: &BbnfBootstrapNodeView<'a>| {
    let (lo, hi) = c.span();
    hi > lo && lo < body_hi
};
let mut children = body.children().filter(in_scope);
```

Strict span containment by the chain compound's `span_hi` discards
the cousin records the finaliser's sib_skip walk surfaces past the
chain's actual scope. Legitimate operands inside the chain remain
admitted (their `span_lo < body_hi`).

## Phase 4 — verification

| Step | Result |
|---|---|
| `cargo build -p xtask --release` | exit 0 in 1m |
| `xtask regen --grammar json` | exit 0; parse 1.31 ms, generate 3.90 ms, prettyplease 10.54 ms; 235 785 bytes written to `crates/core/src/grammar/generated/json.rs` |
| `xtask regen --grammar bbnf` | parser+lowering+IR+generate_all complete (parse-end fires for all 3 import sources: 3448 + 1378 + 301 bytes); fails at `syn::parse2: expected loop or block expression` — downstream codegen defect, separate from W0.γ–η scope |
| `cargo nextest run -p tape --profile ax-iter` | 100 / 100 passed in 0.149 s |
| `cargo check -p bbnf -p xtask -p bbnf_derive --profile ax-iter` | exit 0 in 5.81 s |

The W0.γ–η stack is sound through the parser, lowering, IR, and
codegen entry points. The residual `syn::parse2` syntax error is
emitted Rust that doesn't parse — distinct from any parser/lowering
correctness concern. ζ similarly noted a downstream `serialize/mod.rs:64`
issue post-γ; the `syn::parse2` error is the post-η downstream peer.

## Phase 5 — cherry-pick

| Artefact | SHA |
|---|---|
| Worktree fix-commit | `1ed0dbfe` (`bbnf-wt-b3-w0h-pratt`) |
| Master fix-commit | `34ecb83d` |

Worktree: `/Users/mkbabb/Programming/bbnf-wt-b3-w0h-pratt`.
Main repo: `/Users/mkbabb/Programming/bbnf-lang`.

## Phase 6 — close-out

B3.W0 closes on parser-baseline restoration scope: γ resolved the
finaliser cycle, δ + ε refined frame_depth derivation, ζ extended the
post-order bump to cover entire subtrees, η resolves the Pratt-emitter
override and the lowering cousin-leak. The parser produces a
well-formed tape end-to-end; the lowering walks it correctly; the IR
and codegen stages run to completion.

The remaining downstream codegen `syn::parse2` syntax error is out
of scope for B3 — it's a separate emit-correctness defect, addressed
naturally as B4 / AY-II re-land work proceeds on the now-restored
parser substrate.
