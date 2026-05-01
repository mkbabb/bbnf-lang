# AY.W1.3 — structural-scan consumer coverage

W1.3 closes the substrate-with-consumer cycle for the per-grammar
`StructuralIndex`. This audit catalogs every consumer site W1-C
landed and the residual sites a follow-on wave (AY.W4 SIMD
specialisation work) should pick up.

## Substrate

`crates/tape/src/structural_scan.rs` — `scan_structural(input,
alphabet) -> StructuralIndex` + `next_structural_at_or_after(index,
from) -> Option<u32>`. Re-exported from `crates/tape/src/lib.rs`.

The result type re-uses the existing `stage1::StructuralIndex` —
column-aligned `positions: Vec<u32>` + `kinds: Vec<u8>` — so
downstream consumers can binary-search the position column.

## Per-grammar wiring

`crates/core/src/backend/rust/emitter/shapes/dispatcher.rs::emit_support_module`
extends every grammar's `__shape_support_<g>::ScanState` with:

- `pub(crate) structural_index: ::bbnf::runtime::tape::StructuralIndex` field.
- `pub fn init_for_input(&mut self, input: &[u8])` populator that
  calls `scan_structural(input, GRAMMAR_PROFILE.structural_alphabet)`.

`crates/core/src/backend/rust/emitter/grammar.rs::emit_grammar_impl`
threads two consumer reads:

1. **Capacity refinement** — at parse entry, after `init_for_input`,
   widen `TapeBuilder::with_capacity` to
   `max(profile.capacity_for(input.len()), structural_index.len() *
   2 + 2)`. The index length is a tight upper bound on the parse's
   record count.
2. Same on the visitor-path entry (`parse_with_visitor`).

## Consumer sites — landed

### `skip_space_slow` (plain ASCII variant)

`emit_skip_space_plain` in `dispatcher.rs:92`. The slow path now
opens with a probe:

```rust
if let Some(__next_struct) = ::bbnf::runtime::tape::next_structural_at_or_after(
    &state.structural_index, *p as u32,
) {
    let __next = __next_struct as usize;
    if __next < *p + 64 && __next <= input.len() {
        // Validate `[*p, __next)` is all whitespace; jump on success.
    }
}
```

When the structural-byte probe lands within the next 64-byte stripe
AND every intervening byte is whitespace per the existing scalar
predicate, advance `*p` directly to the structural position and
return — bypassing the SIMD bitmap evaluation entirely. Empty
alphabet → `None` → fall through to unchanged SIMD path.

Coverage: JSON, BBNF, Sheets, GoogleSheets, all `@ws`-default
grammars hit this path.

### Tape capacity refinement

`emit_grammar_impl` in `grammar.rs:497`. Reads
`state.structural_index.len()` per parse to widen the
`TapeBuilder::with_capacity` reservation. Universal — every grammar
with a non-empty mined alphabet.

## Consumer sites — deferred to AY.W4

### `skip_space_slow` (comment-aware variant)

`emit_skip_space_comment_aware` in `dispatcher.rs:177`. CSS L4 is
the sole consumer (its `@ws` regex matches
`RegexClass::WhitespaceWithBlockComment`). The probe semantics are
non-trivial here because CSS L4's structural alphabet includes `/`
(the comment opener) — a naive probe would jump us into a comment
that the loop must then handle. The W1.3 wire skips this variant;
CSS L4's substrate is still verified via the StructuralIndex symbol
+ capacity-refinement consumer at parse entry.

Forward-reference: AY.W4 (regex-scan specialisation + SIMD
unescape) should fold this consumer into the comment-aware path
as part of the broader regex-scan rewrite. The substrate is in
place; the comment-aware path needs an alphabet-aware variant of
the probe that distinguishes "next structural delimiter" from
"next comment opener" (the CSS-specific `/*` digraph).

### CTNS (consume_to_next_structural)

The W1.3 spec mentioned CTNS as a candidate consumer site, but
no `consume_to_next_structural` calls exist in the current emitter
shapes (a grep over `crates/core/src/backend/rust/emitter/` returns
zero matches). The pre-W0b walker had a CTNS helper; W0b retired
it. The structural-scan substrate stands ready for any future
emitter pass that re-introduces CTNS or its successor.

## Verification

`docs/benchmarks/archive/post-AY-W1-phase2c-nm.txt` — symbol presence
table per bench binary. `StructuralIndex` drop_in_place is in 4/4
binaries (proves runtime instantiation). `next_structural_at_or_after`
is in 3/4 (json, sheets, bbnf — comment-aware grammars excluded as
above). `scan_structural` is fully inlined under release LTO and
appears as a symbol in 0/4.

Bootstrap regen cycle-1 = cycle-2 byte-identical (`/tmp/regen-diff.txt`
empty post-W1.3 commits).
