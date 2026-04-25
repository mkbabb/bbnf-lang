# B3.W0.γ — Parser-Hang Root-Cause Analysis

**Verdict**: ROOT CAUSE LOCATED. Architectural fix applied at the source.

The parser hang in `BbnfBootstrap::parse` was a tight inner-loop spin in
`tape::finaliser::derive_frame_depth` caused by a contract violation
between the function's reverse-walk algorithm and the shape emitters'
mixed pre-order / post-order compound emission.

## Failing-record tuple (Phase 1 panic)

`assert!` at `crates/tape/src/finaliser.rs:370` fires on json grammar
(537 B input) producing 637 tape records:

```
parent_idx=585  co=575  child_off=576  pos=576  start=575  end=585  len=637
```

Captured at `.profiles/b3/parser-hang/g-panic-stderr.txt`.

Interpretation:

- Record 585 is a post-order compound (`child_off = 575 < 585`). Its
  direct children sit in `[575, 585)`.
- Record 575 is a pre-order compound (`child_off = 576 = 575 + 1`).
  Its own children sit in `[576, ...)` — *forward* of itself.
- The reverse walk arrives at `pos = 576`, sets `co = pos - 1 = 575`,
  reads `co.child_off = 576`, and "leaps" to `pos = 576`. The leap is
  identity — `pos` does not decrease. The `while pos > start` guard
  (`576 > 575`) stays true forever; the inner loop spins on `co = 575`.

## Source bug location

`crates/tape/src/finaliser.rs:362-373` (pre-fix), in
`derive_frame_depth`:

```rust
let mut pos = end;
while pos > start {
    let co = pos - 1;
    depth[co] = child_depth;
    let co_has_children = columns.has_children_at(co as u32);
    let co_child_off = columns.child_off_at(co as u32);
    pos = if co_has_children && !co_child_off.is_none() {
        co_child_off.0 as usize  // ← unconditional leap; spins when child_off >= pos
    } else {
        co
    };
}
```

The reverse walk's intent is to enumerate the direct children of the
outer post-order parent and stamp `depth[co] = child_depth` for each.
The leap to `co_child_off` skips over a post-order child compound's
own subtree (which sits at indices `[co_child_off, co)`, *strictly
before* `co`). The algorithm therefore presupposes
`child_off < self_idx` for every compound with children — the
canonical post-order invariant.

## Why broken

The shape emitters in `crates/core/src/grammar/generated.rs` mix
emission shapes freely:

- **Post-order** (`builder.end_compound_post_order(open, span_hi, mark)`):
  children emitted *before* the wrapping compound row, with
  `child_off < self_idx` pointing backward at the first child's root.
  ~105 callsites in `generated.rs`, used by Flat / Wrap / Inline /
  ArgList / Unordered / AltDispatch shapes.
- **Pre-order** (`builder.end_compound(open, span_hi)`): wrapping
  compound row emitted *first*, with `child_off = open_offset + 1`
  pointing forward at the first child. ~9 callsites in `generated.rs`,
  used by the Pratt expression shape (the BBNF grammar's `value_path`
  rule emits one such Pratt compound per parsed expression-position
  alternative).

`derive_frame_depth`'s reverse-walk leap is correct for post-order
children but degenerate for pre-order children (`co_child_off > co`
makes the leap go upward instead of downward). When a pre-order
compound appears as a direct child of a post-order parent — exactly
the BBNF Pratt-inside-Seq pattern — the loop spins.

`derive_frame_depth` predates the Pratt-inline shape: it landed at
`f603f549` (AY.W1.1 AoS revert, 2026-04-20) before `49d468f2`
(AY.W1.6 Pratt Option C regen, same day) emitted the first
mixed-shape tape that exposed the contract violation. The latent bug
was masked until W0' / W0-fix changes amplified Pratt usage on the
self-host BBNF grammar.

## The fix — in-builder frame_depth bookkeeping

`derive_frame_depth` is retired entirely. Depth is now tracked inline
in [`FusedBuilder`](crates/tape/src/builder/mod.rs) on every
structural push, with a retroactive bump in
`end_compound_post_order` to migrate post-order leaves to the correct
depth.

### `FusedBuilder` state

- New `current_depth: u8` — incremented on `begin_compound`
  (post-stamp, so the compound's own row sits at the outer frame's
  depth and its children stamp at `outer + 1`); decremented on every
  `end_compound` / `end_compound_post_order`.
- Existing `frame_depth: Vec<u8>` — auto-stamped in lockstep with
  every column push (`push_leaf`, `push_leaf_with`, `begin_compound`,
  every payload-bearing variant).

### Pre-order shape (`begin → children → end_compound`)

`begin_compound` stamps the compound at the outer frame's depth,
bumps `current_depth` for the children's emission. Children push at
the bumped depth. `end_compound` decrements. `frame_depth` is
already correct on every record; no retroactive fix-up.

### Post-order shape (`children → begin → end_compound_post_order`)

Children are pushed *before* `begin_compound`, so they stamp at the
outer frame's depth — initially the *same* depth as the wrapping
compound row that follows. The wrapping `begin_compound` also stamps
at the outer depth, then bumps. `end_compound_post_order` decrements
back, and — when `first_child < open_offset` (children actually
landed) — retroactively bumps `frame_depth[first_child..open_offset]`
by one. The compound's own row (at `open_offset`) is excluded from
the range; only the child range moves.

Nested post-order subtrees compose correctly: each inner
`end_compound_post_order` bumps its own child range first; the outer
close then bumps the entire combined range, accumulating the total
depth offset.

### Patched emission (`crates/tape/src/builder/mod.rs:529-560`)

```rust
pub fn end_compound_post_order(&mut self, open_offset, span_hi, first_child) {
    self.columns.set_span_hi_at(open_offset, span_hi);
    if !first_child.is_none() && first_child.0 < open_offset {
        self.columns.set_child_off_at(open_offset, first_child);
        self.columns.or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
        // Children at [first_child, open_offset) were stamped at the
        // outer frame's depth (same as this compound row). Bump them
        // to (parent + 1) so the finaliser's per-depth sib_skip walk
        // groups them as direct children of THIS compound.
        let lo = first_child.0 as usize;
        let hi = open_offset as usize;
        for slot in &mut self.frame_depth[lo..hi] {
            *slot = slot.saturating_add(1);
        }
    }
    self.current_depth = self.current_depth.saturating_sub(1);
    self.value_end_compound(span_hi);
}
```

### `run_finaliser` simplification

```rust
fn run_finaliser(&mut self) {
    debug_assert_eq!(self.frame_depth.len(), self.columns.len(), ...);
    crate::finaliser::finalise(&mut self.columns, &self.frame_depth);
}
```

The pre-fix two-mode dispatch (`if has_inline_frame_depth { ... } else
{ derive_frame_depth(...) }`) is gone. The `has_inline_frame_depth`
field, the `enable_inline_frame_depth` method, and the
`derive_frame_depth` function are deleted. KISS, one path.

## Why this restores the contract

The finaliser's `finalise` forward scan groups records into
per-depth sibling chains via the `frame_depth` column it consumes.
The retired `derive_frame_depth` produced this column from the
`child_off` graph in a reverse walk that broke on mixed shapes.
In-builder bookkeeping produces the same column from the parser's
own state, with no graph traversal — hence no shape-shape entanglement.

## Verification

| Step | Result |
|---|---|
| `cargo build -p xtask --release` | exit 0 |
| `BBNF_REGEN_PHASE_LOG=1 cargo xtask regen --grammar json` | parse-end fires; total wall < 1 s |
| `BBNF_REGEN_PHASE_LOG=1 cargo xtask regen --grammar bbnf` | parse-end fires; total wall < 5 s |
| `cargo nextest run -p tape --profile ax-iter` | all tape tests pass |

## Artefacts

- `.profiles/b3/parser-hang/g-panic-stderr.txt` — Phase 1 assert capture.
- `.profiles/b3/parser-hang/g-rootcause.md` — this document.
