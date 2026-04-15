//! AU.3.2 — `ChildIter` zero-allocation regression guard.
//!
//! The tranche AU.3.2 hard gate requires a zero-heap-allocation child
//! iterator over a finished `Tape`. This file pins the iterator's
//! structural shape (so a regression that reintroduces a `Vec` /
//! `Box` field is caught at compile time) and exercises the iterator
//! over a hand-built tape to confirm the post-order backward walk
//! lands on every direct child without skipping or recursing into
//! grandchildren.

use bbnf::runtime::tape::{ChildIter, TapeBuilder, TapeCursor, TapeKind, TapeOffset};

/// AU.3.2: the iterator's footprint must stay small. Two
/// [`TapeOffset`]s (`u32` each) plus one shared `&Tape` reference
/// fits in three words on every supported ABI; we accept up to 24
/// bytes (8-byte ref + 4 + 4 + padding to 8 on 64-bit). A regression
/// that drags in a `Vec`, `Box`, or extra `usize` will blow this
/// budget.
#[test]
fn child_iter_size_bounded() {
    let size = std::mem::size_of::<ChildIter<'_>>();
    assert!(
        size <= 24,
        "ChildIter grew past 24 bytes (now {} bytes); AU.3.2 \
         budget exceeded — review for accidental heap-backed fields",
        size
    );
}

/// Build a tape with a compound that holds three leaf children (post-
/// order: [leaf0, leaf1, leaf2, compound]) and verify the zero-alloc
/// iterator yields all three children in reverse source order. The
/// reverse-order semantic is intentional: backward walk is the only
/// O(K) direction over the AoS tape layout (forward-walk discovery
/// of subtree roots is O(subtree-size); see cursor.rs commentary).
#[test]
fn child_iter_yields_three_leaves_in_reverse_order() {
    let mut b = TapeBuilder::new();
    let start = b.mark_children();
    let _l0 = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let _l1 = b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    let _l2 = b.push_leaf(TapeKind::Span, 2, 3, 0, 0);
    let parent = b.push_compound(TapeKind::Seq, start, 0, 3, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, parent);
    let yielded: Vec<(u32, u32)> = cursor
        .children_zero_alloc()
        .map(|c| c.span())
        .collect();

    // Reverse source order — last child first.
    assert_eq!(
        yielded,
        vec![(2, 3), (1, 2), (0, 1)],
        "ChildIter must yield direct children in reverse source order"
    );
}

/// A nested compound (one direct compound child whose subtree
/// contains two leaves) must be reported as a single direct child
/// — the iterator must NOT descend into the compound's grandchildren.
#[test]
fn child_iter_does_not_descend_into_grandchildren() {
    let mut b = TapeBuilder::new();
    // Outer compound's children range begins here.
    let outer_start = b.mark_children();

    // Inner compound: two leaves + their parent compound.
    let inner_start = b.mark_children();
    let _g0 = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let _g1 = b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    let _inner = b.push_compound(TapeKind::Seq, inner_start, 0, 2, 0, 0);

    // A second leaf sibling (so the outer compound has K = 2).
    let _l = b.push_leaf(TapeKind::Span, 2, 3, 0, 0);

    // Outer compound header.
    let outer = b.push_compound(TapeKind::Seq, outer_start, 0, 3, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, outer);
    let yielded: Vec<(u32, u32)> = cursor
        .children_zero_alloc()
        .map(|c| c.span())
        .collect();

    // Two direct children: the trailing leaf (span 2..3) yielded
    // first under reverse-order semantics, then the inner compound
    // (span 0..2). The inner compound's two grandchildren must
    // never appear in this list.
    assert_eq!(yielded.len(), 2, "outer compound has K = 2 direct children");
    assert_eq!(yielded[0], (2, 3), "trailing leaf yielded first in reverse");
    assert_eq!(yielded[1], (0, 2), "inner compound yielded second");
}

/// A leaf cursor (no children) must produce an empty iterator.
#[test]
fn child_iter_empty_on_leaf() {
    let mut b = TapeBuilder::new();
    let leaf = b.push_leaf(TapeKind::Span, 0, 5, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, leaf);
    assert_eq!(cursor.children_zero_alloc().count(), 0);
}

/// An empty compound (no `mark_children` → push_* → push_compound
/// run) must also produce an empty iterator. The `push_compound`
/// helper already clears the `has_children` flag for this case; the
/// iterator must respect it and stop immediately.
#[test]
fn child_iter_empty_on_compound_with_no_children() {
    let mut b = TapeBuilder::new();
    let start = b.mark_children();
    let parent = b.push_compound(TapeKind::Seq, start, 0, 0, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, parent);
    assert_eq!(cursor.children_zero_alloc().count(), 0);
}

/// Iterator equivalence: the zero-alloc `children_zero_alloc()` and
/// the Vec-backed `children()` must yield the same set of direct
/// children — only the order differs. Useful for catching off-by-one
/// regressions in either path.
#[test]
fn child_iter_matches_children_set() {
    let mut b = TapeBuilder::new();
    let start = b.mark_children();
    let _l0 = b.push_leaf(TapeKind::Span, 0, 2, 0, 0);
    // Inner compound with one leaf grandchild.
    let inner_start = b.mark_children();
    let _g = b.push_leaf(TapeKind::Span, 2, 4, 0, 0);
    let _inner = b.push_compound(TapeKind::Seq, inner_start, 2, 4, 0, 0);
    let _l1 = b.push_leaf(TapeKind::Span, 4, 6, 0, 0);
    let parent = b.push_compound(TapeKind::Seq, start, 0, 6, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, parent);
    let mut fwd: Vec<(u32, u32)> = cursor.children().map(|c| c.span()).collect();
    let mut rev: Vec<(u32, u32)> =
        cursor.children_zero_alloc().map(|c| c.span()).collect();
    fwd.sort();
    rev.sort();
    assert_eq!(
        fwd, rev,
        "ChildIter and children() must visit the same direct children"
    );
}

/// Sanity: the iterator's `next` is `#[inline]` — confirmed at the
/// API surface by checking the `ChildIter` public `Iterator` impl
/// can be consumed via standard combinators (`.map`, `.fold`,
/// `.count`) without surprise. The actual inline annotation is
/// behavioural; this test just ensures the shape.
#[test]
fn child_iter_supports_iterator_combinators() {
    let mut b = TapeBuilder::new();
    let start = b.mark_children();
    let _l0 = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let _l1 = b.push_leaf(TapeKind::Span, 1, 2, 0, 0);
    let parent = b.push_compound(TapeKind::Seq, start, 0, 2, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, parent);

    // Sum of span lengths via fold (commutative — order-agnostic).
    let total_span: u32 = cursor
        .children_zero_alloc()
        .fold(0u32, |acc, c| acc + (c.span().1 - c.span().0));
    assert_eq!(total_span, 2);

    // Count via the standard combinator.
    assert_eq!(cursor.children_zero_alloc().count(), 2);
}

// Defensive: verify the `TapeOffset` wraparound boundary. The
// iterator's exit condition is `next.0 <= end.0` (saturating at
// zero); a buggy decrement past zero would underflow `u32` and walk
// forever.
#[test]
fn child_iter_exit_condition_does_not_underflow() {
    let mut b = TapeBuilder::new();
    let start = b.mark_children();
    let _l = b.push_leaf(TapeKind::Span, 0, 1, 0, 0);
    let parent = b.push_compound(TapeKind::Seq, start, 0, 1, 0, 0);
    let tape = b.finish().expect("tape build");

    let cursor = TapeCursor::new(&tape, parent);
    // K = 1: a single iteration then None. If the iterator over-
    // shoots, this collect would never terminate.
    let collected: Vec<TapeOffset> = cursor
        .children_zero_alloc()
        .map(|c| c.offset())
        .collect();
    assert_eq!(collected.len(), 1);
}
