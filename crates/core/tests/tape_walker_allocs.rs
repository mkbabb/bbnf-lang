//! AU.3.2 / AV.2.2 — `ChildIter` zero-allocation regression guard.
//!
//! AU.3.2 introduced the first zero-heap-allocation child iterator
//! over the AoS tape via backward post-order walk (reverse source
//! order). AV.2.2 flipped the substrate to a columnar layout with a
//! `sib_skip` column: forward sibling traversal is now one indexed
//! column load per step, so forward *source* order is also zero-
//! allocation. The pre-AV split between Vec-backed `children()`
//! (source order) and linked-list `children_zero_alloc()` (reverse
//! order) collapses into a single forward-order zero-alloc iterator.
//!
//! This file pins the iterator's structural shape (regression
//! catches on accidental heap-backed fields) and exercises the
//! iterator over hand-built tapes to confirm direct-children
//! enumeration lands on every child without skipping or recursing
//! into grandchildren.

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

/// Build a tape with a compound that holds three leaf children
/// (post-order: [leaf0, leaf1, leaf2, compound]) and verify the
/// zero-alloc iterator yields all three children in forward source
/// order. AV.2.2's sibling-skip column makes forward iteration a
/// single indexed column load per step, unifying what was previously
/// two iterators (`children()` and `children_zero_alloc()`) into one
/// zero-alloc forward iterator.
#[test]
fn child_iter_yields_three_leaves_in_forward_order() {
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

    // Forward source order — first child first.
    assert_eq!(
        yielded,
        vec![(0, 1), (1, 2), (2, 3)],
        "ChildIter must yield direct children in forward source order"
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

    // Two direct children: under forward-order semantics the inner
    // compound (span 0..2) yields first, then the trailing leaf
    // (span 2..3). The inner compound's two grandchildren must
    // never appear in this list.
    assert_eq!(yielded.len(), 2, "outer compound has K = 2 direct children");
    assert_eq!(yielded[0], (0, 2), "inner compound yielded first in forward order");
    assert_eq!(yielded[1], (2, 3), "trailing leaf yielded second");
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
