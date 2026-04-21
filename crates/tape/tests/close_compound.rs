//! AY.W5.1 — `TapeBuilder::open_compound` / `close_compound` contract.
//!
//! Verifies the write-time close-stamping substrate:
//!
//! 1. `open_compound` pushes the parent row in pre-order (offset
//!    `parent`); subsequent pushes at offsets `parent + 1`, `parent
//!    + 2`, … become the compound's direct children, and nested
//!    `open_compound` / `close_compound` pairs preserve the stack
//!    ordering.
//! 2. `close_compound` stamps `span_hi`, `child_off`, and the
//!    `HAS_CHILDREN_BIT` on the parent row.
//! 3. Each direct child gets `SIB_SKIP_STAMPED_BIT` set exactly once
//!    (the last sibling at close time, the non-last siblings at the
//!    moment their next sibling was pushed).
//! 4. Direct children's `sib_skip` column carries the inter-sibling
//!    root distance inline — finaliser has nothing to re-derive.
//! 5. The finished `Tape`'s `TapeCursor` reads the tree exactly as it
//!    would for a `push_compound`-based tape (same children,
//!    same order, same spans).

use tape::{Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset, TapeRec};

/// Build `(root (inner a b) c)` via open/close_compound:
///
/// - open_compound(root)       → row 0   (compound)
///   - open_compound(inner)    → row 1   (compound)
///     - push_leaf(a)          → row 2   (leaf, first child of inner)
///     - push_leaf(b)          → row 3   (leaf, last child of inner)
///   - close_compound(inner)   pops row 1's frame
///   - push_leaf(c)            → row 4   (leaf, last child of root)
/// - close_compound(root)      pops row 0's frame
#[test]
fn nested_open_close_produces_pre_order_tape() {
    let mut b = TapeBuilder::new();

    let root = b.open_compound(TapeKind::Seq, 0, 0, 0);
    assert_eq!(root, TapeOffset(0));

    let inner = b.open_compound(TapeKind::Seq, 0, 0, 0);
    assert_eq!(inner, TapeOffset(1));

    let a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    assert_eq!(a, TapeOffset(2));
    let bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    assert_eq!(bl, TapeOffset(3));

    b.close_compound(inner, 2);

    let c = b.push_leaf(TapeKind::Literal, 2, 3, 0, 0);
    assert_eq!(c, TapeOffset(4));

    b.close_compound(root, 3);

    // Before `finish()` runs the finaliser, we can observe every
    // close-stamped value directly on the columns.
    let cols = b.columns();

    // Parent rows carry the write-time close stamps.
    assert_eq!(cols.span_hi_at(root.0), 3);
    assert_eq!(cols.span_hi_at(inner.0), 2);
    assert_eq!(cols.child_off_at(root.0), TapeOffset(1));
    assert_eq!(cols.child_off_at(inner.0), TapeOffset(2));
    assert!(cols.has_children_at(root.0));
    assert!(cols.has_children_at(inner.0));

    // Sib-skip stamps on direct children.
    //
    // root's direct children: inner (row 1) and c (row 4).
    //   sib_skip[1] = 4 - 1 = 3  (inner → c, stamped by note_push when c arrived)
    //   sib_skip[4] = 0          (c is last sibling; stamped by close_compound(root))
    //
    // inner's direct children: a (row 2) and b (row 3).
    //   sib_skip[2] = 3 - 2 = 1  (a → b, stamped by note_push when b arrived)
    //   sib_skip[3] = 0          (b is last sibling; stamped by close_compound(inner))
    assert_eq!(cols.sib_skip_at(1), 3);
    assert_eq!(cols.sib_skip_at(4), 0);
    assert_eq!(cols.sib_skip_at(2), 1);
    assert_eq!(cols.sib_skip_at(3), 0);

    // Every direct-child record carries the SIB_SKIP_STAMPED_BIT.
    assert_ne!(cols.extra_at(1) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_ne!(cols.extra_at(2) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_ne!(cols.extra_at(3) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_ne!(cols.extra_at(4) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);

    // The root compound itself is never a direct child of any open
    // frame, so its sib_skip stays at the default 0 and the bit
    // stays clear — the finaliser would (correctly) leave it alone
    // as well.
    assert_eq!(cols.sib_skip_at(0), 0);
    assert_eq!(cols.extra_at(0) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);

    // finish() still runs the finaliser; the SIB_SKIP_STAMPED_BIT
    // gates keep the close-stamped values untouched.
    let tape: Tape = b.finish().expect("finish() should succeed");
    assert_eq!(tape.len(), 5);
    let cols = tape.columns();
    assert_eq!(cols.sib_skip_at(1), 3);
    assert_eq!(cols.sib_skip_at(2), 1);

    // Pre-order cursor walk descends via the write-time child_off
    // without falling through to the post-order backward-walk
    // fallback.
    let cursor = TapeCursor::new(&tape, root);
    assert_eq!(cursor.kind(), TapeKind::Seq);
    assert_eq!(cursor.span(), (0, 3));
    assert_eq!(cursor.child_count(), 2);

    let inner_cursor = cursor.child(0).unwrap();
    assert_eq!(inner_cursor.offset(), inner);
    assert_eq!(inner_cursor.child_count(), 2);

    let c_cursor = cursor.child(1).unwrap();
    assert_eq!(c_cursor.offset(), c);
    assert_eq!(c_cursor.span(), (2, 3));
}

/// A compound that closes without any children records the
/// correct provisional state: `child_off` stays at `NONE`,
/// `has_children` stays clear, and the finaliser leaves the empty
/// compound alone.
#[test]
fn close_compound_without_children() {
    let mut b = TapeBuilder::new();
    let root = b.open_compound(TapeKind::Seq, 5, 0, 0);
    b.close_compound(root, 5);

    let cols = b.columns();
    assert_eq!(cols.span_lo_at(root.0), 5);
    assert_eq!(cols.span_hi_at(root.0), 5);
    assert_eq!(cols.child_off_at(root.0), TapeOffset::NONE);
    assert!(!cols.has_children_at(root.0));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);
}

/// Independent `mark_children` + `push_compound` builds continue to
/// work unchanged — an emitter that never uses `open_compound` stays
/// on the legacy path and the finaliser derives its sib_skips via
/// the step-3 scan. Proves the open-frame stack stays empty on the
/// legacy path and the close-stamping API is opt-in.
#[test]
fn push_compound_legacy_path_still_closes_via_finaliser() {
    let mut b = TapeBuilder::new();

    let mark = b.mark_children();
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    let root = b.push_compound(TapeKind::Seq, mark, 0, 2, 0, 0);

    let tape = b.finish().expect("finish() succeeds");
    assert_eq!(tape.len(), 3);

    // Finaliser-derived sib_skips leave SIB_SKIP_STAMPED_BIT clear.
    let cols = tape.columns();
    assert_eq!(cols.sib_skip_at(0), 1);
    assert_eq!(cols.sib_skip_at(1), 0);
    assert_eq!(cols.extra_at(0) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_eq!(cols.extra_at(1) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);

    // Cursor reads the legacy subtree identically to pre-W5.1.
    let cursor = TapeCursor::new(&tape, root);
    assert_eq!(cursor.child_count(), 2);
}

/// Two sibling open/close subtrees under one outer open/close:
///
/// - outer: open_compound ... close_compound
///   - left: open_compound + leaf + close_compound
///   - right: open_compound + leaf + close_compound
///
/// Verifies the stack tracks the innermost frame correctly across
/// nested open/close pairs and the outer's sib_skips between left
/// and right are stamped at write time.
#[test]
fn sibling_open_close_subtrees_under_outer_open() {
    let mut b = TapeBuilder::new();

    let outer = b.open_compound(TapeKind::Seq, 0, 0, 0);
    assert_eq!(outer, TapeOffset(0));

    let left = b.open_compound(TapeKind::Seq, 0, 0, 0);
    assert_eq!(left, TapeOffset(1));
    let _la = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.close_compound(left, 1);

    let right = b.open_compound(TapeKind::Seq, 1, 0, 0);
    assert_eq!(right, TapeOffset(3));
    let _ra = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    b.close_compound(right, 2);

    b.close_compound(outer, 2);

    let cols = b.columns();

    // Outer's direct children: left (row 1) and right (row 3).
    //   sib_skip[1] = 3 - 1 = 2 (stamped when right landed)
    //   sib_skip[3] = 0 (last sibling; stamped by close_compound(outer))
    assert_eq!(cols.sib_skip_at(1), 2);
    assert_eq!(cols.sib_skip_at(3), 0);
    assert_ne!(cols.extra_at(1) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_ne!(cols.extra_at(3) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);

    // Each inner compound's sole leaf child: last sibling, stamped
    // by the inner close_compound.
    assert_eq!(cols.sib_skip_at(2), 0);
    assert_ne!(cols.extra_at(2) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);
    assert_eq!(cols.sib_skip_at(4), 0);
    assert_ne!(cols.extra_at(4) & TapeRec::SIB_SKIP_STAMPED_BIT, 0);

    // Pre-order: each compound's child_off points at parent + 1.
    assert_eq!(cols.child_off_at(outer.0), TapeOffset(1));
    assert_eq!(cols.child_off_at(left.0), TapeOffset(2));
    assert_eq!(cols.child_off_at(right.0), TapeOffset(4));

    let tape = b.finish().expect("finish() succeeds");
    let cursor = TapeCursor::new(&tape, outer);
    assert_eq!(cursor.child_count(), 2);
}
