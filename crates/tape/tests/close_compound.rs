//! AY-II.W0.a/W0-fix — `TapeBuilder::begin_compound` / `end_compound`
//! / `end_compound_post_order` contract.
//!
//! 1. `begin_compound(kind, span_lo, variant_idx, meta_idx,
//!    frame_depth, extra_flags)` emits a compound row with
//!    provisional `span_hi == span_lo`, `child_off = NONE`, and the
//!    `HAS_CHILDREN_BIT` cleared. `variant_idx` lands in the `flags`
//!    byte (rule discriminant, 8-bit); `meta_idx` splits across the
//!    high 4 bits of `kind_meta` and the `extra` `META_IDX_HI_BIT`.
//!    No sibling-skip column write.
//! 2. `end_compound` (pre-order) back-patches `span_hi`
//!    unconditionally; when at least one child landed
//!    (`open_offset + 1 < columns.len()`) it also stamps
//!    `child_off = open_offset + 1` (pre-order fast path) and
//!    `HAS_CHILDREN_BIT`. Still no sibling-skip write.
//! 3. `end_compound_post_order(open_offset, span_hi, first_child)`
//!    back-patches `span_hi`; when `first_child.0 < open_offset`
//!    (children actually landed before the compound row) writes
//!    `child_off = first_child` backward and stamps
//!    `HAS_CHILDREN_BIT`; otherwise leaves both cleared. Used by
//!    walker-parity shape emitters whose compound row lands AFTER
//!    the children subtree.
//! 4. `TapeBuilder::rollback_to(open_offset)` discards every row
//!    pushed at or after `open_offset` and a subsequent
//!    `begin_compound` reuses that offset cleanly.
//! 5. The finaliser derives `sib_skip` unconditionally on every
//!    tape whose inline `frame_depth` stream is populated in
//!    lockstep with the columns (or, for legacy post-order callers
//!    relying on `derive_frame_depth`, stamps `sib_skip` by walking
//!    the `child_off` graph).

use tape::{Tape, TapeBuilder, TapeCursor, TapeKind, TapeOffset};

/// Build `(root (inner a b) c)` via `begin_compound` / `end_compound`
/// with the inline-frame-depth path active:
///
/// - row 0: root compound        (depth 0)
/// - row 1: inner compound       (depth 1)
/// - row 2: leaf a               (depth 2)
/// - row 3: leaf b               (depth 2)
/// - row 4: leaf c               (depth 1)
#[test]
fn nested_begin_end_produces_pre_order_tape() {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();

    // New 6-arg signature: (kind, span_lo, variant_idx, meta_idx,
    // frame_depth, extra_flags). Tests use 0 for variant/meta/extra_flags
    // and populate frame_depth inline as before.
    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    assert_eq!(root, 0);

    let inner = b.begin_compound(TapeKind::Seq, 0, 0, 0, 1, 0);
    assert_eq!(inner, 1);

    // Leaf pushes don't auto-stamp depth; callers that use the
    // begin/end API either route through `driver::emit_leaf*` (which
    // takes `frame_depth` explicitly) or stamp depth inline as the
    // tests below do.
    b.frame_depth_mut().push(2);
    let a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    assert_eq!(a, TapeOffset(2));
    b.frame_depth_mut().push(2);
    let bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    assert_eq!(bl, TapeOffset(3));

    b.end_compound(inner, 2);

    b.frame_depth_mut().push(1);
    let c = b.push_leaf(TapeKind::Literal, 2, 3, 0, 0);
    assert_eq!(c, TapeOffset(4));

    b.end_compound(root, 3);

    // Immediately after end_compound (before finalise): parent rows
    // carry the back-patched span_hi + child_off + HAS_CHILDREN_BIT.
    let cols = b.columns();
    assert_eq!(cols.span_lo_at(root), 0);
    assert_eq!(cols.span_hi_at(root), 3);
    assert_eq!(cols.span_lo_at(inner), 0);
    assert_eq!(cols.span_hi_at(inner), 2);
    assert_eq!(cols.child_off_at(root), TapeOffset(1));
    assert_eq!(cols.child_off_at(inner), TapeOffset(2));
    assert!(cols.has_children_at(root));
    assert!(cols.has_children_at(inner));

    // begin_compound / end_compound MUST NOT touch sib_skip — every
    // slot still reads the default 0 until finalise runs.
    for i in 0..cols.len() as u32 {
        assert_eq!(
            cols.sib_skip_at(i),
            0,
            "row {i} sib_skip must be 0 pre-finalise; AY-II.W0.a delegates sib_skip solely to the finaliser",
        );
    }

    // finish() runs the finaliser; every `sib_skip` slot is derived
    // unconditionally from the inline frame_depth stream.
    let tape: Tape = b.finish().expect("finish() should succeed");
    assert_eq!(tape.len(), 5);
    let cols = tape.columns();

    // root's direct children at depth 1: inner (row 1), c (row 4).
    //   sib_skip[1] = 4 - 1 = 3; sib_skip[4] = 0 (last sibling).
    assert_eq!(cols.sib_skip_at(1), 3);
    assert_eq!(cols.sib_skip_at(4), 0);

    // inner's direct children at depth 2: a (row 2), b (row 3).
    //   sib_skip[2] = 1; sib_skip[3] = 0 (last sibling).
    assert_eq!(cols.sib_skip_at(2), 1);
    assert_eq!(cols.sib_skip_at(3), 0);

    // root's own sib_skip stays at 0 (no outer frame at depth 0).
    assert_eq!(cols.sib_skip_at(0), 0);

    // Pre-order cursor walk descends via the write-time child_off
    // fast path (child_off == parent + 1 at every compound).
    let cursor = TapeCursor::new(&tape, TapeOffset(root));
    assert_eq!(cursor.kind(), TapeKind::Seq);
    assert_eq!(cursor.span(), (0, 3));
    assert_eq!(cursor.child_count(), 2);

    let inner_cursor = cursor.child(0).unwrap();
    assert_eq!(inner_cursor.offset(), TapeOffset(inner));
    assert_eq!(inner_cursor.child_count(), 2);

    let c_cursor = cursor.child(1).unwrap();
    assert_eq!(c_cursor.offset(), c);
    assert_eq!(c_cursor.span(), (2, 3));
}

/// AY-II.W0-fix — post-order compound emission via
/// `end_compound_post_order`.
///
/// The Flat/Wrap/Inline/Pratt/ArgList/Unordered/AltDispatch shape
/// emitters emit their outer compound POST children (walker-tape
/// parity — the compound row lands AFTER its children in emission
/// order, with `child_off` pointing backward to the first child's
/// root).
///
/// Pre-W0-fix the emission triplet was `begin_compound(row lands
/// after children)` + `end_compound` + `set_child_off_at` (manual
/// backward override). `end_compound`'s child-detection heuristic
/// (`open_offset + 1 < columns.len()`) is false for post-order
/// emission, so `HAS_CHILDREN_BIT` stayed cleared and readers saw
/// `has_children == false` on a compound with children — every
/// traversal collapsed to a leaf.
///
/// Post-W0-fix, `end_compound_post_order(open_off, span_hi,
/// first_child)` atomically stamps span_hi + child_off (backward) +
/// `HAS_CHILDREN_BIT` in one call. The override path is retired.
#[test]
fn end_compound_post_order_stamps_backward_child_off_and_has_children() {
    let mut b = TapeBuilder::new();
    // Post-order emission does not opt into inline frame-depth
    // (the shape emitters don't call `enable_inline_frame_depth`),
    // so `finish()` reconstructs frame_depth via `derive_frame_depth`
    // from the `child_off` graph.

    // Simulate Flat emission: capture first-child index PRE children,
    // emit children (each child uses its own begin/end in pre-order),
    // THEN open the compound row and close via end_compound_post_order.
    let outer_child = b.columns().len() as u32; // == 0

    // Child 1: `push_compound` (legacy post-order leaf-walker style).
    let inner_mark = b.mark_children();
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _inner = b.push_compound(TapeKind::Seq, inner_mark, 0, 1, 1, 0);

    // Child 2: a leaf.
    let _c = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);

    // Post-order outer compound: allocated AFTER children at current
    // columns.len(). begin + end_compound_post_order mirrors the
    // emitter pattern.
    let span_lo = 0;
    let span_hi = 2;
    let outer_off = b.begin_compound(TapeKind::Seq, span_lo, 7, 3, 0, 0);
    assert_eq!(
        outer_off, 3,
        "post-order begin_compound lands after children at cols.len()"
    );
    b.end_compound_post_order(outer_off, span_hi, TapeOffset(outer_child));

    let cols = b.columns();
    assert_eq!(cols.child_off_at(outer_off), TapeOffset(outer_child));
    assert!(
        cols.has_children_at(outer_off),
        "post-order compound must advertise HAS_CHILDREN so readers walk the backward child chain; got child_off = {:?} but has_children = false",
        cols.child_off_at(outer_off),
    );
    assert_eq!(cols.span_hi_at(outer_off), span_hi);
    // variant_idx and meta_idx propagate to walker-parity positions.
    assert_eq!(
        cols.materialize(outer_off).variant_idx(),
        7,
        "variant_idx must land in flags byte for rule_kind() dispatch"
    );
    assert_eq!(cols.materialize(outer_off).meta_idx(), 3);

    let tape = b.finish().expect("finish() succeeds");
    // Cursor walks the post-order tape and surfaces inner + leaf
    // as direct children.
    let cursor = TapeCursor::new(&tape, TapeOffset(outer_off));
    assert_eq!(
        cursor.child_count(),
        2,
        "post-order cursor walk must yield 2 children; got {}",
        cursor.child_count(),
    );
}

/// `end_compound_post_order` with `first_child == open_offset` (no
/// children landed before the begin row) keeps `child_off` at NONE
/// and `HAS_CHILDREN_BIT` clear.
#[test]
fn end_compound_post_order_empty_frame() {
    let mut b = TapeBuilder::new();

    // No children pushed before begin; first_child captures cols.len()
    // which equals the open offset.
    let first_child = b.columns().len() as u32;
    let outer_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    assert_eq!(first_child, outer_off);
    b.end_compound_post_order(outer_off, 0, TapeOffset(first_child));

    let cols = b.columns();
    assert_eq!(cols.child_off_at(outer_off), TapeOffset::NONE);
    assert!(!cols.has_children_at(outer_off));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);
}

/// A compound that closes with zero children keeps `child_off` at
/// `NONE` and `HAS_CHILDREN_BIT` clear. `end_compound` back-patches
/// `span_hi` regardless.
#[test]
fn end_compound_without_children() {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();
    let root = b.begin_compound(TapeKind::Seq, 5, 0, 0, 0, 0);
    b.end_compound(root, 5);

    let cols = b.columns();
    assert_eq!(cols.span_lo_at(root), 5);
    assert_eq!(cols.span_hi_at(root), 5);
    assert_eq!(cols.child_off_at(root), TapeOffset::NONE);
    assert!(!cols.has_children_at(root));

    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 1);
}

/// `TapeBuilder::rollback_to(open_offset)` unwinds every row pushed
/// at or after `open_offset`, restoring the builder to its pre-
/// `begin_compound` state. A subsequent `begin_compound` reuses the
/// same offset. The inline `frame_depth` stream rewinds in lockstep.
#[test]
fn rollback_to_unwinds_begin_compound_cleanly() {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();

    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    assert_eq!(root, 0);
    assert_eq!(b.columns().len(), 1);

    // First attempt: open an inner compound, push some children,
    // then roll back. Simulates an emitter retry-IIFE discarding a
    // failed alt branch.
    let attempt_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 1, 0);
    assert_eq!(attempt_off, 1);
    b.frame_depth_mut().push(2);
    let _l0 = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.frame_depth_mut().push(2);
    let _l1 = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    assert_eq!(b.columns().len(), 4);

    b.rollback_to(attempt_off);
    assert_eq!(
        b.columns().len(),
        attempt_off as usize,
        "rollback_to discards every row at-or-after open_offset",
    );
    assert_eq!(b.columns().len(), 1);

    // Second attempt: reuse the same offset for a different subtree.
    let retry_off = b.begin_compound(TapeKind::Alt, 0, 0, 0, 1, 0);
    assert_eq!(
        retry_off, attempt_off,
        "begin_compound reuses the rolled-back offset",
    );
    b.frame_depth_mut().push(2);
    let _l2 = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.end_compound(retry_off, 1);
    b.end_compound(root, 1);

    // The retry produced a valid tape (3 rows: root, retry, leaf).
    let tape = b.finish().unwrap();
    assert_eq!(tape.len(), 3);
    let cursor = TapeCursor::new(&tape, TapeOffset(root));
    assert_eq!(cursor.child_count(), 1);
    let inner = cursor.child(0).unwrap();
    assert_eq!(inner.kind(), TapeKind::Alt);
    assert_eq!(inner.child_count(), 1);
}

/// `rollback_to` is idempotent: calling with an offset beyond the
/// current length is a no-op; calling twice with the same offset is
/// a no-op on the second call.
#[test]
fn rollback_to_idempotent() {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();
    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    b.frame_depth_mut().push(1);
    let _leaf = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let len_before = b.columns().len();

    // Rollback to a future offset is a no-op.
    b.rollback_to(u32::MAX);
    assert_eq!(b.columns().len(), len_before);

    // Rollback to the current end is a no-op (no rows at or after
    // `len()`).
    b.rollback_to(len_before as u32);
    assert_eq!(b.columns().len(), len_before);

    // Rollback to `root + 1` discards the leaf only.
    b.rollback_to(root + 1);
    assert_eq!(b.columns().len(), 1);

    // Second rollback to the same offset is a no-op.
    b.rollback_to(root + 1);
    assert_eq!(b.columns().len(), 1);
}

/// The legacy `mark_children` + `push_compound` path continues to
/// work unchanged — post-order tapes, finaliser derives `sib_skip`
/// via `derive_frame_depth` over the `child_off` graph.
#[test]
fn legacy_push_compound_path_still_closes_via_finaliser() {
    let mut b = TapeBuilder::new();

    let mark = b.mark_children();
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    let root = b.push_compound(TapeKind::Seq, mark, 0, 2, 0, 0);

    let tape = b.finish().expect("finish() succeeds");
    assert_eq!(tape.len(), 3);

    let cols = tape.columns();
    // Finaliser-derived sib_skips on the two children.
    assert_eq!(cols.sib_skip_at(0), 1);
    assert_eq!(cols.sib_skip_at(1), 0);

    // Cursor reads the legacy subtree identically to pre-W5.1.
    let cursor = TapeCursor::new(&tape, root);
    assert_eq!(cursor.child_count(), 2);
}

/// Two sibling begin/end subtrees under one outer begin/end: the
/// outer's child count is 2 and each inner's child count is 1. The
/// finaliser derives `sib_skip` across all three compound frames.
#[test]
fn sibling_begin_end_subtrees_under_outer_begin() {
    let mut b = TapeBuilder::new();
    b.enable_inline_frame_depth();

    let outer = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0, 0);
    assert_eq!(outer, 0);

    let left = b.begin_compound(TapeKind::Seq, 0, 0, 0, 1, 0);
    assert_eq!(left, 1);
    b.frame_depth_mut().push(2);
    let _la = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.end_compound(left, 1);

    let right = b.begin_compound(TapeKind::Seq, 1, 0, 0, 1, 0);
    assert_eq!(right, 3);
    b.frame_depth_mut().push(2);
    let _ra = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    b.end_compound(right, 2);

    b.end_compound(outer, 2);

    let cols = b.columns();

    // Pre-order child_off: each compound points at parent + 1.
    assert_eq!(cols.child_off_at(outer), TapeOffset(1));
    assert_eq!(cols.child_off_at(left), TapeOffset(2));
    assert_eq!(cols.child_off_at(right), TapeOffset(4));

    let tape = b.finish().expect("finish() succeeds");
    let cols = tape.columns();

    // Outer's direct children at depth 1: left (row 1), right (row 3).
    //   sib_skip[1] = 3 - 1 = 2; sib_skip[3] = 0 (last sibling).
    assert_eq!(cols.sib_skip_at(1), 2);
    assert_eq!(cols.sib_skip_at(3), 0);

    // Each inner's sole leaf at depth 2: last sibling → 0.
    assert_eq!(cols.sib_skip_at(2), 0);
    assert_eq!(cols.sib_skip_at(4), 0);

    let cursor = TapeCursor::new(&tape, TapeOffset(outer));
    assert_eq!(cursor.child_count(), 2);
}
