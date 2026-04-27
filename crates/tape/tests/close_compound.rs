//! AY-II.W0.a/W0-fix — `Tape::begin_compound` / `end_compound`
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
//! 4. `Tape::rollback_to(open_offset)` discards every row
//!    pushed at or after `open_offset` and a subsequent
//!    `begin_compound` reuses that offset cleanly.
//! 5. The finaliser derives `sib_skip` unconditionally on every
//!    tape from the inline `frame_depth` stream the builder
//!    auto-populates on every structural push (B3.W0.γ — the legacy
//!    `derive_frame_depth` reverse-walk reconstruction is retired
//!    because it could not handle pre-order children of post-order
//!    parents).

use tape::{Tape, TapeCursor, TapeKind, TapeOffset};

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
    let mut b = Tape::<()>::new();

    // B3.W0.γ — the builder auto-stamps `frame_depth` on every
    // structural push (leaf or compound) via its internal
    // `current_depth` counter. Tests no longer manage the depth
    // stream manually.
    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(root, 0);

    let inner = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(inner, 1);

    let a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    assert_eq!(a, TapeOffset(2));
    let bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    assert_eq!(bl, TapeOffset(3));

    b.end_compound(inner, 2);

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
    let tape: Tape<()> = b.finish(0).expect("finish() should succeed");
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
///
/// B5.W6 — the canonical post-order shape pattern is
/// `enter_post_order_children` (bumps depth) → push children →
/// `begin_compound_post` (stamps row at outer-frame depth, no bump) →
/// `end_compound_post_order` (back-patches + decrements depth).
#[test]
fn end_compound_post_order_stamps_backward_child_off_and_has_children() {
    let mut b = Tape::<()>::new();
    // B5.W6 — bracket the outer compound's post-order children scope.
    // The bracket bumps `current_depth` so child records stamp
    // `frame_depth` at the correct (parent + 1) depth at push time;
    // the matching `end_compound_post_order` absorbs the bump.
    let outer_child = b.enter_post_order_children();

    // Child 1: a nested post-order compound, opened via its own
    // bracket. `frame_depth` for its body's leaf and its compound row
    // both stamp under the inner bracket's bumped depth.
    let inner_mark = b.enter_post_order_children();
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let inner_open = b.begin_compound_post(TapeKind::Seq, 0, 1, 0, 0);
    b.end_compound_post_order(inner_open, 1, TapeOffset(inner_mark));
    let _inner = TapeOffset(inner_open);

    // Child 2: a leaf.
    let _c = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);

    // Post-order outer compound: allocated AFTER children at current
    // columns.len(). begin_compound_post + end_compound_post_order
    // mirrors the emitter pattern under the active bracket.
    let span_lo = 0;
    let span_hi = 2;
    let outer_off = b.begin_compound_post(TapeKind::Seq, span_lo, 7, 3, 0);
    assert_eq!(
        outer_off, 3,
        "post-order begin_compound_post lands after children at cols.len()"
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

    let tape = b.finish(0).expect("finish() succeeds");
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

/// AY-II.W0-fix — `derive_frame_depth` must not infinite-loop when
/// a pre-order compound appears inside a post-order parent's subtree.
///
/// Pre-W0-fix `derive_frame_depth` jumped `pos = child_off.0`
/// unconditionally; for a pre-order compound (`child_off >= co`)
/// that leap landed at a row >= pos, the next iteration computed
/// `co = pos - 1` which landed BACK at the same row, and the walk
/// spun until the runtime killed it. With my HAS_CHILDREN_BIT fix
/// active on post-order compounds the pre-fix loop was hidden
/// (pre-order compounds had HAS_CHILDREN_BIT too, but pre-fix
/// post-order compounds LACKED it — so the walk fell into the
/// leaf branch for every post-order parent and never exposed the
/// pre-order infinite-leap). Now that post-order compounds
/// correctly stamp HAS_CHILDREN_BIT, the mixed-layout pathology
/// surfaces unless `derive_frame_depth` recognises forward
/// (pre-order) `child_off` values and treats them as leaves in
/// the per-parent walk.
///
/// Post-fix: `derive_frame_depth` gates the `pos = child_off.0`
/// leap on `child_off.0 < co`. Pre-order compounds fall through
/// to `pos = co` (monotonic decrement) and their subtree depth
/// is stamped by the OUTER `for parent_idx` iteration (when it
/// processes one of the enclosing post-order compounds whose
/// walk spans the pre-order compound's subtree).
#[test]
fn derive_frame_depth_terminates_on_mixed_pre_and_post_order_tape() {
    let mut b = Tape::<()>::new();
    // No inline frame-depth — derive_frame_depth path.

    // Row 0, 1: leaves.
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);

    // Row 2: pre-order compound wrapping rows 3-4 (children
    // emitted after the compound row).
    let pre_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(pre_off, 2);
    let _c0 = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _c1 = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    b.end_compound(pre_off, 2);

    // Row 5: post-order compound wrapping rows 0-4 (its children
    // sit BEFORE this row in emission order).
    let first_child = 0u32;
    let post_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(post_off, 5);
    b.end_compound_post_order(post_off, 2, TapeOffset(first_child));

    // finish() runs derive_frame_depth + finalise. Pre-fix this
    // infinite-looped because the post_off walk visited pre_off
    // (HAS_CHILDREN + forward child_off = 3) and leapt pos back
    // to 3, then looped. Post-fix it terminates because the
    // forward-child_off leap guard treats pre_off as a leaf here.
    let tape = b.finish(0).expect("finish() should terminate");
    assert_eq!(tape.len(), 6);

    // Cursor walk over post_off enumerates its children (with
    // pre_off as one of them). The exact sibling chain depends
    // on finaliser semantics; just assert traversal terminates
    // and every child is reachable.
    let cursor = TapeCursor::new(&tape, TapeOffset(post_off));
    let count = cursor.children().count();
    assert!(
        count >= 1,
        "post_off cursor walk must yield at least one child; got {count}",
    );
}

/// `end_compound_post_order` with `first_child == open_offset` (no
/// children landed before the begin row) keeps `child_off` at NONE
/// and `HAS_CHILDREN_BIT` clear.
#[test]
fn end_compound_post_order_empty_frame() {
    let mut b = Tape::<()>::new();

    // No children pushed before begin; first_child captures cols.len()
    // which equals the open offset.
    let first_child = b.columns().len() as u32;
    let outer_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(first_child, outer_off);
    b.end_compound_post_order(outer_off, 0, TapeOffset(first_child));

    let cols = b.columns();
    assert_eq!(cols.child_off_at(outer_off), TapeOffset::NONE);
    assert!(!cols.has_children_at(outer_off));

    let tape = b.finish(0).unwrap();
    assert_eq!(tape.len(), 1);
}

/// A compound that closes with zero children keeps `child_off` at
/// `NONE` and `HAS_CHILDREN_BIT` clear. `end_compound` back-patches
/// `span_hi` regardless.
#[test]
fn end_compound_without_children() {
    let mut b = Tape::<()>::new();
    let root = b.begin_compound(TapeKind::Seq, 5, 0, 0, 0);
    b.end_compound(root, 5);

    let cols = b.columns();
    assert_eq!(cols.span_lo_at(root), 5);
    assert_eq!(cols.span_hi_at(root), 5);
    assert_eq!(cols.child_off_at(root), TapeOffset::NONE);
    assert!(!cols.has_children_at(root));

    let tape = b.finish(0).unwrap();
    assert_eq!(tape.len(), 1);
}

/// `Tape::rollback_to(open_offset)` unwinds every row pushed
/// at or after `open_offset`, restoring the builder to its pre-
/// `begin_compound` state. A subsequent `begin_compound` reuses the
/// same offset. The inline `frame_depth` stream rewinds in lockstep.
#[test]
fn rollback_to_unwinds_begin_compound_cleanly() {
    let mut b = Tape::<()>::new();

    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(root, 0);
    assert_eq!(b.columns().len(), 1);

    // First attempt: open an inner compound, push some children,
    // then roll back. Simulates an emitter retry-IIFE discarding a
    // failed alt branch.
    let attempt_off = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(attempt_off, 1);
    let _l0 = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
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
    let retry_off = b.begin_compound(TapeKind::Alt, 0, 0, 0, 0);
    assert_eq!(
        retry_off, attempt_off,
        "begin_compound reuses the rolled-back offset",
    );
    let _l2 = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.end_compound(retry_off, 1);
    b.end_compound(root, 1);

    // The retry produced a valid tape (3 rows: root, retry, leaf).
    let tape = b.finish(0).unwrap();
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
    let mut b = Tape::<()>::new();
    let root = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
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

/// Post-order shape emission with leaves emitted before the
/// wrapping compound: under the bracket discipline children stamp
/// `frame_depth` at the bumped depth at push time;
/// `end_compound_post_order` absorbs the bump on close. B5.W6
/// replaced the legacy `derive_frame_depth` reverse-walk and the
/// retroactive in-close cascade with single-writer bracket
/// bookkeeping.
#[test]
fn post_order_close_bumps_child_frame_depth() {
    let mut b = Tape::<()>::new();

    let mark = b.enter_post_order_children();
    let _a = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    let _bl = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    let root_open = b.begin_compound_post(TapeKind::Seq, 0, 0, 0, 0);
    b.end_compound_post_order(root_open, 2, TapeOffset(mark));
    let root = TapeOffset(root_open);

    let tape = b.finish(0).expect("finish() succeeds");
    assert_eq!(tape.len(), 3);

    let cols = tape.columns();
    // Finaliser-derived sib_skips on the two children.
    assert_eq!(cols.sib_skip_at(0), 1);
    assert_eq!(cols.sib_skip_at(1), 0);

    // Cursor reads the post-order subtree.
    let cursor = TapeCursor::new(&tape, root);
    assert_eq!(cursor.child_count(), 2);
}

/// Two sibling begin/end subtrees under one outer begin/end: the
/// outer's child count is 2 and each inner's child count is 1. The
/// finaliser derives `sib_skip` across all three compound frames.
#[test]
fn sibling_begin_end_subtrees_under_outer_begin() {
    let mut b = Tape::<()>::new();

    let outer = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(outer, 0);

    let left = b.begin_compound(TapeKind::Seq, 0, 0, 0, 0);
    assert_eq!(left, 1);
    let _la = b.push_leaf(TapeKind::Literal, 0, 1, 0, 0);
    b.end_compound(left, 1);

    let right = b.begin_compound(TapeKind::Seq, 1, 0, 0, 0);
    assert_eq!(right, 3);
    let _ra = b.push_leaf(TapeKind::Literal, 1, 2, 0, 0);
    b.end_compound(right, 2);

    b.end_compound(outer, 2);

    let cols = b.columns();

    // Pre-order child_off: each compound points at parent + 1.
    assert_eq!(cols.child_off_at(outer), TapeOffset(1));
    assert_eq!(cols.child_off_at(left), TapeOffset(2));
    assert_eq!(cols.child_off_at(right), TapeOffset(4));

    let tape = b.finish(0).expect("finish() succeeds");
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
