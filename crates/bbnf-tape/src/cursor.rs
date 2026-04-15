//! `TapeCursor` — the view layer's read interface into a finished [`Tape`].
//!
//! Generated view types hold a `TapeCursor` keyed at the root record
//! and walk children via the cursor's accessor methods. Every accessor
//! is a `#[inline]` offset read + bounds check.
//!
//! # Child access (Tranche AJ.0, AU.3.2)
//!
//! The tape is written in **post-order**: each compound record sits
//! AFTER all its transitive children, and its `child_off` points to
//! the first child's offset in the tape.
//!
//! Direct children are recovered by **backward walking** from the
//! parent's offset: each step lands on a direct child's record, and
//! `child_off` lets us jump past its subtree to the previous sibling.
//!
//! - [`child(i)`](TapeCursor::child) — O(K) backward walk, **zero
//!   allocation**. Two passes: count K, then walk to index.
//! - [`child_count()`](TapeCursor::child_count) — O(K) backward
//!   walk, **zero allocation**.
//! - [`children()`](TapeCursor::children) — zero-alloc backward walk
//!   (AU.3.2). Yields children in **reverse source order** (last
//!   child first); the hot-path walker uses commutative folds so
//!   order is irrelevant. `ChildIter` is the returned iterator type.
//! - [`children_forward()`](TapeCursor::children_forward) — Vec-based
//!   collection yielding children in forward source order. Used by
//!   the generated `NodeView::children()` accessor so that positional
//!   consumers (directives, seq slots, lowering / analysis) see
//!   child[0] first. Allocates one `Vec` per call.

use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

/// A pointer into a specific [`TapeRec`] in a specific [`Tape`].
///
/// Tied to `'tape` — the lifetime of the underlying tape — so the
/// borrow checker guarantees the cursor never outlives the arena it
/// points into.
#[derive(Clone, Copy, Debug)]
pub struct TapeCursor<'tape> {
    tape: &'tape Tape,
    offset: TapeOffset,
}

impl<'tape> TapeCursor<'tape> {
    /// Construct a cursor pointing at `offset` within `tape`.
    #[inline]
    pub fn new(tape: &'tape Tape, offset: TapeOffset) -> Self {
        Self { tape, offset }
    }

    /// Access the underlying tape.
    #[inline]
    pub fn tape(&self) -> &'tape Tape {
        self.tape
    }

    /// The current record's offset.
    #[inline]
    pub fn offset(&self) -> TapeOffset {
        self.offset
    }

    /// The current record.
    ///
    /// Uses unchecked indexing — safe because every `TapeCursor` is
    /// constructed from offsets that originate from `TapeBuilder`
    /// pushes into the same tape, and the tape is immutable during
    /// reads.
    #[inline]
    pub fn record(&self) -> &'tape TapeRec {
        // SAFETY: `self.offset` was produced by a `TapeBuilder::push_*`
        // call on the same tape and is never the NONE sentinel (cursors
        // are only constructed with valid offsets).
        unsafe { self.tape.get_unchecked(self.offset) }
    }

    /// Classification tag of the current record.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        self.record().kind()
    }

    /// Source span `(lo, hi)` of the current record.
    #[inline]
    pub fn span(&self) -> (u32, u32) {
        let rec = self.record();
        (rec.span_lo, rec.span_hi)
    }

    /// Variant index stored in flags (low 6 bits).
    #[inline]
    pub fn variant_idx(&self) -> u8 {
        self.record().variant_idx()
    }

    /// Meta index decoded from the packed `TapeRec::kind_meta` and
    /// `TapeRec::flags` fields.
    ///
    /// For Alt-bodied rules this is the branch index; for everything
    /// else it is `0`.
    #[inline]
    pub fn meta_idx(&self) -> u8 {
        self.record().meta_idx()
    }

    // ── Child access (Tranche AJ.0, AU.3.2) ────────────────────────

    /// Construct a cursor over the i-th direct child of the current
    /// compound record. Returns `None` if the current record is a
    /// leaf or `i` is out of range.
    ///
    /// **Zero allocation.** Uses a two-pass backward walk:
    /// 1. Count K direct children (O(K))
    /// 2. Walk to the (K−1−i)-th backward step (O(K−i))
    ///
    /// Total cost: O(K). For the hot-path `child(0)` on Alt (K=1)
    /// and Seq (K=2–5) nodes, this is effectively O(1).
    #[inline]
    pub fn child(self, i: usize) -> Option<TapeCursor<'tape>> {
        let rec = self.record();
        if !rec.has_children() || rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        let end = self.offset.0 as usize;
        if start >= end {
            return None;
        }

        // Pass 1: count direct children.
        let count = count_backward(self.tape, start, end);
        if i >= count {
            return None;
        }

        // Pass 2: walk backward to the target.
        // Backward walk yields child[K-1] first, so to reach
        // child[i] we walk (K-1-i) steps.
        nth_backward(self.tape, start, end, count - 1 - i)
    }

    /// Number of direct children of the current compound record.
    ///
    /// **Zero allocation.** O(K) backward walk.
    #[inline]
    pub fn child_count(self) -> usize {
        let rec = self.record();
        if !rec.has_children() || rec.child_off.is_none() {
            return 0;
        }
        let start = rec.child_off.0 as usize;
        let end = self.offset.0 as usize;
        if start >= end {
            return 0;
        }
        count_backward(self.tape, start, end)
    }

    /// Iterate every direct child of the current compound record,
    /// in emission (source) order.
    ///
    /// Collects via a backward walk then reverses so the iterator
    /// yields children in forward order. One `Vec` allocation per
    /// call. Hot-path walkers that fold commutatively over children
    /// should use [`children_zero_alloc`](Self::children_zero_alloc)
    /// (AU.3.2) instead — it walks the tape backward without any
    /// heap allocation.
    pub fn children(self) -> impl Iterator<Item = TapeCursor<'tape>> + 'tape {
        let tape = self.tape;
        let rec = self.record();
        let parent_offset = self.offset.0 as usize;
        let mut out: Vec<TapeCursor<'tape>> = Vec::new();
        if rec.has_children() && !rec.child_off.is_none() {
            let start = rec.child_off.0 as usize;
            if start >= parent_offset {
                return out.into_iter();
            }
            let count = count_backward(tape, start, parent_offset);
            out.reserve_exact(count);
            let mut pos = parent_offset;
            while pos > start {
                let co = pos - 1;
                // SAFETY: `co` is in range [start, parent_offset), both
                // valid tape offsets from `child_off` / parent offset.
                let child_rec = unsafe { tape.get_unchecked(TapeOffset(co as u32)) };
                out.push(TapeCursor::new(tape, TapeOffset(co as u32)));
                pos = backward_step(child_rec, co);
            }
            out.reverse();
        }
        out.into_iter()
    }

    /// AU.3.2: zero-alloc iterator over direct children, yielding in
    /// **reverse source order** (last child first).
    ///
    /// Backward walk over the post-order tape: each [`Iterator::next`]
    /// call reads one record and follows its `child_off` link (or
    /// steps back by one for leaves) to locate the previous sibling's
    /// root. Total traversal cost is O(K) for K direct children; no
    /// heap allocation occurs.
    ///
    /// The reverse-order yield is intentional: the backward walk is
    /// the only O(K) direction over the current tape layout (the
    /// forward direction requires subtree-size information that the
    /// AoS substrate does not carry). Callers that need source-order
    /// iteration use [`children`](Self::children), which collects
    /// the reverse walk into a `Vec` and reverses.
    ///
    /// # When to use which
    ///
    /// | Use case | Method | Allocation |
    /// |----------|--------|------------|
    /// | Commutative fold (sum, count, validate) | `children_zero_alloc()` | zero |
    /// | Positional binding (slot[0], slot[1]) | `children()` | one `Vec` |
    /// | Indexed access (`child(i)`) | `child(i)` | zero |
    #[inline]
    pub fn children_zero_alloc(self) -> ChildIter<'tape> {
        let rec = self.record();
        if !rec.has_children() || rec.child_off.is_none() {
            return ChildIter {
                tape: self.tape,
                next: TapeOffset(0),
                end: TapeOffset(0),
            };
        }
        let start = rec.child_off.0;
        let end = self.offset.0;
        if start >= end {
            return ChildIter {
                tape: self.tape,
                next: TapeOffset(0),
                end: TapeOffset(0),
            };
        }
        ChildIter {
            tape: self.tape,
            next: TapeOffset(end),
            end: TapeOffset(start),
        }
    }
}

/// Zero-alloc backward iterator over a compound's direct children.
///
/// Yields [`TapeCursor`] items in **reverse source order** (last
/// child first). Each step follows one `child_off` link in the
/// post-order tape; `size_of::<ChildIter>` is 24 bytes (two
/// [`TapeOffset`] + one `&Tape`). No heap allocation occurs.
///
/// Returned by [`TapeCursor::children`]. See that method's
/// documentation for when forward order is required instead.
#[derive(Clone, Copy, Debug)]
pub struct ChildIter<'tape> {
    tape: &'tape Tape,
    /// Exclusive upper bound on the unconsumed portion of the
    /// children range. Each `next()` call yields the child whose
    /// root is at `next - 1`, then resets `next` to that child's
    /// subtree start (`backward_step` — `child_off` for compounds,
    /// `next - 1` for leaves).
    next: TapeOffset,
    /// Inclusive lower bound: the parent's `child_off`. When
    /// `next <= end`, iteration is complete.
    end: TapeOffset,
}

// (size assertion handled at runtime in tests/tape_walker_allocs.rs)

impl<'tape> Iterator for ChildIter<'tape> {
    type Item = TapeCursor<'tape>;

    #[inline]
    fn next(&mut self) -> Option<TapeCursor<'tape>> {
        if self.next.0 <= self.end.0 {
            return None;
        }
        let co = self.next.0 - 1;
        // SAFETY: `co` is in `[self.end.0, self.next.0)`, which by
        // construction from `TapeCursor::children` is the parent's
        // [child_off, parent_offset) range — all valid offsets in
        // the shared tape. The post-order `backward_step` can only
        // decrease `next`, so it stays within the range.
        let rec = unsafe { self.tape.get_unchecked(TapeOffset(co)) };
        let step = backward_step_u32(rec, co);
        self.next = TapeOffset(step);
        Some(TapeCursor::new(self.tape, TapeOffset(co)))
    }
}

// ── Backward-walk helpers ──────────────────────────────────────────

/// One backward step in the post-order child walk, `u32` variant.
///
/// For a compound child at `offset` with `child_off = C`, jumps to
/// `C` (the start of its subtree — the previous sibling's record
/// sits at `C - 1`). For a leaf, jumps to `offset` (the previous
/// record is at `offset - 1`).
#[inline]
fn backward_step_u32(rec: &TapeRec, offset: u32) -> u32 {
    if rec.has_children() && !rec.child_off.is_none() {
        rec.child_off.0
    } else {
        offset
    }
}

/// One backward step in the post-order child walk, `usize` variant
/// (used by [`count_backward`] and [`nth_backward`]).
#[inline]
fn backward_step(rec: &TapeRec, offset: usize) -> usize {
    if rec.has_children() && !rec.child_off.is_none() {
        rec.child_off.0 as usize
    } else {
        offset
    }
}

/// Count direct children via backward walk from `end` to `start`.
#[inline]
fn count_backward(tape: &Tape, start: usize, end: usize) -> usize {
    let mut count = 0usize;
    let mut pos = end;
    while pos > start {
        let co = pos - 1;
        // SAFETY: `co` is in range [start, end), both of which are
        // valid tape offsets from `child_off` / parent offset.
        let rec = unsafe { tape.get_unchecked(TapeOffset(co as u32)) };
        count += 1;
        pos = backward_step(rec, co);
    }
    count
}

/// Walk backward `n` steps from `end` and return the cursor at
/// that position. Step 0 = the last child (at `end - 1`).
#[inline]
fn nth_backward<'tape>(
    tape: &'tape Tape,
    start: usize,
    end: usize,
    n: usize,
) -> Option<TapeCursor<'tape>> {
    let mut pos = end;
    let mut step = 0usize;
    while pos > start {
        let co = pos - 1;
        // SAFETY: `co` is in range [start, end), both valid tape offsets.
        let rec = unsafe { tape.get_unchecked(TapeOffset(co as u32)) };
        if step == n {
            return Some(TapeCursor::new(tape, TapeOffset(co as u32)));
        }
        step += 1;
        pos = backward_step(rec, co);
    }
    None
}
