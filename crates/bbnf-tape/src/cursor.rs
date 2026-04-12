//! `TapeCursor` — the view layer's read interface into a finished [`Tape`].
//!
//! Generated view types hold a `TapeCursor` keyed at the root record
//! and walk children via the cursor's accessor methods. Every accessor
//! is a `#[inline]` offset read + bounds check.
//!
//! # Child access (Tranche AJ.0)
//!
//! The tape is written in **post-order**: each compound record sits
//! AFTER all its transitive children, and its `child_off` points to
//! the first child's offset in the tape.
//!
//! Direct children are recovered by **backward walking** from the
//! parent's offset: each step lands on a direct child's compound
//! record, and `child_off` lets us jump past its subtree to the
//! previous sibling.
//!
//! - [`child(i)`](TapeCursor::child) — O(K) backward walk, **zero
//!   allocation**. Two passes: count K, then walk to index.
//! - [`child_count()`](TapeCursor::child_count) — O(K) backward
//!   walk, **zero allocation**.
//! - [`children()`](TapeCursor::children) — collects via backward
//!   walk, yields in forward (source) order. One `Vec` allocation
//!   per call.

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

    /// The current record (panics if `offset` is the sentinel).
    #[inline]
    pub fn record(&self) -> &'tape TapeRec {
        self.tape.get(self.offset)
    }

    /// Classification tag of the current record.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        self.record().kind
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

    // ── Child access (Tranche AJ.0) ────────────────────────────────

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
    /// call. Callers that only need a count or indexed access should
    /// prefer [`child_count`](Self::child_count) /
    /// [`child`](Self::child) which are zero-allocation.
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
                let Some(child_rec) = tape.try_get(TapeOffset(co as u32))
                else {
                    break;
                };
                out.push(TapeCursor::new(tape, TapeOffset(co as u32)));
                pos = backward_step(child_rec, co);
            }
            out.reverse();
        }
        out.into_iter()
    }
}

// ── Backward-walk helpers ──────────────────────────────────────────

/// One backward step in the post-order child walk.
///
/// For a compound child at `offset` with `child_off = C`, jumps to
/// `C` (the start of its subtree — the previous sibling's record
/// sits at `C - 1`). For a leaf, jumps to `offset` (the previous
/// record is at `offset - 1`).
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
        match tape.try_get(TapeOffset(co as u32)) {
            Some(rec) => {
                count += 1;
                pos = backward_step(rec, co);
            }
            None => break,
        }
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
        let rec = tape.try_get(TapeOffset(co as u32))?;
        if step == n {
            return Some(TapeCursor::new(tape, TapeOffset(co as u32)));
        }
        step += 1;
        pos = backward_step(rec, co);
    }
    None
}
