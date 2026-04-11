//! `TapeCursor` — the view layer's read interface into a finished [`Tape`].
//!
//! Generated view types hold a `TapeCursor` keyed at the root record
//! and walk children via the cursor's accessor methods. Every accessor
//! is a `#[inline]` offset read + bounds check.

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

    /// Construct a cursor over the i-th direct child of the current
    /// compound record. Returns `None` if the current record is a
    /// leaf or `i` is out of range.
    ///
    /// Forwards to `children()` — under the post-order tape layout
    /// there is no O(1) short-cut because direct children's own
    /// offsets are scattered between grandchildren's subtrees, so
    /// we walk the iterator and `nth(i)`. In practice view
    /// accessors call `child(0)`, `child(1)`, … for a small
    /// compile-time constant number of fields so the linear walk is
    /// cheap.
    #[inline]
    pub fn child(self, i: usize) -> Option<TapeCursor<'tape>> {
        self.children().nth(i)
    }

    /// Iterate every direct child of the current compound record,
    /// in emission (post-order) order.
    ///
    /// The tape builder writes records in post-order: each direct
    /// child's full sub-tree is laid down contiguously, then the
    /// child's own compound record, then the next direct child's
    /// sub-tree, and finally the parent compound at a higher
    /// offset than all children.
    ///
    /// To recover the direct children, we walk BACKWARD from the
    /// record just before the parent. Each step back lands on a
    /// direct child; the child's `child_off` gives us the start of
    /// its sub-tree, so the previous direct child sits at
    /// `child_off - 1`. For leaves the step back is `1`. We collect
    /// the children into a `Vec` and reverse so the iterator yields
    /// them in forward (emission) order.
    pub fn children(self) -> impl Iterator<Item = TapeCursor<'tape>> + 'tape {
        let tape = self.tape;
        let rec = self.record();
        let parent_offset = self.offset.0 as usize;
        let mut out: Vec<TapeCursor<'tape>> = Vec::new();
        if rec.has_children() && !rec.child_off.is_none() {
            let start = rec.child_off.0 as usize;
            // Defensive: a malformed compound whose `child_off >=
            // parent_offset` would create a backward-walk that
            // never terminates because each iteration would jump
            // forward (or stay put). Bail early — there are no
            // children to recover under post-order layout when
            // the start sentinel sits at or past the parent.
            if start >= parent_offset {
                return out.into_iter();
            }
            let mut pos = parent_offset;
            while pos > start {
                let child_offset = pos - 1;
                let Some(child_rec) = tape.try_get(TapeOffset(child_offset as u32))
                else {
                    break;
                };
                out.push(TapeCursor::new(tape, TapeOffset(child_offset as u32)));
                let next_pos = if child_rec.has_children()
                    && !child_rec.child_off.is_none()
                {
                    // Jump to the child's sub-tree start (exclusive
                    // of the child itself — we already accounted for
                    // the compound record with `pos - 1`).
                    child_rec.child_off.0 as usize
                } else {
                    child_offset
                };
                // Defensive: each iteration must strictly decrease
                // `pos`. A malformed child_off that points at or
                // past the child itself would create an infinite
                // loop and grow `out` until OOM. Bail on the first
                // non-monotonic step.
                if next_pos >= pos {
                    break;
                }
                pos = next_pos;
            }
            out.reverse();
        }
        out.into_iter()
    }
}

/// Compute the number of records spanned by the sub-tree rooted at
/// `rec`, where `rec` is located at `rec_offset` in the tape. Returns
/// 0 for leaves (a leaf occupies exactly one slot, so advancing past
/// it costs +1 handled by the caller).
///
/// The tape is written in post-order: a compound record always sits
/// AFTER all of its transitive children, and its `child_off` points
/// at the FIRST child's offset. That means the full sub-tree spans
/// exactly `rec_offset - rec.child_off` records — no recursion
/// required, no cycle risk.
fn subtree_size(rec: &TapeRec, rec_offset: usize) -> usize {
    if !rec.has_children() || rec.child_off.is_none() {
        return 0;
    }
    let first = rec.child_off.0 as usize;
    rec_offset.saturating_sub(first)
}

/// Iterator over direct children of a compound tape record.
struct ChildIter<'tape> {
    tape: &'tape Tape,
    next: Option<usize>,
    end: usize,
}

impl<'tape> Iterator for ChildIter<'tape> {
    type Item = TapeCursor<'tape>;

    fn next(&mut self) -> Option<Self::Item> {
        let offset = self.next?;
        if offset >= self.end {
            return None;
        }
        let rec = self.tape.try_get(TapeOffset(offset as u32))?;
        let cursor = TapeCursor::new(self.tape, TapeOffset(offset as u32));
        let child_subtree = if rec.has_children() && !rec.child_off.is_none() {
            subtree_size(rec, offset)
        } else {
            0
        };
        self.next = Some(offset + 1 + child_subtree);
        Some(cursor)
    }
}
