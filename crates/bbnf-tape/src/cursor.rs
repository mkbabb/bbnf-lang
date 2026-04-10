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

    /// Construct a cursor over the i-th child of the current compound
    /// record. Returns `None` if the current record is a leaf or `i`
    /// is out of range.
    ///
    /// Children are stored as a contiguous run starting at
    /// `child_off`. The run's length depends on the compound kind —
    /// for `Alt` it's always 1 (the chosen branch), for `Seq` and
    /// `Repeat` it's determined by walking records until their
    /// `child_off` field indicates the next sibling begins.
    ///
    /// The walk is O(1) for Alt and O(n) for Seq/Repeat — but the
    /// "n" is bounded by the rule's child count (compile-time
    /// constant) so in practice view accessors just call `child(0)`,
    /// `child(1)`, etc., generated from the grammar's known shape.
    #[inline]
    pub fn child(&self, i: usize) -> Option<TapeCursor<'tape>> {
        let rec = self.record();
        if !rec.has_children() || rec.child_off.is_none() {
            return None;
        }
        // Linear walk: children are laid out in pre-order. This is
        // correct for all compound kinds; generated view accessors
        // can shortcut when they know the rule's child count.
        let start = rec.child_off.0 as usize;
        let mut offset = start;
        let mut idx = 0;
        while idx < i {
            let r = self.tape.try_get(TapeOffset(offset as u32))?;
            // Skip over the entire sub-tree rooted at `offset`. For a
            // leaf that's one record; for a compound, it's the
            // compound plus its transitive children. The pre-order
            // layout means we advance by `1 + subtree_size(r)`.
            offset += 1 + subtree_size(self.tape, r);
            idx += 1;
        }
        if self.tape.try_get(TapeOffset(offset as u32)).is_some() {
            Some(TapeCursor::new(self.tape, TapeOffset(offset as u32)))
        } else {
            None
        }
    }

    /// Iterate every direct child of the current compound record.
    ///
    /// The tape builder writes records in pre-order: a compound
    /// header is pushed AFTER all of its children, so the compound's
    /// record index is always greater than every child's index. This
    /// gives a natural termination condition for the iterator —
    /// stop when the cursor reaches the parent's own offset.
    pub fn children(self) -> impl Iterator<Item = TapeCursor<'tape>> + 'tape {
        let tape = self.tape;
        let rec = self.record();
        let parent_offset = self.offset.0 as usize;
        let start = if rec.has_children() && !rec.child_off.is_none() {
            Some(rec.child_off.0 as usize)
        } else {
            None
        };
        ChildIter {
            tape,
            next: start,
            end: parent_offset,
        }
    }
}

/// Walk the tape starting at `rec` and return the number of records
/// spanned by its full sub-tree (not counting `rec` itself).
///
/// Leaves have sub-tree size 0 — they occupy exactly one slot.
/// Compounds recursively add up their immediate children's sub-tree
/// sizes. This is used by `TapeCursor::child` to skip over one child
/// at a time when walking a compound's direct children; multi-child
/// iteration is preferred through the `ChildIter` path which has a
/// termination condition independent of this helper.
fn subtree_size(tape: &Tape, rec: &TapeRec) -> usize {
    if !rec.has_children() || rec.child_off.is_none() {
        return 0;
    }
    // Return the size of the FIRST direct child's sub-tree (1 + its
    // own transitive children). Callers iterate siblings via
    // `ChildIter` which uses this to advance one step at a time.
    let first = rec.child_off.0 as usize;
    let Some(first_rec) = tape.try_get(TapeOffset(first as u32)) else {
        return 0;
    };
    let transitive = if first_rec.has_children() && !first_rec.child_off.is_none() {
        subtree_size(tape, first_rec)
    } else {
        0
    };
    1 + transitive
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
            subtree_size(self.tape, rec)
        } else {
            0
        };
        self.next = Some(offset + 1 + child_subtree);
        Some(cursor)
    }
}
