//! `TapeCursor` — a zero-sized focus over `(tape records, payload arena,
//! offset)`. Decodes a [`super::record::TapeRec`] on demand and walks the
//! post-order child layout.
//!
//! Post-order layout (recovered from the A-series `crates/bbnf-tape`): a
//! compound's children occupy the records immediately preceding it,
//! `[child_off .. self_off)`. Because children are themselves nested
//! post-order subtrees, the immediate children are found by walking
//! *backward* from `self_off - 1`, skipping each child's own subtree via
//! its `child_off`. [`TapeCursor::children_forward`] reverses that walk
//! to yield children in source order for positional consumers.

use super::arena::PayloadArena;
use super::record::{LeafType, TapeKind, TapeOffset, TapeRec, TAPE_NONE};

/// Shared borrow of the flat tape: records + payload arena + the source
/// the spans index into.
#[derive(Debug, Clone, Copy)]
pub struct TapeRef<'t> {
    pub records: &'t [TapeRec],
    pub arena: &'t PayloadArena,
    pub source: &'t str,
}

/// A focus on one record of the tape.
#[derive(Debug, Clone, Copy)]
pub struct TapeCursor<'t> {
    tape: TapeRef<'t>,
    off: u32,
}

impl<'t> TapeCursor<'t> {
    #[inline]
    pub fn new(tape: TapeRef<'t>, off: u32) -> Self {
        Self { tape, off }
    }

    #[inline]
    pub fn tape(&self) -> TapeRef<'t> {
        self.tape
    }

    #[inline]
    pub fn offset(&self) -> u32 {
        self.off
    }

    #[inline]
    pub fn rec(&self) -> &'t TapeRec {
        &self.tape.records[self.off as usize]
    }

    #[inline]
    pub fn kind(&self) -> TapeKind {
        self.rec().kind()
    }

    #[inline]
    pub fn meta_idx(&self) -> u8 {
        self.rec().meta_idx()
    }

    #[inline]
    pub fn branch(&self) -> u8 {
        self.rec().branch()
    }

    /// The source slice this record spans.
    #[inline]
    pub fn span(&self) -> &'t str {
        let (lo, hi) = self.rec().span();
        let (lo, hi) = (lo as usize, hi as usize);
        if lo <= hi && hi <= self.tape.source.len() {
            self.tape.source.get(lo..hi).unwrap_or("")
        } else {
            ""
        }
    }

    // ---- leaf payload decode ---------------------------------------

    #[inline]
    pub fn leaf_type(&self) -> LeafType {
        self.rec().leaf_type()
    }

    #[inline]
    pub fn as_f64(&self) -> f64 {
        let rec = self.rec();
        debug_assert!(matches!(rec.leaf_type(), LeafType::F64));
        if rec.child_off == TAPE_NONE {
            0.0
        } else {
            self.tape.arena.read_f64(rec.child_off)
        }
    }

    #[inline]
    pub fn as_i64(&self) -> i64 {
        let rec = self.rec();
        if rec.child_off == TAPE_NONE {
            0
        } else {
            self.tape.arena.read_i64(rec.child_off)
        }
    }

    #[inline]
    pub fn as_u64(&self) -> u64 {
        let rec = self.rec();
        if rec.child_off == TAPE_NONE {
            0
        } else if rec.span_lo == 1 {
            // arena-resident wide magnitude (see push_leaf_with_u64).
            self.tape.arena.read_u64(rec.child_off)
        } else {
            // inline-packed (<= u32::MAX).
            rec.child_off as u64
        }
    }

    /// A `u32` payload inline-packed into `child_off` (e.g. a packed hex
    /// color, or a bool 0/1).
    #[inline]
    pub fn as_u32_inline(&self) -> u32 {
        let v = self.rec().child_off;
        if v == TAPE_NONE {
            0
        } else {
            v
        }
    }

    #[inline]
    pub fn as_bool(&self) -> bool {
        self.as_u32_inline() != 0
    }

    /// A string leaf — zero-copy source borrow when [`TapeRec`] carries
    /// the borrow bit, else arena-decoded bytes.
    #[inline]
    pub fn as_str(&self) -> &'t str {
        let rec = self.rec();
        if rec.is_string_borrow() {
            self.span()
        } else if rec.child_off == TAPE_NONE {
            ""
        } else {
            // span_hi reused as arena length for non-borrow strings.
            self.tape.arena.read_str(rec.child_off, rec.span_hi)
        }
    }

    // ---- child navigation (post-order backward walk) ----------------

    /// The post-order offset of this compound's first child (the lowest
    /// record offset in its subtree), or `None` for a leaf / empty
    /// compound.
    #[inline]
    fn first_child_off(&self) -> Option<u32> {
        let rec = self.rec();
        if rec.kind() != TapeKind::Open || !rec.has_children() || rec.child_off == TAPE_NONE {
            None
        } else {
            Some(rec.child_off)
        }
    }

    /// Number of immediate children, found by the backward post-order
    /// walk: start at `self_off - 1`, and for each immediate child jump
    /// past its subtree to the record before its own `first_child`.
    pub fn child_count(&self) -> usize {
        let Some(first) = self.first_child_off() else {
            return 0;
        };
        let mut count = 0usize;
        let mut idx = self.off; // exclusive upper bound
        while idx > first {
            // child header is at idx-1 (post-order: subtree's last record);
            // continue the backward walk at the child's subtree start.
            idx = self.subtree_start(idx - 1);
            count += 1;
        }
        count
    }

    /// The lowest record offset of the post-order subtree rooted at
    /// `at` (inclusive). For a leaf this is `at`; for a compound it is
    /// the compound's `child_off` (its first child), which is itself the
    /// start of the whole subtree under post-order layout.
    #[inline]
    fn subtree_start(&self, at: u32) -> u32 {
        let rec = &self.tape.records[at as usize];
        match rec.kind() {
            TapeKind::Leaf => at,
            TapeKind::Open => {
                if rec.has_children() && rec.child_off != TAPE_NONE {
                    rec.child_off
                } else {
                    at
                }
            }
        }
    }

    /// The immediate children in source order (forward). Materialises one
    /// transient cursor per child; no allocation of the children
    /// themselves.
    pub fn children_forward(&self) -> impl Iterator<Item = TapeCursor<'t>> + '_ {
        let mut offsets: Vec<u32> = Vec::new();
        if let Some(first) = self.first_child_off() {
            let mut idx = self.off;
            while idx > first {
                let header = idx - 1; // the child's own record (post-order last)
                offsets.push(header);
                idx = self.subtree_start(header);
            }
        }
        offsets.reverse();
        let tape = self.tape;
        offsets.into_iter().map(move |o| TapeCursor::new(tape, o))
    }

    /// The `i`-th immediate child in source order, or `None`.
    pub fn child(&self, i: usize) -> Option<TapeCursor<'t>> {
        self.children_forward().nth(i)
    }
}

#[inline]
pub(crate) fn root_offset(records: &[TapeRec]) -> Option<TapeOffset> {
    // Post-order: the root compound is the last record written.
    if records.is_empty() {
        None
    } else {
        Some((records.len() - 1) as u32)
    }
}
