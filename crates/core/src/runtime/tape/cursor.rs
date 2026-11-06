//! `TapeCursor` — a lightweight focus over `(tape records, payload arena,
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
use super::record::{LeafType, TAPE_NONE, TapeKind, TapeRec};

/// Shared borrow of the flat tape: records + payload arena + the
/// builder-owned source that compound spans index into.
#[derive(Debug, Clone, Copy)]
pub struct TapeRef<'t> {
    records: &'t [TapeRec],
    rule_ids: &'t [u32],
    arena: &'t PayloadArena,
    source: &'t [u8],
}

impl<'t> TapeRef<'t> {
    #[inline]
    pub(crate) fn new(
        records: &'t [TapeRec],
        rule_ids: &'t [u32],
        arena: &'t PayloadArena,
        source: &'t [u8],
    ) -> Self {
        Self {
            records,
            rule_ids,
            arena,
            source,
        }
    }
}

/// A focus on one record of the tape.
#[derive(Debug, Clone, Copy)]
pub struct TapeCursor<'t> {
    tape: TapeRef<'t>,
    off: u32,
}

impl<'t> TapeCursor<'t> {
    #[inline]
    pub(crate) fn new(tape: TapeRef<'t>, off: u32) -> Option<Self> {
        let index = off as usize;
        (tape.records.get(index).is_some() && tape.rule_ids.len() == tape.records.len())
            .then_some(Self { tape, off })
    }

    #[inline]
    pub fn offset(&self) -> u32 {
        self.off
    }

    #[inline]
    fn rec(&self) -> Option<&'t TapeRec> {
        self.tape.records.get(self.off as usize)
    }

    #[inline]
    pub fn kind(&self) -> Option<TapeKind> {
        self.rec().map(TapeRec::kind)
    }

    /// Exact grammar rule identity for a compound. Leaf records have no
    /// rule identity even though the parallel lane contains a placeholder.
    #[inline]
    pub fn rule_id(&self) -> Option<u32> {
        (self.kind()? == TapeKind::Open)
            .then(|| self.tape.rule_ids.get(self.off as usize).copied())
            .flatten()
    }

    #[inline]
    pub fn branch(&self) -> Option<bbnf_ir::AltBranchId> {
        self.rec().and_then(TapeRec::branch)
    }

    /// The source slice this record spans.
    #[inline]
    pub fn span(&self) -> Option<&'t str> {
        let rec = self.rec()?;
        if rec.kind() != TapeKind::Open {
            return None;
        }
        let (lo, hi) = rec.span();
        let (lo, hi) = (lo as usize, hi as usize);
        (lo <= hi)
            .then(|| self.tape.source.get(lo..hi))
            .flatten()
            .and_then(|bytes| core::str::from_utf8(bytes).ok())
    }

    // ---- leaf payload decode ---------------------------------------

    #[inline]
    pub fn leaf_type(&self) -> Option<LeafType> {
        self.rec().and_then(TapeRec::leaf_type)
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        let rec = self.rec()?;
        (rec.leaf_type() == Some(LeafType::F64) && rec.child_off != TAPE_NONE)
            .then(|| self.tape.arena.read_f64(rec.child_off))
            .flatten()
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        let rec = self.rec()?;
        (rec.leaf_type() == Some(LeafType::I64) && rec.child_off != TAPE_NONE)
            .then(|| self.tape.arena.read_i64(rec.child_off))
            .flatten()
    }

    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        let rec = self.rec()?;
        if rec.leaf_type() != Some(LeafType::U64) || rec.child_off == TAPE_NONE {
            None
        } else if rec.has_u64_arena_payload() {
            self.tape.arena.read_u64(rec.child_off)
        } else {
            Some(u64::from(rec.child_off))
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Option<u8> {
        let rec = self.rec()?;
        (rec.leaf_type() == Some(LeafType::U8))
            .then(|| u8::try_from(rec.child_off).ok())
            .flatten()
    }

    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        let rec = self.rec()?;
        (rec.leaf_type() == Some(LeafType::Bool))
            .then_some(rec.child_off)
            .filter(|value| *value <= 1)
            .map(|value| value != 0)
    }

    /// An owned string leaf decoded from the payload arena.
    #[inline]
    pub fn as_str(&self) -> Option<&'t str> {
        let rec = self.rec()?;
        if rec.leaf_type() != Some(LeafType::Str) {
            None
        } else if rec.child_off == TAPE_NONE {
            None
        } else {
            // `span_hi` is reused as the arena length for string leaves.
            self.tape.arena.read_str(rec.child_off, rec.span_hi)
        }
    }

    // ---- child navigation (post-order backward walk) ----------------

    /// The post-order offset of this compound's first child (the lowest
    /// record offset in its subtree), or `None` for a leaf / empty
    /// compound.
    #[inline]
    fn first_child_off(&self) -> Option<u32> {
        let rec = self.rec()?;
        if rec.kind() != TapeKind::Open || !rec.has_children() || rec.child_off == TAPE_NONE {
            None
        } else if rec.child_off >= self.off
            || self.tape.records.get(rec.child_off as usize).is_none()
        {
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
        let mut idx = self.off;
        while idx > first {
            let Some(header) = idx.checked_sub(1) else {
                return 0;
            };
            let Some(start) = self.subtree_start(header) else {
                return 0;
            };
            if start < first || start >= idx {
                return 0;
            }
            count += 1;
            idx = start;
        }
        (idx == first).then_some(count).unwrap_or(0)
    }

    /// The lowest record offset of the post-order subtree rooted at
    /// `at` (inclusive). For a leaf this is `at`; for a compound it is
    /// the compound's `child_off` (its first child), which is itself the
    /// start of the whole subtree under post-order layout.
    #[inline]
    fn subtree_start(&self, at: u32) -> Option<u32> {
        let rec = self.tape.records.get(at as usize)?;
        let start = match rec.kind() {
            TapeKind::Leaf => at,
            TapeKind::Open => {
                if rec.has_children() && rec.child_off != TAPE_NONE {
                    rec.child_off
                } else {
                    at
                }
            }
        };
        (start <= at && self.tape.records.get(start as usize).is_some()).then_some(start)
    }

    fn child_offsets(&self) -> Vec<u32> {
        let Some(first) = self.first_child_off() else {
            return Vec::new();
        };
        let mut offsets = Vec::new();
        let mut idx = self.off;
        while idx > first {
            let Some(header) = idx.checked_sub(1) else {
                return Vec::new();
            };
            let Some(start) = self.subtree_start(header) else {
                return Vec::new();
            };
            if start < first || start >= idx {
                return Vec::new();
            }
            offsets.push(header);
            idx = start;
        }
        if idx != first {
            return Vec::new();
        }
        offsets.reverse();
        offsets
    }

    /// The immediate children in source order (forward). Materialises one
    /// transient cursor per child; no allocation of the children
    /// themselves.
    pub fn children_forward(&self) -> impl Iterator<Item = TapeCursor<'t>> + '_ {
        let tape = self.tape;
        self.child_offsets()
            .into_iter()
            .filter_map(move |off| TapeCursor::new(tape, off))
    }

    /// The `i`-th immediate child in source order, or `None`.
    pub fn child(&self, i: usize) -> Option<TapeCursor<'t>> {
        self.children_forward().nth(i)
    }
}
