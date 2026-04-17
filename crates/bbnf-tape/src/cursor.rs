//! `TapeCursor` — the view layer's read interface into a finished
//! [`Tape`].
//!
//! Generated view types hold a `TapeCursor` keyed at the root record
//! and walk children via the cursor's accessor methods. Every
//! accessor is a `#[inline]` column-indexed read.
//!
//! # Tranche AV.2.2 — sibling-skip traversal
//!
//! Pre-AV, child traversal backward-walked from the parent offset
//! toward `child_off`, using each compound's `child_off` to leap past
//! its subtree to the previous sibling's root. The walk was O(K)
//! per compound and emitted children in reverse source order.
//!
//! Post-AV the [`Columns::sib_skip`](crate::columns::Columns::sib_skip)
//! column holds `next_sibling_root - this_root` for every record
//! (or `0` for the last sibling in its run). A forward sibling walk
//! is therefore a single indexed column read per step. The first
//! child's root is still seeded by a backward walk from the parent
//! offset (the post-order layout keeps the first direct child's
//! ROOT as the LAST record emitted inside the compound's subtree),
//! but every subsequent step through the siblings is O(1).
//!
//! # Tranche AV.2.3 — monotonic payload rank
//!
//! The old `payload_idx` sentinel on `TapeRec` is gone. Payload
//! lookups route through the record's `child_off` which carries
//! either the `pay_narrow` / `pay_wide` column rank (scalar
//! payloads) or the `pay_agg` arena byte offset (aggregates). The
//! cursor additionally carries an optional [`ColumnRank`] so walker
//! paths that walk in push order can increment rank counters
//! monotonically without re-reading `child_off` — this is the path
//! V2.5's reordered-unrolling codegen compiles into.

use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::profile::ShapeEntry;
use crate::tape::{Tape, TapeOffset, TapeRec};

/// Monotonic per-column rank maintained by a walker stepping through
/// the tape in push order.
///
/// Written by the walker itself — every time the walker advances to
/// a record carrying a payload of a given column, it increments the
/// matching counter. V2.5's typed-visitor codegen reads the columns
/// at these rank offsets without dereferencing `child_off`.
#[derive(Clone, Copy, Debug, Default)]
pub struct ColumnRank {
    /// Next read index into [`Columns::pay_narrow`](crate::columns::Columns::pay_narrow).
    pub pay_narrow: u32,
    /// Next read index into [`Columns::pay_wide`](crate::columns::Columns::pay_wide).
    pub pay_wide: u32,
    /// Next aggregate-payload index — counts records, not bytes.
    pub pay_agg: u32,
}

/// A pointer into a specific record in a specific [`Tape`].
///
/// Tied to `'tape` — the lifetime of the underlying tape — so the
/// borrow checker guarantees the cursor never outlives the arena it
/// points into.
#[derive(Clone, Copy, Debug)]
pub struct TapeCursor<'tape> {
    tape: &'tape Tape,
    offset: TapeOffset,
    /// Per-column rank as of this cursor's position.
    ///
    /// Zero on cursors constructed via random access ([`Self::new`],
    /// [`Self::child`]). Monotonic walkers (the generated view-fill
    /// path landing in V2.5/V2.6) thread rank through explicitly via
    /// [`Self::with_rank`].
    rank: ColumnRank,
}

impl<'tape> TapeCursor<'tape> {
    /// Construct a cursor pointing at `offset` within `tape`.
    ///
    /// The column-rank counters start at zero; callers that want to
    /// honour the monotonic-rank invariant use [`Self::with_rank`].
    #[inline]
    pub fn new(tape: &'tape Tape, offset: TapeOffset) -> Self {
        Self {
            tape,
            offset,
            rank: ColumnRank::default(),
        }
    }

    /// Construct a cursor pointing at `offset` within `tape`, with
    /// an explicit starting [`ColumnRank`].
    #[inline]
    pub fn with_rank(tape: &'tape Tape, offset: TapeOffset, rank: ColumnRank) -> Self {
        Self {
            tape,
            offset,
            rank,
        }
    }

    /// Access the underlying tape.
    #[inline]
    pub fn tape(&self) -> &'tape Tape {
        self.tape
    }

    /// Access the underlying [`Columns`].
    #[inline]
    pub fn columns(&self) -> &'tape Columns {
        self.tape.columns()
    }

    /// The current record's offset.
    #[inline]
    pub fn offset(&self) -> TapeOffset {
        self.offset
    }

    /// The cursor's current [`ColumnRank`].
    #[inline]
    pub fn rank(&self) -> ColumnRank {
        self.rank
    }

    /// The current record, materialised from the structural columns.
    ///
    /// Uses unchecked indexing — safe because every `TapeCursor` is
    /// constructed from offsets that originate from `TapeBuilder`
    /// pushes into the same tape, and the tape is immutable during
    /// reads.
    #[inline]
    pub fn record(&self) -> TapeRec {
        // SAFETY: `self.offset` was produced by a `TapeBuilder::push_*`
        // call on the same tape and is never the NONE sentinel (cursors
        // are only constructed with valid offsets).
        unsafe { self.tape.get_unchecked(self.offset) }
    }

    /// Classification tag of the current record.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        self.tape.columns().kind_at(self.offset.0)
    }

    /// Source span `(lo, hi)` of the current record.
    #[inline]
    pub fn span(&self) -> (u32, u32) {
        self.tape.columns().span_at(self.offset.0)
    }

    /// Full 8-bit variant index from `flags`. AW-III.W1.A widened
    /// from 6 → 8 bits; rules with ids ≥ 64 no longer alias.
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

    /// Read the bytes of an aggregate payload whose width is known
    /// to the caller from the grammar's payload-layout table.
    ///
    /// Reads from the unified arena at the record's `child_off`.
    /// Works for both [`PayloadData::Aggregate`](crate::PayloadData::Aggregate)
    /// (≤ 16 B inline) and
    /// [`PayloadData::LargeAggregate`](crate::PayloadData::LargeAggregate)
    /// (> 16 B arena-backed) — the on-arena representation is
    /// identical; only the caller-supplied `byte_count` differs.
    #[inline]
    pub fn payload_aggregate_bytes(&self, byte_count: usize) -> Option<&'tape [u8]> {
        let rec = self.record();
        self.tape.payload_bytes(rec, byte_count)
    }

    // ── Child access (AV.2.2 sibling-skip traversal) ──────────────

    /// Construct a cursor over the i-th direct child of the current
    /// compound record. Returns `None` if the current record is a
    /// leaf or `i` is out of range.
    ///
    /// Uses forward sibling-skip traversal from the first-child
    /// root: locate the first child's root via a single backward
    /// seed, then step `i` times via `sib_skip` column reads.
    #[inline]
    pub fn child(self, i: usize) -> Option<TapeCursor<'tape>> {
        let columns = self.tape.columns();
        if !columns.has_children_at(self.offset.0) {
            return None;
        }
        let first_child_root = first_child_root(columns, self.offset.0)?;
        // Walk forward across sib_skip.
        let mut current = first_child_root;
        for _ in 0..i {
            let step = columns.sib_skip_at(current);
            if step == 0 {
                return None;
            }
            current = current.checked_add(step)?;
        }
        Some(TapeCursor {
            tape: self.tape,
            offset: TapeOffset(current),
            // Reset rank — random-access via `child(i)` is not a
            // monotonic-push walk, and the child's `child_off` is
            // the authoritative pointer for payload lookup.
            rank: ColumnRank::default(),
        })
    }

    /// Number of direct children of the current compound record.
    ///
    /// Forward walk via `sib_skip`. Zero-allocation.
    #[inline]
    pub fn child_count(self) -> usize {
        let columns = self.tape.columns();
        if !columns.has_children_at(self.offset.0) {
            return 0;
        }
        let Some(first_child_root) = first_child_root(columns, self.offset.0) else {
            return 0;
        };
        let mut count = 1usize;
        let mut current = first_child_root;
        loop {
            let step = columns.sib_skip_at(current);
            if step == 0 {
                return count;
            }
            let Some(next) = current.checked_add(step) else {
                return count;
            };
            current = next;
            count += 1;
        }
    }

    /// Iterate every direct child of the current compound record,
    /// in emission (source) order.
    ///
    /// Forward walk via `sib_skip`; zero heap allocations per call.
    #[inline]
    pub fn children(self) -> ChildIter<'tape> {
        let columns = self.tape.columns();
        if !columns.has_children_at(self.offset.0) {
            return ChildIter::empty(self.tape);
        }
        let Some(first_child_root) = first_child_root(columns, self.offset.0) else {
            return ChildIter::empty(self.tape);
        };
        ChildIter {
            tape: self.tape,
            next: Some(first_child_root),
        }
    }

    /// Alias for [`Self::children`] — retained for parity with the
    /// pre-AV API, which exposed `children` (vec-backed, source
    /// order) and `children_zero_alloc` (linked-list backward walk,
    /// reverse order) as separate methods. Post-AV the substrate
    /// makes forward source-order iteration zero-alloc, so the two
    /// methods collapse into one.
    #[inline]
    pub fn children_zero_alloc(self) -> ChildIter<'tape> {
        self.children()
    }

    // ── ShapeRef lazy expansion (AV.5.1) ─────────────────────────

    /// Lazily expand the children of a `ShapeRef` record using the
    /// supplied [`ShapeEntry`] template.
    ///
    /// Returns an iterator that synthesises child cursors from the
    /// packed payload blob. Each child's `TapeKind` comes from
    /// `entry.child_kinds`; leaf-hole children read their span from
    /// the packed blob at the offset given by
    /// `entry.leaf_payload_offsets`.
    ///
    /// For compound (non-hole) children in the skeleton, the iterator
    /// yields a cursor at a synthetic position whose kind is read
    /// from `child_kinds` but whose span is inherited from the parent
    /// ShapeRef. Callers (the generated view layer) use the child
    /// kind to dispatch into the appropriate typed accessor.
    ///
    /// Returns an empty iterator when the current record is not
    /// `TapeKind::ShapeRef`.
    #[inline]
    pub fn shape_ref_children(self, entry: &'tape ShapeEntry) -> ShapeRefChildIter<'tape> {
        if self.kind() != TapeKind::ShapeRef {
            return ShapeRefChildIter::empty(self.tape);
        }
        let rec = self.record();
        let payload_start = if rec.shape_ref_has_payload() {
            rec.child_off.0 as usize
        } else {
            0
        };
        ShapeRefChildIter {
            tape: self.tape,
            parent_offset: self.offset,
            payload_start,
            entry,
            child_idx: 0,
        }
    }

    /// Number of children a `ShapeRef` record expands to, given its
    /// [`ShapeEntry`] template.
    #[inline]
    pub fn shape_ref_child_count(self, entry: &ShapeEntry) -> usize {
        if self.kind() != TapeKind::ShapeRef {
            return 0;
        }
        entry.child_kinds.len()
    }
}

/// Forward-order iterator over a compound's direct children.
///
/// Zero heap allocation. Each step reads the current record's
/// [`Columns::sib_skip`](crate::columns::Columns::sib_skip) slot in
/// one indexed column load; iteration ends when that slot reads zero.
#[derive(Clone, Copy, Debug)]
pub struct ChildIter<'tape> {
    tape: &'tape Tape,
    /// Next record offset to yield. `None` when iteration is over.
    next: Option<u32>,
}

impl<'tape> ChildIter<'tape> {
    /// Iterator that immediately yields `None`.
    #[inline]
    fn empty(tape: &'tape Tape) -> Self {
        Self { tape, next: None }
    }
}

impl<'tape> Iterator for ChildIter<'tape> {
    type Item = TapeCursor<'tape>;

    #[inline]
    fn next(&mut self) -> Option<TapeCursor<'tape>> {
        let current = self.next?;
        let columns = self.tape.columns();
        let step = columns.sib_skip_at(current);
        self.next = if step == 0 {
            None
        } else {
            current.checked_add(step)
        };
        Some(TapeCursor {
            tape: self.tape,
            offset: TapeOffset(current),
            rank: ColumnRank::default(),
        })
    }
}

// ── ShapeRef lazy child expansion (AV.5.1) ───────────────────────

/// Forward-order iterator over the synthetic children of a
/// `ShapeRef` record. Each item is a [`ShapeRefSyntheticChild`]
/// that carries the child's `TapeKind`, its span (read from the
/// packed blob for leaf holes, or inherited from the parent for
/// structural positions), and the byte offset into the packed blob
/// where the child's typed payload begins (for leaf holes).
///
/// Zero heap allocation.
#[derive(Clone, Copy, Debug)]
pub struct ShapeRefChildIter<'tape> {
    tape: &'tape Tape,
    parent_offset: TapeOffset,
    payload_start: usize,
    entry: &'tape ShapeEntry,
    child_idx: usize,
}

impl<'tape> ShapeRefChildIter<'tape> {
    /// Iterator that immediately yields `None`.
    #[inline]
    fn empty(tape: &'tape Tape) -> Self {
        Self {
            tape,
            parent_offset: TapeOffset::NONE,
            payload_start: 0,
            entry: &EMPTY_SHAPE_ENTRY,
            child_idx: 0,
        }
    }
}

/// Sentinel used by `ShapeRefChildIter::empty`.
static EMPTY_SHAPE_ENTRY: ShapeEntry = ShapeEntry {
    shape_hash: 0,
    rule: crate::profile::RuleId(0),
    child_kinds: &[],
    leaf_payload_offsets: &[],
    payload_bytes: 0,
};

/// A synthetic child yielded by [`ShapeRefChildIter`].
///
/// Not backed by a real tape record — the `TapeKind` and span are
/// derived from the shape template + packed blob. Callers use the
/// kind and blob offset to decode the child's typed payload via the
/// same `Tape::payload_*` family (keyed by arena byte offset).
#[derive(Clone, Copy, Debug)]
pub struct ShapeRefSyntheticChild {
    /// The child's `TapeKind` from the shape template skeleton.
    pub kind: TapeKind,
    /// Source span start, read from the packed blob for leaf holes
    /// or inherited from the parent for structural positions.
    pub span_lo: u32,
    /// Source span end.
    pub span_hi: u32,
    /// Byte offset into `pay_agg` where this child's typed payload
    /// begins. `u32::MAX` for structural (non-hole) children.
    pub payload_offset: u32,
    /// True iff this child is a leaf hole (its data lives in the
    /// packed blob).
    pub is_leaf_hole: bool,
}

impl<'tape> Iterator for ShapeRefChildIter<'tape> {
    type Item = ShapeRefSyntheticChild;

    #[inline]
    fn next(&mut self) -> Option<ShapeRefSyntheticChild> {
        if self.child_idx >= self.entry.child_kinds.len() {
            return None;
        }
        let idx = self.child_idx;
        self.child_idx += 1;

        let kind = TapeKind::from_u8(self.entry.child_kinds[idx]);
        let hole_offset = self.entry.leaf_payload_offsets[idx];
        let is_leaf_hole = hole_offset != u16::MAX;

        if is_leaf_hole {
            // Leaf hole: read span (lo, hi) as two u32 LE values from
            // the packed blob at the declared offset.
            let abs_offset = self.payload_start + hole_offset as usize;
            let arena = self.tape.arena();
            let (span_lo, span_hi) = if abs_offset + 8 <= arena.len() {
                let lo = u32::from_le_bytes(
                    arena[abs_offset..abs_offset + 4].try_into().unwrap_or([0; 4]),
                );
                let hi = u32::from_le_bytes(
                    arena[abs_offset + 4..abs_offset + 8].try_into().unwrap_or([0; 4]),
                );
                (lo, hi)
            } else {
                (0, 0)
            };
            Some(ShapeRefSyntheticChild {
                kind,
                span_lo,
                span_hi,
                payload_offset: abs_offset as u32 + 8, // after the span pair
                is_leaf_hole: true,
            })
        } else {
            // Structural (non-hole) child: inherit parent span.
            let columns = self.tape.columns();
            let (span_lo, span_hi) = if !self.parent_offset.is_none() {
                columns.span_at(self.parent_offset.0)
            } else {
                (0, 0)
            };
            Some(ShapeRefSyntheticChild {
                kind,
                span_lo,
                span_hi,
                payload_offset: u32::MAX,
                is_leaf_hole: false,
            })
        }
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.entry.child_kinds.len() - self.child_idx;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for ShapeRefChildIter<'_> {}

// ── First-child seed (O(1) pre-order, fallback post-order walk) ─

/// Locate the ROOT offset of the first direct child of the compound
/// at `parent_idx`.
///
/// # Pre-order fast path (Tranche AW.1.10)
///
/// Post-AW.1.2 the DTA driver writes records in pre-order: a
/// compound sits BEFORE its transitive subtree, with `child_off`
/// pointing directly at the first child's ROOT — equivalently
/// `parent_idx + 1`. The fast path recognises this by checking
/// `child_off == parent_idx + 1` and returns the child pointer
/// directly, degrading the lookup to O(1) as AW.md §AW.1.10
/// specifies.
///
/// # Post-order fallback
///
/// Legacy tapes built via direct [`TapeBuilder::push_leaf`] /
/// [`TapeBuilder::push_compound`] calls (pre-DTA test harnesses
/// and the in-tree `tape_basic` regression suite) still use
/// post-order emission — a compound sits AFTER its transitive
/// subtree, with `child_off` pointing at the first descendant's
/// offset rather than the first direct child's root. The first
/// direct child's ROOT is the LAST record emitted inside that
/// subtree, recovered by a bounded backward walk from the parent
/// down to `child_off`.
///
/// Returns `None` when the parent has no children.
#[inline]
fn first_child_root(columns: &Columns, parent_idx: u32) -> Option<u32> {
    if !columns.has_children_at(parent_idx) {
        return None;
    }
    let child_off = columns.child_off_at(parent_idx);
    if child_off.is_none() {
        return None;
    }
    let start = child_off.0;
    // AW.1.10 pre-order fast path: `child_off == parent + 1`.
    // The DTA driver's `close_compound` stamps `child_off` to the
    // frame's `child_mark`, which is `columns.len()` at the instant
    // the parent row was reserved — one record before the first
    // child's row. So `child_off == parent + 1` iff the layout is
    // pre-order. Degrade to O(1) in that case; fall through to the
    // bounded backward walk only for the post-order legacy layout.
    if start == parent_idx + 1 {
        return Some(start);
    }
    let end = parent_idx;
    if start >= end {
        return None;
    }
    let mut pos = end;
    let mut first = end - 1;
    while pos > start {
        let co = pos - 1;
        first = co;
        let has_children = columns.has_children_at(co);
        let co_child_off = columns.child_off_at(co);
        pos = if has_children && !co_child_off.is_none() {
            co_child_off.0
        } else {
            co
        };
    }
    Some(first)
}
