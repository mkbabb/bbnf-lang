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
//!
//! # Tranche AY-II.W0.a — single-stamping finaliser path
//!
//! Compounds emitted via
//! [`TapeBuilder::begin_compound`](crate::TapeBuilder::begin_compound)
//! / [`TapeBuilder::end_compound`](crate::TapeBuilder::end_compound)
//! point at the first direct child's root via
//! `child_off == parent + 1` (pre-order layout), hitting
//! [`first_child_root`]'s O(1) fast path. The `sib_skip` column is
//! written exclusively by [`crate::finaliser::finalise`] after the
//! parse completes; the cursor's forward walk reads it without a
//! branch on any stamp bit. Legacy `push_compound` tapes are the
//! post-order fallback and continue through the same primitive.

use crate::columns::Columns;
use crate::kind::TapeKind;
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

    /// Scan the source-byte range `[cur.span_lo, end_span)` for
    /// structural positions (quotes, brackets, commas — whatever the
    /// grammar's structural-scan policy admits) and return an
    /// iterator over the matching absolute byte offsets.
    ///
    /// AY-II.W0.a lands the signature; AY-II.W0.e wires per-grammar
    /// activation through the emitter's `STRUCTURAL_SCAN_POLICY`
    /// table. Until the consumer wiring lands, the default
    /// implementation yields the empty iterator — grammars whose
    /// structural-scan alphabet is unresolved at emit time quietly
    /// fall back to per-byte scanning upstream rather than panicking
    /// or allocating.
    #[inline]
    pub fn scan_structural_bounded(&self, end_span: u32) -> ScanResult<'tape> {
        let _ = end_span;
        ScanResult {
            tape: self.tape,
            positions: &[],
            cursor: 0,
        }
    }

}

/// Lightweight iterator over structural byte positions discovered by
/// [`TapeCursor::scan_structural_bounded`].
///
/// AY-II.W0.a — empty-iter scaffold. AY-II.W0.e populates the body
/// over the structural-scan substrate service. The shape is a
/// borrowed `&[u32]` slice so the populated form can point directly
/// into the scanner's pre-computed position column without a fresh
/// allocation on the hot path; the scaffold holds an empty slice.
#[derive(Debug)]
pub struct ScanResult<'tape> {
    /// Carries the tape reference so the W0.e body can materialise
    /// [`TapeCursor`] handles against the scanner-produced offsets
    /// without re-threading `'tape` through every caller.
    tape: &'tape Tape,
    /// Pre-computed structural byte positions within the bounded
    /// range. Empty on the scaffold returned by the W0.a default.
    positions: &'tape [u32],
    /// Read cursor into [`Self::positions`].
    cursor: usize,
}

impl<'tape> ScanResult<'tape> {
    /// Borrow the tape this result was derived from.
    #[inline]
    pub fn tape(&self) -> &'tape Tape {
        self.tape
    }

    /// Borrow the underlying position slice. Empty on the W0.a
    /// scaffold; populated by W0.e's structural-scan wiring.
    #[inline]
    pub fn positions(&self) -> &'tape [u32] {
        self.positions
    }
}

impl<'tape> Iterator for ScanResult<'tape> {
    type Item = u32;

    #[inline]
    fn next(&mut self) -> Option<u32> {
        let pos = self.positions.get(self.cursor).copied()?;
        self.cursor += 1;
        Some(pos)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.positions.len().saturating_sub(self.cursor);
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for ScanResult<'_> {}

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
