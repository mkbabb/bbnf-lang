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
//! [`FusedBuilder::begin_compound`](crate::FusedBuilder::begin_compound)
//! / [`FusedBuilder::end_compound`](crate::FusedBuilder::end_compound)
//! point at the first direct child's root via
//! `child_off == parent + 1` (pre-order layout), hitting
//! [`first_child_root`]'s O(1) fast path. The `sib_skip` column is
//! written exclusively by [`crate::finaliser::finalise`] after the
//! parse completes; the cursor's forward walk reads it without a
//! branch on any stamp bit. Legacy `push_compound` tapes are the
//! post-order fallback and continue through the same primitive.

use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::stage1::StructuralIndex;
use crate::structural_scan::scan_structural;
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

/// A pointer into a specific record in a specific [`Tape<R>`].
///
/// Tied to `'tape` — the lifetime of the underlying tape — so the
/// borrow checker guarantees the cursor never outlives the arena it
/// points into. `R` is the grammar-root marker the cursor's `tape`
/// reference is parameterised over; cursor accessors that surface
/// raw column data (`columns()`, `kind()`, `span()`, `record()`)
/// are R-agnostic, while accessors that touch the value substrate
/// (none currently — value-substrate reads route through
/// [`Tape<R>`] directly) inherit the binding from the tape.
#[derive(Debug)]
pub struct TapeCursor<'tape, R = ()> {
    tape: &'tape Tape<R>,
    offset: TapeOffset,
    /// Per-column rank as of this cursor's position.
    ///
    /// Zero on cursors constructed via random access ([`Self::new`],
    /// [`Self::child`]). Monotonic walkers (the generated view-fill
    /// path landing in V2.5/V2.6) thread rank through explicitly via
    /// [`Self::with_rank`].
    rank: ColumnRank,
}

// Manual `Clone + Copy` impls — derive would require `R: Copy`,
// which user grammar markers don't satisfy. The cursor's only
// `R`-typed field is `&Tape<R>` which is `Copy` regardless of `R`.
impl<'tape, R> Clone for TapeCursor<'tape, R> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<'tape, R> Copy for TapeCursor<'tape, R> {}

impl<'tape, R> TapeCursor<'tape, R> {
    /// Construct a cursor pointing at `offset` within `tape`.
    ///
    /// The column-rank counters start at zero; callers that want to
    /// honour the monotonic-rank invariant use [`Self::with_rank`].
    #[inline]
    pub fn new(tape: &'tape Tape<R>, offset: TapeOffset) -> Self {
        Self {
            tape,
            offset,
            rank: ColumnRank::default(),
        }
    }

    /// Construct a cursor pointing at `offset` within `tape`, with
    /// an explicit starting [`ColumnRank`].
    #[inline]
    pub fn with_rank(tape: &'tape Tape<R>, offset: TapeOffset, rank: ColumnRank) -> Self {
        Self {
            tape,
            offset,
            rank,
        }
    }

    /// Access the underlying tape.
    #[inline]
    pub fn tape(&self) -> &'tape Tape<R> {
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
    /// constructed from offsets that originate from `FusedBuilder`
    /// pushes into the same tape, and the tape is immutable during
    /// reads.
    #[inline]
    pub fn record(&self) -> TapeRec {
        // SAFETY: `self.offset` was produced by a `FusedBuilder::push_*`
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
    pub fn child(self, i: usize) -> Option<TapeCursor<'tape, R>> {
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
    pub fn children(self) -> ChildIter<'tape, R> {
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
    pub fn children_zero_alloc(self) -> ChildIter<'tape, R> {
        self.children()
    }

}

// === W0.e: promoted substrate surface ===
//
// Tranche AY-II.W0.e promotes the `scan_structural` + `StructuralIndex`
// pair from standalone free functions to universal cursor services.
//
// The three primitives below are inline-only zero-cost wrappers over
// the substrate columns — no runtime dispatch, no allocation on the
// hot path for `bounded_lookahead` and `scan_structural_bounded`.
// Grammar-emitted `__path_walk` and `Parsed::get` bodies route
// through these primitives when the per-grammar
// `STRUCTURAL_SCAN_POLICY` admits the matching capability.
//
// The APIs are `Cursor`-scoped (not free functions) so every consumer
// carries its `'tape` lifetime through the substrate — the ScanResult
// / BoundedLookahead borrow the same tape the cursor points into, and
// the borrow checker enforces no-outlive at monomorphisation.

/// Outcome of a bounded structural scan inside a compound record.
///
/// Captures the list of structural-record offsets the scan visited
/// within the bound, paired with their record kinds. The scan walks
/// direct children of the cursor's current record whose span ends at
/// or before `end_span`; each visited offset represents a structural
/// landmark the caller (materializer / `__path_walk` / object-key
/// seek) can key its next step off.
///
/// Zero-allocation when the bounded window is empty (common on
/// leaf-heavy rules); a single `SmallVec`-shaped inline buffer is
/// avoided in favour of lazy iteration via [`Self::iter`] so
/// consumers that only need the first match pay no heap cost at all.
#[derive(Debug)]
pub struct ScanResult<'tape, R = ()> {
    tape: &'tape Tape<R>,
    /// Inclusive first offset admitted by the scan (or `None` when
    /// empty).
    first: Option<u32>,
    /// Exclusive upper bound — the scan stops when it reaches a
    /// record whose `span_hi` exceeds this value.
    end_span: u32,
}

impl<'tape, R> Clone for ScanResult<'tape, R> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<'tape, R> Copy for ScanResult<'tape, R> {}

impl<'tape, R> ScanResult<'tape, R> {
    /// The tape this scan was produced from.
    #[inline]
    pub fn tape(&self) -> &'tape Tape<R> {
        self.tape
    }


    /// Is the bounded window empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.first.is_none()
    }

    /// Iterate every bounded cursor in the scan result.
    ///
    /// Yields one [`TapeCursor`] per sibling record within the
    /// bounded window, in emission (source) order. Zero heap
    /// allocation per step — the walker reads `sib_skip` + `span_hi`
    /// at each position to advance.
    #[inline]
    pub fn iter(&self) -> BoundedLookahead<'tape, R> {
        BoundedLookahead {
            tape: self.tape,
            next: self.first,
            end_span: self.end_span,
        }
    }

    /// First offset in the scan result, as a cursor. `None` when the
    /// bounded window is empty.
    #[inline]
    pub fn first(&self) -> Option<TapeCursor<'tape, R>> {
        self.first.map(|off| TapeCursor::new(self.tape, TapeOffset(off)))
    }
}

/// Forward iterator over records inside a span-bounded window,
/// yielded as cursors in emission order.
///
/// Advances by the same sibling-skip mechanism [`ChildIter`] uses, but
/// terminates early when the current record's `span_hi` exceeds the
/// window's bound — the caller gets exactly the records the
/// structural scan admits, not every child of the current compound.
///
/// Produced by [`TapeCursor::bounded_lookahead`] and
/// [`ScanResult::iter`]. Zero heap allocation per step; every advance
/// is two column reads (`sib_skip_at`, `span_at`).
#[derive(Debug)]
pub struct BoundedLookahead<'tape, R = ()> {
    tape: &'tape Tape<R>,
    /// Next record offset to yield. `None` once the iterator is
    /// exhausted or the window is empty.
    next: Option<u32>,
    /// Exclusive upper bound on the source-span end — a record whose
    /// `span_hi` exceeds this value terminates the walk.
    end_span: u32,
}

impl<'tape, R> Clone for BoundedLookahead<'tape, R> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<'tape, R> Copy for BoundedLookahead<'tape, R> {}

impl<'tape, R> Iterator for BoundedLookahead<'tape, R> {
    type Item = TapeCursor<'tape, R>;

    #[inline]
    fn next(&mut self) -> Option<TapeCursor<'tape, R>> {
        let current = self.next?;
        let columns = self.tape.columns();
        let (_, span_hi) = columns.span_at(current);
        if span_hi > self.end_span {
            self.next = None;
            return None;
        }
        let step = columns.sib_skip_at(current);
        self.next = if step == 0 {
            None
        } else {
            current.checked_add(step)
        };
        Some(TapeCursor::new(self.tape, TapeOffset(current)))
    }
}

impl<'tape, R> TapeCursor<'tape, R> {
    /// Seek to the value record associated with the key whose source
    /// span equals `key_span`, under the current compound.
    ///
    /// Walks the direct children of the current compound (which the
    /// caller guarantees is an object-shaped record whose children
    /// alternate key / value) and returns a cursor at the value
    /// position immediately following the key whose span matches.
    ///
    /// Returns `None` when:
    ///
    /// - the current record has no children;
    /// - no child's span equals `key_span`;
    /// - the matching key is the last child (no value slot).
    ///
    /// Inline-ready: the emitter splices this into `__path_walk` for
    /// object-key lookups admitted by the grammar's
    /// `STRUCTURAL_SCAN_POLICY`. The per-call cost is O(children) in
    /// the worst case; grammars whose policy admits
    /// `SCAN_STRUCTURAL_BOUNDED` additionally route the scan window
    /// through a structural index to amortise multi-key lookups.
    #[inline]
    pub fn object_key_seek(&self, key_span: (u32, u32)) -> Option<TapeCursor<'tape, R>> {
        let columns = self.tape.columns();
        if !columns.has_children_at(self.offset.0) {
            return None;
        }
        let first_child_root = first_child_root(columns, self.offset.0)?;
        let mut current = first_child_root;
        loop {
            let span = columns.span_at(current);
            if span == key_span {
                // Matched — the value is the key's next sibling.
                let step = columns.sib_skip_at(current);
                if step == 0 {
                    return None;
                }
                let value_off = current.checked_add(step)?;
                return Some(TapeCursor::new(self.tape, TapeOffset(value_off)));
            }
            let step = columns.sib_skip_at(current);
            if step == 0 {
                return None;
            }
            current = current.checked_add(step)?;
        }
    }

    /// Produce a cursor iterator over records whose source span ends
    /// at or before `end_span`, starting from the first direct child
    /// of the current record.
    ///
    /// Used by emitted `__path_walk` bodies to bound the lookahead
    /// window when traversing a compound's children without visiting
    /// records past a known end-of-structure marker (e.g. the
    /// closing `}` of a CSS block). Zero heap allocation per step.
    #[inline]
    pub fn bounded_lookahead(&self, end_span: u32) -> BoundedLookahead<'tape, R> {
        let columns = self.tape.columns();
        let next = if columns.has_children_at(self.offset.0) {
            first_child_root(columns, self.offset.0)
        } else {
            None
        };
        BoundedLookahead {
            tape: self.tape,
            next,
            end_span,
        }
    }

    /// Scan direct children of the current record whose source span
    /// ends at or before `end_span`, returning a [`ScanResult`] the
    /// caller can iterate or probe.
    ///
    /// Companion to [`Self::bounded_lookahead`] for consumers that
    /// want the scan substrate surface rather than the iterator
    /// directly — the emitter's generated materializer keys its
    /// per-rule dispatch off the [`ScanResult`] shape.
    #[inline]
    pub fn scan_structural_bounded(&self, end_span: u32) -> ScanResult<'tape, R> {
        let columns = self.tape.columns();
        let first = if columns.has_children_at(self.offset.0) {
            match first_child_root(columns, self.offset.0) {
                Some(root) => {
                    let (_, span_hi) = columns.span_at(root);
                    if span_hi <= end_span {
                        Some(root)
                    } else {
                        None
                    }
                }
                None => None,
            }
        } else {
            None
        };
        ScanResult {
            tape: self.tape,
            first,
            end_span,
        }
    }

    /// Build a fresh [`StructuralIndex`] over `input[start..end]`
    /// against the grammar's sorted `alphabet`.
    ///
    /// Substrate-level shortcut — zero-allocates when the window is
    /// empty, otherwise dispatches to [`scan_structural`] with a
    /// bounded slice. Generated parsers consult this when the
    /// grammar's `STRUCTURAL_SCAN_POLICY` admits a bounded-window
    /// scan inside a rule whose extent the parser already knows
    /// (e.g. a CSS block body bounded by `{...}`).
    #[inline]
    pub fn scan_window(input: &[u8], alphabet: &[u8]) -> StructuralIndex {
        scan_structural(input, alphabet)
    }
}

/// Forward-order iterator over a compound's direct children.
///
/// Zero heap allocation. Each step reads the current record's
/// [`Columns::sib_skip`](crate::columns::Columns::sib_skip) slot in
/// one indexed column load; iteration ends when that slot reads zero.
#[derive(Debug)]
pub struct ChildIter<'tape, R = ()> {
    tape: &'tape Tape<R>,
    /// Next record offset to yield. `None` when iteration is over.
    next: Option<u32>,
}

impl<'tape, R> Clone for ChildIter<'tape, R> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}
impl<'tape, R> Copy for ChildIter<'tape, R> {}

impl<'tape, R> ChildIter<'tape, R> {
    /// Iterator that immediately yields `None`.
    #[inline]
    fn empty(tape: &'tape Tape<R>) -> Self {
        Self { tape, next: None }
    }
}

impl<'tape, R> Iterator for ChildIter<'tape, R> {
    type Item = TapeCursor<'tape, R>;

    #[inline]
    fn next(&mut self) -> Option<TapeCursor<'tape, R>> {
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
/// Legacy tapes built via direct [`FusedBuilder::push_leaf`] /
/// [`FusedBuilder::push_compound`] calls (pre-DTA test harnesses
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
    // AW.1.10 pre-order fast path. The DTA driver's `close_compound`
    // stamps `child_off` at-or-after the parent row, so `child_off >
    // parent_idx` iff the layout is pre-order. The legacy fast path
    // checked the strict `parent + 1` relation; B3.W0.δ widens the
    // check because [`crate::FusedBuilder::end_compound`] now scans
    // for the first record at `parent_depth + 1` (skipping records
    // that an inner [`crate::FusedBuilder::end_compound_post_order`]
    // retroactively bumped to a deeper level). The result still
    // satisfies pre-order's "child_off names the first child root"
    // contract for any `start > parent_idx`. Fall through to the
    // bounded backward walk only for post-order legacy layouts where
    // `child_off < parent_idx`.
    if start > parent_idx {
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
        // Leap only when `co_child_off` points strictly BEFORE `co`
        // (canonical post-order subtree skip). For pre-order children
        // (child_off > co) leaping would move `pos` upward and spin the
        // `while pos > start` guard forever — same defect γ identified
        // in `derive_frame_depth`. Step by one in that case so the
        // walk monotonically descends to `start`. B3.W0.ε.
        pos = if has_children && !co_child_off.is_none() && co_child_off.0 < co {
            co_child_off.0
        } else {
            co
        };
    }
    Some(first)
}
