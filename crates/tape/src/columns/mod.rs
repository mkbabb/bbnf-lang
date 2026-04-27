//! `Columns` — flat-AoS structural substrate (post-AY.W1.1).
//!
//! # Architectural role (Tranche AY.W1.1 — invariant 22)
//!
//! Pre-AY the substrate carried seven independent `Vec`s for the
//! structural rows (`kinds`, `flags`, `extra`, `span_lo`, `span_hi`,
//! `sib_skip`, `child_off`). Every push paid seven bounds checks +
//! seven heterogeneous stores, and `push_structural` rendered as a
//! cross-crate cold call in every bench binary's `nm` output. The
//! AU-era flat `Vec<TapeRec>` substrate was the speed-ceiling oracle
//! (twitter 1,967 MB/s = 0.615 bytes/cyc = 76% of sonic-rs); AY.W1.1
//! restores it. Per AY.md prop 1: 7 stores → 2 stores per push.
//!
//! Layout:
//!
//! - **`records: Vec<TapeRec>`** — 16-byte AoS row, one cache-line-
//!   friendly stride. Holds `kind_meta`, `flags`, `extra`, `span_lo`,
//!   `span_hi`, and `child_off` in the same shape every external
//!   reader binds.
//! - **`sib_skip: Vec<u32>`** — parallel column (4 B per row). Stays
//!   separate so the pre-AY consumers reading `cols.sib_skip[i]`
//!   continue to compile. `push_structural` writes both vectors as a
//!   fused pair: `records.push(rec); sib_skip.push(0);` (LLVM sees
//!   two stores per push instead of seven).
//! - **`pay_narrow: Vec<u32>`** / **`pay_wide: Vec<u64>`** /
//!   **`pay_agg: Vec<u8>`** — the three typed-payload columns. Kept
//!   unchanged: they are orthogonal to structural layout and carry
//!   the AY.W4 Eisel-Lemire direct-column work surface.
//!
//! # Sibling skip (Tranche AV.2.2 contract preserved)
//!
//! `sib_skip[i]` holds the distance from record `i` to the root of
//! its next-emission sibling within the shared parent's children
//! run, or `0` if the record is the last sibling (or the root). The
//! distance is `next_sibling_root - i`; since the current tape is
//! laid out in post-order, `sib_skip` is not constant-1 — a leaf
//! followed by a compound sibling reports a distance equal to the
//! sibling subtree's size.
//!
//! Forward sibling traversal reads `sib_skip[i]` exactly once per
//! step: `i → i + sib_skip[i]`; iteration ends when the read is
//! zero.
//!
//! # Payload routing (AV.2.3 contract preserved)
//!
//! `PayloadData::InlineScalar(u32)` lands in `pay_narrow`; 8-byte
//! scalars (`WideScalar`) land in `pay_wide`; aggregates and byte
//! frames flow through `pay_agg`. In every case the record's
//! `child_off` slot holds the lookup pointer — a column rank into
//! `pay_narrow` / `pay_wide`, or the arena byte offset for arena-
//! backed payloads. Compound records use `child_off` for first-child
//! pointer, or `TapeOffset::NONE` for empty runs.
//!
//! # Typed-column reducer (AW-IV.W5.1, surface preserved)
//!
//! [`Columns::reduce_column`] is the generic consumer surface: a
//! compile-time column selector ([`ColumnTag`]) picks which payload
//! column to walk, a reducer ([`Reducer`]) supplies identity + fold +
//! combine, and the inner loop promotes to packed SIMD on the `f64`
//! lanes — `vaddq_f64` pairs on NEON (logical 4-lane accumulator
//! over two `float64x2_t` registers) and `_mm256_add_pd` on AVX2
//! (native `__m256d` 4-lane accumulator). Scalar tail handles
//! `n % 4`. The portable fallback uses a 4-lane reordered scalar
//! fold so the API stays portable.
//!
//! Module layout (B5.W3):
//! - [`scalar_payload`] — fused scalar-payload writers (f64 / u8 /
//!   bool / hex_u32 / i64) + stp_span + reduce_column consumer.
//! - [`column_tag`] — [`ColumnTag`] trait + 4 zero-sized impls
//!   (PayWideF64 / PayWideU64 / PayNarrowU32 / PayAggU8).
//! - [`reducer`] — [`Reducer`] trait + 6 impls (SumF64 / MinF64 /
//!   MaxF64 / SumU32 / SumU64 / Count).

use crate::kind::TapeKind;
use crate::tape::{TapeOffset, TapeRec};
use crate::value::{ValueCheckpoint, ValueFrame};

mod column_tag;
mod reducer;
mod scalar_payload;

pub use column_tag::{ColumnTag, PayAggU8, PayNarrowU32, PayWideF64, PayWideU64};
pub use reducer::{Count, MaxF64, MinF64, Reducer, SumF64, SumU32, SumU64};

/// Flat-AoS structural substrate.
///
/// `records` is the structural row column (16 B per record); `sib_skip`
/// is the post-finalise sibling-stride column (4 B per record); the
/// three `pay_*` columns hold typed payloads.
#[derive(Debug, Default)]
pub struct Columns {
    /// Flat AoS structural rows. Each entry is 16 bytes (kind_meta,
    /// flags, extra, span_lo, span_hi, child_off). One push per row.
    pub(crate) records: Vec<TapeRec>,

    /// Distance to the next-emission sibling within the parent's
    /// children run. `0` for the last sibling (and for the root).
    /// Stamped at [`FusedBuilder::finish`](crate::FusedBuilder::finish)
    /// time (AY.W1.2 fold pending).
    pub(crate) sib_skip: Vec<u32>,

    /// Per-record nesting depth column (B3.W0.γ + B3.W0.δ). One byte
    /// per structural record, stamped in lockstep with `records` /
    /// `sib_skip` on every push and rolled back atomically with them
    /// on retry. The Stage-C finaliser walks this column to derive
    /// sibling skips; emitters never read it.
    ///
    /// Lives inside `Columns` (not on the builder) because the parser-
    /// emitted retry paths bypass the builder's full rollback by
    /// calling `columns_mut().rollback_to(open)` directly. Owning
    /// `frame_depth` here guarantees rollback parity with `records`
    /// without requiring every parser site to thread the builder
    /// surface — the contract documented on `rollback_to` (every
    /// per-record column rewinds in lockstep) holds by construction.
    pub(crate) frame_depth: Vec<u8>,

    /// Currently-open compound depth — the depth at which the NEXT
    /// structural push will stamp its `frame_depth` byte. Bumped by
    /// [`crate::FusedBuilder::begin_compound`] (after stamping the
    /// compound row at the OUTER depth), decremented by
    /// [`crate::FusedBuilder::end_compound`] /
    /// [`crate::FusedBuilder::end_compound_post_order`].
    ///
    /// Lives inside `Columns` (alongside `frame_depth`, B3.W0.δ)
    /// because the parser-emitted retry paths bypass the builder's
    /// full rollback by calling `columns_mut().rollback_to(open)`
    /// directly. [`Self::rollback_to`] reads `frame_depth[open]`
    /// before truncation and restores `current_depth` to that
    /// value, so the next `begin_compound` re-opens at the correct
    /// depth.
    pub(crate) current_depth: u8,

    // ── Typed payload columns (dense-packed in push order) ─────────
    /// Inline scalars ≤ 4 B. The record's `child_off` holds the
    /// column rank.
    pub pay_narrow: Vec<u32>,
    /// 8-byte scalars — `u64` / `i64` / `f64`-bits / packed `Span`.
    /// Stored as raw bits; readers reinterpret via the typed cast.
    /// The record's `child_off` holds the column rank.
    ///
    /// B5.W2.4 — the pre-W2 split into `pay_wide` + `pay_f64` collapsed
    /// onto this single column. The Eisel-Lemire-decoded `f64` numeric
    /// leaves (AY.W4.2) write `f64::to_bits()` here directly and the
    /// record carries [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] so the
    /// reader projects via `f64::from_bits(pay_wide[rank])` rather
    /// than through the arena round-trip. The bit selects the
    /// interpretation (f64 vs u64/i64) at READ time; column selection
    /// is no longer load-bearing.
    pub pay_wide: Vec<u64>,
    /// Unified payload arena — aggregate tuple bytes, decoded
    /// strings, and byte-string frames all land here. Continues to
    /// back [`Tape::arena`](crate::Tape::arena) for external
    /// consumers that slice raw bytes.
    pub pay_agg: Vec<u8>,

    // ── B5.W1 — value-side substrate (promoted from FusedBuilder) ──
    //
    // Pre-B5.W1 these four fields lived on a separate `FusedBuilder`
    // type that welded the structural tape to a parallel value arena.
    // B5.W1 promotes them onto `Columns` directly so the substrate is
    // one type and `rollback_to` is the sole rollback primitive
    // covering both column families atomically.
    /// Nested value-frame arena — one entry per compound open + one
    /// per leaf push. Laid out in emitter push order; compounds
    /// reference their children via
    /// `(ValueFrame::first_child, ValueFrame::child_count)`.
    pub(crate) value_frames: Vec<ValueFrame>,
    /// Narrow-column scalar payloads (u32 / bool / u8). Indexed by
    /// `PayloadTag::narrow` rank.
    pub(crate) value_payloads_narrow: Vec<u32>,
    /// Wide-column scalar payloads (f64 / u64 / u32-pair). Indexed
    /// by `PayloadTag::wide` rank.
    pub(crate) value_payloads_wide: Vec<u64>,
    /// Open compound stack — one entry per `begin_compound` without
    /// a matching `end_compound`. Each entry carries the
    /// `ValueCheckpoint` recorded at open time so rollback truncates
    /// every column family to the pre-open state atomically.
    pub(crate) value_open_stack: Vec<ValueCheckpoint>,
}

impl Columns {
    /// Construct an empty column set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a column set sized for `expected` structural records.
    ///
    /// Only the structural columns + the AoS sidecar's underlying
    /// vectors pre-allocate; typed-payload columns are lazy, since
    /// most grammars exercise only a subset. Value-side columns
    /// (B5.W1) pre-allocate proportionally so the hot push path never
    /// trips a `Vec::push` realloc on corpus input: one frame per tape
    /// record worst-case, narrow / wide payloads at `expected / 4`.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            records: Vec::with_capacity(expected),
            sib_skip: Vec::with_capacity(expected),
            frame_depth: Vec::with_capacity(expected),
            current_depth: 0,
            pay_narrow: Vec::new(),
            pay_wide: Vec::new(),
            pay_agg: Vec::with_capacity(expected / 8 * 8),
            value_frames: Vec::with_capacity(expected),
            value_payloads_narrow: Vec::with_capacity(expected / 4),
            value_payloads_wide: Vec::with_capacity(expected / 4),
            value_open_stack: Vec::with_capacity(16),
        }
    }

    /// Number of structural records.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.records.len()
    }

    /// True iff no structural records have been appended.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }

    /// Direct reference to the flat AoS row column. Used by readers
    /// that want to slice the records dense (the `dedup` hash, the
    /// range-equality checks).
    #[inline(always)]
    pub(crate) fn records(&self) -> &[TapeRec] {
        &self.records
    }

    /// Mutable reference to the flat AoS row column. Used by the
    /// finaliser's back-patch pass (sib_skip + child_off + span_hi at
    /// tree-close time) and by the [`crate::driver::close_compound`]
    /// shape-emitter helper.
    #[inline(always)]
    pub(crate) fn records_mut(&mut self) -> &mut Vec<TapeRec> {
        &mut self.records
    }

    /// Rewind every column family — structural tape, inline
    /// `frame_depth`, and the value substrate — back to the state at
    /// the matching `begin_compound` whose `open_offset` the caller
    /// passes in.
    ///
    /// B5.W1: the sole rollback primitive across both column families.
    /// Pre-B5.W1 three rollback shapes coexisted (`FusedBuilder::rollback_to`,
    /// `Columns::rollback_to`, `Columns::truncate`); each documented
    /// itself as canonical. The substrate has one rollback semantic
    /// and B5.W1 collapses it onto this method.
    ///
    /// Tape-side: rewinds `records`, `sib_skip`, `frame_depth`, and
    /// restores `current_depth` to the depth recorded at `open_offset`
    /// (B3.W0.δ). Idempotent: calling with `open_offset >= len()` is
    /// a no-op.
    ///
    /// Value-side: pops every open-stack entry whose paired tape row
    /// lives at or above `open_offset`, truncating the value frame
    /// arena + narrow/wide payload columns to the deepest surviving
    /// checkpoint's pre-open state. The first survivor's
    /// `direct_child_count` is decremented once — the failed branch's
    /// outermost compound was a direct child of that survivor at the
    /// instant `begin_compound` ran — so a subsequent successful retry
    /// re-opens the same conceptual position without double-counting.
    ///
    /// Typed-payload columns (`pay_narrow`, `pay_wide`, `pay_agg`)
    /// are NOT rewound — the compound row itself never
    /// wrote into them, so they already match the state the caller
    /// observed at `begin_compound` time. Leaf-payload writes that
    /// landed after the open point are discarded along with their
    /// structural rows and the payload columns accumulate dead
    /// entries in those slots; that budget is the same one the
    /// pre-AY-II `columns_mut().truncate` convention accepted.
    #[inline]
    pub fn rollback_to(&mut self, open_offset: u32) {
        // Value-side first: pop every open frame whose paired tape
        // row landed at or above `open_offset`. The open-stack is a
        // faithful LIFO of `begin_compound` calls, monotonically
        // ordered by `tape_idx`, so the survivors form a strict
        // prefix and a single pop loop unwinds the entire failed
        // branch.
        let mut popped_any = false;
        let mut deepest_narrow = u32::MAX;
        let mut deepest_wide = u32::MAX;
        let mut deepest_frame = u32::MAX;
        while let Some(checkpoint) = self.value_open_stack.last().copied() {
            if checkpoint.tape_idx < open_offset {
                break;
            }
            // Track the deepest (lowest-index) surviving truncation
            // boundary across nested rolled-back compounds — the
            // outermost rolled-back compound was opened FIRST, so
            // ITS checkpoint carries the deepest pre-open state.
            deepest_frame = checkpoint.frame_offset;
            deepest_narrow = checkpoint.narrow_rank;
            deepest_wide = checkpoint.wide_rank;
            self.value_open_stack.pop();
            popped_any = true;
        }
        if popped_any {
            self.value_frames.truncate(deepest_frame as usize);
            self.value_payloads_narrow
                .truncate(deepest_narrow as usize);
            self.value_payloads_wide
                .truncate(deepest_wide as usize);
            // Decrement the survivor's `direct_child_count` once —
            // the outermost rolled-back compound was registered as a
            // direct child of the survivor when it opened. A
            // subsequent successful retry will re-open + close in the
            // same parent and bump the counter symmetrically.
            if let Some(parent) = self.value_open_stack.last_mut() {
                parent.direct_child_count =
                    parent.direct_child_count.saturating_sub(1);
            }
        }

        // Tape-side: rewind structural / depth columns.
        let new_len = open_offset as usize;
        if new_len >= self.records.len() {
            return;
        }
        // Recover `current_depth` from the row about to be discarded.
        // The compound at `open_offset` was emitted at its outer
        // frame's depth (B3.W0.γ — the row's `frame_depth` byte holds
        // the pre-bump value), so restoring `current_depth` to that
        // byte re-establishes the bookkeeping the next
        // `begin_compound` will consume.
        self.current_depth = self.frame_depth[new_len];
        self.records.truncate(new_len);
        self.sib_skip.truncate(new_len);
        self.frame_depth.truncate(new_len);
    }

    /// Run the Stage-C finaliser over `self`, reading the per-record
    /// `frame_depth` column and back-patching `sib_skip` / `child_off`
    /// / `span_hi` on every compound record. Wraps the disjoint-borrow
    /// dance the `FusedBuilder::finish` path needs (the finaliser
    /// signature takes a `&[u8]` for the depth slice alongside a
    /// `&mut Columns`).
    ///
    /// SAFETY: the depth slice aliases the heap buffer of the
    /// in-place `frame_depth` `Vec`. The finaliser's mutating writes
    /// touch only `records` / `sib_skip`; it never
    /// reads or writes `frame_depth`. So the slice and the `&mut
    /// Columns` borrow over `self` describe disjoint memory.
    #[inline(always)]
    pub(crate) fn run_finalise(&mut self) {
        let depth_slice: &[u8] = unsafe {
            let ptr = self.frame_depth.as_ptr();
            let len = self.frame_depth.len();
            std::slice::from_raw_parts(ptr, len)
        };
        crate::finaliser::finalise(self, depth_slice);
    }

    /// Append one structural row, returning the row's position.
    ///
    /// Caller supplies pre-packed `kind_meta` / `flags` bytes via
    /// [`TapeRec::pack_kind_meta`]. The `sib_skip` column is stamped
    /// `0`; it gets back-filled at finalise time.
    ///
    /// Two stores per call: one 16-byte `TapeRec` push + one 4-byte
    /// `sib_skip` push. `#[inline(always)]` so cross-crate emit-site
    /// calls collapse to inline stores in the bench binaries (W1
    /// hard gate 3, invariant 22).
    #[inline(always)]
    pub(crate) fn push_structural(
        &mut self,
        kind_meta: u8,
        flags: u8,
        extra: u16,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> u32 {
        let idx = self.records.len() as u32;
        self.records.push(TapeRec {
            kind_meta,
            flags,
            extra,
            span_lo,
            span_hi,
            child_off,
        });
        self.sib_skip.push(0);
        // B3.W0.δ — `frame_depth` is a per-record column owned by
        // `Columns`. Builders that wrap `push_structural` overwrite the
        // stamped byte with the live `current_depth`; direct callers
        // (test-only `push_compound_fused` / `push_leaf_fused`) accept
        // the default-0 stamp, matching the depth a freshly-built
        // `Columns` would assign at the root.
        self.frame_depth.push(self.current_depth);
        // AX.W1.D — primary mutation invalidates the AoS sidecar.
        idx
    }

    /// Materialise a 16-byte [`TapeRec`] view of row `i`.
    ///
    /// Post-AY.W1.1 the records ARE the AoS row, so this is a single
    /// `Copy` indexed read — same shape pre-AV consumers consumed.
    #[inline(always)]
    pub fn materialize(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.records.len(), "materialize: idx {} out of range", i);
        self.records[idx]
    }

    /// Unchecked variant of [`Self::materialize`].
    ///
    /// # Safety
    ///
    /// The caller must guarantee `i < self.len()`. The `TapeCursor`
    /// honours this via `TapeOffset`s produced by the builder.
    #[inline(always)]
    pub unsafe fn materialize_unchecked(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.records.len(), "materialize_unchecked: idx {} out of range", i);
        // SAFETY: caller guarantees idx < self.records.len().
        unsafe { *self.records.get_unchecked(idx) }
    }

    /// Read a record's `TapeKind` directly without materialising the
    /// whole 16-byte view (cheap-cheap one-byte projection).
    #[inline(always)]
    pub fn kind_at(&self, i: u32) -> TapeKind {
        TapeKind::from_u8(self.records[i as usize].kind_meta & 0x0F)
    }

    /// Read a record's `has_children` bit. Migrated from `flags[6]`
    /// in AW-III.W1.A alongside the `variant_idx` widening.
    #[inline(always)]
    pub fn has_children_at(&self, i: u32) -> bool {
        (self.records[i as usize].extra & TapeRec::HAS_CHILDREN_BIT) != 0
    }

    /// Read a record's `child_off` directly.
    #[inline(always)]
    pub fn child_off_at(&self, i: u32) -> TapeOffset {
        self.records[i as usize].child_off
    }

    /// Read a record's `sib_skip` directly.
    #[inline(always)]
    pub fn sib_skip_at(&self, i: u32) -> u32 {
        self.sib_skip[i as usize]
    }

    /// Read a record's `span_lo` / `span_hi` pair directly.
    #[inline(always)]
    pub fn span_at(&self, i: u32) -> (u32, u32) {
        let rec = self.records[i as usize];
        (rec.span_lo, rec.span_hi)
    }

    /// Read a record's `flags` byte (full 8-bit `variant_idx`) directly.
    #[inline(always)]
    pub fn flags_at(&self, i: u32) -> u8 {
        self.records[i as usize].flags
    }

    /// AY.W4.2 — read a numeric `f64`-bits payload from the unified
    /// `pay_wide` column at `idx`. Caller is responsible for having
    /// stamped the leaf via [`crate::Tape::push_leaf_with_f64_direct`]
    /// (which sets [`crate::TapeRec::PAYLOAD_F64_DIRECT_BIT`] and
    /// writes the column rank into `child_off`).
    ///
    /// B5.W2.4 — `pay_f64` collapsed onto `pay_wide`; the bit on
    /// `extra` selects f64-vs-u64 interpretation at read time. The
    /// accessor stays as a named entry point so the number-shape
    /// emitter's read site documents intent without re-asserting the
    /// bit lookup.
    #[inline(always)]
    pub fn pay_f64_at(&self, idx: usize) -> u64 {
        debug_assert!(
            idx < self.pay_wide.len(),
            "pay_f64_at: idx {} out of range (pay_wide len {})",
            idx,
            self.pay_wide.len(),
        );
        self.pay_wide[idx]
    }

    /// Read a record's `extra` packed flag word directly.
    #[inline(always)]
    pub fn extra_at(&self, i: u32) -> u16 {
        self.records[i as usize].extra
    }

    /// Read a record's `span_lo` directly.
    #[inline(always)]
    pub fn span_lo_at(&self, i: u32) -> u32 {
        self.records[i as usize].span_lo
    }

    /// Read a record's `span_hi` directly.
    #[inline(always)]
    pub fn span_hi_at(&self, i: u32) -> u32 {
        self.records[i as usize].span_hi
    }

    /// Set `sib_skip[i]` directly. Used by the finaliser back-patch.
    /// Invalidates the AoS sidecar so the next read re-transposes.
    #[inline(always)]
    pub fn set_sib_skip_at(&mut self, i: u32, value: u32) {
        self.sib_skip[i as usize] = value;
    }

    /// Set `child_off` on row `i` directly. Used by `close_compound`
    /// back-patch.
    #[inline(always)]
    pub fn set_child_off_at(&mut self, i: u32, value: TapeOffset) {
        self.records[i as usize].child_off = value;
    }

    /// Set `span_hi` on row `i` directly. Used by `close_compound`
    /// back-patch.
    #[inline(always)]
    pub fn set_span_hi_at(&mut self, i: u32, value: u32) {
        self.records[i as usize].span_hi = value;
    }

    /// OR bits into `extra` on row `i`. Used by `close_compound` to
    /// stamp `HAS_CHILDREN_BIT` once the children have been emitted.
    #[inline(always)]
    pub fn or_extra_at(&mut self, i: u32, mask: u16) {
        self.records[i as usize].extra |= mask;
    }

    /// AY.W1.1 — flat-AoS compound-row push.
    ///
    /// Single 16-byte `TapeRec` store + single 4-byte `sib_skip`
    /// push (LLVM sees two pushes per call). The hot path is a
    /// single predicted-not-taken capacity branch when callers pre-
    /// allocate via `GRAMMAR_PROFILE.capacity_for(input.len())`.
    ///
    /// Returns the row's index.
    #[inline(always)]
    pub fn push_compound_fused(&mut self, kind: TapeKind, span_lo: u32) -> u32 {
        let kind_meta = (kind as u8) & 0x0F;
        self.push_structural(
            kind_meta,
            0,
            0,
            span_lo,
            span_lo, // provisional span_hi; back-patched at close_compound
            TapeOffset::NONE,
        )
    }

    /// AY.W1.1 — flat-AoS leaf-row push.
    ///
    /// Single 16-byte `TapeRec` store + single 4-byte `sib_skip`
    /// push. Returns the row's index.
    #[inline(always)]
    pub fn push_leaf_fused(
        &mut self,
        kind: TapeKind,
        flags: u8,
        extra: u16,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> u32 {
        let kind_meta = (kind as u8) & 0x0F;
        self.push_structural(kind_meta, flags, extra, span_lo, span_hi, child_off)
    }

    /// Reserve at least one additional slot. Pre-AY this called a
    /// `#[cold]` `grow_all` chain that doubled seven `Vec`s in
    /// lockstep; post-AY the work collapses to two `Vec::reserve(1)`
    /// calls. Kept as a `pub` surface for cold-path replay tests.
    #[cold]
    #[inline(never)]
    pub fn grow_all(&mut self) {
        self.records.reserve(1);
        self.sib_skip.reserve(1);
    }

    /// Cold-path capacity guard. Same semantics as `grow_all` —
    /// reserves one extra slot on `records` + `sib_skip` if the
    /// current minimum capacity is exhausted.
    #[cold]
    #[inline(never)]
    pub fn reserve_one_cold(&mut self) {
        if self.records.len() >= self.records.capacity() {
            self.grow_all();
        }
    }
}
