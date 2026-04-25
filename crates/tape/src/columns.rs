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
//! - **`packed_cache: OnceLock<Vec<PackedRecord>>`** — the AoS read-
//!   side sidecar (W1.D). Source of truth changes from the SoA
//!   columns to `records` + `sib_skip`; transpose becomes near-
//!   identity (copy + 32-byte align). Invalidated per push.
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

use crate::kind::TapeKind;
use crate::packed::PackedRecord;
use crate::tape::{TapeOffset, TapeRec};

use std::sync::OnceLock;

/// Flat-AoS structural substrate.
///
/// `records` is the structural row column (16 B per record); `sib_skip`
/// is the post-finalise sibling-stride column (4 B per record); the
/// three `pay_*` columns hold typed payloads. The lazy AoS sidecar
/// (`packed_cache`) is populated on first random-access read.
#[derive(Debug, Default)]
pub struct Columns {
    /// Flat AoS structural rows. Each entry is 16 bytes (kind_meta,
    /// flags, extra, span_lo, span_hi, child_off). One push per row.
    pub(crate) records: Vec<TapeRec>,

    /// Distance to the next-emission sibling within the parent's
    /// children run. `0` for the last sibling (and for the root).
    /// Stamped at [`TapeBuilder::finish`](crate::TapeBuilder::finish)
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
    /// [`crate::TapeBuilder::begin_compound`] (after stamping the
    /// compound row at the OUTER depth), decremented by
    /// [`crate::TapeBuilder::end_compound`] /
    /// [`crate::TapeBuilder::end_compound_post_order`].
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
    /// 8-byte scalars (`u64` / `i64` / packed `Span`). Stored as raw
    /// bits; readers reinterpret via the typed cast. The record's
    /// `child_off` holds the column rank.
    ///
    /// Numeric `f64` leaves whose value comes through the Eisel-Lemire
    /// fast path (AY.W4.2) bypass this column entirely — they land in
    /// [`Self::pay_f64`] and the record carries
    /// [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] so the reader knows which
    /// column to consult.
    pub pay_wide: Vec<u64>,
    /// AY.W4.2 — direct-write column for Eisel-Lemire-decoded `f64`
    /// numeric leaves. Bypasses the arena and the generic `pay_wide`
    /// `PayloadData::WideScalar` round-trip: the number-shape emitter
    /// writes `f64::to_bits()` straight into this column and stamps
    /// the record's `child_off` with the column rank +
    /// [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] so the reader hits this
    /// column directly. Saves one load + one store per number literal
    /// on heavy-numeric fixtures (canada).
    pub pay_f64: Vec<u64>,
    /// Unified payload arena — aggregate tuple bytes, decoded
    /// strings, and byte-string frames all land here. Continues to
    /// back [`Tape::arena`](crate::Tape::arena) for external
    /// consumers that slice raw bytes.
    pub pay_agg: Vec<u8>,

    // ── AX.W1.D — AoS sidecar for hybrid random-access reads ──────
    /// Lazy AoS projection. Populated on first call to
    /// [`Self::packed_cache`]; invalidated by every mutating call
    /// that touches a structural column.
    ///
    /// Post-AY.W1.1 the source of truth is `records` directly — the
    /// transpose is near-identity: copy each [`TapeRec`] into a
    /// 32-byte aligned [`PackedRecord`] paired with its `sib_skip`.
    packed_cache: OnceLock<Vec<PackedRecord>>,
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
    /// most grammars exercise only a subset.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            records: Vec::with_capacity(expected),
            sib_skip: Vec::with_capacity(expected),
            frame_depth: Vec::with_capacity(expected),
            current_depth: 0,
            pay_narrow: Vec::new(),
            pay_wide: Vec::new(),
            pay_f64: Vec::new(),
            pay_agg: Vec::with_capacity(expected / 8 * 8),
            packed_cache: OnceLock::new(),
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
    /// `packed_cache` transpose, range-equality checks).
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

    /// Truncate the structural columns to `new_len` records. Typed-
    /// payload columns are NOT truncated here — they are written
    /// independently and a backtracking caller must manage them
    /// separately if it wants to discard partial payload writes.
    ///
    /// Used by the DTA walker's `AltLinear` backtracking to discard
    /// structural rows pushed by a failed branch before probing the
    /// next one.
    #[inline]
    pub fn truncate(&mut self, new_len: usize) {
        if new_len < self.frame_depth.len() {
            // Mirror `rollback_to`: restore `current_depth` to the
            // depth of the row at `new_len` before truncation, so
            // subsequent pushes stamp at the right level.
            self.current_depth = self.frame_depth[new_len];
        }
        self.records.truncate(new_len);
        self.sib_skip.truncate(new_len);
        self.frame_depth.truncate(new_len);
        self.invalidate_packed();
    }

    /// Rewind the columnar substrate back to `open_offset` structural
    /// slots.
    ///
    /// This is the canonical rollback primitive emitters call when an
    /// emitter retry-IIFE discards everything pushed after a
    /// [`TapeBuilder::begin_compound`](crate::TapeBuilder::begin_compound)
    /// open point. AY-II.W0.a introduced it in place of the ad-hoc
    /// `columns_mut().truncate(save)` incantation that every retry
    /// site had evolved into — that primitive only touched `records`
    /// + `sib_skip` and left the AoS sidecar stale; this one owns
    /// rewinding every per-record column in lockstep.
    ///
    /// Idempotent: calling with `open_offset >= len()` is a no-op
    /// (nothing to rewind). Callers never pass a value that would
    /// extend the columns.
    ///
    /// Typed-payload columns (`pay_narrow`, `pay_wide`, `pay_f64`,
    /// `pay_agg`) are NOT rewound — the compound row itself never
    /// wrote into them, so they already match the state the caller
    /// observed at `begin_compound` time. Leaf-payload writes that
    /// landed after the open point are discarded along with their
    /// structural rows and the payload columns accumulate dead
    /// entries in those slots; that budget is the same one the
    /// pre-AY-II `columns_mut().truncate` convention accepted.
    #[inline]
    pub fn rollback_to(&mut self, open_offset: u32) {
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
        self.invalidate_packed();
    }

    /// Run the Stage-C finaliser over `self`, reading the per-record
    /// `frame_depth` column and back-patching `sib_skip` / `child_off`
    /// / `span_hi` on every compound record. Wraps the disjoint-borrow
    /// dance the `TapeBuilder::finish` path needs (the finaliser
    /// signature takes a `&[u8]` for the depth slice alongside a
    /// `&mut Columns`).
    ///
    /// SAFETY: the depth slice aliases the heap buffer of the
    /// in-place `frame_depth` `Vec`. The finaliser's mutating writes
    /// touch only `records` / `sib_skip` / `packed_cache`; it never
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

    /// Split-borrow accessor: hand back `(&mut Columns, &mut Vec<u8>)`
    /// where the first borrow ranges over the `Columns` fields the
    /// per-grammar specialised walker mutates (records, sib_skip,
    /// payload columns) and the second projects into the depth
    /// column. Mirrors the pre-W0.δ shape `TapeBuilder` exposed
    /// when `frame_depth` lived on the builder directly.
    ///
    /// SAFETY: callers must not mutate `frame_depth` through the
    /// returned `&mut Columns` for the lifetime of the returned
    /// `&mut Vec<u8>`. Every emitted walker this call services
    /// pushes structural rows into `records` and depth bytes into the
    /// `Vec<u8>`; neither path reaches `frame_depth` through the
    /// `Columns` handle.
    #[inline]
    pub(crate) fn split_off_frame_depth_mut(
        &mut self,
    ) -> (&mut Columns, &mut Vec<u8>) {
        let cols_ptr: *mut Columns = self;
        unsafe {
            let depth: &mut Vec<u8> = &mut (*cols_ptr).frame_depth;
            (&mut *cols_ptr, depth)
        }
    }

    // ── AX.W1.D — AoS sidecar (`packed_cache`) readers/invalidators ──

    /// Get (populating on first call) the AoS sidecar view.
    ///
    /// The first call transposes `records` + `sib_skip` into a dense
    /// `Vec<PackedRecord>` — O(n) one-time cost. Subsequent calls
    /// return the cached slice without re-transposing.
    ///
    /// The cache is invalidated whenever the structural primary is
    /// mutated (any `push_*` or `truncate`); the next call re-
    /// transposes the updated state.
    ///
    /// Uses [`OnceLock::get_or_init`], so concurrent readers across
    /// threads observe the same immutable transpose without racing.
    /// The populate closure captures `self` by `&` so the transpose
    /// reads the columns directly; no extra clone.
    #[inline]
    pub fn packed_cache(&self) -> &[PackedRecord] {
        self.packed_cache
            .get_or_init(|| self.transpose_to_packed())
            .as_slice()
    }

    /// `None` if the AoS sidecar is not currently populated; `Some`
    /// if a prior read has materialised it and no subsequent write
    /// has invalidated it. Exposed so consumers wanting to probe
    /// whether a hot path exercised the sidecar — e.g. the AX.W1.7
    /// Twitter lazy-field bench — can assert the contract without
    /// forcing the transpose.
    #[inline]
    pub fn packed_cache_populated(&self) -> bool {
        self.packed_cache.get().is_some()
    }

    /// Invalidate the AoS sidecar. Called by every mutation that
    /// touches a structural column; the next `packed_cache()` call
    /// re-transposes from the (updated) primary.
    ///
    /// Idempotent: invalidating an already-empty cache is a no-op.
    /// `#[inline(always)]` because the hot-path push sites call this
    /// once per row — the body compiles down to a single
    /// `OnceLock::take` which is a conditional pointer-write.
    #[inline(always)]
    pub fn invalidate_packed(&mut self) {
        let _ = self.packed_cache.take();
    }

    /// Transpose `records` + `sib_skip` into a fresh AoS `Vec`.
    /// Internal helper for [`Self::packed_cache`]; kept at module
    /// scope so tests can call it directly without going through the
    /// lock.
    fn transpose_to_packed(&self) -> Vec<PackedRecord> {
        let n = self.records.len();
        debug_assert_eq!(self.sib_skip.len(), n, "sib_skip length mismatch");
        let mut out: Vec<PackedRecord> = Vec::with_capacity(n);
        for i in 0..n {
            let rec = self.records[i];
            out.push(PackedRecord {
                kind_meta: rec.kind_meta,
                flags: rec.flags,
                extra: rec.extra,
                span_lo: rec.span_lo,
                span_hi: rec.span_hi,
                child_off: rec.child_off,
                sib_skip: self.sib_skip[i],
                _pad: [0u8; 12],
            });
        }
        out
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
        self.invalidate_packed();
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

    /// AY.W4.2 — read a numeric `f64` payload from the dedicated
    /// direct-write column at `idx`. Caller is responsible for having
    /// stamped the leaf via [`crate::TapeBuilder::push_leaf_with_f64_direct`]
    /// (which sets [`crate::TapeRec::PAYLOAD_F64_DIRECT_BIT`] and writes
    /// the column rank into `child_off`).
    #[inline(always)]
    pub fn pay_f64_at(&self, idx: usize) -> u64 {
        debug_assert!(
            idx < self.pay_f64.len(),
            "pay_f64_at: idx {} out of range (pay_f64 len {})",
            idx,
            self.pay_f64.len(),
        );
        self.pay_f64[idx]
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
        self.invalidate_packed();
    }

    /// Set `child_off` on row `i` directly. Used by `close_compound`
    /// back-patch.
    #[inline(always)]
    pub fn set_child_off_at(&mut self, i: u32, value: TapeOffset) {
        self.records[i as usize].child_off = value;
        self.invalidate_packed();
    }

    /// Set `span_hi` on row `i` directly. Used by `close_compound`
    /// back-patch.
    #[inline(always)]
    pub fn set_span_hi_at(&mut self, i: u32, value: u32) {
        self.records[i as usize].span_hi = value;
        self.invalidate_packed();
    }

    /// OR bits into `extra` on row `i`. Used by `close_compound` to
    /// stamp `HAS_CHILDREN_BIT` once the children have been emitted.
    #[inline(always)]
    pub fn or_extra_at(&mut self, i: u32, mask: u16) {
        self.records[i as usize].extra |= mask;
        self.invalidate_packed();
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

    // ── AW-V.W1.3 — scalar-payload direct-write fused API ────────────
    //
    // Per AW-V.md §W1.3 + B2 §3: the per-shape emitter's leaf-emission
    // arms know the scalar payload statically; the hot path writes the
    // decoded value directly into `pay_agg` alongside the structural
    // slot, bypassing PSI's Stage-A `push` / Stage-B rayon fan-out.
    // Kept verbatim post-AY.W1.1 — the structural-row push internally
    // routes through `push_leaf_fused` (now flat-AoS) but the surface
    // is unchanged.

    /// Write a scalar `f64` payload into `pay_agg` at `child_off` (8 B
    /// little-endian via `f64::to_bits().to_le_bytes()`) and append the
    /// structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_f64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: f64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_f64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_bits().to_le_bytes();
        // SAFETY: the `debug_assert!` above (enforced in debug builds;
        // the emitter's monotonic arena-cursor pre-condition in release
        // builds) guarantees the 8-byte range `[dst_off, dst_off+8)` is
        // in-bounds of `pay_agg`'s initialised region.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `u8` payload into `pay_agg` at `child_off` (1 B)
    /// and append the structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_u8(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u8,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_u8: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `bool` payload into `pay_agg` at `child_off`
    /// (1 B — `0` for `false`, `1` for `true`) and append the
    /// structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_bool(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: bool,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_bool: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value as u8;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar hex `u32` payload into `pay_agg` at `child_off`
    /// (4 B little-endian) and append the structural slot for the
    /// leaf record. CSS hex colours pass the pre-decoded `#rrggbbaa`
    /// u32 from the emitter's `parse_hex_u32` inline body.
    #[inline(always)]
    pub fn push_scalar_payload_hex_u32(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u32,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 4 <= self.pay_agg.len(),
            "push_scalar_payload_hex_u32: arena offset {} + 4 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                4,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `i64` payload into `pay_agg` at `child_off`
    /// (8 B little-endian via `(value as u64).to_le_bytes()`) and
    /// append the structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_i64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: i64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_i64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = (value as u64).to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// AY.W1.1 — paired-column span write degenerates to a single
    /// AoS field write on the flat substrate.
    ///
    /// Pre-AY this routed through inline-asm-pinned adjacent stores
    /// targeting two distinct `Vec<u32>` allocations to coax LLVM /
    /// the M-series Firestorm front-end into macro-op fusion. Post-
    /// AY both span endpoints live in the same 16-byte `TapeRec`;
    /// the writer is one indexed AoS field-update + one
    /// `invalidate_packed` call. Same semantics, simpler code.
    #[inline(always)]
    pub fn stp_span(&mut self, idx: usize, span_lo_val: u32, span_hi_val: u32) {
        debug_assert!(
            idx < self.records.len(),
            "stp_span: idx {} out of range (records len {})",
            idx,
            self.records.len(),
        );
        let rec = &mut self.records[idx];
        rec.span_lo = span_lo_val;
        rec.span_hi = span_hi_val;
        self.invalidate_packed();
    }

    // ── AW-IV.W5.1 — reduce_column<C, R> consumer API ────────────────
    //
    // The generic consumer surface over the three typed payload
    // columns (`pay_narrow`, `pay_wide`, `pay_agg`). Surface preserved
    // verbatim post-AY.W1.1 — the column tags + reducers are
    // orthogonal to the structural layout revert.

    /// Reduce a single payload column via a compile-time-selected
    /// fold.
    #[inline]
    pub fn reduce_column<C, R>(&self) -> R::Acc
    where
        C: ColumnTag,
        R: Reducer<C::Value>,
    {
        R::reduce_slice(C::column(self))
    }
}

// ─────────────────────────────────────────────────────────────────────
// ColumnTag — compile-time column selector
// ─────────────────────────────────────────────────────────────────────

/// Compile-time selector for a typed payload column on [`Columns`].
///
/// Implementors are zero-sized marker types — [`PayWideF64`],
/// [`PayWideU64`], [`PayNarrowU32`], [`PayAggU8`] — that bind a column
/// identity to an element type. [`Columns::reduce_column`] takes a
/// `ColumnTag` as its `C` type parameter; the emitter passes the tag
/// matching each active payload column per grammar.
pub trait ColumnTag {
    /// Element type of this column — the scalar the reducer folds over.
    type Value: Copy;
    /// Project the column out of `Columns` as a `&[Self::Value]`.
    fn column(cols: &Columns) -> &[Self::Value];
}

/// Column tag for `pay_wide` interpreted as `f64` (canonical
/// numeric-leaf payload column — JSON numbers, CSS dimensions, Sheets
/// numbers). 8-byte column entries reinterpret-cast via
/// [`f64::from_bits`] against the stored `u64` bits.
pub struct PayWideF64;

impl ColumnTag for PayWideF64 {
    type Value = f64;

    #[inline]
    fn column(cols: &Columns) -> &[f64] {
        // SAFETY: `u64` and `f64` have the same size + alignment;
        // reinterpretation is defined behaviour. `self.pay_wide` is
        // never uninitialised — every entry was populated by the
        // payload-writer before any reader could observe the slice.
        unsafe {
            core::slice::from_raw_parts(
                cols.pay_wide.as_ptr() as *const f64,
                cols.pay_wide.len(),
            )
        }
    }
}

/// Column tag for `pay_wide` interpreted as `u64` (packed integer
/// leaves, timestamps, raw 8-byte scalars).
pub struct PayWideU64;

impl ColumnTag for PayWideU64 {
    type Value = u64;

    #[inline]
    fn column(cols: &Columns) -> &[u64] {
        &cols.pay_wide
    }
}

/// Column tag for `pay_narrow` — 4-byte inline scalars (`u32`, `u16`,
/// `u8`, `bool`, widened unit enums). The column is stored as
/// `Vec<u32>` and the reducer reads it as `&[u32]`.
pub struct PayNarrowU32;

impl ColumnTag for PayNarrowU32 {
    type Value = u32;

    #[inline]
    fn column(cols: &Columns) -> &[u32] {
        &cols.pay_narrow
    }
}

/// Column tag for `pay_agg` — the arena byte column. Useful for
/// checksum-style reductions (`count_bytes`, `sum_bytes`, `xor_bytes`)
/// over the byte-addressable arena.
pub struct PayAggU8;

impl ColumnTag for PayAggU8 {
    type Value = u8;

    #[inline]
    fn column(cols: &Columns) -> &[u8] {
        &cols.pay_agg
    }
}

// ─────────────────────────────────────────────────────────────────────
// Reducer — identity + fold + combine + slice driver
// ─────────────────────────────────────────────────────────────────────

/// A reduction operator over `&[T]`.
///
/// Captures the four facts the driver needs to fold a column into a
/// single accumulator:
///
/// 1. [`Acc`](Reducer::Acc) — the accumulator type (usually `T`;
///    sometimes wider, e.g. `usize` for `Count`).
/// 2. [`IDENT`](Reducer::IDENT) — the identity value (`0.0` for Sum,
///    `T::MAX` for Min, `T::MIN` for Max, `0` for Count).
/// 3. [`fold`](Reducer::fold) — the binary fold step.
/// 4. [`combine`](Reducer::combine) — the horizontal reduce.
/// 5. [`reduce_slice`](Reducer::reduce_slice) — the slice driver;
///    default implementation is the 4-lane reordered scalar fold,
///    overridable per `(Reducer, T)` pair.
pub trait Reducer<T: Copy> {
    /// The accumulator type.
    type Acc: Copy;
    /// The identity value — the accumulator's starting state.
    const IDENT: Self::Acc;
    /// Fold one element into a lane accumulator.
    fn fold(acc: Self::Acc, x: T) -> Self::Acc;
    /// Combine two lane accumulators horizontally.
    fn combine(a: Self::Acc, b: Self::Acc) -> Self::Acc;

    /// Drive the reducer over a slice. Default implementation is the
    /// 4-lane reordered scalar fold.
    #[inline]
    fn reduce_slice(col: &[T]) -> Self::Acc {
        let n = col.len();
        let mut acc: [Self::Acc; 4] = [Self::IDENT; 4];
        let mut i = 0usize;
        while i + 4 <= n {
            acc[0] = Self::fold(acc[0], col[i]);
            acc[1] = Self::fold(acc[1], col[i + 1]);
            acc[2] = Self::fold(acc[2], col[i + 2]);
            acc[3] = Self::fold(acc[3], col[i + 3]);
            i += 4;
        }
        let mut tail = Self::combine(
            Self::combine(acc[0], acc[1]),
            Self::combine(acc[2], acc[3]),
        );
        while i < n {
            tail = Self::fold(tail, col[i]);
            i += 1;
        }
        tail
    }
}

/// Sum reducer over `f64`. Identity `0.0`; fold `+`; combine `+`.
///
/// Overrides [`Reducer::reduce_slice`] to invoke the packed-SIMD
/// kernel in [`simd::sum_f64`] — `vaddq_f64` pairs on NEON,
/// `_mm256_add_pd` on AVX2, 4-lane reordered scalar fold otherwise.
pub struct SumF64;

impl Reducer<f64> for SumF64 {
    type Acc = f64;
    const IDENT: f64 = 0.0;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc + x
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a + b
    }

    #[inline]
    fn reduce_slice(col: &[f64]) -> f64 {
        simd::sum_f64(col)
    }
}

/// Min reducer over `f64`. Identity `f64::INFINITY`; fold `f64::min`;
/// combine `f64::min`.
pub struct MinF64;

impl Reducer<f64> for MinF64 {
    type Acc = f64;
    const IDENT: f64 = f64::INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.min(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.min(b)
    }
}

/// Max reducer over `f64`. Identity `f64::NEG_INFINITY`; fold
/// `f64::max`; combine `f64::max`.
pub struct MaxF64;

impl Reducer<f64> for MaxF64 {
    type Acc = f64;
    const IDENT: f64 = f64::NEG_INFINITY;

    #[inline]
    fn fold(acc: f64, x: f64) -> f64 {
        acc.max(x)
    }

    #[inline]
    fn combine(a: f64, b: f64) -> f64 {
        a.max(b)
    }
}

/// Sum reducer over `u32`. Wrapping arithmetic so overflow is
/// saturating-defined rather than a panic site.
pub struct SumU32;

impl Reducer<u32> for SumU32 {
    type Acc = u32;
    const IDENT: u32 = 0;

    #[inline]
    fn fold(acc: u32, x: u32) -> u32 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u32, b: u32) -> u32 {
        a.wrapping_add(b)
    }
}

/// Sum reducer over `u64`. Wrapping arithmetic.
pub struct SumU64;

impl Reducer<u64> for SumU64 {
    type Acc = u64;
    const IDENT: u64 = 0;

    #[inline]
    fn fold(acc: u64, x: u64) -> u64 {
        acc.wrapping_add(x)
    }

    #[inline]
    fn combine(a: u64, b: u64) -> u64 {
        a.wrapping_add(b)
    }
}

/// Count reducer. Collapses to `col.len()` — O(1) on every column
/// since the `Vec<T>` carries its length.
pub struct Count;

impl<T: Copy> Reducer<T> for Count {
    type Acc = usize;
    const IDENT: usize = 0;

    #[inline]
    fn fold(acc: usize, _x: T) -> usize {
        acc + 1
    }

    #[inline]
    fn combine(a: usize, b: usize) -> usize {
        a + b
    }

    /// O(1) specialisation — `col.len()`.
    #[inline]
    fn reduce_slice(col: &[T]) -> usize {
        col.len()
    }
}

// ─────────────────────────────────────────────────────────────────────
// simd — packed-SIMD kernels for the hot reducer paths
// ─────────────────────────────────────────────────────────────────────

/// Packed-SIMD kernels for the [`Reducer`] hot paths.
///
/// The entry points dispatch to per-arch SIMD (NEON `vaddq_f64` pairs
/// on aarch64, AVX2 `_mm256_add_pd` on x86_64) or the portable 4-lane
/// reordered scalar fold on other targets. The public API is
/// arch-agnostic; callers use [`Reducer::reduce_slice`] which resolves
/// to the correct kernel at monomorphisation time.
pub mod simd {
    /// Packed-SIMD f64 sum.
    #[inline]
    pub fn sum_f64(col: &[f64]) -> f64 {
        #[cfg(target_arch = "aarch64")]
        {
            sum_f64_neon(col)
        }
        #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
        {
            // SAFETY: the cfg above gates the call on compile-time
            // `target_feature = "avx2"`.
            unsafe { sum_f64_avx2(col) }
        }
        #[cfg(not(any(
            target_arch = "aarch64",
            all(target_arch = "x86_64", target_feature = "avx2"),
        )))]
        {
            sum_f64_scalar_4lane(col)
        }
    }

    /// NEON kernel.
    #[cfg(target_arch = "aarch64")]
    #[inline]
    fn sum_f64_neon(col: &[f64]) -> f64 {
        use core::arch::aarch64::*;
        let n = col.len();
        // SAFETY: NEON intrinsics require aarch64 + neon, both of which
        // are gated by `#[cfg(target_arch = "aarch64")]` + NEON being
        // a baseline feature of aarch64-*-* targets.
        unsafe {
            let ptr = col.as_ptr();
            let mut acc0 = vdupq_n_f64(0.0);
            let mut acc1 = vdupq_n_f64(0.0);
            let mut acc2 = vdupq_n_f64(0.0);
            let mut acc3 = vdupq_n_f64(0.0);
            let mut i = 0usize;
            while i + 8 <= n {
                let v0 = vld1q_f64(ptr.add(i));
                let v1 = vld1q_f64(ptr.add(i + 2));
                let v2 = vld1q_f64(ptr.add(i + 4));
                let v3 = vld1q_f64(ptr.add(i + 6));
                acc0 = vaddq_f64(acc0, v0);
                acc1 = vaddq_f64(acc1, v1);
                acc2 = vaddq_f64(acc2, v2);
                acc3 = vaddq_f64(acc3, v3);
                i += 8;
            }
            let merged_lo = vaddq_f64(acc0, acc1);
            let merged_hi = vaddq_f64(acc2, acc3);
            let merged = vaddq_f64(merged_lo, merged_hi);
            let mut tail = vgetq_lane_f64(merged, 0) + vgetq_lane_f64(merged, 1);
            while i < n {
                tail += *ptr.add(i);
                i += 1;
            }
            tail
        }
    }

    /// AVX2 kernel.
    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn sum_f64_avx2(col: &[f64]) -> f64 {
        use core::arch::x86_64::*;
        let n = col.len();
        let ptr = col.as_ptr();
        let mut acc0 = _mm256_setzero_pd();
        let mut acc1 = _mm256_setzero_pd();
        let mut i = 0usize;
        while i + 8 <= n {
            let v0 = _mm256_loadu_pd(ptr.add(i));
            let v1 = _mm256_loadu_pd(ptr.add(i + 4));
            acc0 = _mm256_add_pd(acc0, v0);
            acc1 = _mm256_add_pd(acc1, v1);
            i += 8;
        }
        let merged = _mm256_add_pd(acc0, acc1);
        let hi = _mm256_extractf128_pd::<1>(merged);
        let lo = _mm256_castpd256_pd128(merged);
        let sum2 = _mm_add_pd(lo, hi);
        let shuf = _mm_unpackhi_pd(sum2, sum2);
        let reduced = _mm_add_sd(sum2, shuf);
        let mut tail = _mm_cvtsd_f64(reduced);
        while i < n {
            tail += *ptr.add(i);
            i += 1;
        }
        tail
    }

    /// Reordered-scalar 4-lane fold for targets without NEON / AVX2.
    #[cfg(not(any(
        target_arch = "aarch64",
        all(target_arch = "x86_64", target_feature = "avx2"),
    )))]
    #[inline]
    fn sum_f64_scalar_4lane(col: &[f64]) -> f64 {
        let n = col.len();
        let mut acc: [f64; 4] = [0.0; 4];
        let mut i = 0usize;
        while i + 4 <= n {
            acc[0] += col[i];
            acc[1] += col[i + 1];
            acc[2] += col[i + 2];
            acc[3] += col[i + 3];
            i += 4;
        }
        let mut tail = acc[0] + acc[1] + acc[2] + acc[3];
        while i < n {
            tail += col[i];
            i += 1;
        }
        tail
    }
}
