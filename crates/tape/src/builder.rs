//! `TapeBuilder` — the parser's write interface to the [`Tape`].
//!
//! The generated Rust parser calls `TapeBuilder::push_*` methods to
//! append records as each rule / Seq / Alt matches. The builder owns
//! the growing [`Tape`] plus sticky error state so failed sub-tree
//! matches don't poison the rest of the parse.
//!
//! Tranche AV.2.1 flipped the underlying storage from a flat
//! `Vec<TapeRec>` to the columnar [`Columns`](crate::columns::Columns)
//! substrate. The push entry points (`push_leaf`, `push_compound`,
//! `push_leaf_with`, `push_leaf_borrowed_string`,
//! `push_leaf_with_arena_frame`) keep their pre-AV signatures so the
//! generated code's push sites are untouched; internally, each push
//! writes into the structural + payload columns instead of allocating
//! a 16-byte `TapeRec`.

use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

/// Payload data handed to [`TapeBuilder::push_leaf_with`] — the single
/// entry point for payload-bearing leaves.
///
/// Each variant names one of the payload shapes the columnar
/// substrate recognises:
///
/// - [`PayloadData::None`] — pure span leaf, no payload. Equivalent
///   to [`TapeBuilder::push_leaf`]; exposed on `PayloadData` so
///   callers that build a payload conditionally don't need to switch
///   to a different entry point when the payload is absent.
/// - [`PayloadData::InlineScalar`] — scalar ≤ 4 bytes written into
///   the [`Columns::pay_narrow`](crate::columns::Columns::pay_narrow)
///   column. The record's `child_off` stores the column rank.
/// - [`PayloadData::WideScalar`] — 8-byte scalar (`f64`/`u64`/`i64`/
///   packed `Span`) written into the
///   [`Columns::pay_wide`](crate::columns::Columns::pay_wide) column;
///   the record's `child_off` stores the column rank.
/// - [`PayloadData::Aggregate`] — packed tuple bytes (colour tuples,
///   dimension `(f64, u8)` pairs, kv-pair values). Length up to 16
///   bytes; written verbatim into the shared
///   [`Columns::pay_agg`](crate::columns::Columns::pay_agg) arena
///   rounded up to the next 8-byte boundary. `child_off` holds the
///   arena byte offset.
/// - [`PayloadData::LargeAggregate`] — aggregate payload exceeding
///   the 16-byte inline budget (CSS colour-functions at 33+ B).
///   Identical arena encoding to `Aggregate` — bytes verbatim into
///   an 8-aligned slot — but the width is recovered from the
///   grammar's payload-layout table keyed by
///   `(kind, variant_idx)` rather than a frame header.
/// - [`PayloadData::Bytes`] — variable-length byte string (decoded
///   JSON strings, comment bodies, regex patterns). Framed into the
///   arena as `(len: u32 LE, bytes: [u8; len])`.
#[derive(Debug, Clone, Copy)]
pub enum PayloadData<'a> {
    /// No payload.
    None,
    /// Scalar value ≤ 4 bytes stored in `Columns::pay_narrow`. The
    /// `u32` carries the value's raw little-endian bytes.
    InlineScalar(u32),
    /// 8-byte scalar (`f64`, `u64`, `i64`, packed `Span`) stored in
    /// `Columns::pay_wide` as raw bits. Callers convert at the push
    /// site via `f64::to_bits()` / `to_le_bytes()` as needed.
    WideScalar(u64),
    /// Packed aggregate tuple bytes written verbatim into
    /// `Columns::pay_agg`. Length up to 16 bytes.
    Aggregate(&'a [u8]),
    /// Aggregate bytes exceeding the 16-byte inline budget — arena-
    /// backed, unframed. Written verbatim into an 8-aligned slot in
    /// `Columns::pay_agg` (trailing pad zero-initialised, matching
    /// [`Self::Aggregate`]). Width recovered from the grammar's
    /// payload-layout table via `(kind, variant_idx)`.
    LargeAggregate(&'a [u8]),
    /// Byte string framed as `(len: u32 LE, bytes)` into
    /// `Columns::pay_agg`.
    Bytes(&'a [u8]),
}

/// The parser's write interface to the tape.
///
/// Held by `&mut` for the duration of a parse. The generated parser
/// functions thread it through every rule call alongside the
/// `ParserState`:
///
/// ```ignore
/// fn __pair<'i>(
///     state: &mut parse_that::ParserState<'i>,
///     tape: &mut tape::TapeBuilder,
/// ) -> Option<tape::TapeOffset> {
///     let start_off = tape.mark_children();
///     let _key = __string(state, tape)?;
///     state.eat_byte(b':')?;
///     state.skip_ws();
///     let _value = __value(state, tape)?;
///     Some(tape.push_compound(TapeKind::Rule, start_off, state.offset))
/// }
/// ```
#[derive(Debug, Default)]
pub struct TapeBuilder {
    /// Column storage under construction. Consumed by [`Self::finish`]
    /// which computes the sibling-skip column and packages the result
    /// into a [`Tape`].
    pub(crate) columns: Columns,
    /// Sticky error state. Once set, subsequent `push_*` calls are
    /// still accepted (so mid-recovery parses can continue producing
    /// records for partial success), but `finish` returns the error.
    pub(crate) error: Option<TapeBuildError>,
    /// Stage-C activation gate.
    ///
    /// When `true`, the DTA driver is emitting `frame_depth` inline
    /// during stage A and [`Self::finish`] consumes [`Self::frame_depth`]
    /// directly via [`crate::finaliser::finalise`] — no
    /// `derive_frame_depth` reconstruction pass.
    ///
    /// When `false` (the legacy fn-per-rule path through the AW W0
    /// window), `push_compound` writes `sib_skip` / `span_hi` /
    /// `child_off` inline; [`Self::finish`] still runs
    /// [`crate::finaliser::derive_frame_depth`] to synthesise the
    /// depth stream a finalise pass demands from a post-order tape,
    /// then [`crate::finaliser::finalise`] closes the sibling-skip
    /// column off it.
    ///
    /// Default `false`. The DTA driver flips this to `true` at init
    /// time in W1 once the generated parser emits `frame_depth` inline.
    pub(crate) has_inline_frame_depth: bool,
    /// Per-record depth byte stream.
    ///
    /// Populated by the DTA driver when the builder is opted into
    /// inline frame-depth emission (see
    /// [`Self::enable_inline_frame_depth`]). Each `push` or
    /// `reserve_compound` in the driver stamps one byte here in
    /// lockstep with the structural columns. [`Self::finish`] hands
    /// this slice to [`crate::finaliser::finalise`] directly when the
    /// flag is set, skipping the legacy
    /// [`crate::finaliser::derive_frame_depth`] reconstruction pass.
    pub(crate) frame_depth: Vec<u8>,
}

/// Error state surfaced through [`TapeBuilder::finish`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TapeBuildError {
    /// The caller reported an unrecoverable parse failure. Constructed
    /// by the generated parser when a non-recoverable branch fails.
    ParseFailed {
        /// Input byte offset where the failure was detected.
        offset: u32,
        /// Optional rule-name id / label for diagnostics.
        rule_label: u32,
    },
}

impl TapeBuilder {
    /// Construct a fresh builder with an empty tape.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a builder sized for `expected` records.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            columns: Columns::with_capacity(expected),
            error: None,
            has_inline_frame_depth: false,
            frame_depth: Vec::new(),
        }
    }

    /// Record the current tape length as the start of a children run.
    ///
    /// Call this before pushing a compound's children. The returned
    /// offset is passed to [`Self::push_compound`] as the `child_off`
    /// field.
    #[inline(always)]
    pub fn mark_children(&self) -> TapeOffset {
        TapeOffset(self.columns.len() as u32)
    }

    /// Append a leaf record with a concrete kind + span.
    ///
    /// Leaves have no children, so `child_off` is forced to
    /// [`TapeOffset::NONE`].
    ///
    /// `meta_idx` is the branch index for Alt-bodied rules (`0` for
    /// everything else). Packed into `TapeRec::kind_meta` (high 4
    /// bits) and `TapeRec::flags` (bit 7). 5-bit range: 0-31.
    #[inline(always)]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(kind.is_leaf(), "push_leaf on compound kind {:?}", kind);
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset::NONE,
        );
        TapeOffset(idx)
    }

    /// Append a compound record pointing at a previously-marked
    /// children run.
    ///
    /// The caller must have called [`Self::mark_children`] BEFORE
    /// pushing the first child, and must pass the returned offset as
    /// `child_off`. `span_hi` is the parser state's current offset
    /// (end of the compound's source range).
    ///
    /// `meta_idx` is the branch index for Alt-bodied rules (`0` for
    /// everything else). Packed into `TapeRec::kind_meta` (high 4
    /// bits) and `TapeRec::flags` (bit 7). 5-bit range: 0-31.
    #[inline(always)]
    pub fn push_compound(
        &mut self,
        kind: TapeKind,
        child_off: TapeOffset,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_compound(),
            "push_compound on leaf/annotation kind {:?}",
            kind
        );
        // `has_children` is true iff the caller actually pushed
        // records between `mark_children` and this call. When the
        // child run is empty (`child_off` equals the parent's own
        // index), the compound has no payload and must report that
        // symmetrically with leaves — a single
        // `TapeOffset::NONE` sentinel on `child_off` drives both
        // [`TapeRec::has_children`] and [`TapeRec::has_payload`] to
        // false. Leaving the raw marked offset would form a cycle
        // in [`TapeCursor::children`] and, since the raw offset is
        // not `NONE`, would spuriously light up `has_payload` on
        // any reader that checks `child_off != NONE` to gate payload
        // decoding.
        let parent_idx = self.columns.len();
        let has_children = (child_off.0 as usize) < parent_idx;
        let effective_child_off = if has_children {
            child_off
        } else {
            TapeOffset::NONE
        };
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let extra = extra_meta_bit | if has_children { TapeRec::HAS_CHILDREN_BIT } else { 0 };
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra,
            span_lo,
            span_hi,
            effective_child_off,
        );
        TapeOffset(idx)
    }

    // ── Payload-bearing leaf push ──────────────────────────────────

    /// Append a leaf record carrying the supplied [`PayloadData`].
    ///
    /// Unified entry point for every payload-bearing leaf. The
    /// `PayloadData` variants cover the complete runtime payload
    /// taxonomy; the record's `child_off` ends up holding a column
    /// rank (for scalar payloads) or an arena byte offset (for
    /// aggregates and byte frames).
    #[inline(always)]
    pub fn push_leaf_with(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        payload: PayloadData<'_>,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with on compound kind {:?}",
            kind
        );
        let child_off = match payload {
            PayloadData::None => TapeOffset::NONE,
            PayloadData::InlineScalar(v) => {
                // AV.2.3: inline scalars land in `pay_narrow`; the
                // record's `child_off` carries the column rank. The
                // earlier collision with `TapeOffset::NONE` when
                // inlining `u32::MAX` directly into `child_off` is
                // resolved — column ranks don't approach `u32::MAX`
                // unless a grammar emits four billion inline scalars.
                let rank = self.columns.pay_narrow.len() as u32;
                self.columns.pay_narrow.push(v);
                TapeOffset(rank)
            }
            PayloadData::WideScalar(v) => {
                let rank = self.columns.pay_wide.len() as u32;
                self.columns.pay_wide.push(v);
                TapeOffset(rank)
            }
            PayloadData::Aggregate(bytes) => {
                if bytes.is_empty() {
                    TapeOffset::NONE
                } else {
                    let offset = self.alloc_aggregate_slot(bytes);
                    TapeOffset(offset)
                }
            }
            PayloadData::LargeAggregate(bytes) => {
                if bytes.is_empty() {
                    TapeOffset::NONE
                } else {
                    let offset = self.alloc_large_aggregate_slot(bytes);
                    TapeOffset(offset)
                }
            }
            PayloadData::Bytes(bytes) => {
                let offset = self.alloc_bytes_frame(bytes);
                TapeOffset(offset)
            }
        };
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            child_off,
        );
        TapeOffset(idx)
    }

    /// Append aggregate bytes into a `pay_agg` slot rounded up to
    /// the next 8-byte boundary and return the byte offset.
    ///
    /// The slot is zero-initialised so any unused trailing bytes
    /// (between `bytes.len()` and the rounded-up total) are
    /// deterministic.
    #[inline]
    fn alloc_aggregate_slot(&mut self, bytes: &[u8]) -> u32 {
        debug_assert!(bytes.len() <= 16, "aggregate payload exceeds 16 bytes");
        let slot_count = bytes.len().div_ceil(8);
        let slot_total = slot_count * 8;
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        arena.resize(start + slot_total, 0);
        // SAFETY: the resize above guarantees `slot_total` bytes are
        // available starting at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
    }

    /// Append a large aggregate payload (> 16 bytes) into a `pay_agg`
    /// slot rounded up to the next 8-byte boundary and return the
    /// byte offset.
    ///
    /// Identical in on-arena layout to
    /// [`Self::alloc_aggregate_slot`]: bytes are written verbatim,
    /// the slot is padded to 8-byte boundary with zero-initialised
    /// trailing bytes, no length prefix. The only distinction is the
    /// size bound — `LargeAggregate` carries payloads that exceed
    /// the inline 16-byte budget. Readers recover the byte count
    /// from the grammar's payload-layout table keyed by
    /// `(kind, variant_idx)`.
    #[inline]
    fn alloc_large_aggregate_slot(&mut self, bytes: &[u8]) -> u32 {
        debug_assert!(
            bytes.len() > crate::MAX_INLINE_AGGREGATE_BYTES,
            "LargeAggregate payload {} bytes fits inline (≤ {})",
            bytes.len(),
            crate::MAX_INLINE_AGGREGATE_BYTES,
        );
        let slot_count = bytes.len().div_ceil(8);
        let slot_total = slot_count * 8;
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        arena.resize(start + slot_total, 0);
        // SAFETY: the resize above guarantees `slot_total` bytes are
        // available starting at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
    }

    /// Append a `(len: u32 LE, bytes)` frame into `pay_agg` and
    /// return the byte offset of the length prefix.
    #[inline]
    fn alloc_bytes_frame(&mut self, bytes: &[u8]) -> u32 {
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        let len = bytes.len() as u32;
        arena.extend_from_slice(&len.to_le_bytes());
        arena.extend_from_slice(bytes);
        start as u32
    }

    /// Borrow the `pay_agg` arena for direct variable-length payload
    /// writes.
    ///
    /// The JSON-string decode kernel uses this to stream decoded
    /// bytes into the arena without an intermediate allocation.
    /// After decoding, the caller commits the record via
    /// [`Self::push_leaf_with`] with a zero-copy `PayloadData::Bytes`
    /// pointing at a buffer that was built via this accessor.
    ///
    /// The typical pattern:
    ///
    /// 1. Reserve the 4-byte length prefix (`extend_from_slice(&0u32.to_le_bytes())`).
    /// 2. Stream decoded bytes after the prefix.
    /// 3. Back-stamp the length via [`Self::stamp_arena_len_prefix`].
    /// 4. Push the leaf via [`Self::push_leaf_with_arena_frame`] with
    ///    the offset of the prefix.
    #[inline(always)]
    pub fn arena_mut(&mut self) -> &mut Vec<u8> {
        &mut self.columns.pay_agg
    }

    /// The current length of the `pay_agg` arena — equivalently, the
    /// byte offset where the next write will land.
    #[inline(always)]
    pub fn arena_len(&self) -> u32 {
        self.columns.pay_agg.len() as u32
    }

    /// Append a leaf record whose payload bytes (with length prefix)
    /// have already been written to `pay_agg` at `arena_offset`.
    ///
    /// Used by the JSON-string decode kernel which streams decoded
    /// bytes directly into the arena via [`Self::arena_mut`] and then
    /// commits the record by calling this method with the frame's
    /// offset. The `arena_offset` must point at the 4-byte length
    /// prefix; the length itself has been back-stamped via
    /// [`Self::stamp_arena_len_prefix`] after decoding.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    #[inline(always)]
    pub fn push_leaf_with_arena_frame(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        arena_offset: u32,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_arena_frame on compound kind {:?}",
            kind
        );
        debug_assert!(
            (arena_offset as usize) + 4 <= self.columns.pay_agg.len(),
            "push_leaf_with_arena_frame: offset {} + 4 exceeds arena len {}",
            arena_offset,
            self.columns.pay_agg.len()
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset(arena_offset),
        );
        TapeOffset(idx)
    }

    /// Append a leaf record whose payload is an in-arena scalar of
    /// `payload_width` bytes already written at `arena_offset`.
    ///
    /// Distinct from [`Self::push_leaf_with_arena_frame`], which
    /// assumes a 4-byte length prefix convention for decoded-string
    /// frames. This entry point mirrors the walker's
    /// [`crate::driver::emit_leaf_with_payload`] semantics: the
    /// caller has pushed `payload_width` bytes (1 for the Pratt
    /// op-discriminant, 8 for wide scalars) directly into
    /// [`Self::arena_mut`], and the resulting record carries
    /// [`TapeRec::PAYLOAD_IN_ARENA_BIT`] set so scalar readers
    /// ([`Tape::payload_u8`], [`Tape::payload_u64`]) slice the arena
    /// at `arena_offset` instead of indirecting through a column
    /// rank.
    ///
    /// AX.W0a.2.l — the Pratt shape emitter uses this for the
    /// per-operator Span leaf's 1-byte `op_discriminant` payload,
    /// reaching parity with the walker's
    /// [`crate::driver::emit_leaf_with_payload`] layout.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    /// `payload_width` must be one of 1 / 2 / 4 / 8 — the widths the
    /// scalar readers honour; asserted in debug builds.
    #[inline(always)]
    pub fn push_leaf_with_arena_payload(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
        arena_offset: u32,
        payload_width: u32,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_arena_payload on compound kind {:?}",
            kind
        );
        debug_assert!(
            matches!(payload_width, 1 | 2 | 4 | 8),
            "push_leaf_with_arena_payload: payload_width {} must be 1 / 2 / 4 / 8",
            payload_width,
        );
        debug_assert!(
            (arena_offset as usize) + (payload_width as usize)
                <= self.columns.pay_agg.len(),
            "push_leaf_with_arena_payload: offset {} + {} exceeds arena len {}",
            arena_offset,
            payload_width,
            self.columns.pay_agg.len()
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let extra = extra_meta_bit | TapeRec::PAYLOAD_IN_ARENA_BIT;
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra,
            span_lo,
            span_hi,
            TapeOffset(arena_offset),
        );
        TapeOffset(idx)
    }

    /// Append a borrow-safe string leaf — zero arena writes.
    ///
    /// Companion to [`Self::push_leaf_with_arena_frame`] for the
    /// JSON decode kernel's fast path. When
    /// [`parse_that::parsers::scan::decode_json_string_to_arena`]
    /// returns `StringPayload::Borrowed`, the source bytes are already
    /// the decoded UTF-8 (no escapes); the emitter calls this method
    /// instead of copying those bytes into an arena frame. The record
    /// stores no arena pointer; the reader recovers content via
    /// [`Tape::payload_string_with_source`], which slices
    /// `source[span_lo + 1 .. span_hi - 1]`.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
    #[inline(always)]
    pub fn push_leaf_borrowed_string(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_borrowed_string on compound kind {:?}",
            kind
        );
        debug_assert!(
            span_hi >= span_lo + 2,
            "borrowed string span too short to carry quotes: [{}, {})",
            span_lo,
            span_hi,
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            TapeRec::STRING_BORROW_BIT | extra_meta_bit,
            span_lo,
            span_hi,
            TapeOffset::NONE,
        );
        TapeOffset(idx)
    }

    /// AY.W4.2 — Eisel-Lemire direct-column f64 leaf push.
    ///
    /// Bypasses the [`PayloadData::WideScalar`] → `pay_wide` round-trip:
    /// the supplied `f64_bits` are written straight into the dedicated
    /// [`Columns::pay_f64`](crate::columns::Columns::pay_f64) column,
    /// the record's `child_off` carries the column rank, and
    /// [`TapeRec::PAYLOAD_F64_DIRECT_BIT`] is stamped on `extra` so
    /// readers ([`crate::Tape::payload_f64`]) project the value via
    /// `f64::from_bits(cols.pay_f64_at(rank))` directly.
    ///
    /// This saves one load + one store per number literal vs the
    /// generic [`Self::push_leaf_with`] route — significant on heavy-
    /// numeric fixtures (canada).
    ///
    /// `meta_idx` is fixed at `0` for number leaves; the number-shape
    /// emitter uses `variant_idx` for the rule discriminant.
    #[inline(always)]
    pub fn push_leaf_with_f64_direct(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        f64_bits: u64,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_f64_direct on compound kind {:?}",
            kind
        );
        let rank = self.columns.pay_f64.len() as u32;
        self.columns.pay_f64.push(f64_bits);
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, 0);
        let extra = extra_meta_bit | TapeRec::PAYLOAD_F64_DIRECT_BIT;
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra,
            span_lo,
            span_hi,
            TapeOffset(rank),
        );
        TapeOffset(idx)
    }

    /// Write the 4-byte length prefix at the `pay_agg` slot reserved
    /// by the decode kernel.
    ///
    /// The kernel writes decoded bytes into the arena after reserving
    /// a 4-byte slot at `arena_offset` (via `arena_mut()` +
    /// `extend_from_slice(&[0u8; 4])`). Once the bytes have been
    /// decoded, the kernel calls this helper to stamp the actual
    /// length into the reserved slot. The slot must exist.
    #[inline(always)]
    pub fn stamp_arena_len_prefix(&mut self, arena_offset: u32, len: u32) {
        let start = arena_offset as usize;
        debug_assert!(
            start + 4 <= self.columns.pay_agg.len(),
            "stamp_arena_len_prefix: offset {} + 4 exceeds arena len {}",
            start,
            self.columns.pay_agg.len()
        );
        self.columns.pay_agg[start..start + 4].copy_from_slice(&len.to_le_bytes());
    }

    /// Mark the parse as failed with an offset and optional rule
    /// label. The builder continues to accept pushes (so recovery
    /// paths can produce partial tapes) but [`Self::finish`] returns
    /// the error.
    pub fn set_error(&mut self, offset: u32, rule_label: u32) {
        if self.error.is_none() {
            self.error = Some(TapeBuildError::ParseFailed {
                offset,
                rule_label,
            });
        }
    }

    /// Consume the builder and return the finished tape. Returns the
    /// sticky error if one was set during parsing.
    ///
    /// [`crate::finaliser::finalise`] always runs — it is the sole
    /// writer for `sib_skip` (and the compound-closure columns
    /// `child_off` / `span_hi`). What the
    /// [`Self::has_inline_frame_depth`] flag gates is only the
    /// *derivation* of `frame_depth`:
    ///
    /// - **Legacy fn-per-rule path** (`has_inline_frame_depth ==
    ///   false`, the default through the AW W0 window):
    ///   `push_compound` populates `child_off` / `span_hi` inline
    ///   but emits no `frame_depth`, so `finish` reconstructs it via
    ///   [`crate::finaliser::derive_frame_depth`] (one backward
    ///   pass) before the Stage-C forward sweep.
    /// - **DTA-driven path** (`has_inline_frame_depth == true`,
    ///   post-W1): the DTA emits `frame_depth` directly during
    ///   stage A, so `derive_frame_depth` is skipped and the
    ///   in-column stream feeds [`crate::finaliser::finalise`]
    ///   directly — one linear pass instead of two.
    ///
    /// The flag is the AW.0.1 substrate; the win materialises at
    /// W1 when the DTA begins emitting `frame_depth` inline.
    ///
    /// AY.W1.5 — `#[inline(always)]` so the cross-crate call from
    /// the emitted `<Grammar>::parse` entry collapses; the finalise
    /// pass becomes part of the parser's straight-line code rather
    /// than a function-call boundary at the tape-crate edge.
    #[inline(always)]
    pub fn finish(mut self) -> Result<Tape, TapeBuildError> {
        match self.error {
            Some(err) => Err(err),
            None => {
                // AW.1.4: when the DTA driver has populated
                // `self.frame_depth` inline (via the
                // `enable_inline_frame_depth` opt-in), pass the pre-
                // populated stream to `finalise` directly and skip
                // the `derive_frame_depth` reconstruction. Otherwise
                // (legacy post-order path), reconstruct the depth
                // column from `child_off` first.
                if self.has_inline_frame_depth {
                    debug_assert_eq!(
                        self.frame_depth.len(),
                        self.columns.len(),
                        "inline frame_depth length {} != columns length {}",
                        self.frame_depth.len(),
                        self.columns.len(),
                    );
                    crate::finaliser::finalise(&mut self.columns, &self.frame_depth);
                } else {
                    let frame_depth = crate::finaliser::derive_frame_depth(&self.columns);
                    crate::finaliser::finalise(&mut self.columns, &frame_depth);
                }
                Ok(Tape {
                    columns: self.columns,
                })
            }
        }
    }

    /// Access the in-progress columns for debug / intermediate
    /// inspection. Primarily a test hook — production parsers use
    /// `finish()`.
    pub fn columns(&self) -> &Columns {
        &self.columns
    }

    /// Mutable handle on the in-progress columns.
    ///
    /// The DTA driver (post-AW-III.W4: per-grammar emitted walker;
    /// pre-W4: [`crate::driver::dta_run_cold`]) writes directly into
    /// the builder's column substrate instead of threading through
    /// [`Self::push_leaf`] / [`Self::push_compound`]. The generated
    /// `parse()` entry point post-AW.1.2 constructs a `TapeBuilder`,
    /// calls [`Self::enable_inline_frame_depth`] to opt into the
    /// Stage-C activation the DTA presupposes, then hands the mutable
    /// column reference returned here to the driver.
    #[inline]
    pub fn columns_mut(&mut self) -> &mut Columns {
        &mut self.columns
    }

    /// Mutable handle on the in-progress per-record frame_depth
    /// stream.
    ///
    /// Populated by the DTA driver in lockstep with column writes
    /// when the builder is opted into inline frame-depth emission.
    /// [`Self::finish`] reads this slice directly to skip the
    /// `derive_frame_depth` reconstruction pass the legacy path
    /// needs.
    #[inline]
    pub fn frame_depth_mut(&mut self) -> &mut Vec<u8> {
        &mut self.frame_depth
    }

    /// AW-III.W4.d — split-borrow accessor for the parallel-column
    /// pair the per-grammar specialised walker writes into.
    ///
    /// Returns a tuple of `(&mut Columns, &mut Vec<u8>)` borrowing
    /// the `columns` and `frame_depth` fields disjointly so the
    /// emitted `dta_run_<grammar>` can pass them as adjacent
    /// arguments without the caller dancing around the borrow
    /// checker. Both references live for the same scope; the runtime
    /// walker treats them as a single SoA pair (every leaf emit /
    /// compound reservation pushes to both in lockstep).
    #[inline]
    pub fn columns_and_frame_depth_mut(
        &mut self,
    ) -> (&mut Columns, &mut Vec<u8>) {
        (&mut self.columns, &mut self.frame_depth)
    }

    /// Access the in-progress tape view for debug inspection.
    ///
    /// Returns a snapshot `Tape` built from a clone of the current
    /// columns; useful in tests that want to inspect mid-build
    /// state. Sibling-skip is NOT computed on this snapshot — the
    /// authoritative path is [`Self::finish`].
    ///
    /// AY.W1.1 — flat-AoS substrate: the snapshot clones the
    /// `records` AoS column + `sib_skip` parallel column + the three
    /// payload columns. The `packed_cache` AoS sidecar is never
    /// cloned; the snapshot starts with a fresh `OnceLock` so the
    /// first reader on the snapshot re-transposes.
    pub fn tape_snapshot(&self) -> Tape {
        let mut columns = Columns::new();
        *columns.records_mut() = self.columns.records().to_vec();
        columns.sib_skip = self.columns.sib_skip.clone();
        columns.pay_narrow = self.columns.pay_narrow.clone();
        columns.pay_wide = self.columns.pay_wide.clone();
        columns.pay_f64 = self.columns.pay_f64.clone();
        columns.pay_agg = self.columns.pay_agg.clone();
        Tape { columns }
    }
}
