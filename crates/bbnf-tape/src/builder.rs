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
///     tape: &mut bbnf_tape::TapeBuilder,
/// ) -> Option<bbnf_tape::TapeOffset> {
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
        }
    }

    /// Record the current tape length as the start of a children run.
    ///
    /// Call this before pushing a compound's children. The returned
    /// offset is passed to [`Self::push_compound`] as the `child_off`
    /// field.
    #[inline]
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
    #[inline]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        meta_idx: u8,
    ) -> TapeOffset {
        debug_assert!(kind.is_leaf(), "push_leaf on compound kind {:?}", kind);
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | flags_meta_bit;
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            0,
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
    #[inline]
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
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | if has_children { 0x40 } else { 0 } | flags_meta_bit;
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            0,
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
    #[inline]
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
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | flags_meta_bit;
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            0,
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
    #[inline]
    pub fn arena_mut(&mut self) -> &mut Vec<u8> {
        &mut self.columns.pay_agg
    }

    /// The current length of the `pay_agg` arena — equivalently, the
    /// byte offset where the next write will land.
    #[inline]
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
    #[inline]
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
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | flags_meta_bit;
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            0,
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
    #[inline]
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
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | flags_meta_bit;
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            TapeRec::STRING_BORROW_BIT,
            span_lo,
            span_hi,
            TapeOffset::NONE,
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
    #[inline]
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

    // ── Shape-dictionary leaf push (AV.5.1) ─────────────────────────

    /// Append a `ShapeRef` leaf record — a single record that replaces
    /// an entire fixed-shape compound subtree.
    ///
    /// `shape_dict_idx` is the index into
    /// [`GrammarProfile::shape_dict`](crate::GrammarProfile::shape_dict)
    /// (0..=31, 5-bit packed into `flags`). `packed_payload` is the
    /// per-instance payload blob (non-constant leaf spans + typed
    /// payloads following the shape template's layout); the bytes are
    /// written into `pay_agg` and the arena offset stored in
    /// `child_off`.
    ///
    /// Bit 5 of `flags` is set when `packed_payload` is non-empty
    /// (`has_payload`). The remaining bits (6 and 7) are zero — ShapeRef
    /// records do not carry `has_children` or `meta_idx[4]`.
    #[inline]
    pub fn push_shape_ref(
        &mut self,
        span_lo: u32,
        span_hi: u32,
        shape_dict_idx: u8,
        packed_payload: &[u8],
    ) -> TapeOffset {
        debug_assert!(
            shape_dict_idx <= 31,
            "shape_dict_idx {} exceeds 5-bit maximum (31)",
            shape_dict_idx,
        );
        let has_payload = !packed_payload.is_empty();
        let child_off = if has_payload {
            let offset = self.alloc_aggregate_shape_ref(packed_payload);
            TapeOffset(offset)
        } else {
            TapeOffset::NONE
        };
        // kind_meta: low 4 bits = ShapeRef discriminant (13), high 4
        // bits = 0 (no meta_idx for ShapeRef).
        let kind_meta = TapeKind::ShapeRef as u8 & 0x0F;
        let flags = (shape_dict_idx & 0x1F) | if has_payload { 0x20 } else { 0 };
        let idx = self.columns.push_structural(
            kind_meta,
            flags,
            0,
            span_lo,
            span_hi,
            child_off,
        );
        TapeOffset(idx)
    }

    /// Allocate an arena slot for a ShapeRef packed payload. Identical
    /// to the aggregate slot path but without the 16-byte inline limit.
    #[inline]
    fn alloc_aggregate_shape_ref(&mut self, bytes: &[u8]) -> u32 {
        let slot_total = bytes.len().div_ceil(8) * 8;
        let arena = &mut self.columns.pay_agg;
        let start = arena.len();
        arena.resize(start + slot_total, 0);
        // SAFETY: resize guarantees `slot_total` bytes at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
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
    /// Routes through the Stage-C segmented prefix scan
    /// ([`crate::finaliser::finalise`]) over the `frame_depth` column
    /// the DTA emits inline during stage A. The scan closes
    /// [`Columns::sib_skip`](crate::Columns::sib_skip),
    /// [`Columns::span_hi`](crate::Columns::span_hi) (for compounds),
    /// and [`Columns::child_off`](crate::Columns::child_off) (for
    /// compounds) in a single linear backward pass — `O(N)` over the
    /// tape with `O(max_depth)` working-set memory.
    ///
    /// Until `av4-psi` lands the inline stage-A emission of
    /// `frame_depth`, the column is reconstructed at finish time from
    /// the `child_off` pointers the parser already wrote
    /// ([`crate::finaliser::derive_frame_depth`]). The reconstruction
    /// is itself `O(N)` and reuses the canonical post-order layout,
    /// so the production path is the *same* Stage-C scan whether
    /// `frame_depth` arrives from stage A or from the helper — there
    /// is no fallback to V2's backward-walk
    /// ([`Columns::compute_sibling_skip`](crate::Columns::compute_sibling_skip));
    /// V2's method survives only as the bit-equality reference for
    /// the regression suite.
    pub fn finish(mut self) -> Result<Tape, TapeBuildError> {
        match self.error {
            Some(err) => Err(err),
            None => {
                let frame_depth = crate::finaliser::derive_frame_depth(&self.columns);
                crate::finaliser::finalise(&mut self.columns, &frame_depth);
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

    /// Access the in-progress tape view for debug inspection.
    ///
    /// Returns a snapshot `Tape` built from a clone of the current
    /// columns; useful in tests that want to inspect mid-build
    /// state. Sibling-skip is NOT computed on this snapshot — the
    /// authoritative path is [`Self::finish`].
    pub fn tape_snapshot(&self) -> Tape {
        let columns = Columns {
            kinds: self.columns.kinds.clone(),
            flags: self.columns.flags.clone(),
            extra: self.columns.extra.clone(),
            span_lo: self.columns.span_lo.clone(),
            span_hi: self.columns.span_hi.clone(),
            sib_skip: self.columns.sib_skip.clone(),
            child_off: self.columns.child_off.clone(),
            pay_narrow: self.columns.pay_narrow.clone(),
            pay_wide: self.columns.pay_wide.clone(),
            pay_agg: self.columns.pay_agg.clone(),
        };
        Tape { columns }
    }
}
