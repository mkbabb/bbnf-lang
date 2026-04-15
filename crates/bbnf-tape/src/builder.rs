//! `TapeBuilder` — the parser's write interface to the [`Tape`].
//!
//! The generated Rust parser calls `TapeBuilder::push_*` methods to
//! append records as each rule / Seq / Alt matches. The builder owns
//! the growing [`Tape`] plus sticky error state so failed sub-tree
//! matches don't poison the rest of the parse.

use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

/// Payload data handed to [`TapeBuilder::push_leaf_with`] — the single
/// entry point for payload-bearing leaves.
///
/// Each variant corresponds to one of the four payload shapes the
/// unified arena recognises:
///
/// - [`PayloadData::None`] — pure span leaf, no payload. Equivalent
///   to [`TapeBuilder::push_leaf`]; exposed on `PayloadData` so
///   callers that build a payload conditionally don't need to switch
///   to a different entry point when the payload is absent.
/// - [`PayloadData::InlineScalar`] — scalar ≤ 4 bytes packed directly
///   into the record's `child_off`. The `u32` carries the value's
///   little-endian bytes; callers typically construct this via
///   `u32::from_le_bytes(...)` from a typed scalar's byte
///   representation, or for `u8/u16/u32` the value itself.
/// - [`PayloadData::WideScalar`] — 8-byte scalar (`f64/u64/i64/Span`)
///   written into an 8-aligned arena slot.
/// - [`PayloadData::Aggregate`] — packed tuple bytes (colour tuples,
///   dimension `(f64, u8)` pairs, kv-pair values). Length up to 16
///   bytes; written verbatim into an arena slot rounded up to the
///   next 8-byte boundary.
/// - [`PayloadData::Bytes`] — variable-length byte string (decoded
///   JSON strings, comment bodies, regex patterns). Framed into the
///   arena as `(len: u32 LE, bytes: [u8; len])`.
#[derive(Debug, Clone, Copy)]
pub enum PayloadData<'a> {
    /// No payload.
    None,
    /// Scalar value ≤ 4 bytes, packed inline into `child_off`.
    /// The `u32` carries the raw bytes in little-endian order.
    InlineScalar(u32),
    /// 8-byte scalar (`f64`, `u64`, `i64`, packed `Span`) written
    /// into an arena slot. The `u64` carries the raw bytes in
    /// little-endian order (via `f64::to_bits()` or `to_le_bytes()`
    /// conversions at the call site).
    WideScalar(u64),
    /// Packed aggregate tuple bytes written verbatim into the arena.
    /// Length up to 16 bytes.
    Aggregate(&'a [u8]),
    /// Byte string framed as `(len: u32 LE, bytes)` into the arena.
    /// The caller supplies the decoded bytes; the builder writes the
    /// length prefix.
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
    /// The tape being assembled. Owned by the builder for the
    /// duration of the parse; consumed via [`Self::finish`] at the
    /// end.
    pub(crate) tape: Tape,
    /// Sticky error state. Once set, subsequent `push_*` calls are
    /// still accepted (so mid-recovery parses can continue producing
    /// records for partial success), but `finish` returns the error.
    pub(crate) error: Option<TapeBuildError>,
    /// Unified payload arena. Staged here during builds and
    /// transferred to [`Tape::arena`] at [`Self::finish`]. Holds:
    /// - Wide scalars (f64/u64/i64/Span) — one 8-byte slot per value.
    /// - Aggregates — packed tuple bytes rounded up to 8-byte slots.
    /// - Byte strings — `(len: u32 LE, bytes)` frames.
    ///
    /// Inline scalars (`u8/i8/u16/i16/u32/i32/f32/bool`) bypass the
    /// arena entirely: the value packs into each record's `child_off`
    /// at push time, so the arena touches zero bytes per inline push.
    pub(crate) arena: Vec<u8>,
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
    ///
    /// Pre-allocates records and reserves an arena heuristic sized
    /// for typical scalar-heavy grammars (JSON numbers ~50% of
    /// records). Compound-heavy grammars (CSS, Sheets) may leave the
    /// reserve unused, but the over-reservation cost is cheap
    /// compared to a runtime `RawVec::grow_one` on the hot path.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            tape: Tape::with_capacity(expected),
            error: None,
            arena: Vec::with_capacity(expected / 8 * 8),
        }
    }

    /// Record the current tape length as the start of a children run.
    ///
    /// Call this before pushing a compound's children. The returned
    /// offset is passed to [`Self::push_compound`] as the `child_off`
    /// field.
    #[inline]
    pub fn mark_children(&self) -> TapeOffset {
        TapeOffset(self.tape.len() as u32)
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
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind_meta,
            flags: (variant_idx & 0x3F) | flags_meta_bit,
            _reserved: 0,
            span_lo,
            span_hi,
            child_off: TapeOffset::NONE,
        });
        TapeOffset(idx as u32)
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
        // child run is empty, `child_off` equals the parent's own
        // index, which would form a cycle for `TapeCursor::children`
        // / `subtree_size`. The safe thing is to clear the flag and
        // leave the child_off field untouched — cursor accessors
        // check `has_children` first.
        let parent_idx = self.tape.records.len();
        let has_children = (child_off.0 as usize) < parent_idx;
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let flags = (variant_idx & 0x3F) | if has_children { 0x40 } else { 0 } | flags_meta_bit;
        let idx = parent_idx;
        self.tape.records.push(TapeRec {
            kind_meta,
            flags,
            _reserved: 0,
            span_lo,
            span_hi,
            child_off,
        });
        TapeOffset(idx as u32)
    }

    // ── Payload-bearing leaf push ──────────────────────────────────

    /// Append a leaf record carrying the supplied [`PayloadData`].
    ///
    /// Unified entry point for every payload-bearing leaf. The four
    /// `PayloadData` variants cover the complete set of runtime
    /// payload shapes:
    ///
    /// - [`PayloadData::None`] — equivalent to [`Self::push_leaf`].
    /// - [`PayloadData::InlineScalar`] — scalar ≤ 4 bytes packed
    ///   directly into `child_off`; no arena touch.
    /// - [`PayloadData::WideScalar`] — 8-byte scalar at an 8-aligned
    ///   arena slot; `child_off` is the arena byte offset.
    /// - [`PayloadData::Aggregate`] — packed tuple bytes (≤ 16)
    ///   written verbatim into an 8-aligned arena slot; `child_off`
    ///   is the arena byte offset.
    /// - [`PayloadData::Bytes`] — framed `(len: u32 LE, bytes)`
    ///   written into the arena; `child_off` is the frame byte
    ///   offset.
    ///
    /// `meta_idx` range is 0-31 (5-bit packed field).
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
                // The inline-scalar caller guarantees the payload
                // fits in 4 bytes. NONE's sentinel value (u32::MAX)
                // would collide with an inline u32 of exactly
                // u32::MAX; callers that project to u32 must route
                // through WideScalar instead (the grammar rarely
                // emits u32::MAX literally, and any collision will
                // appear as a payload-absence false positive in
                // debug builds — guarded below).
                debug_assert!(
                    v != u32::MAX,
                    "inline scalar collides with TapeOffset::NONE sentinel"
                );
                TapeOffset(v)
            }
            PayloadData::WideScalar(v) => {
                let offset = self.alloc_wide_slot(v);
                TapeOffset(offset)
            }
            PayloadData::Aggregate(bytes) => {
                if bytes.is_empty() {
                    TapeOffset::NONE
                } else {
                    let offset = self.alloc_aggregate_slot(bytes);
                    TapeOffset(offset)
                }
            }
            PayloadData::Bytes(bytes) => {
                let offset = self.alloc_bytes_frame(bytes);
                TapeOffset(offset)
            }
        };
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind_meta,
            flags: (variant_idx & 0x3F) | flags_meta_bit,
            _reserved: 0,
            span_lo,
            span_hi,
            child_off,
        });
        TapeOffset(idx as u32)
    }

    /// Append a wide (8-byte) scalar into an 8-aligned arena slot
    /// and return the byte offset.
    #[inline]
    fn alloc_wide_slot(&mut self, value: u64) -> u32 {
        let start = self.arena.len();
        self.arena.extend_from_slice(&value.to_le_bytes());
        start as u32
    }

    /// Append aggregate bytes into an arena slot rounded up to the
    /// next 8-byte boundary and return the byte offset.
    ///
    /// The slot is zero-initialised so any unused trailing bytes
    /// (between `bytes.len()` and the rounded-up total) are
    /// deterministic.
    #[inline]
    fn alloc_aggregate_slot(&mut self, bytes: &[u8]) -> u32 {
        debug_assert!(bytes.len() <= 16, "aggregate payload exceeds 16 bytes");
        let slot_count = bytes.len().div_ceil(8);
        let slot_total = slot_count * 8;
        let start = self.arena.len();
        self.arena.resize(start + slot_total, 0);
        // SAFETY: the resize above guarantees `slot_total` bytes are
        // available starting at `start`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.arena.as_mut_ptr().add(start),
                bytes.len(),
            );
        }
        start as u32
    }

    /// Append a `(len: u32 LE, bytes)` frame into the arena and
    /// return the byte offset of the length prefix.
    #[inline]
    fn alloc_bytes_frame(&mut self, bytes: &[u8]) -> u32 {
        let start = self.arena.len();
        let len = bytes.len() as u32;
        self.arena.extend_from_slice(&len.to_le_bytes());
        self.arena.extend_from_slice(bytes);
        start as u32
    }

    /// Borrow the arena buffer for direct variable-length payload
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
        &mut self.arena
    }

    /// The current length of the arena — equivalently, the byte
    /// offset where the next write will land.
    #[inline]
    pub fn arena_len(&self) -> u32 {
        self.arena.len() as u32
    }

    /// Append a leaf record whose payload bytes (with length prefix)
    /// have already been written to the arena at `arena_offset`.
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
            (arena_offset as usize) + 4 <= self.arena.len(),
            "push_leaf_with_arena_frame: offset {} + 4 exceeds arena len {}",
            arena_offset,
            self.arena.len()
        );
        let (kind_meta, flags_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind_meta,
            flags: (variant_idx & 0x3F) | flags_meta_bit,
            _reserved: 0,
            span_lo,
            span_hi,
            child_off: TapeOffset(arena_offset),
        });
        TapeOffset(idx as u32)
    }

    /// Write the 4-byte length prefix at the arena slot reserved by
    /// the decode kernel.
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
            start + 4 <= self.arena.len(),
            "stamp_arena_len_prefix: offset {} + 4 exceeds arena len {}",
            start,
            self.arena.len()
        );
        self.arena[start..start + 4].copy_from_slice(&len.to_le_bytes());
    }

    /// Mark the parse as failed with an offset and optional rule label.
    /// The builder continues to accept pushes (so recovery paths can
    /// produce partial tapes) but [`Self::finish`] returns the error.
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
    pub fn finish(mut self) -> Result<Tape, TapeBuildError> {
        match self.error {
            Some(err) => Err(err),
            None => {
                self.tape.arena = self.arena;
                Ok(self.tape)
            }
        }
    }

    /// Access the in-progress tape for debug / intermediate inspection.
    /// Primarily a test hook — production parsers use `finish()`.
    pub fn tape(&self) -> &Tape {
        &self.tape
    }
}
