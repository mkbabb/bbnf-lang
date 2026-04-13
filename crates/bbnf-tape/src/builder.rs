//! `TapeBuilder` — the parser's write interface to the [`Tape`].
//!
//! The generated Rust parser calls `TapeBuilder::push_*` methods to
//! append records as each rule / Seq / Alt matches. The builder owns
//! the growing [`Tape`] plus sticky error state so failed sub-tree
//! matches don't poison the rest of the parse.

use crate::kind::TapeKind;
use crate::tape::{Tape, TapeOffset, TapeRec};

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
    /// Staging buffer for typed leaf payloads. Each slot is 8 bytes,
    /// transferred to the finished `Tape` at [`Self::finish`].
    pub(crate) payloads: Vec<u8>,
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
            tape: Tape::with_capacity(expected),
            error: None,
            payloads: Vec::new(),
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
    #[inline]
    pub fn push_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
    ) -> TapeOffset {
        debug_assert!(kind.is_leaf(), "push_leaf on compound kind {:?}", kind);
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind,
            flags: variant_idx & 0x3F,
            payload_idx: 0,
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
    #[inline]
    pub fn push_compound(
        &mut self,
        kind: TapeKind,
        child_off: TapeOffset,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
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
        let flags = (variant_idx & 0x3F) | if has_children { 0x40 } else { 0 };
        let idx = parent_idx;
        self.tape.records.push(TapeRec {
            kind,
            flags,
            payload_idx: 0,
            span_lo,
            span_hi,
            child_off,
        });
        TapeOffset(idx as u32)
    }

    // ── Payload-bearing leaf pushes ─────────────────────────────────

    /// Append a payload slot (8 bytes) and return its 1-based index.
    ///
    /// Every slot is allocated 8-byte-wide regardless of the stored
    /// type so the `payload_idx` continues to denote a fixed-stride
    /// 1-based slot index. Sub-8-byte scalars consume their leading
    /// bytes; the trailing padding is left zeroed.
    #[inline]
    fn alloc_payload_slot(&mut self) -> u16 {
        let slot = self.payloads.len() / 8;
        self.payloads.extend_from_slice(&[0u8; 8]);
        // 1-based: slot 0 in the buffer is payload_idx=1.
        (slot + 1) as u16
    }

    /// Append `slot_count` consecutive 8-byte payload slots and
    /// return the byte offset of the first slot.
    ///
    /// The returned offset is the raw byte index into
    /// `self.payloads` (not a 1-based payload_idx). The caller is
    /// responsible for translating it into a `payload_idx` via
    /// `(byte_offset / 8) + 1` when the slots are committed to a
    /// `TapeRec`. Used by [`Self::push_leaf_with_aggregate`] for
    /// multi-slot aggregate payloads.
    #[inline]
    fn alloc_aggregate_slots(&mut self, slot_count: usize) -> usize {
        let start = self.payloads.len();
        self.payloads.resize(start + slot_count * 8, 0);
        start
    }

    /// Append a leaf record with an arbitrary scalar payload.
    ///
    /// Writes `size_of::<T>()` bytes into the payload buffer (LE-host
    /// order — the writer + reader are always the same process, so
    /// host endianness is fine) and sets the record's `payload_idx`
    /// to the corresponding 1-based 8-byte slot.
    ///
    /// `T` must be `Copy` and ≤ 8 bytes. Sub-8 types occupy the
    /// leading bytes of the slot; trailing bytes stay zeroed.
    #[inline]
    pub fn push_leaf_with_scalar<T: Copy>(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: T,
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_scalar on compound kind {:?}",
            kind
        );
        debug_assert!(
            std::mem::size_of::<T>() <= 8,
            "push_leaf_with_scalar: size_of::<T>() must be ≤ 8, got {}",
            std::mem::size_of::<T>()
        );
        let pidx = self.alloc_payload_slot();
        let start = (pidx as usize - 1) * 8;
        // SAFETY: `value` is `Copy` and `size_of::<T>() ≤ 8 ==
        // payload slot width`. The destination has been just-
        // allocated (zeroed) and is non-overlapping with the source.
        unsafe {
            std::ptr::copy_nonoverlapping(
                &value as *const T as *const u8,
                self.payloads.as_mut_ptr().add(start),
                std::mem::size_of::<T>(),
            );
        }
        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind,
            flags: variant_idx & 0x3F,
            payload_idx: pidx,
            span_lo,
            span_hi,
            child_off: TapeOffset::NONE,
        });
        TapeOffset(idx as u32)
    }

    /// Append a leaf record with an `f64` payload.
    #[inline]
    pub fn push_leaf_with_f64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: f64,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<f64>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with a `bool` payload.
    #[inline]
    pub fn push_leaf_with_bool(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: bool,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<bool>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with an `i8` payload.
    #[inline]
    pub fn push_leaf_with_i8(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: i8,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<i8>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with a `u8` payload.
    #[inline]
    pub fn push_leaf_with_u8(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: u8,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<u8>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with an `i16` payload.
    #[inline]
    pub fn push_leaf_with_i16(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: i16,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<i16>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with a `u16` payload.
    #[inline]
    pub fn push_leaf_with_u16(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: u16,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<u16>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with an `i32` payload.
    #[inline]
    pub fn push_leaf_with_i32(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: i32,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<i32>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with a `u32` payload.
    #[inline]
    pub fn push_leaf_with_u32(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: u32,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<u32>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with an `i64` payload.
    #[inline]
    pub fn push_leaf_with_i64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: i64,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<i64>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record with a `u64` payload.
    #[inline]
    pub fn push_leaf_with_u64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        value: u64,
    ) -> TapeOffset {
        self.push_leaf_with_scalar::<u64>(kind, span_lo, span_hi, variant_idx, value)
    }

    /// Append a leaf record carrying an aggregate (multi-field)
    /// payload of up to 16 bytes.
    ///
    /// The caller supplies a slice of bytes representing a packed
    /// scalar tuple — for example, `[f64 value, u8 unit]` packed
    /// into 9 bytes by the codegen following the offsets recorded
    /// in [`bbnf_ir::passes::PayloadLayout`]. The builder rounds
    /// the byte count up to the next 8-byte slot, allocates that
    /// many consecutive payload slots, and copies the bytes in
    /// verbatim.
    ///
    /// The corresponding reader is [`crate::Tape::payload_bytes`],
    /// which returns the leading `byte_count` bytes of the
    /// allocated region.
    ///
    /// Empty aggregates (`bytes.is_empty()`) push a leaf with no
    /// payload, equivalent to [`Self::push_leaf`].
    #[inline]
    pub fn push_leaf_with_aggregate(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        bytes: &[u8],
    ) -> TapeOffset {
        debug_assert!(
            kind.is_leaf(),
            "push_leaf_with_aggregate on compound kind {:?}",
            kind
        );
        debug_assert!(
            bytes.len() <= 16,
            "aggregate payload exceeds 16 bytes: {}",
            bytes.len()
        );

        let payload_idx: u16 = if bytes.is_empty() {
            0
        } else {
            // Round up to next 8-byte slot.
            let slot_count = bytes.len().div_ceil(8);
            let start = self.alloc_aggregate_slots(slot_count);
            // SAFETY: `start..start + bytes.len()` was just
            // allocated by `alloc_aggregate_slots` and is non-
            // overlapping with the source.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    bytes.as_ptr(),
                    self.payloads.as_mut_ptr().add(start),
                    bytes.len(),
                );
            }
            ((start / 8) + 1) as u16
        };

        let idx = self.tape.records.len();
        self.tape.records.push(TapeRec {
            kind,
            flags: variant_idx & 0x3F,
            payload_idx,
            span_lo,
            span_hi,
            child_off: TapeOffset::NONE,
        });
        TapeOffset(idx as u32)
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
                self.tape.payloads = self.payloads;
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
