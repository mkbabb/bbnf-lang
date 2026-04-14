//! `Tape`, `TapeRec`, `TapeOffset` — the core fixed-size record
//! substrate that replaces the eager typed AST.

use crate::kind::TapeKind;

/// Stable index into a [`Tape`]'s record stream.
///
/// Constructed by [`TapeBuilder::push_leaf`] /
/// [`TapeBuilder::push_compound`] and consumed by view-layer accessors
/// via [`Tape::get`]. Two offsets compare equal iff they point to the
/// same record in the same tape; cross-tape comparison is a logic bug
/// the view codegen prevents by tying every view type to a `'tape`
/// lifetime parameter.
///
/// [`TapeBuilder::push_leaf`]: crate::builder::TapeBuilder::push_leaf
/// [`TapeBuilder::push_compound`]: crate::builder::TapeBuilder::push_compound
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct TapeOffset(pub u32);

impl TapeOffset {
    /// Sentinel used for "no children" / "end of a compound run".
    pub const NONE: TapeOffset = TapeOffset(u32::MAX);

    /// Treat this offset as a raw integer index.
    #[inline]
    pub fn as_u32(self) -> u32 {
        self.0
    }

    /// Is this the sentinel "none" offset?
    #[inline]
    pub fn is_none(self) -> bool {
        self.0 == u32::MAX
    }
}

/// Fixed-size tape record.
///
/// Layout: 16 bytes, `#[repr(C)]`. One quarter of a 64-byte cache line
/// so four consecutive records fit in one line and sequential scans
/// enjoy the hardware prefetcher without strided pointer chasing.
///
/// - `kind_meta` — packed byte: low 4 bits = [`TapeKind`] discriminant
///   (values 0-15), high 4 bits = `meta_idx` bits \[0:3\].
/// - `flags` — bitfield: variant index (low 6 bits), `has_children`
///   (bit 6), `meta_idx` bit \[4\] (bit 7). Five-bit `meta_idx`
///   (0-31) is split across `kind_meta[7:4]` and `flags[7]`.
/// - `span_lo` / `span_hi` — byte offsets into the source input.
///   `span_hi == span_lo` means a zero-width record (epsilon match).
/// - `child_off` — `TapeOffset` of the first child record for
///   compound nodes. `TapeOffset::NONE` for leaves. The children run
///   from `child_off` (inclusive) to the next compound's `child_off`
///   (exclusive) — the tape is written in pre-order, so siblings are
///   contiguous.
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TapeRec {
    /// Packed byte: low 4 bits = [`TapeKind`] discriminant, high 4
    /// bits = `meta_idx` bits \[0:3\]. Use [`TapeRec::kind()`] and
    /// [`TapeRec::meta_idx()`] to decode.
    pub kind_meta: u8,
    /// Bitfield: variant index (low 6 bits), has_children (bit 6),
    /// meta_idx bit \[4\] (bit 7).
    pub flags: u8,
    /// Index into the tape's payload buffer (`Tape::payloads`).
    ///
    /// - `0` = no payload (the default for all existing codegen).
    /// - Non-zero = 1-based slot index. The byte offset into the
    ///   payload buffer is `(payload_idx - 1) * 8`. All payload
    ///   slots are 8-byte aligned regardless of the stored type
    ///   (f64 uses all 8 bytes; bool/u8 use byte 0, the rest is
    ///   padding).
    pub payload_idx: u16,
    /// Byte offset into the source input where this record's span begins.
    pub span_lo: u32,
    /// Byte offset into the source input where this record's span ends.
    /// `span_hi == span_lo` represents epsilon / zero-width matches.
    pub span_hi: u32,
    /// First child's offset, or [`TapeOffset::NONE`] for leaves.
    pub child_off: TapeOffset,
}

// Compile-time size + alignment sanity check — any change to the
// layout that grows the record unexpectedly breaks CI.
const _: () = {
    assert!(std::mem::size_of::<TapeRec>() == 16);
    assert!(std::mem::align_of::<TapeRec>() == 4);
};

impl TapeRec {
    /// Maximum `meta_idx` value encodable in the 5-bit packed field
    /// (4 bits in `kind_meta[7:4]` + 1 bit in `flags[7]`).
    pub const MAX_META_IDX: u8 = 31;

    /// Pack a [`TapeKind`] and `meta_idx` into the `kind_meta` and
    /// `flags` bytes. Returns `(kind_meta, flags_meta_bit)` where
    /// `flags_meta_bit` is `0x00` or `0x80` — the caller ORs it into
    /// the flags byte.
    #[inline]
    pub(crate) fn pack_kind_meta(kind: TapeKind, meta_idx: u8) -> (u8, u8) {
        debug_assert!(
            meta_idx <= Self::MAX_META_IDX,
            "meta_idx {} exceeds 5-bit maximum ({})",
            meta_idx,
            Self::MAX_META_IDX,
        );
        let kind_meta = (kind as u8 & 0x0F) | ((meta_idx & 0x0F) << 4);
        let flags_meta_bit = (meta_idx >> 4) << 7; // bit [4] → bit 7 of flags
        (kind_meta, flags_meta_bit)
    }

    /// Extract the [`TapeKind`] from the packed `kind_meta` byte.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        TapeKind::from_u8(self.kind_meta & 0x0F)
    }

    /// Extract the 5-bit `meta_idx` from the packed `kind_meta` and
    /// `flags` bytes.
    ///
    /// For Alt-bodied rules this is the branch index; for everything
    /// else it is `0`.
    #[inline]
    pub fn meta_idx(&self) -> u8 {
        let lo4 = (self.kind_meta >> 4) & 0x0F;
        let hi1 = (self.flags >> 7) & 0x01;
        lo4 | (hi1 << 4)
    }

    /// Extract the variant index from `flags` (low 6 bits).
    #[inline]
    pub fn variant_idx(&self) -> u8 {
        self.flags & 0x3F
    }

    /// Does this record have children? (Bit 6 of `flags`.)
    #[inline]
    pub fn has_children(&self) -> bool {
        (self.flags & 0x40) != 0
    }

    /// Byte length of the record's source span.
    #[inline]
    pub fn span_len(&self) -> u32 {
        self.span_hi.saturating_sub(self.span_lo)
    }
}

/// The parser's output tape.
///
/// Owns a flat `Vec<TapeRec>` — one contiguous allocation with zero
/// indirection per access. Built by [`crate::TapeBuilder`] during
/// parsing; read by the generated view layer via [`Tape::get`] /
/// children offsets. Lives for the lifetime of the input buffer — the
/// `'tape` lifetime on every view type ties back to this.
///
/// # Tranche AK.0 — flat Vec substrate
///
/// Replaces the `ChunkedArena<TapeRec>` (Vec<Vec<TapeRec>>) with a
/// single flat Vec. Eliminates 2 pointer dereferences per push and
/// ensures `with_capacity(N)` pre-allocates the full buffer in one
/// allocation.
///
/// # Tranche AT.2.2 — meta_idx folded into TapeRec
///
/// The parallel `meta: Vec<u8>` buffer has been eliminated. The
/// per-record `meta_idx` (branch index for Alt-bodied rules) is now
/// packed into the `TapeRec` itself: 4 bits in the high nibble of
/// `kind_meta` + 1 bit in `flags[7]`, giving a 5-bit range (0-31).
#[derive(Debug)]
pub struct Tape {
    /// Flat record storage. Append-only during parsing; immutable
    /// during view-layer reads.
    pub(crate) records: Vec<TapeRec>,
    /// Side-channel payload buffer for typed leaf values.
    ///
    /// Each payload slot is 8 bytes, regardless of the stored type.
    /// A `TapeRec` with `payload_idx > 0` stores its value at byte
    /// offset `(payload_idx - 1) * 8`. Empty for all current codegen
    /// (existing records set `payload_idx = 0`).
    pub(crate) payloads: Vec<u8>,
}

impl Tape {
    /// Construct an empty tape with default initial capacity.
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
            payloads: Vec::new(),
        }
    }

    /// Construct an empty tape sized for `expected` records.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            records: Vec::with_capacity(expected),
            payloads: Vec::new(),
        }
    }

    /// Number of records appended to the tape so far.
    #[inline]
    pub fn len(&self) -> usize {
        self.records.len()
    }

    /// True iff no records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.records.is_empty()
    }

    /// Look up a record by offset. Panics on out-of-range offsets —
    /// view codegen never produces out-of-range offsets because every
    /// offset originates from a `TapeBuilder::push_*` call.
    #[inline]
    pub fn get(&self, offset: TapeOffset) -> &TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get called with TapeOffset::NONE sentinel"
        );
        &self.records[offset.0 as usize]
    }

    /// Look up a record by offset **without bounds checking**.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `offset` is not the
    /// [`TapeOffset::NONE`] sentinel and that `offset.0 as usize`
    /// is less than `self.len()`. Both invariants hold for every
    /// offset produced by [`TapeBuilder::push_leaf`] /
    /// [`TapeBuilder::push_compound`] when reading from the same
    /// tape that produced them.
    #[inline]
    pub unsafe fn get_unchecked(&self, offset: TapeOffset) -> &TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get_unchecked called with TapeOffset::NONE sentinel"
        );
        debug_assert!(
            (offset.0 as usize) < self.records.len(),
            "Tape::get_unchecked: offset {} out of range (len {})",
            offset.0,
            self.records.len()
        );
        // SAFETY: caller guarantees offset is in bounds.
        unsafe { self.records.get_unchecked(offset.0 as usize) }
    }

    /// Look up a record by offset, returning `None` for the sentinel
    /// or out-of-range offsets.
    #[inline]
    pub fn try_get(&self, offset: TapeOffset) -> Option<&TapeRec> {
        if offset.is_none() {
            return None;
        }
        self.records.get(offset.0 as usize)
    }

    /// Iterate every record in insertion order.
    pub fn iter(&self) -> impl Iterator<Item = &TapeRec> + '_ {
        self.records.iter()
    }

    // ── Payload accessors ─────────────────────────────────────────

    /// Read an arbitrary scalar payload from the record's payload
    /// slot.
    ///
    /// Returns `None` if `rec.payload_idx == 0` (no payload) or if
    /// the payload buffer is too short to hold `T` at that slot
    /// (defensive — should never happen with well-formed builder
    /// output).
    ///
    /// `T` must be `Copy` and ≤ 8 bytes, mirroring the contract on
    /// `TapeBuilder::push_leaf_with_scalar`.
    #[inline]
    /// AU.1: payload byte offset is in `child_off` for scalar leaves.
    /// `payload_idx == 1` is the sentinel for "payload present."
    pub fn payload_scalar<T: Copy>(&self, rec: &TapeRec) -> Option<T> {
        if rec.payload_idx == 0 {
            return None;
        }
        debug_assert!(std::mem::size_of::<T>() <= 8);
        let start = rec.child_off.0 as usize;
        if start + std::mem::size_of::<T>() > self.payloads.len() {
            return None;
        }
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.payloads.as_ptr().add(start),
                v.as_mut_ptr() as *mut u8,
                std::mem::size_of::<T>(),
            );
            Some(v.assume_init())
        }
    }

    /// Read an `f64` payload from the record's payload slot.
    #[inline]
    pub fn payload_f64(&self, rec: &TapeRec) -> Option<f64> {
        self.payload_scalar::<f64>(rec)
    }

    /// Read a `bool` payload from the record's payload slot.
    #[inline]
    pub fn payload_bool(&self, rec: &TapeRec) -> Option<bool> {
        if rec.payload_idx == 0 {
            return None;
        }
        let start = rec.child_off.0 as usize;
        Some(*self.payloads.get(start)? != 0)
    }

    /// Read an `i8` payload from the record's payload slot.
    #[inline]
    pub fn payload_i8(&self, rec: &TapeRec) -> Option<i8> {
        self.payload_scalar::<i8>(rec)
    }

    /// Read a `u8` payload from the record's payload slot.
    #[inline]
    pub fn payload_u8(&self, rec: &TapeRec) -> Option<u8> {
        self.payload_scalar::<u8>(rec)
    }

    /// Read an `i16` payload from the record's payload slot.
    #[inline]
    pub fn payload_i16(&self, rec: &TapeRec) -> Option<i16> {
        self.payload_scalar::<i16>(rec)
    }

    /// Read a `u16` payload from the record's payload slot.
    #[inline]
    pub fn payload_u16(&self, rec: &TapeRec) -> Option<u16> {
        self.payload_scalar::<u16>(rec)
    }

    /// Read an `i32` payload from the record's payload slot.
    #[inline]
    pub fn payload_i32(&self, rec: &TapeRec) -> Option<i32> {
        self.payload_scalar::<i32>(rec)
    }

    /// Read a `u32` payload from the record's payload slot.
    #[inline]
    pub fn payload_u32(&self, rec: &TapeRec) -> Option<u32> {
        self.payload_scalar::<u32>(rec)
    }

    /// Read an `i64` payload from the record's payload slot.
    #[inline]
    pub fn payload_i64(&self, rec: &TapeRec) -> Option<i64> {
        self.payload_scalar::<i64>(rec)
    }

    /// Read a `u64` payload from the record's payload slot.
    #[inline]
    pub fn payload_u64(&self, rec: &TapeRec) -> Option<u64> {
        self.payload_scalar::<u64>(rec)
    }

    /// Read a `Span` payload (lo: u32, hi: u32) from a leaf record.
    ///
    /// The two u32 offsets were packed into a single 8-byte slot by
    /// [`crate::TapeBuilder::push_leaf_with_Span`] as
    /// `lo | (hi << 32)`. Returns `None` when the record carries no
    /// payload (`payload_idx == 0`).
    #[inline]
    #[allow(non_snake_case)]
    pub fn payload_Span(&self, rec: &TapeRec) -> Option<(u32, u32)> {
        let raw = self.payload_u64(rec)?;
        let lo = raw as u32;
        let hi = (raw >> 32) as u32;
        Some((lo, hi))
    }

    /// Read a slice of raw aggregate payload bytes for a record
    /// that was pushed via
    /// [`crate::TapeBuilder::push_leaf_with_aggregate`].
    ///
    /// The caller knows the total byte width from the rule's
    /// [`bbnf_ir::passes::PayloadLayout::total_bytes`]; pass it as
    /// `byte_count`. Returns `None` when the record carries no
    /// payload (`payload_idx == 0`) or when the buffer is too
    /// short to satisfy the request.
    ///
    /// Field-level reads (`f64::from_le_bytes`,
    /// `u32::from_le_bytes`, etc.) slice into the returned buffer
    /// at the per-field `offset` recorded in the layout.
    #[inline]
    pub fn payload_bytes(&self, rec: &TapeRec, byte_count: usize) -> Option<&[u8]> {
        if rec.payload_idx == 0 {
            return None;
        }
        let start = rec.child_off.0 as usize;
        if start + byte_count > self.payloads.len() {
            return None;
        }
        Some(&self.payloads[start..start + byte_count])
    }
}

impl Default for Tape {
    fn default() -> Self {
        Self::new()
    }
}
