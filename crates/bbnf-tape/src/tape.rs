//! `Tape`, `TapeRec`, `TapeOffset` — the record substrate the parser
//! emits into.
//!
//! Tranche AV Phase 2 (AV.2.1 – AV.2.3) flipped the storage shape
//! from row-major `Vec<TapeRec>` to column-major [`Columns`]. The
//! 16-byte `TapeRec` survives as the materialised-view shape every
//! reader consumes; it is reconstructed on demand from the
//! structural columns, so the substrate pays no per-record pointer
//! dereference cost during typed-payload visitor kernels (which is
//! what unlocks the 4-lane reordered unrolling landing in V2.5).
//!
//! `TapeOffset` is unchanged on the wire: a `u32` index into the
//! column set, with `u32::MAX` reserved as the `NONE` sentinel.

use crate::columns::Columns;
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
    /// Sentinel used for "no children" / "end of a compound run" and
    /// on leaf records "no payload". Every payload-bearing leaf's
    /// `child_off` stores either a column rank (for inline / wide
    /// scalars) or an arena byte offset (for aggregates and byte
    /// frames); a leaf whose `child_off == NONE` carries a pure span
    /// and no payload.
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

/// Materialised-view of a single structural record.
///
/// Post-AV the flat `Vec<TapeRec>` is gone; this struct is the shape
/// [`Columns`] reconstructs on demand. It stays at 16 bytes with the
/// same field set + packing for two reasons:
///
/// 1. External consumers that captured `&TapeRec` or unpacked its
///    bit layout continue to work verbatim — readers only change
///    from `&TapeRec` to owned `TapeRec`.
/// 2. Any substrate layout change lands as an edit to the
///    [`Columns`] materialiser, not a cascade through tests /
///    generated code / view layer.
///
/// Field semantics track [`Columns`] exactly:
///
/// - `kind_meta` — packed byte: low 4 bits = [`TapeKind`]
///   discriminant, high 4 bits = `meta_idx` bits \[0:3\].
/// - `flags` — bitfield: variant index (low 6 bits), `has_children`
///   (bit 6), `meta_idx` bit \[4\] (bit 7).
/// - `extra` — packed per-record flags ([`Self::STRING_BORROW_BIT`]
///   today; 15 bits free).
/// - `span_lo` / `span_hi` — source-byte offsets.
/// - `child_off` — polymorphic pointer (see [`Columns::child_off`]).
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TapeRec {
    /// Packed byte: low 4 bits = [`TapeKind`] discriminant, high 4
    /// bits = `meta_idx` bits \[0:3\].
    pub kind_meta: u8,
    /// Bitfield: variant index (low 6 bits), has_children (bit 6),
    /// meta_idx bit \[4\] (bit 7).
    pub flags: u8,
    /// Packed per-record flags. Bit 0 ([`Self::STRING_BORROW_BIT`])
    /// marks a string leaf whose content is a zero-copy slice of
    /// the parser input (`source[span_lo+1..span_hi-1]`). Remaining
    /// 15 bits are free for future packed metadata.
    pub extra: u16,
    /// Byte offset into the source input where this record's span
    /// begins.
    pub span_lo: u32,
    /// Byte offset into the source input where this record's span
    /// ends. `span_hi == span_lo` represents epsilon / zero-width
    /// matches.
    pub span_hi: u32,
    /// Polymorphic payload / child pointer. For compounds: first
    /// child's offset (or [`TapeOffset::NONE`] on empty runs). For
    /// leaves with inline / wide scalar payload: the column rank
    /// into [`Columns::pay_narrow`] / [`Columns::pay_wide`]. For
    /// leaves with an arena-backed payload (`Bytes` / `Aggregate` /
    /// `LargeAggregate`): the [`Columns::pay_agg`] byte offset. For
    /// leaves with no payload: [`TapeOffset::NONE`].
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

    /// Bit in [`TapeRec::extra`] that marks a decode-kernel string
    /// leaf whose content is a zero-copy borrow of the parser input.
    /// When set, the leaf's `child_off` is [`TapeOffset::NONE`] and
    /// the decoded content is `source[span_lo + 1 .. span_hi - 1]`.
    pub const STRING_BORROW_BIT: u16 = 0x0001;

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

    /// True iff this record is a leaf that carries a payload (a
    /// column rank or an arena byte offset in `child_off`).
    ///
    /// False for compounds (`has_children() == true`) and for pure-
    /// span leaves (`child_off == NONE`).
    #[inline]
    pub fn has_payload(&self) -> bool {
        !self.has_children() && !self.child_off.is_none()
    }

    /// True iff this leaf record is a borrow-safe string (decoded
    /// JSON or similar). The decoded content is
    /// `source[span_lo + 1 .. span_hi - 1]` — no arena read required.
    #[inline]
    pub fn is_string_borrowed(&self) -> bool {
        (self.extra & Self::STRING_BORROW_BIT) != 0
    }
}

/// The parser's output tape.
///
/// Owns a [`Columns`] substrate. The view layer reads records via
/// [`Tape::get`] (which materialises a 16-byte [`TapeRec`] from the
/// structural columns) and payload data through the `payload_*`
/// accessors (which dispatch on the record's `child_off` semantics).
///
/// # Tranche AV Phase 2 (AV.2.1 – AV.2.3)
///
/// - Records are stored across six structural columns plus
///   `child_off`; bulk typed visitors see dense 8-byte `pay_wide`
///   and 4-byte `pay_narrow` columns.
/// - Sibling traversal runs forward over a `sib_skip` column; the
///   earlier backward-walk child enumeration is gone.
/// - `InlineScalar` payloads now land in `pay_narrow` (never packed
///   into `child_off`). The AU-era collision with
///   `TapeOffset::NONE` for `u32::MAX` inline values is resolved:
///   `child_off` carries the column rank, which is a push-ordered
///   counter.
#[derive(Debug, Default)]
pub struct Tape {
    pub(crate) columns: Columns,
}

impl Tape {
    /// Construct an empty tape.
    pub fn new() -> Self {
        Self {
            columns: Columns::new(),
        }
    }

    /// Construct an empty tape sized for `expected` records.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            columns: Columns::with_capacity(expected),
        }
    }

    /// Number of records appended to the tape so far.
    #[inline]
    pub fn len(&self) -> usize {
        self.columns.len()
    }

    /// True iff no records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.columns.is_empty()
    }

    /// Borrow the underlying [`Columns`] substrate.
    ///
    /// Exposed so the view codegen (wave V2.5) and walker migration
    /// (wave V2.6) can emit column-indexed reads without going
    /// through per-record materialisation.
    #[inline]
    pub fn columns(&self) -> &Columns {
        &self.columns
    }

    /// Look up a record by offset. Panics on out-of-range offsets —
    /// view codegen never produces out-of-range offsets because every
    /// offset originates from a `TapeBuilder::push_*` call.
    ///
    /// Returns a 16-byte [`TapeRec`] by value, materialised from the
    /// structural columns. Callers that used to bind `&TapeRec` now
    /// bind owned `TapeRec`; all access is through `Copy` methods so
    /// the ergonomic surface is unchanged.
    #[inline]
    pub fn get(&self, offset: TapeOffset) -> TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get called with TapeOffset::NONE sentinel"
        );
        self.columns.materialize(offset.0)
    }

    /// Look up a record by offset **without bounds checking**.
    ///
    /// # Safety
    ///
    /// The caller must guarantee that `offset` is not
    /// [`TapeOffset::NONE`] and that `offset.0 as usize` is less
    /// than `self.len()`. Both invariants hold for every offset
    /// produced by [`TapeBuilder::push_leaf`] /
    /// [`TapeBuilder::push_compound`].
    ///
    /// [`TapeBuilder::push_leaf`]: crate::builder::TapeBuilder::push_leaf
    /// [`TapeBuilder::push_compound`]: crate::builder::TapeBuilder::push_compound
    #[inline]
    pub unsafe fn get_unchecked(&self, offset: TapeOffset) -> TapeRec {
        debug_assert!(
            !offset.is_none(),
            "Tape::get_unchecked called with TapeOffset::NONE sentinel"
        );
        debug_assert!(
            (offset.0 as usize) < self.columns.len(),
            "Tape::get_unchecked: offset {} out of range (len {})",
            offset.0,
            self.columns.len()
        );
        // SAFETY: caller guarantees offset is in bounds.
        unsafe { self.columns.materialize_unchecked(offset.0) }
    }

    /// Look up a record by offset, returning `None` for the sentinel
    /// or out-of-range offsets.
    #[inline]
    pub fn try_get(&self, offset: TapeOffset) -> Option<TapeRec> {
        if offset.is_none() {
            return None;
        }
        let idx = offset.0 as usize;
        if idx >= self.columns.len() {
            return None;
        }
        Some(self.columns.materialize(offset.0))
    }

    /// Iterate every record in insertion order.
    ///
    /// The iterator materialises a fresh [`TapeRec`] per yield; this
    /// is free (16 bytes, `Copy`) but removes the `&TapeRec` borrow
    /// the pre-AV API handed out.
    pub fn iter(&self) -> TapeIter<'_> {
        TapeIter {
            columns: &self.columns,
            idx: 0,
        }
    }

    // ── Payload accessors ─────────────────────────────────────────
    //
    // AV.2.3: inline scalars land in `pay_narrow`, wide scalars in
    // `pay_wide`, and aggregates / byte-frames in `pay_agg`. The
    // record's `child_off` carries the appropriate pointer — column
    // rank for scalars, arena byte offset for aggregates.

    /// Read an inline-packed scalar payload (≤ 4 bytes) from the
    /// `pay_narrow` column.
    ///
    /// `T` must be `Copy` and its size must be ≤ 4 bytes. Wider
    /// scalars live in `pay_wide` and must be read via
    /// [`Self::payload_wide`].
    #[inline]
    fn payload_inline<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 4);
        if rec.child_off.is_none() {
            return None;
        }
        let rank = rec.child_off.0 as usize;
        if rank >= self.columns.pay_narrow.len() {
            return None;
        }
        let raw = self.columns.pay_narrow[rank];
        let bytes = raw.to_le_bytes();
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` and size_of::<T>() <= 4, matching the
        // width of `bytes`. The copy writes size_of::<T>() bytes from
        // the column entry, which is always initialised.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                std::mem::size_of::<T>(),
            );
            Some(v.assume_init())
        }
    }

    /// Read a wide (8-byte) scalar payload from the `pay_wide`
    /// column.
    #[inline]
    fn payload_wide<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() == 8);
        if rec.child_off.is_none() {
            return None;
        }
        let rank = rec.child_off.0 as usize;
        if rank >= self.columns.pay_wide.len() {
            return None;
        }
        let raw = self.columns.pay_wide[rank].to_le_bytes();
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` of size 8; the source is the full
        // 8-byte LE representation of the stored `u64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                raw.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                8,
            );
            Some(v.assume_init())
        }
    }

    /// Read an arbitrary scalar payload from the record.
    ///
    /// `T` must be `Copy` and ≤ 8 bytes. Sizes ≤ 4 bytes hit
    /// `pay_narrow`; 8-byte sizes hit `pay_wide`.
    #[inline]
    pub fn payload_scalar<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 8);
        if std::mem::size_of::<T>() <= 4 {
            self.payload_inline::<T>(rec)
        } else {
            self.payload_wide::<T>(rec)
        }
    }

    /// Read an `f64` payload from `pay_wide`.
    #[inline]
    pub fn payload_f64(&self, rec: TapeRec) -> Option<f64> {
        self.payload_wide::<f64>(rec)
    }

    /// Read a `bool` payload from `pay_narrow`.
    #[inline]
    pub fn payload_bool(&self, rec: TapeRec) -> Option<bool> {
        self.payload_inline::<u8>(rec).map(|b| b != 0)
    }

    /// Read an `i8` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i8(&self, rec: TapeRec) -> Option<i8> {
        self.payload_inline::<i8>(rec)
    }

    /// Read a `u8` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u8(&self, rec: TapeRec) -> Option<u8> {
        self.payload_inline::<u8>(rec)
    }

    /// Read an `i16` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i16(&self, rec: TapeRec) -> Option<i16> {
        self.payload_inline::<i16>(rec)
    }

    /// Read a `u16` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u16(&self, rec: TapeRec) -> Option<u16> {
        self.payload_inline::<u16>(rec)
    }

    /// Read an `i32` payload from `pay_narrow`.
    #[inline]
    pub fn payload_i32(&self, rec: TapeRec) -> Option<i32> {
        self.payload_inline::<i32>(rec)
    }

    /// Read a `u32` payload from `pay_narrow`.
    #[inline]
    pub fn payload_u32(&self, rec: TapeRec) -> Option<u32> {
        self.payload_inline::<u32>(rec)
    }

    /// Read an `i64` payload from `pay_wide`.
    #[inline]
    pub fn payload_i64(&self, rec: TapeRec) -> Option<i64> {
        self.payload_wide::<i64>(rec)
    }

    /// Read a `u64` payload from `pay_wide`.
    #[inline]
    pub fn payload_u64(&self, rec: TapeRec) -> Option<u64> {
        self.payload_wide::<u64>(rec)
    }

    /// Read a `Span` payload (lo: u32, hi: u32) from `pay_wide`.
    #[inline]
    #[allow(non_snake_case)]
    pub fn payload_Span(&self, rec: TapeRec) -> Option<(u32, u32)> {
        let raw = self.payload_u64(rec)?;
        let lo = raw as u32;
        let hi = (raw >> 32) as u32;
        Some((lo, hi))
    }

    /// Read a variable-length decoded payload as `&str`.
    ///
    /// Returns the decoded UTF-8 slice for a record whose payload
    /// was pushed via [`crate::TapeBuilder::push_leaf_with`] with a
    /// [`crate::PayloadData::Bytes`] shape. The arena frame layout
    /// is `(len: u32 LE, bytes: [u8; len])` at the byte offset
    /// stored in `rec.child_off`.
    #[inline]
    pub fn payload_string(&self, rec: TapeRec) -> Option<&str> {
        let bytes = self.payload_string_bytes(rec)?;
        debug_assert!(
            std::str::from_utf8(bytes).is_ok(),
            "Tape::payload_string: malformed UTF-8 in arena slot at offset {}",
            rec.child_off.0,
        );
        // SAFETY: byte-string callers route UTF-8 through the decoder
        // kernels that enforce well-formed output; the debug_assert
        // round-trips `std::str::from_utf8` in debug builds.
        Some(unsafe { std::str::from_utf8_unchecked(bytes) })
    }

    /// Read a variable-length decoded payload as raw bytes.
    ///
    /// Same slot semantics as [`Self::payload_string`] but without
    /// the UTF-8 check.
    #[inline]
    pub fn payload_string_bytes(&self, rec: TapeRec) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        let arena = self.arena();
        if start + 4 > arena.len() {
            return None;
        }
        let len_bytes: [u8; 4] = arena[start..start + 4].try_into().ok()?;
        let len = u32::from_le_bytes(len_bytes) as usize;
        let body_start = start + 4;
        let body_end = body_start + len;
        if body_end > arena.len() {
            return None;
        }
        Some(&arena[body_start..body_end])
    }

    /// Source-aware string accessor — returns the decoded UTF-8 of
    /// a string leaf without touching the arena when the record is
    /// borrow-safe.
    ///
    /// Decode-kernel leaves push themselves either as borrowed (no
    /// arena write — see
    /// [`crate::TapeBuilder::push_leaf_borrowed_string`]) or owned
    /// ([`Self::payload_string`] for the arena-frame path). This
    /// accessor honours both.
    #[inline]
    pub fn payload_string_with_source<'s, 'a: 's, 't: 's>(
        &'t self,
        rec: TapeRec,
        source: &'a [u8],
    ) -> Option<&'s str> {
        if rec.is_string_borrowed() {
            let lo = rec.span_lo as usize + 1;
            let hi = (rec.span_hi as usize).checked_sub(1)?;
            if hi > source.len() || lo > hi {
                return None;
            }
            let bytes = unsafe { source.get_unchecked(lo..hi) };
            debug_assert!(
                std::str::from_utf8(bytes).is_ok(),
                "borrowed string at span [{}, {}) is not UTF-8",
                lo,
                hi,
            );
            // SAFETY: callers route bytes through the JSON decoder
            // kernel which only emits Borrowed for ASCII-clean
            // sources; the debug_assert round-trips std::str::from_utf8
            // in debug builds.
            return Some(unsafe { std::str::from_utf8_unchecked(bytes) });
        }
        self.payload_string(rec)
    }

    /// Borrow the tape's unified payload arena (read-only).
    ///
    /// Backed by [`Columns::pay_agg`]. Primarily used by view-layer
    /// accessors that want to slice the arena directly (e.g. a
    /// borrowed fast path that skipped copying the decoded bytes).
    #[inline]
    pub fn arena(&self) -> &[u8] {
        &self.columns.pay_agg
    }

    /// Read a slice of raw aggregate payload bytes for a record
    /// whose payload was written via [`crate::PayloadData::Aggregate`].
    ///
    /// The caller knows the total byte width from the rule's
    /// [`bbnf_ir::passes::PayloadLayout::total_bytes`]; pass it as
    /// `byte_count`. Returns `None` when the record carries no
    /// payload or when the arena is too short to satisfy the
    /// request.
    #[inline]
    pub fn payload_bytes(&self, rec: TapeRec, byte_count: usize) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        let arena = self.arena();
        if start + byte_count > arena.len() {
            return None;
        }
        Some(&arena[start..start + byte_count])
    }
}

/// Iterator over every record in a tape, materialising owned
/// [`TapeRec`]s in column order.
#[derive(Debug)]
pub struct TapeIter<'t> {
    columns: &'t Columns,
    idx: u32,
}

impl<'t> Iterator for TapeIter<'t> {
    type Item = TapeRec;

    #[inline]
    fn next(&mut self) -> Option<TapeRec> {
        if (self.idx as usize) >= self.columns.len() {
            return None;
        }
        let rec = self.columns.materialize(self.idx);
        self.idx += 1;
        Some(rec)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.columns.len() - self.idx as usize;
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for TapeIter<'_> {}
