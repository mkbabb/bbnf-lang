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
    /// Sentinel used for "no children" / "end of a compound run" and —
    /// on leaf records — "no payload". Every payload-bearing leaf
    /// stores either an arena byte offset or an inline-packed scalar
    /// value in `child_off`; a leaf whose `child_off == NONE` carries
    /// a pure span and no payload.
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
/// - `extra` — 2 bytes of packed per-record flags. Currently only
///   bit 0 is allocated: [`TapeRec::STRING_BORROW_BIT`] marks a
///   decode-kernel leaf whose content is a zero-copy slice of the
///   source (`source[span_lo+1..span_hi-1]`). Remaining 15 bits are
///   free for future packed metadata.
/// - `span_lo` / `span_hi` — byte offsets into the source input.
///   `span_hi == span_lo` means a zero-width record (epsilon match).
/// - `child_off` — polymorphic. For compound nodes, `TapeOffset` of
///   the first child record; children run from `child_off` (inclusive)
///   to the next compound's `child_off` (exclusive). For payload-
///   bearing leaves, the arena byte offset of the payload
///   (wide scalars / aggregates / byte strings) OR the inline-packed
///   scalar value itself (≤ 4-byte scalars). For leaves with no
///   payload or empty compounds, `TapeOffset::NONE`.
///
/// # Tranche AU.6.7 — unified arena payload encoding
///
/// The `payload_idx: u16` field was deleted. A record's payload
/// presence is now encoded by:
/// - `has_children() == false` AND `child_off.is_none()` → pure leaf
///   (no payload).
/// - `has_children() == false` AND `child_off != NONE` → payload-
///   bearing leaf. The reader's caller decides how to decode
///   `child_off` based on the grammar-declared type:
///     * Inline scalars (`u8/i8/u16/i16/u32/i32/f32/bool`) — value
///       packed directly into `child_off.0`.
///     * Wide scalars (`u64/i64/f64/Span`) — `child_off.0` is an
///       8-aligned arena byte offset.
///     * Aggregates — `child_off.0` is an 8-aligned arena byte
///       offset; layout bytes follow verbatim.
///     * Byte strings — `child_off.0` is the arena byte offset of a
///       `(len: u32 LE, bytes: [u8; len])` frame.
/// - `has_children() == true` → compound (Seq/Alt/Repeat/Rule/...).
///   `child_off` is the first-child record offset.
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
    /// Packed per-record flags. Bit 0 ([`Self::STRING_BORROW_BIT`])
    /// marks a string leaf whose content is a zero-copy slice of the
    /// parser input (`source[span_lo+1..span_hi-1]`); the remaining
    /// 15 bits are reserved for future packed metadata. Kept so the
    /// record stays at exactly 16 bytes — a quarter of a 64-byte
    /// cache line — without padding bytes elsewhere.
    pub extra: u16,
    /// Byte offset into the source input where this record's span begins.
    pub span_lo: u32,
    /// Byte offset into the source input where this record's span ends.
    /// `span_hi == span_lo` represents epsilon / zero-width matches.
    pub span_hi: u32,
    /// First child's offset (compounds), arena offset / inline-packed
    /// scalar (payload-bearing leaves), or [`TapeOffset::NONE`]
    /// (no payload / empty compound).
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
    /// When set, the leaf's `child_off` is [`TapeOffset::NONE`] (no
    /// arena write) and the decoded content is
    /// `source[span_lo + 1 .. span_hi - 1]` — JSON strings carry the
    /// quote bytes in the record's span.
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

    /// True iff this record is a leaf that carries a payload (an
    /// arena byte offset or an inline-packed scalar in `child_off`).
    ///
    /// False for compounds (`has_children() == true`) and for pure-
    /// span leaves (`child_off == NONE`).
    #[inline]
    pub fn has_payload(&self) -> bool {
        !self.has_children() && !self.child_off.is_none()
    }

    /// True iff this leaf record is a borrow-safe string (decoded JSON
    /// or similar). The decoded content is
    /// `source[span_lo + 1 .. span_hi - 1]` — no arena read required.
    /// Set by [`crate::TapeBuilder::push_leaf_borrowed_string`] when
    /// the decode kernel reports a `Borrowed` payload.
    #[inline]
    pub fn is_string_borrowed(&self) -> bool {
        (self.extra & Self::STRING_BORROW_BIT) != 0
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
///
/// # Tranche AU.6.7 — unified payload arena
///
/// The AU.1 scalar `payloads: Vec<u8>` slab and the AU.3.1 string
/// `arena: Vec<u8>` slab have been unified into a single
/// [`Tape::arena`]. Wide scalars (f64/u64/i64/Span), aggregate tuples,
/// and byte strings all live there; inline scalars (≤ 4 bytes) pack
/// directly into the record's `child_off` and bypass the arena.
/// The former `payload_idx: u16` sentinel on each record is gone;
/// `child_off != NONE` on a leaf is sufficient to identify a
/// payload-bearing record, and the caller's statically-known type
/// selects the decode path.
#[derive(Debug)]
pub struct Tape {
    /// Flat record storage. Append-only during parsing; immutable
    /// during view-layer reads.
    pub(crate) records: Vec<TapeRec>,
    /// Unified payload arena. Stores:
    ///
    /// - **Wide scalars** (`f64/u64/i64/Span`) — one 8-byte slot per
    ///   value, 8-aligned. `child_off` holds the byte offset.
    /// - **Aggregates** — packed tuple bytes sized by the rule's
    ///   [`PayloadLayout`]. 8-byte aligned slot allocations, with
    ///   the leading `total_bytes` of each slot carrying the
    ///   typed fields.
    /// - **Byte strings** (decoded JSON strings, CSS quoted strings,
    ///   BBNF identifier/literal bodies) — framed as
    ///   `(len: u32 LE, bytes: [u8; len])`. `child_off` points at
    ///   the length prefix.
    ///
    /// Inline scalars (`u8/i8/u16/i16/u32/i32/f32/bool`) are NOT
    /// stored here — they pack into each record's `child_off` directly.
    pub(crate) arena: Vec<u8>,
}

impl Tape {
    /// Construct an empty tape with default initial capacity.
    pub fn new() -> Self {
        Self {
            records: Vec::new(),
            arena: Vec::new(),
        }
    }

    /// Construct an empty tape sized for `expected` records.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            records: Vec::with_capacity(expected),
            arena: Vec::new(),
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
    //
    // AU.6.7: every accessor below dispatches on the record's
    // `child_off`. A `NONE` child_off indicates the absence of a
    // payload and each accessor returns `None`. Inline scalars
    // recover their value from `child_off.0` directly; wide scalars,
    // aggregates, and byte strings read from the unified arena.

    /// Read an inline-packed scalar payload (≤ 4 bytes) directly
    /// out of `rec.child_off`.
    ///
    /// `T` must be `Copy` and its size must be ≤ 4 bytes. Wider
    /// scalars live in the arena and must be read via
    /// [`Self::payload_wide`].
    ///
    /// Returns `None` for non-payload records (`child_off == NONE`).
    #[inline]
    fn payload_inline<T: Copy>(&self, rec: &TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 4);
        if rec.child_off.is_none() {
            return None;
        }
        let bytes = rec.child_off.0.to_le_bytes();
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` and size_of::<T>() <= 4, matching the
        // width of `bytes`. The copy writes size_of::<T>() bytes from
        // the record's own `child_off` bytes, which are always
        // initialised (the record itself is initialised).
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                std::mem::size_of::<T>(),
            );
            Some(v.assume_init())
        }
    }

    /// Read a wide (8-byte) scalar payload from the arena slot at
    /// `rec.child_off`.
    ///
    /// Returns `None` for non-payload records or when the arena is
    /// too short to carry an 8-byte slot at the recorded offset.
    #[inline]
    fn payload_wide<T: Copy>(&self, rec: &TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() == 8);
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        if start + 8 > self.arena.len() {
            return None;
        }
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: bounds check above; `T` is `Copy` of size 8.
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.arena.as_ptr().add(start),
                v.as_mut_ptr() as *mut u8,
                8,
            );
            Some(v.assume_init())
        }
    }

    /// Read an arbitrary scalar payload from the record.
    ///
    /// `T` must be `Copy` and ≤ 8 bytes. Sizes ≤ 4 bytes pack inline
    /// into `child_off`; wider sizes read from the arena. Returns
    /// `None` for records with no payload.
    #[inline]
    pub fn payload_scalar<T: Copy>(&self, rec: &TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 8);
        if std::mem::size_of::<T>() <= 4 {
            self.payload_inline::<T>(rec)
        } else {
            self.payload_wide::<T>(rec)
        }
    }

    /// Read an `f64` payload.
    #[inline]
    pub fn payload_f64(&self, rec: &TapeRec) -> Option<f64> {
        self.payload_wide::<f64>(rec)
    }

    /// Read a `bool` payload (inline-packed).
    #[inline]
    pub fn payload_bool(&self, rec: &TapeRec) -> Option<bool> {
        self.payload_inline::<u8>(rec).map(|b| b != 0)
    }

    /// Read an `i8` payload (inline-packed).
    #[inline]
    pub fn payload_i8(&self, rec: &TapeRec) -> Option<i8> {
        self.payload_inline::<i8>(rec)
    }

    /// Read a `u8` payload (inline-packed).
    #[inline]
    pub fn payload_u8(&self, rec: &TapeRec) -> Option<u8> {
        self.payload_inline::<u8>(rec)
    }

    /// Read an `i16` payload (inline-packed).
    #[inline]
    pub fn payload_i16(&self, rec: &TapeRec) -> Option<i16> {
        self.payload_inline::<i16>(rec)
    }

    /// Read a `u16` payload (inline-packed).
    #[inline]
    pub fn payload_u16(&self, rec: &TapeRec) -> Option<u16> {
        self.payload_inline::<u16>(rec)
    }

    /// Read an `i32` payload (inline-packed).
    #[inline]
    pub fn payload_i32(&self, rec: &TapeRec) -> Option<i32> {
        self.payload_inline::<i32>(rec)
    }

    /// Read a `u32` payload (inline-packed).
    #[inline]
    pub fn payload_u32(&self, rec: &TapeRec) -> Option<u32> {
        self.payload_inline::<u32>(rec)
    }

    /// Read an `i64` payload (wide; arena-backed).
    #[inline]
    pub fn payload_i64(&self, rec: &TapeRec) -> Option<i64> {
        self.payload_wide::<i64>(rec)
    }

    /// Read a `u64` payload (wide; arena-backed).
    #[inline]
    pub fn payload_u64(&self, rec: &TapeRec) -> Option<u64> {
        self.payload_wide::<u64>(rec)
    }

    /// Read a `Span` payload (lo: u32, hi: u32) from a leaf record.
    ///
    /// The two u32 offsets are packed into a single 8-byte arena slot
    /// as `lo | (hi << 32)`. Returns `None` for non-payload records.
    #[inline]
    #[allow(non_snake_case)]
    pub fn payload_Span(&self, rec: &TapeRec) -> Option<(u32, u32)> {
        let raw = self.payload_u64(rec)?;
        let lo = raw as u32;
        let hi = (raw >> 32) as u32;
        Some((lo, hi))
    }

    /// Read a variable-length decoded payload as `&str`.
    ///
    /// Returns the decoded UTF-8 slice for a record whose payload was
    /// pushed via [`crate::TapeBuilder::push_leaf_with`] with a
    /// `PayloadData::Bytes(...)` shape. The arena frame layout is
    /// `(len: u32 LE, bytes: [u8; len])` at the byte offset stored in
    /// `rec.child_off`.
    ///
    /// Returns `None` when the record has no payload or when the
    /// arena slot is truncated.
    ///
    /// # Safety / contract
    ///
    /// Callers that route bytes through `PayloadData::Bytes` must
    /// supply well-formed UTF-8 (the JSON decode kernel guarantees
    /// this by construction). The accessor uses
    /// `std::str::from_utf8_unchecked` on the hot path; a
    /// `debug_assert!` round-trips `std::str::from_utf8` to catch
    /// regressions in debug builds.
    #[inline]
    pub fn payload_string(&self, rec: &TapeRec) -> Option<&str> {
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
    /// the UTF-8 check. Returns `None` for non-payload records or
    /// truncated slots; never panics.
    #[inline]
    pub fn payload_string_bytes(&self, rec: &TapeRec) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        if start + 4 > self.arena.len() {
            return None;
        }
        let len_bytes: [u8; 4] = self.arena[start..start + 4].try_into().ok()?;
        let len = u32::from_le_bytes(len_bytes) as usize;
        let body_start = start + 4;
        let body_end = body_start + len;
        if body_end > self.arena.len() {
            return None;
        }
        Some(&self.arena[body_start..body_end])
    }

    /// Source-aware string accessor — returns the decoded UTF-8 of a
    /// string leaf without touching the arena when the record is
    /// borrow-safe.
    ///
    /// Decode-kernel leaves push themselves either as borrowed (no
    /// arena write — see [`crate::TapeBuilder::push_leaf_borrowed_string`])
    /// or owned ([`Self::payload_string`] for the arena-frame path).
    /// This accessor honours both: when [`TapeRec::is_string_borrowed`]
    /// is set, the content is `source[span_lo + 1 .. span_hi - 1]`;
    /// otherwise it falls through to [`Self::payload_string`].
    ///
    /// `source` MUST be the same byte slice the parser consumed.
    /// Returns `None` for non-string records, malformed UTF-8, or
    /// out-of-range spans.
    #[inline]
    pub fn payload_string_with_source<'s, 'a: 's, 't: 's>(
        &'t self,
        rec: &TapeRec,
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
    /// Primarily used by view-layer accessors that want to slice the
    /// arena directly (e.g. a borrowed fast path that skipped
    /// copying the decoded bytes). Exposed for test / debug
    /// introspection; prefer [`Self::payload_string`] for the
    /// decoded accessor contract.
    #[inline]
    pub fn arena(&self) -> &[u8] {
        &self.arena
    }

    /// Read a slice of raw aggregate payload bytes for a record
    /// whose payload was written via [`crate::PayloadData::Aggregate`].
    ///
    /// The caller knows the total byte width from the rule's
    /// [`bbnf_ir::passes::PayloadLayout::total_bytes`]; pass it as
    /// `byte_count`. Returns `None` when the record carries no
    /// payload or when the arena is too short to satisfy the request.
    ///
    /// Field-level reads (`f64::from_le_bytes`,
    /// `u32::from_le_bytes`, etc.) slice into the returned buffer at
    /// the per-field `offset` recorded in the layout.
    #[inline]
    pub fn payload_bytes(&self, rec: &TapeRec, byte_count: usize) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        let start = rec.child_off.0 as usize;
        if start + byte_count > self.arena.len() {
            return None;
        }
        Some(&self.arena[start..start + byte_count])
    }
}

impl Default for Tape {
    fn default() -> Self {
        Self::new()
    }
}
