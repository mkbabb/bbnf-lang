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
/// - `flags` — full 8-bit `variant_idx` (rule discriminant in
///   `[0, 256)`). AW-III.W1.A widened from 6 to 8 bits to admit
///   grammars with > 64 typed rules (CSS L4 has 186); the prior
///   mask collided distinct rules sharing low-6-bit ids.
/// - `extra` — packed per-record flags. Bit 0
///   ([`Self::STRING_BORROW_BIT`]), bit 1 ([`Self::PAYLOAD_IN_ARENA_BIT`]),
///   bit 2 ([`Self::HAS_CHILDREN_BIT`]), bit 3 ([`Self::META_IDX_HI_BIT`]).
/// - `span_lo` / `span_hi` — source-byte offsets.
/// - `child_off` — polymorphic pointer (see [`Columns::child_off`]).
#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TapeRec {
    /// Packed byte: low 4 bits = [`TapeKind`] discriminant, high 4
    /// bits = `meta_idx` bits \[0:3\].
    pub kind_meta: u8,
    /// Full 8-bit `variant_idx` (rule discriminant in `[0, 256)`).
    /// AW-III.W1.A widened from 6 → 8 bits; the prior `& 0x3F` mask
    /// collided distinct rules whose ids shared their low six bits
    /// (CSS L4's `colorProps` and `namedColor`, BBNF's `> 64` rules).
    /// `has_children` and `meta_idx` bit \[4\] moved to
    /// [`Self::extra`].
    pub flags: u8,
    /// Packed per-record flags.
    ///
    /// - bit 0: [`Self::STRING_BORROW_BIT`] — string leaf borrows
    ///   from source.
    /// - bit 1: [`Self::PAYLOAD_IN_ARENA_BIT`] — `child_off` is an
    ///   arena byte offset.
    /// - bit 2: [`Self::HAS_CHILDREN_BIT`] — compound has emitted
    ///   children (was bit 6 of `flags` pre-AW-III.W1.A).
    /// - bit 3: [`Self::META_IDX_HI_BIT`] — high bit (bit \[4\]) of
    ///   the 5-bit `meta_idx` (was bit 7 of `flags` pre-AW-III.W1.A).
    /// Remaining 12 bits are free for future packed metadata.
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

    /// AW-III.W1 — bit in [`TapeRec::extra`] that marks a leaf whose
    /// `child_off` is an arena byte offset (`pay_agg`) rather than a
    /// column rank (`pay_narrow` / `pay_wide`).
    ///
    /// Set by the DTA walker for every typed-leaf payload (constant
    /// from `Map { Literal, IntLit }`, decoded from `Map { Regex,
    /// FnDescriptor }`). The scalar readers (`payload_u8` / etc.)
    /// branch on this bit: when set, they slice `pay_agg` directly;
    /// when clear, they read the legacy `pay_narrow` / `pay_wide`
    /// column at the column-rank index. The dual path is kept so
    /// pre-DTA `push_leaf_with(InlineScalar / WideScalar)` plumbing
    /// (unit tests) survives unchanged.
    pub const PAYLOAD_IN_ARENA_BIT: u16 = 0x0002;

    /// AW-III.W1.A — bit in [`TapeRec::extra`] marking a compound
    /// record whose children run is non-empty. Migrated from the
    /// pre-W1.A `flags` bit 6 to free the full `flags` byte for an
    /// 8-bit `variant_idx`.
    pub const HAS_CHILDREN_BIT: u16 = 0x0004;

    /// AW-III.W1.A — bit in [`TapeRec::extra`] carrying the high bit
    /// (bit \[4\]) of the 5-bit `meta_idx`. Migrated from the
    /// pre-W1.A `flags` bit 7 alongside [`Self::HAS_CHILDREN_BIT`].
    pub const META_IDX_HI_BIT: u16 = 0x0008;

    /// AY.W4.2 — bit in [`TapeRec::extra`] marking a numeric `f64`
    /// leaf whose payload was written directly into
    /// [`crate::Columns::pay_f64`] by the Eisel-Lemire fast path
    /// (`crate::TapeBuilder::push_leaf_with_f64_direct`). When set,
    /// the record's `child_off` carries the column rank into
    /// `pay_f64`; the reader projects `f64::from_bits(pay_f64[rank])`
    /// directly without the `pay_wide` / arena round-trip.
    ///
    /// Mutually exclusive with [`Self::PAYLOAD_IN_ARENA_BIT`] — the
    /// arena-bit path slices `pay_agg` for an 8-byte LE encoding;
    /// this bit selects the dense `pay_f64` column.
    pub const PAYLOAD_F64_DIRECT_BIT: u16 = 0x0010;

    // Bit `0x0020` reserved for future packed metadata. Vacated in
    // AY-II.W0.a when write-time sibling-skip stamping retired and
    // the finaliser became the sole writer of `sib_skip`.

    /// Pack a [`TapeKind`] and `meta_idx` into the `kind_meta` byte
    /// and an `extra` companion bit. Returns
    /// `(kind_meta, extra_meta_bit)` where `extra_meta_bit` is
    /// either `0` or [`Self::META_IDX_HI_BIT`] — the caller ORs it
    /// into the `extra` u16.
    ///
    /// AW-III.W1.A — the high bit of `meta_idx` migrated from
    /// `flags[7]` to `extra[3]` to free the full `flags` byte for an
    /// 8-bit `variant_idx`.
    #[inline]
    pub(crate) fn pack_kind_meta(kind: TapeKind, meta_idx: u8) -> (u8, u16) {
        debug_assert!(
            meta_idx <= Self::MAX_META_IDX,
            "meta_idx {} exceeds 5-bit maximum ({})",
            meta_idx,
            Self::MAX_META_IDX,
        );
        let kind_meta = (kind as u8 & 0x0F) | ((meta_idx & 0x0F) << 4);
        let extra_meta_bit = if meta_idx & 0x10 != 0 {
            Self::META_IDX_HI_BIT
        } else {
            0
        };
        (kind_meta, extra_meta_bit)
    }

    /// Extract the [`TapeKind`] from the packed `kind_meta` byte.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        TapeKind::from_u8(self.kind_meta & 0x0F)
    }

    /// Extract the 5-bit `meta_idx` from the packed `kind_meta` byte
    /// (low 4 bits) and `extra` (high bit).
    #[inline]
    pub fn meta_idx(&self) -> u8 {
        let lo4 = (self.kind_meta >> 4) & 0x0F;
        let hi1 = if (self.extra & Self::META_IDX_HI_BIT) != 0 {
            1
        } else {
            0
        };
        lo4 | (hi1 << 4)
    }

    /// Extract the 8-bit variant index from `flags`.
    ///
    /// AW-III.W1.A widened from 6 to 8 bits — the prior `& 0x3F`
    /// mask is gone. Distinct rules whose ids share low six bits
    /// (CSS L4's `colorProps` and `namedColor`) no longer collide.
    #[inline]
    pub fn variant_idx(&self) -> u8 {
        self.flags
    }

    /// Does this record have children? Bit [`Self::HAS_CHILDREN_BIT`]
    /// of `extra` (post-AW-III.W1.A; was `flags` bit 6).
    #[inline]
    pub fn has_children(&self) -> bool {
        (self.extra & Self::HAS_CHILDREN_BIT) != 0
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

    /// AW-III.W1 — true iff this leaf's `child_off` is an arena byte
    /// offset (`pay_agg`) rather than a column rank.
    #[inline]
    pub fn payload_in_arena(&self) -> bool {
        (self.extra & Self::PAYLOAD_IN_ARENA_BIT) != 0
    }

    /// AY.W4.2 — true iff this leaf's `child_off` is a column rank
    /// into [`crate::Columns::pay_f64`] (the Eisel-Lemire direct-write
    /// column). Set by [`crate::TapeBuilder::push_leaf_with_f64_direct`].
    #[inline]
    pub fn payload_f64_direct(&self) -> bool {
        (self.extra & Self::PAYLOAD_F64_DIRECT_BIT) != 0
    }

    // ── ShapeRef accessors (AV.5.1) ──────────────────────────────

    /// Extract the shape-dictionary index from a `ShapeRef` record's
    /// `flags` (low 5 bits).
    ///
    /// Only meaningful when `kind() == TapeKind::ShapeRef`.
    #[inline]
    pub fn shape_dict_idx(&self) -> u8 {
        self.flags & 0x1F
    }

    /// True iff this `ShapeRef` record carries a packed payload blob.
    /// Bit 5 of `flags`.
    ///
    /// Only meaningful when `kind() == TapeKind::ShapeRef`.
    #[inline]
    pub fn shape_ref_has_payload(&self) -> bool {
        (self.flags & 0x20) != 0
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
    ///
    /// Callers presize via the per-grammar push fingerprint:
    /// `GRAMMAR_PROFILE.capacity_for(input.len())`. The reservation
    /// covers `records` (16 B AoS rows) + `sib_skip` (4 B parallel
    /// column) in lockstep so the hot push path never trips a
    /// `Vec::push` realloc on corpus input.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            columns: Columns::with_capacity(expected),
        }
    }

    /// Construct an empty tape sized from a [`crate::GrammarProfile`]
    /// + `input_len`.
    ///
    /// Convenience for the parser entry point — equivalent to
    /// `Tape::with_capacity(profile.capacity_for(input_len))`. The
    /// per-grammar density coefficients (`compounds_per_input_byte`
    /// + `leaves_per_input_byte`, AU.6.2) drive the reservation,
    /// with the AR-audit `input_len / 2 + 2` floor as backstop.
    #[inline]
    pub fn with_capacity_for(profile: &crate::GrammarProfile, input_len: usize) -> Self {
        Self::with_capacity(profile.capacity_for(input_len))
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
    #[inline(always)]
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
    #[inline(always)]
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
    /// `pay_narrow` column or — when [`TapeRec::payload_in_arena`]
    /// is set — from the unified arena (`pay_agg`).
    ///
    /// `T` must be `Copy` and its size must be ≤ 4 bytes. Wider
    /// scalars live in `pay_wide` and must be read via
    /// [`Self::payload_wide`].
    ///
    /// AW-III.W1 dual path: pre-W1 `push_leaf_with(InlineScalar)`
    /// plumbing wrote into `pay_narrow` and stamped `child_off` to
    /// the column rank — those records keep `payload_in_arena() ==
    /// false`. Post-W1 the DTA walker writes the constant byte into
    /// `pay_agg` and sets the arena bit; the reader slices the arena
    /// at `child_off`.
    #[inline]
    fn payload_inline<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() <= 4);
        if rec.child_off.is_none() {
            return None;
        }
        let n = std::mem::size_of::<T>();
        let bytes = if rec.payload_in_arena() {
            let off = rec.child_off.0 as usize;
            let arena = &self.columns.pay_agg;
            if off + n > arena.len() {
                return None;
            }
            let mut buf = [0u8; 4];
            buf[..n].copy_from_slice(&arena[off..off + n]);
            buf
        } else {
            let rank = rec.child_off.0 as usize;
            if rank >= self.columns.pay_narrow.len() {
                return None;
            }
            self.columns.pay_narrow[rank].to_le_bytes()
        };
        let mut v: std::mem::MaybeUninit<T> = std::mem::MaybeUninit::uninit();
        // SAFETY: `T` is `Copy` and size_of::<T>() <= 4, matching the
        // width of `bytes`. The copy writes size_of::<T>() bytes from
        // a fully-initialised 4-byte buffer.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                v.as_mut_ptr() as *mut u8,
                n,
            );
            Some(v.assume_init())
        }
    }

    /// Read a wide (8-byte) scalar payload from the `pay_wide` column
    /// or — when [`TapeRec::payload_in_arena`] is set — from the
    /// unified arena, or — when [`TapeRec::payload_f64_direct`] is
    /// set (AY.W4.2) — from the dedicated `pay_f64` direct-column.
    #[inline]
    fn payload_wide<T: Copy>(&self, rec: TapeRec) -> Option<T> {
        debug_assert!(std::mem::size_of::<T>() == 8);
        if rec.child_off.is_none() {
            return None;
        }
        let raw = if rec.payload_f64_direct() {
            let rank = rec.child_off.0 as usize;
            if rank >= self.columns.pay_f64.len() {
                return None;
            }
            self.columns.pay_f64[rank].to_le_bytes()
        } else if rec.payload_in_arena() {
            let off = rec.child_off.0 as usize;
            let arena = &self.columns.pay_agg;
            if off + 8 > arena.len() {
                return None;
            }
            let arr: [u8; 8] = arena[off..off + 8].try_into().ok()?;
            arr
        } else {
            let rank = rec.child_off.0 as usize;
            if rank >= self.columns.pay_wide.len() {
                return None;
            }
            self.columns.pay_wide[rank].to_le_bytes()
        };
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
