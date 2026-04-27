//! `Tape<R>`, `TapeRec`, `TapeOffset` — the unified parser substrate.
//!
//! # Architectural role (B5.W1 substrate boundary restoration)
//!
//! Pre-B5.W1 the substrate was two welded halves: a read-side `Tape`
//! type and a separately-owned `FusedBuilder` (write-side) +
//! `FusedOutput<R>` / `ValueFramesOutput<R>` (read-side wrappers).
//! Every push paired a tape-side stamp with a value-side stamp
//! mediated through the builder; rollback split into three primitives
//! across the two halves. B5.W1 restores the gestalt: `Tape<R>` is the
//! single substrate carrying both the structural tape columns and the
//! value-side state ([`Columns`] absorbs the value frames + payload
//! columns + open-stack), exposing the unified write API
//! (`begin_compound`, `end_compound`, `push_leaf*`) AND the unified
//! read API (`frame`, `payload_for`, `children`, etc.) on one type.
//!
//! `R` is the grammar-root marker the typed value projection binds
//! against. The substrate is grammar-agnostic in storage; the phantom
//! ties parsing and projection per grammar so multiple grammars can
//! coexist without runtime confusion.
//!
//! # Storage shape (Tranche AV Phase 2 contract preserved)
//!
//! Tranche AV Phase 2 (AV.2.1 – AV.2.3) flipped the storage shape
//! from row-major `Vec<TapeRec>` to column-major [`Columns`]. The
//! 16-byte `TapeRec` survives as the materialised-view shape every
//! reader consumes; it is reconstructed on demand from the
//! structural columns. B5.W1 augments [`Columns`] with the value-side
//! state without disturbing this storage discipline.
//!
//! `TapeOffset` is unchanged on the wire: a `u32` index into the
//! column set, with `u32::MAX` reserved as the `NONE` sentinel.

use core::cell::Cell;
use core::marker::PhantomData;

use crate::columns::Columns;
use crate::kind::TapeKind;
use crate::value::{
    PayloadTag, PayloadValue, ValueChildren, ValueCheckpoint, ValueFrame,
};

/// Stable index into a [`Tape`]'s record stream.
///
/// Constructed by [`Tape::push_leaf`] / [`Tape::begin_compound`] and
/// consumed by view-layer accessors via [`Tape::get`]. Two offsets
/// compare equal iff they point to the same record in the same tape;
/// cross-tape comparison is a logic bug the view codegen prevents by
/// tying every view type to a `'tape` lifetime parameter.
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
    /// [`crate::Columns::pay_f64`] by the Eisel-Lemire fast path.
    /// When set, the record's `child_off` carries the column rank
    /// into `pay_f64`; the reader projects `f64::from_bits(pay_f64[rank])`
    /// directly without the `pay_wide` / arena round-trip.
    ///
    /// Mutually exclusive with [`Self::PAYLOAD_IN_ARENA_BIT`] — the
    /// arena-bit path slices `pay_agg` for an 8-byte LE encoding;
    /// this bit selects the dense `pay_f64` column.
    pub const PAYLOAD_F64_DIRECT_BIT: u16 = 0x0010;

    /// Pack a [`TapeKind`] and `meta_idx` into the `kind_meta` byte
    /// and an `extra` companion bit. Returns
    /// `(kind_meta, extra_meta_bit)` where `extra_meta_bit` is
    /// either `0` or [`Self::META_IDX_HI_BIT`] — the caller ORs it
    /// into the `extra` u16.
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
    #[inline]
    pub fn variant_idx(&self) -> u8 {
        self.flags
    }

    /// Does this record have children? Bit [`Self::HAS_CHILDREN_BIT`]
    /// of `extra`.
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
    /// column).
    #[inline]
    pub fn payload_f64_direct(&self) -> bool {
        (self.extra & Self::PAYLOAD_F64_DIRECT_BIT) != 0
    }

    // ── ShapeRef accessors (AV.5.1) ──────────────────────────────

    /// Extract the shape-dictionary index from a `ShapeRef` record's
    /// `flags` (low 5 bits).
    #[inline]
    pub fn shape_dict_idx(&self) -> u8 {
        self.flags & 0x1F
    }

    /// True iff this `ShapeRef` record carries a packed payload blob.
    /// Bit 5 of `flags`.
    #[inline]
    pub fn shape_ref_has_payload(&self) -> bool {
        (self.flags & 0x20) != 0
    }
}

// ─── B5.W1 instrumentation: per-thread Tape::new call counter ──────
//
// Pre-B5.W1 this counter lived on `FusedBuilder::new` /
// `FusedBuilder::with_capacity`; B5.W1 absorbs the same shape onto
// `Tape::new` / `Tape::with_capacity` so the parse-count invariant
// (`Parsed::to_value()` must not increment the counter) survives the
// substrate transposition.
thread_local! {
    static NEW_CALL_COUNT: Cell<u64> = const { Cell::new(0) };
}

/// Return the count of [`Tape::new`] / [`Tape::with_capacity`]
/// invocations on the current thread.
///
/// `Parsed::to_value()` must not increment this counter — that is
/// the invariant the `value_api_apples_to_apples` parse-count test
/// asserts.
pub fn tape_new_call_count() -> u64 {
    NEW_CALL_COUNT.with(|c| c.get())
}

/// Reset the [`Tape::new`] counter to `0`.
pub fn reset_tape_new_call_count() {
    NEW_CALL_COUNT.with(|c| c.set(0));
}

// ─── TapeBuildError ───────────────────────────────────────────────

/// Error state surfaced through [`Tape::finish`].
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

// ─── PayloadData — payload taxonomy for push_leaf_with ─────────────

/// Payload data handed to [`Tape::push_leaf_with`] — the single entry
/// point for payload-bearing leaves.
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
    /// backed, unframed.
    LargeAggregate(&'a [u8]),
    /// Byte string framed as `(len: u32 LE, bytes)` into
    /// `Columns::pay_agg`.
    Bytes(&'a [u8]),
}

// ─── B5.W1 transitional compat aliases (REMOVED post-regen) ────────
//
// The pre-B5.W1 `FusedBuilder` / `FusedOutput<R>` / `ValueFramesOutput<R>`
// surface is collapsed into [`Tape<R>`]. These aliases let the
// pre-regen generated grammars compile during the regen sweep; they
// retire as part of the wave-close commit.
/// Transitional alias for [`Tape<()>`] — pre-B5.W1 builder type.
#[deprecated(note = "B5.W1 transitional alias; use Tape<R> directly")]
pub type FusedBuilder = Tape<()>;
/// Transitional alias for [`Tape<R>`] — pre-B5.W1 output type.
#[deprecated(note = "B5.W1 transitional alias; use Tape<R> directly")]
pub type FusedOutput<R> = Tape<R>;
/// Transitional alias for [`Tape<R>`] — pre-B5.W1 value output type.
#[deprecated(note = "B5.W1 transitional alias; use Tape<R> directly")]
pub type ValueFramesOutput<R> = Tape<R>;

// ─── Tape<R> ───────────────────────────────────────────────────────

/// The unified parser substrate — write surface + read surface +
/// value-side projection in one type.
///
/// # Roles
///
/// 1. **Write surface** during parse — generated parser code threads a
///    `&mut Tape<Self>` through every rule call, invoking
///    [`Self::begin_compound`] / [`Self::end_compound`] /
///    [`Self::push_leaf*`] / [`Self::rollback_to`] to stamp records.
/// 2. **Read surface** post-finalise — view-layer accessors call
///    [`Self::get`] / [`Self::iter`] / payload accessors to materialise
///    individual records.
/// 3. **Value-side projection** — [`Self::frame`] / [`Self::children`]
///    / [`Self::payload_for`] expose the value substrate the
///    grammar-emitted projection logic consumes at `to_value()` time.
///
/// `R` is the grammar-root marker. The substrate is grammar-agnostic
/// in storage; the phantom binds parse-time and projection-time so
/// multiple grammars can coexist in one binary without runtime
/// confusion.
///
/// # Substrate
///
/// All state lives in [`Columns`]: structural columns
/// (`records`, `sib_skip`, `frame_depth`), typed payload columns
/// (`pay_narrow`, `pay_wide`, `pay_f64`, `pay_agg`), value-side state
/// (`value_frames`, `value_payloads_narrow`, `value_payloads_wide`,
/// `value_open_stack`).
#[derive(Debug)]
pub struct Tape<R = ()> {
    pub(crate) columns: Columns,
    pub(crate) error: Option<TapeBuildError>,
    pub(crate) root_offset: u32,
    pub(crate) _root_marker: PhantomData<fn() -> R>,
}

impl<R> Default for Tape<R> {
    #[inline]
    fn default() -> Self {
        Self {
            columns: Columns::default(),
            error: None,
            root_offset: 0,
            _root_marker: PhantomData,
        }
    }
}

impl<R> Tape<R> {
    /// Construct an empty tape.
    #[inline]
    pub fn new() -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self::default()
    }

    /// Construct an empty tape sized for `expected` records.
    ///
    /// Callers presize via the per-grammar push fingerprint:
    /// `GRAMMAR_PROFILE.capacity_for(input.len())`. The reservation
    /// covers `records` (16 B AoS rows) + `sib_skip` (4 B parallel
    /// column) + value-side substrate columns in lockstep so the hot
    /// push path never trips a `Vec::push` realloc on corpus input.
    #[inline]
    pub fn with_capacity(expected: usize) -> Self {
        NEW_CALL_COUNT.with(|c| c.set(c.get() + 1));
        Self {
            columns: Columns::with_capacity(expected),
            error: None,
            root_offset: 0,
            _root_marker: PhantomData,
        }
    }

    /// Construct an empty tape sized from a [`crate::GrammarProfile`]
    /// + `input_len`.
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

    /// The current write position — the offset where the NEXT
    /// `push_*` will land.
    ///
    /// B5.W1: replaces the pre-W1 `builder.columns_mut().len() as u32`
    /// idiom every emitter retry-IIFE used to capture an open offset
    /// before probing a branch. Generated parsers call
    /// `tape.position()` before each rollback-eligible branch and pass
    /// the returned `u32` to [`Self::rollback_to`] on failure.
    #[inline(always)]
    pub fn position(&self) -> u32 {
        self.columns.records.len() as u32
    }

    /// Borrow the underlying [`Columns`] substrate.
    #[inline]
    pub fn columns(&self) -> &Columns {
        &self.columns
    }

    /// Set `child_off` on row `i` directly. Used by the Pratt
    /// reducer's walker-parity override after `end_compound` to
    /// re-stamp the outer row's `child_off` to the final reducer's
    /// root (B4.W2 substrate parity).
    #[inline(always)]
    pub fn set_child_off_at(&mut self, i: u32, value: TapeOffset) {
        self.columns.set_child_off_at(i, value);
    }

    // ── Read accessors — column-indexed materialisation ──────────────

    /// Look up a record by offset. Panics on out-of-range offsets.
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
    /// than `self.len()`.
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
    pub fn iter(&self) -> TapeIter<'_, R> {
        TapeIter {
            columns: &self.columns,
            idx: 0,
            _marker: PhantomData,
        }
    }

    // ── Payload accessors ─────────────────────────────────────────

    /// Read an inline-packed scalar payload (≤ 4 bytes) from
    /// `pay_narrow` / `pay_agg`.
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

    /// Read a wide (8-byte) scalar payload from `pay_wide` / `pay_agg`
    /// / `pay_f64`.
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
    #[inline]
    pub fn arena(&self) -> &[u8] {
        &self.columns.pay_agg
    }

    /// Read a slice of raw aggregate payload bytes for a record
    /// whose payload was written via [`PayloadData::Aggregate`].
    ///
    /// # B5.W0 bonus — `PAYLOAD_IN_ARENA_BIT` precondition assert
    ///
    /// The `debug_assert!` below validates that the leaf's record
    /// kind belongs to the set the arena conventions admit — a record
    /// that reaches this reader without falling into either
    /// convention trips the assert in debug runs (release elides),
    /// enforcing the audit-flagged invariant at zero release cost.
    #[inline]
    pub fn payload_bytes(&self, rec: TapeRec, byte_count: usize) -> Option<&[u8]> {
        if rec.child_off.is_none() {
            return None;
        }
        debug_assert!(
            rec.payload_in_arena()
                || matches!(
                    rec.kind(),
                    crate::TapeKind::Span
                        | crate::TapeKind::KvPair
                        | crate::TapeKind::ShapeRef
                ),
            "payload_bytes precondition: record kind {:?} did not fall into \
             either arena convention (PAYLOAD_IN_ARENA_BIT clear AND kind not \
             in {{Span, KvPair, ShapeRef}}); `child_off` likely names a column \
             rank, not an arena byte offset",
            rec.kind(),
        );
        let start = rec.child_off.0 as usize;
        let arena = self.arena();
        if start + byte_count > arena.len() {
            return None;
        }
        Some(&arena[start..start + byte_count])
    }

    // ── Write surface — pre-order compound emission API ──────────────

    /// Begin a compound in pre-order.
    ///
    /// Emits a compound row with provisional `span_hi == span_lo`,
    /// `child_off = TapeOffset::NONE`, and `HAS_CHILDREN_BIT` cleared
    /// on the tape AND opens a matching value-arena frame + pushes
    /// the value checkpoint onto the open-stack.
    ///
    /// Returns the tape row offset the caller passes back to
    /// [`Self::end_compound`] (pre-order) or
    /// [`Self::end_compound_post_order`] (post-order). Emitter retry
    /// paths rewind via [`Self::rollback_to`] with the returned
    /// offset; the next `begin_compound` reuses the same row.
    #[inline(always)]
    pub fn begin_compound(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        variant_idx: u8,
        meta_idx: u8,
        _frame_depth: u8,
        extra_flags: u16,
    ) -> u32 {
        debug_assert!(
            kind.is_compound(),
            "begin_compound on leaf/annotation kind {:?}",
            kind
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_flags | extra_meta_bit,
            span_lo,
            span_lo,
            TapeOffset::NONE,
        );
        // Bump after stamping so the children of this compound stamp
        // at `current_depth + 1`. Saturate to `u8::MAX` — grammars
        // that nest deeper than 255 compounds are diagnosed by the
        // finaliser's depth-overflow path; saturation keeps the push
        // path branchless.
        self.columns.current_depth = self.columns.current_depth.saturating_add(1);
        self.value_begin_compound(kind, span_lo, variant_idx, idx);
        idx
    }

    /// Finalise a compound opened via [`Self::begin_compound`] in
    /// pre-order — the caller emitted the compound row BEFORE its
    /// children, so the first child's root sits at `open_offset + 1`.
    #[inline(always)]
    pub fn end_compound(&mut self, open_offset: u32, span_hi: u32) {
        self.columns.set_span_hi_at(open_offset, span_hi);
        // The compound was emitted at its outer frame's depth; its
        // direct children are stamped at `open_depth + 1`. Pre-order
        // layout normally puts the first child at `open_offset + 1`,
        // BUT when an inner [`Self::end_compound_post_order`] retro-
        // actively bumps `frame_depth` over its child range, records
        // that landed between this compound's open and the nested
        // post-order close move one level deeper. The true first
        // child is then the first row at exactly `open_depth + 1`,
        // bounded by the compound's structural scope.
        let open_depth = self.columns.frame_depth[open_offset as usize];
        let target_depth = open_depth.saturating_add(1);
        let n = self.columns.len() as u32;
        let mut first_child = open_offset + 1;
        let mut found = false;
        while first_child < n {
            let d = self.columns.frame_depth[first_child as usize];
            if d == target_depth {
                found = true;
                break;
            }
            if d <= open_depth {
                break;
            }
            first_child += 1;
        }
        if found {
            self.columns
                .set_child_off_at(open_offset, TapeOffset(first_child));
            self.columns
                .or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
        }
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
        self.value_end_compound(span_hi);
    }

    /// Finalise a compound opened via [`Self::begin_compound`] in
    /// post-order — the compound row was allocated AFTER its
    /// children, so `open_offset` is the LAST record and the first
    /// child's root is `first_child` (captured at children-enter via
    /// `tape.position()`).
    #[inline(always)]
    pub fn end_compound_post_order(
        &mut self,
        open_offset: u32,
        span_hi: u32,
        first_child: TapeOffset,
    ) {
        self.columns.set_span_hi_at(open_offset, span_hi);
        if !first_child.is_none() && first_child.0 < open_offset {
            self.columns.set_child_off_at(open_offset, first_child);
            self.columns
                .or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
            // B3.W0.ζ — bump the entire subtree's `frame_depth`, not
            // just the offset range `[first_child, open_offset)`.
            // Walking the leftmost-descendant chain finds the lowest
            // offset of any record in our subtree; bumping
            // `[leftmost, open_offset)` covers every descendant.
            let lo = leftmost_descendant_offset(&self.columns, first_child.0) as usize;
            let hi = open_offset as usize;
            for slot in &mut self.columns.frame_depth[lo..hi] {
                *slot = slot.saturating_add(1);
            }
        }
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
        self.value_end_compound(span_hi);
    }

    // ── Write surface — leaf push API ────────────────────────────────

    /// Append a leaf record with a concrete kind + span.
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
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// Append a leaf record carrying the supplied [`PayloadData`].
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
        let (child_off, value_tag) = match payload {
            PayloadData::None => (TapeOffset::NONE, PayloadTag::NONE),
            PayloadData::InlineScalar(v) => {
                // AV.2.3: inline scalars land in `pay_narrow`; the
                // record's `child_off` carries the column rank.
                let rank = self.columns.pay_narrow.len() as u32;
                self.columns.pay_narrow.push(v);
                let v_rank = self.columns.value_payloads_narrow.len() as u32;
                self.columns.value_payloads_narrow.push(v);
                (TapeOffset(rank), PayloadTag::narrow(v_rank))
            }
            PayloadData::WideScalar(v) => {
                let rank = self.columns.pay_wide.len() as u32;
                self.columns.pay_wide.push(v);
                let v_rank = self.columns.value_payloads_wide.len() as u32;
                self.columns.value_payloads_wide.push(v);
                (TapeOffset(rank), PayloadTag::wide(v_rank))
            }
            PayloadData::Aggregate(bytes) => {
                if bytes.is_empty() {
                    (TapeOffset::NONE, PayloadTag::NONE)
                } else {
                    let offset = self.alloc_aggregate_slot(bytes);
                    (TapeOffset(offset), PayloadTag::NONE)
                }
            }
            PayloadData::LargeAggregate(bytes) => {
                if bytes.is_empty() {
                    (TapeOffset::NONE, PayloadTag::NONE)
                } else {
                    let offset = self.alloc_large_aggregate_slot(bytes);
                    (TapeOffset(offset), PayloadTag::NONE)
                }
            }
            PayloadData::Bytes(bytes) => {
                let offset = self.alloc_bytes_frame(bytes);
                (TapeOffset(offset), PayloadTag::NONE)
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
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, value_tag);
        TapeOffset(idx)
    }

    /// Append aggregate bytes into a `pay_agg` slot rounded up to the
    /// next 8-byte boundary and return the byte offset.
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
    /// slot.
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
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// Append a leaf record whose payload is an in-arena scalar of
    /// `payload_width` bytes already written at `arena_offset`.
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
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// Append a borrow-safe string leaf — zero arena writes.
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
        self.push_value_leaf(kind, span_lo, span_hi, variant_idx, PayloadTag::NONE);
        TapeOffset(idx)
    }

    /// AY.W4.2 — Eisel-Lemire direct-column f64 leaf push.
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
        let v_rank = self.columns.value_payloads_wide.len() as u32;
        self.columns.value_payloads_wide.push(f64_bits);
        self.push_value_leaf(
            kind,
            span_lo,
            span_hi,
            variant_idx,
            PayloadTag::wide(v_rank),
        );
        TapeOffset(idx)
    }

    /// Write the 4-byte length prefix at the `pay_agg` slot reserved
    /// by the decode kernel.
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
    /// label.
    pub fn set_error(&mut self, offset: u32, rule_label: u32) {
        if self.error.is_none() {
            self.error = Some(TapeBuildError::ParseFailed {
                offset,
                rule_label,
            });
        }
    }

    // ── Rollback ──────────────────────────────────────────────────

    /// Rewind every column family — structural tape, inline
    /// `frame_depth`, and the value substrate — back to the state at
    /// the matching `begin_compound` whose `open_offset` the caller
    /// passes in.
    ///
    /// B5.W1: the sole rollback primitive across both column families.
    /// Delegates to [`Columns::rollback_to`] which handles tape-side +
    /// value-side substrates atomically in one call.
    #[inline(always)]
    pub fn rollback_to(&mut self, open_offset: u32) {
        self.columns.rollback_to(open_offset);
    }

    // ── Finalisation — Stage-C sib_skip + close-compound back-patch ──

    /// B5.W1 transitional: pre-W1 generated grammars call
    /// `builder.finish_fused::<Self>(root_off.0)`. The substrate
    /// transposition collapses this to `tape.finish(root_off)`; this
    /// alias preserves source compatibility for the regen sweep
    /// only, and retires alongside the other transitional aliases at
    /// wave-close.
    #[doc(hidden)]
    #[inline(always)]
    pub fn finish_fused<R2>(self, root_off: u32) -> Result<Tape<R2>, TapeBuildError>
    where
        R: 'static,
        R2: 'static,
    {
        let finished = self.finish(root_off)?;
        // R is a phantom — Tape<R> and Tape<R2> have identical
        // memory layout (the marker is `PhantomData<fn() -> R>`).
        // SAFETY: transmute is sound because R is `PhantomData`-only.
        Ok(unsafe { core::mem::transmute::<Tape<R>, Tape<R2>>(finished) })
    }

    /// B5.W1 transitional: pre-W1 generated grammars call
    /// `builder.columns_mut()` to get a `&mut Columns` for direct
    /// rollback / position queries. Post-W1 the canonical accessors
    /// are [`Self::position`] (read-only) and [`Self::rollback_to`].
    #[doc(hidden)]
    #[inline(always)]
    pub fn columns_mut(&mut self) -> &mut Columns {
        &mut self.columns
    }

    /// B5.W1 transitional: pre-W1 generated grammars call
    /// `output.tape()` to access the tape from a `FusedOutput<R>`.
    /// Post-W1 the substrate IS the tape.
    #[doc(hidden)]
    #[inline(always)]
    pub fn tape(&self) -> &Self {
        self
    }

    /// B5.W1 transitional alias for [`Self::frame`].
    #[doc(hidden)]
    #[inline]
    pub fn value_frame_at(&self, offset: u32) -> Option<&ValueFrame> {
        self.frame(offset)
    }

    /// B5.W1 transitional alias for [`Self::payload_for`].
    #[doc(hidden)]
    #[inline]
    pub fn value_payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        self.payload_for(frame)
    }

    /// B5.W1 transitional alias for [`Self::children`].
    #[doc(hidden)]
    #[inline]
    pub fn value_children(&self, offset: u32) -> ValueChildren<'_, R> {
        self.children(offset)
    }

    /// B5.W1 transitional alias for [`Self::root_offset`].
    #[doc(hidden)]
    #[inline]
    pub fn value_root_offset(&self) -> u32 {
        self.root_offset
    }

    /// B5.W1 transitional alias for `Tape<R>` itself — the value
    /// substrate is now part of the tape directly.
    #[doc(hidden)]
    #[inline]
    pub fn as_value_output(&self) -> &Self {
        self
    }

    /// B5.W1 transitional alias for [`Self::frame_count`].
    #[doc(hidden)]
    #[inline]
    pub fn into_value(self) -> Self {
        self
    }

    /// B5.W1 transitional alias — into the unified tape.
    #[doc(hidden)]
    #[inline]
    pub fn into_tape(self) -> Self {
        self
    }

    /// B5.W1 transitional alias — into the unified tape.
    #[doc(hidden)]
    #[inline]
    pub fn into_parts(self) -> (Self, Self) where R: Default + Copy {
        // Pre-W1 `FusedOutput<R>::into_parts() -> (Tape, ValueFramesOutput<R>)`.
        // Post-W1 the substrate IS one tape; this alias is unused
        // post-regen.
        unreachable!("B5.W1: Tape::into_parts is a regen-time placeholder")
    }

    /// Consume the tape's write surface, run the Stage-C finaliser,
    /// stamp the root offset, and return `Self` ready for read access.
    ///
    /// B5.W1: replaces the pre-W1 `FusedBuilder::finish_fused` /
    /// `FusedBuilder::finish` pair with a single `finish` that
    /// preserves both the structural tape and the value substrate
    /// inside the same `Tape<R>`. The grammar-emitted parse entry
    /// calls `tape.finish(root_off)` and the returned tape feeds
    /// `Parsed::new`.
    #[inline(always)]
    pub fn finish(mut self, root_off: u32) -> Result<Self, TapeBuildError> {
        if let Some(err) = self.error.take() {
            return Err(err);
        }
        debug_assert!(
            self.columns.value_open_stack.is_empty(),
            "Tape::finish called with {} open value frames remaining",
            self.columns.value_open_stack.len(),
        );
        debug_assert_eq!(
            self.columns.frame_depth.len(),
            self.columns.len(),
            "frame_depth length {} != records length {} \
             (every structural push must stamp frame_depth in lockstep)",
            self.columns.frame_depth.len(),
            self.columns.len(),
        );
        self.columns.run_finalise();
        self.root_offset = root_off;
        Ok(self)
    }

    // ── Value substrate read accessors (was FusedOutput<R>) ──────────

    /// Total value-frame count.
    #[inline]
    pub fn frame_count(&self) -> usize {
        self.columns.value_frames.len()
    }

    /// `true` iff the value substrate carries no frames.
    #[inline]
    pub fn frames_is_empty(&self) -> bool {
        self.columns.value_frames.is_empty()
    }

    /// Borrow the value-frame arena directly.
    #[inline]
    pub fn frames(&self) -> &[ValueFrame] {
        &self.columns.value_frames
    }

    /// The root frame's offset within the value-frame arena.
    /// Projection consumers begin descent here.
    #[inline]
    pub fn root_offset(&self) -> u32 {
        self.root_offset
    }

    /// Borrow a value frame by offset.
    #[inline]
    pub fn frame(&self, offset: u32) -> Option<&ValueFrame> {
        self.columns.value_frames.get(offset as usize)
    }

    /// Borrow the root value frame directly. Returns `None` for
    /// substrate-empty tapes.
    #[inline]
    pub fn root_frame(&self) -> Option<&ValueFrame> {
        self.frame(self.root_offset)
    }

    /// Read a narrow-column value-substrate payload by rank.
    #[inline]
    pub fn value_payload_narrow(&self, rank: u32) -> Option<u32> {
        self.columns.value_payloads_narrow.get(rank as usize).copied()
    }

    /// Read a wide-column value-substrate payload by rank.
    #[inline]
    pub fn value_payload_wide(&self, rank: u32) -> Option<u64> {
        self.columns.value_payloads_wide.get(rank as usize).copied()
    }

    /// Look up the scalar payload for a leaf value-substrate frame.
    #[inline]
    pub fn payload_for(&self, frame: &ValueFrame) -> Option<PayloadValue> {
        let tag = frame.payload_tag;
        if tag.is_none() {
            None
        } else if tag.is_narrow() {
            self.value_payload_narrow(tag.rank())
                .map(PayloadValue::Narrow)
        } else {
            self.value_payload_wide(tag.rank()).map(PayloadValue::Wide)
        }
    }

    /// Iterator over the direct children of the value-substrate
    /// compound frame at `offset`. For leaf frames the iterator is
    /// empty.
    #[inline]
    pub fn children(&self, offset: u32) -> ValueChildren<'_, R> {
        let frame = match self.columns.value_frames.get(offset as usize) {
            Some(f) => f,
            None => {
                return ValueChildren {
                    tape: self,
                    next: u32::MAX,
                    remaining: 0,
                };
            }
        };
        ValueChildren {
            tape: self,
            next: frame.first_child,
            remaining: frame.child_count,
        }
    }

    // ── Value substrate write helpers (was FusedBuilder internals) ──

    /// Open a value-arena frame in lockstep with the tape's
    /// `begin_compound`. Pushes a compound frame + checkpoint onto
    /// the open-stack and bumps the parent checkpoint's
    /// `direct_child_count`.
    #[inline(always)]
    fn value_begin_compound(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        variant_idx: u8,
        tape_idx: u32,
    ) {
        if let Some(parent) = self.columns.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        let frame_offset = self.columns.value_frames.len() as u32;
        self.columns.value_frames.push(ValueFrame {
            span_lo,
            span_hi: span_lo,
            first_child: frame_offset + 1,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag: PayloadTag::NONE,
        });
        self.columns.value_open_stack.push(ValueCheckpoint {
            frame_offset,
            narrow_rank: self.columns.value_payloads_narrow.len() as u32,
            wide_rank: self.columns.value_payloads_wide.len() as u32,
            direct_child_count: 0,
            tape_idx,
        });
    }

    /// Close the most recently opened value frame.
    #[inline(always)]
    fn value_end_compound(&mut self, span_hi: u32) {
        let checkpoint = self
            .columns
            .value_open_stack
            .pop()
            .expect("Tape::value_end_compound called with empty open_stack");
        let frame =
            &mut self.columns.value_frames[checkpoint.frame_offset as usize];
        frame.span_hi = span_hi;
        frame.child_count = checkpoint.direct_child_count;
    }

    /// Append a leaf value frame.
    #[inline(always)]
    fn push_value_leaf(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        variant_idx: u8,
        payload_tag: PayloadTag,
    ) {
        if let Some(parent) = self.columns.value_open_stack.last_mut() {
            parent.direct_child_count += 1;
        }
        self.columns.value_frames.push(ValueFrame {
            span_lo,
            span_hi,
            first_child: 0,
            child_count: 0,
            variant_idx,
            kind,
            payload_tag,
        });
    }
}

/// Walk the leftmost-descendant chain from `start` to find the lowest
/// offset of any record in `start`'s subtree.
///
/// Used by `end_compound_post_order` to extend its `frame_depth` bump
/// range to cover descendants whose offsets sit strictly below
/// `first_child`. When `first_child` is itself a post-order compound,
/// its body lives at offsets below `first_child` (post-order layout);
/// those descendants belong to the closing compound's subtree and
/// need the same `+1` adjustment.
///
/// The walk follows `child_off` while it points strictly backward
/// (`co_child_off < co` — canonical post-order subtree root). For
/// pre-order children (`child_off > co`) and leaves the walk stops:
/// pre-order child ranges live at offsets ABOVE the parent, so the
/// parent's offset is already the leftmost in that subtree's prefix.
#[inline]
fn leftmost_descendant_offset(columns: &Columns, start: u32) -> u32 {
    let mut off = start;
    while columns.has_children_at(off) {
        let co = columns.child_off_at(off);
        if co.is_none() || co.0 >= off {
            break;
        }
        off = co.0;
    }
    off
}

/// Iterator over every record in a tape, materialising owned
/// [`TapeRec`]s in column order.
#[derive(Debug)]
pub struct TapeIter<'t, R = ()> {
    columns: &'t Columns,
    idx: u32,
    _marker: PhantomData<fn() -> R>,
}

impl<'t, R> Iterator for TapeIter<'t, R> {
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

impl<R> ExactSizeIterator for TapeIter<'_, R> {}
