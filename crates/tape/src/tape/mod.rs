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

mod construct;
mod push;
mod scalar_accessors;
mod value_substrate;

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

    /// AY.W4.2 + B5.W2.4 — bit in [`TapeRec::extra`] marking a
    /// numeric `f64` leaf whose payload bits live in
    /// [`crate::Columns::pay_wide`] under f64 interpretation. Set by
    /// the Eisel-Lemire fast path; the reader projects via
    /// `f64::from_bits(pay_wide[rank])` rather than re-reading bytes
    /// through the arena.
    ///
    /// Mutually exclusive with [`Self::PAYLOAD_IN_ARENA_BIT`] — the
    /// arena-bit path slices `pay_agg` for an 8-byte LE encoding;
    /// this bit selects the f64 reinterpretation of the unified
    /// `pay_wide` column entry.
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

    /// AY.W4.2 + B5.W2.4 — true iff this leaf's `child_off` is a
    /// column rank into [`crate::Columns::pay_wide`] under f64
    /// interpretation (the Eisel-Lemire direct-write path).
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
    pub(super) static NEW_CALL_COUNT: Cell<u64> = const { Cell::new(0) };
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
/// (`pay_narrow`, `pay_wide`, `pay_agg`), value-side state
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

/// Iterator over every record in a tape, materialising owned
/// [`TapeRec`]s in column order.
#[derive(Debug)]
pub struct TapeIter<'t, R = ()> {
    columns: &'t Columns,
    idx: u32,
    _marker: PhantomData<fn() -> R>,
}

impl<'t, R> TapeIter<'t, R> {
    pub(super) fn new(columns: &'t Columns) -> Self {
        Self {
            columns,
            idx: 0,
            _marker: PhantomData,
        }
    }
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
