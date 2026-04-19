//! `PackedRecord` — the AoS sidecar record format for the hybrid tape
//! layout landing in AX.W1.D.
//!
//! # Role
//!
//! The [`Columns`](crate::columns::Columns) SoA primary is the write-
//! side substrate — every parser push lands in the six structural
//! columns plus `child_off`, keeping the dense payload columns
//! cache-friendly for bulk typed visitors (the `reduce_column<C, R>`
//! kernels the AW-IV.W5.1 SIMD reducer walks). That layout is
//! pathological for single-record random-access reads: a consumer
//! materialising one record via
//! [`Columns::materialize`](crate::columns::Columns::materialize) pays
//! seven distinct column loads plus the field-packing, each a separate
//! cache line in the SoA substrate. On a hot lazy-field extractor —
//! the Twitter `statuses[].text` shape — the seven cache misses per
//! record dominate the access cost.
//!
//! The AoS sidecar makes those reads single 32-byte cache-line loads.
//! Every `PackedRecord` is 32 bytes — tighter than the six-column SoA
//! scatter yet wider than the 16-byte [`TapeRec`](crate::TapeRec) view
//! so both span endpoints plus the `sib_skip` + `child_off` fit without
//! unpacking bit-layouts. At `align(32)` the records sit on cache-line
//! boundaries, so a random-access read of a single record is ONE
//! 32-byte aligned load + a pointer fixup — no column hops, no 16-byte
//! reconstruction.
//!
//! # Hybrid contract
//!
//! The SoA primary is ALWAYS the source of truth for writes. The AoS
//! sidecar is populated lazily by
//! [`Columns::packed_cache`](crate::columns::Columns::packed_cache) on
//! first random-access read; every push-site through
//! [`Columns::push_structural`](crate::columns::Columns::push_structural)
//! / [`Columns::push_compound_fused`](crate::columns::Columns::push_compound_fused)
//! / [`Columns::push_leaf_fused`](crate::columns::Columns::push_leaf_fused)
//! invalidates the cache so subsequent readers re-transpose from the
//! updated SoA. The [`std::sync::OnceLock`] guard is the
//! thread-safety primitive: readers on separate threads observe the
//! same immutable transpose without racing; mutators take `&mut self`
//! and therefore cannot race with any reader.
//!
//! # Layout
//!
//! ```text
//! offset | size | field       | notes
//! -------|------|-------------|----------------------------------
//!   0    |   1  | kind_meta   | low 4 bits = TapeKind; high 4 bits = meta_idx[0:3]
//!   1    |   1  | flags       | 8-bit variant_idx (AW-III.W1.A)
//!   2    |   2  | extra       | packed flag bits
//!   4    |   4  | span_lo     | source-byte start offset
//!   8    |   4  | span_hi     | source-byte end offset
//!  12    |   4  | child_off   | TapeOffset (u32 payload / child pointer)
//!  16    |   4  | sib_skip    | distance to next-emission sibling
//!  20    |  12  | _pad        | pad to 32 B cache-line alignment
//! ```
//!
//! The layout mirrors
//! [`TapeRec`](crate::TapeRec)'s field set verbatim — the record
//! materialiser on the SoA path ([`Columns::materialize`]) reconstructs
//! the same shape. The AoS sidecar simply stores that shape dense,
//! aligned, and cache-line sized so random reads are one load.
//!
//! [`Columns::packed_cache`]: crate::columns::Columns::packed_cache
//! [`Columns::materialize`]: crate::columns::Columns::materialize

use crate::kind::TapeKind;
use crate::tape::{TapeOffset, TapeRec};

/// 32-byte aligned AoS record — the random-access sidecar for the
/// hybrid tape layout.
///
/// # Layout invariants
///
/// - `#[repr(C, align(32))]` — field order matches the declaration,
///   alignment matches the cache-line granularity on every modern
///   arch (Intel 64 B, Apple M1/M2 128 B, AMD 64 B; 32 B is the
///   tightest bound every host satisfies natively).
/// - Total size is exactly 32 bytes; the trailing `_pad` bytes bring
///   the layout to the declared alignment. A compile-time
///   `size_of::<PackedRecord>() == 32` assertion in
///   [`crate::columns`] blocks any layout drift at build time.
///
/// # Field semantics
///
/// Field-for-field isomorphism with [`TapeRec`]; readers can construct
/// either representation from either substrate without loss. The
/// primary difference is wire layout — `TapeRec` is 16 bytes with
/// tightly-packed field widths, `PackedRecord` is 32 bytes with
/// cache-line alignment and the full `sib_skip` column inlined
/// (`TapeRec` materialises `sib_skip` separately via
/// [`Columns::sib_skip_at`](crate::columns::Columns::sib_skip_at)).
#[repr(C, align(32))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackedRecord {
    /// Packed byte: low 4 bits = [`TapeKind`] discriminant, high 4
    /// bits = `meta_idx` bits \[0:3\]. Same semantics as
    /// [`TapeRec::kind_meta`].
    pub kind_meta: u8,
    /// Full 8-bit `variant_idx` (rule discriminant). Same semantics
    /// as [`TapeRec::flags`].
    pub flags: u8,
    /// Packed per-record flags (`STRING_BORROW_BIT`,
    /// `PAYLOAD_IN_ARENA_BIT`, `HAS_CHILDREN_BIT`, `META_IDX_HI_BIT`).
    /// Same semantics as [`TapeRec::extra`].
    pub extra: u16,
    /// Source-byte start offset.
    pub span_lo: u32,
    /// Source-byte end offset (`span_hi == span_lo` for epsilon).
    pub span_hi: u32,
    /// Polymorphic child / payload pointer — see
    /// [`TapeRec::child_off`] for the routing rules.
    pub child_off: TapeOffset,
    /// Distance to the next-emission sibling within the parent's
    /// children run, or `0` for the last sibling and the root.
    pub sib_skip: u32,
    /// Tail padding to bring the record to 32-byte alignment. Never
    /// read; preserved for layout stability.
    pub _pad: [u8; 12],
}

impl PackedRecord {
    /// Construct a `PackedRecord` from a materialised [`TapeRec`] plus
    /// the explicit `sib_skip` column entry.
    ///
    /// The SoA primary stores `sib_skip` separately from the `TapeRec`
    /// view; the AoS sidecar inlines it. Transpose code reads the
    /// column and pairs it with the materialised record here.
    #[inline]
    pub fn from_rec_and_skip(rec: TapeRec, sib_skip: u32) -> Self {
        Self {
            kind_meta: rec.kind_meta,
            flags: rec.flags,
            extra: rec.extra,
            span_lo: rec.span_lo,
            span_hi: rec.span_hi,
            child_off: rec.child_off,
            sib_skip,
            _pad: [0u8; 12],
        }
    }

    /// Extract the [`TapeKind`] from the packed `kind_meta` byte.
    #[inline]
    pub fn kind(&self) -> TapeKind {
        TapeKind::from_u8(self.kind_meta & 0x0F)
    }

    /// Byte length of the record's source span.
    #[inline]
    pub fn span_len(&self) -> u32 {
        self.span_hi.saturating_sub(self.span_lo)
    }

    /// Project back into the 16-byte [`TapeRec`] view — the shape
    /// every pre-hybrid consumer reads.
    ///
    /// The AoS sidecar and the SoA primary are both isomorphic to
    /// `TapeRec`; this is the round-trip converter for consumers that
    /// want the legacy view shape from the sidecar.
    #[inline]
    pub fn as_tape_rec(&self) -> TapeRec {
        TapeRec {
            kind_meta: self.kind_meta,
            flags: self.flags,
            extra: self.extra,
            span_lo: self.span_lo,
            span_hi: self.span_hi,
            child_off: self.child_off,
        }
    }
}

/// Compile-time assertion that the layout is exactly 32 bytes and
/// aligned to 32-byte boundaries. Any drift breaks the AoS sidecar's
/// one-load random-read contract.
const _: () = {
    assert!(core::mem::size_of::<PackedRecord>() == 32);
    assert!(core::mem::align_of::<PackedRecord>() == 32);
};
