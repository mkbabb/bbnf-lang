//! [`ColumnTag`] trait + 4 zero-sized marker types.
//!
//! Compile-time selector for the typed payload columns on [`Columns`].
//! [`Columns::reduce_column`] takes a `ColumnTag` as its `C` type
//! parameter; the emitter passes the tag matching each active payload
//! column per grammar.

use super::Columns;


/// Compile-time selector for a typed payload column on [`Columns`].
///
/// Implementors are zero-sized marker types — [`PayWideF64`],
/// [`PayWideU64`], [`PayNarrowU32`], [`PayAggU8`] — that bind a column
/// identity to an element type. [`Columns::reduce_column`] takes a
/// `ColumnTag` as its `C` type parameter; the emitter passes the tag
/// matching each active payload column per grammar.
pub trait ColumnTag {
    /// Element type of this column — the scalar the reducer folds over.
    type Value: Copy;
    /// Project the column out of `Columns` as a `&[Self::Value]`.
    fn column(cols: &Columns) -> &[Self::Value];
}

/// Column tag for `pay_wide` interpreted as `f64` (canonical
/// numeric-leaf payload column — JSON numbers, CSS dimensions, Sheets
/// numbers). 8-byte column entries reinterpret-cast via
/// [`f64::from_bits`] against the stored `u64` bits.
pub struct PayWideF64;

impl ColumnTag for PayWideF64 {
    type Value = f64;

    #[inline]
    fn column(cols: &Columns) -> &[f64] {
        // SAFETY: `u64` and `f64` have the same size + alignment;
        // reinterpretation is defined behaviour. `self.pay_wide` is
        // never uninitialised — every entry was populated by the
        // payload-writer before any reader could observe the slice.
        unsafe {
            core::slice::from_raw_parts(
                cols.pay_wide.as_ptr() as *const f64,
                cols.pay_wide.len(),
            )
        }
    }
}

/// Column tag for `pay_wide` interpreted as `u64` (packed integer
/// leaves, timestamps, raw 8-byte scalars).
pub struct PayWideU64;

impl ColumnTag for PayWideU64 {
    type Value = u64;

    #[inline]
    fn column(cols: &Columns) -> &[u64] {
        &cols.pay_wide
    }
}

/// Column tag for `pay_narrow` — 4-byte inline scalars (`u32`, `u16`,
/// `u8`, `bool`, widened unit enums). The column is stored as
/// `Vec<u32>` and the reducer reads it as `&[u32]`.
pub struct PayNarrowU32;

impl ColumnTag for PayNarrowU32 {
    type Value = u32;

    #[inline]
    fn column(cols: &Columns) -> &[u32] {
        &cols.pay_narrow
    }
}

/// Column tag for `pay_agg` — the arena byte column. Useful for
/// checksum-style reductions (`count_bytes`, `sum_bytes`, `xor_bytes`)
/// over the byte-addressable arena.
pub struct PayAggU8;

impl ColumnTag for PayAggU8 {
    type Value = u8;

    #[inline]
    fn column(cols: &Columns) -> &[u8] {
        &cols.pay_agg
    }
}
