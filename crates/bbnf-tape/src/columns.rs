//! `Columns` — the columnar (Struct-of-Arrays) substrate the tape
//! is built from after Tranche AV Phase 2.
//!
//! # Architectural role (Tranche AV.2.1 – AV.2.3)
//!
//! Pre-AV, the tape was a single `Vec<TapeRec>` — one 16-byte record
//! per parsed node, storage in row-major Array-of-Structs layout.
//! That shape is cache-friendly for the common "walk each record and
//! look at every field" case but is pathological for bulk typed
//! visitors (`sum_all_f64(canada)`), where a scalar left-fold over
//! the f64 payloads traverses every 16-byte record even though only
//! 8 of those bytes matter. The W4 prototype measured 1.94× on that
//! visitor; a 4-lane reordered-unrolling cleared 6.64× — but only
//! once the `f64`s lived in a dense `Vec<f64>`.
//!
//! `Columns` is that dense SoA layout. Six *structural* columns
//! hold per-record metadata (kind, spans, flags, sibling-skip). A
//! set of *typed payload* columns hold typed payloads dense-packed
//! in push order; every record that carries a payload of a given
//! type occupies exactly one slot in the matching column.
//!
//! | Column      | Type            | Role                                            |
//! |-------------|-----------------|-------------------------------------------------|
//! | `kinds`     | `Vec<u8>`       | `TapeKind` + meta_idx nibble (packed byte)      |
//! | `flags`     | `Vec<u8>`       | variant_idx (6b) + has_children + meta_idx\[4\] |
//! | `extra`     | `Vec<u16>`      | packed per-record flags (STRING_BORROW_BIT…)    |
//! | `span_lo`   | `Vec<u32>`      | source-byte start offset                        |
//! | `span_hi`   | `Vec<u32>`      | source-byte end offset                          |
//! | `sib_skip`  | `Vec<u32>`      | distance to next sibling (0 = last)             |
//! | `child_off` | `Vec<TapeOffset>` | compound first-child / leaf payload pointer   |
//! | `pay_narrow`| `Vec<u32>`      | inline ≤ 4 B scalars (u8/u16/u32/bool/i-tagged) |
//! | `pay_wide`  | `Vec<u64>`      | 8 B scalars (f64/u64/i64/packed Span)           |
//! | `pay_agg`   | `Vec<u8>`       | unified arena for aggregates & byte-string frames |
//!
//! # Payload routing (AV.2.3)
//!
//! The AU-era `PayloadData::InlineScalar(u32)` variant used to pack
//! its 4 bytes directly into [`TapeRec::child_off`], relying on the
//! sentinel `TapeOffset::NONE` (`u32::MAX`) to signal absence. That
//! overload is unwound. Inline scalars land in the `pay_narrow`
//! column; 8-byte scalars (`WideScalar`) land in `pay_wide`;
//! aggregates (`Aggregate` / `LargeAggregate`) and byte-string
//! frames (`Bytes`) still flow through the unified arena
//! [`Columns::pay_agg`]. In every case the record's
//! [`child_off`] slot holds the lookup pointer — a column rank into
//! `pay_narrow` / `pay_wide`, or the arena byte offset for arena-
//! backed payloads. Compound records use `child_off` for first-child
//! pointer, or `TapeOffset::NONE` for empty runs.
//!
//! # Sibling skip (AV.2.2)
//!
//! `sib_skip[i]` holds the distance from record `i` to the root of
//! its next-emission sibling within the shared parent's children
//! run, or `0` if the record is the last sibling (or the root). The
//! distance is `next_sibling_root - i`; since the current tape is
//! laid out in post-order, `sib_skip` is not constant-1 — a leaf
//! followed by a compound sibling reports a distance equal to the
//! sibling subtree's size.
//!
//! Forward sibling traversal reads `sib_skip[i]` exactly once per
//! step: `i → i + sib_skip[i]`; iteration ends when the read is
//! zero. The old backward child-enumeration path (`count_backward`
//! / `nth_backward`) is removed.
//!
//! [`TapeRec::child_off`]: crate::TapeRec::child_off
//! [`child_off`]: crate::TapeRec::child_off

use crate::kind::TapeKind;
use crate::tape::{TapeOffset, TapeRec};

/// The columnar record substrate.
///
/// Every column is a plain `Vec` — `alloc::vec::Vec` on stable — so
/// the whole struct is `Default`/`Debug` without extra bounds. The
/// six structural columns grow in lockstep (`push_structural` is the
/// only `fn` that appends to them, and it appends to all six); the
/// typed-payload columns grow independently as payloads of each type
/// are written.
#[derive(Debug, Default)]
pub struct Columns {
    // ── Structural columns (per-record, one entry each) ────────────
    /// Packed byte: low 4 bits = [`TapeKind`], high 4 bits = low 4
    /// bits of `meta_idx`. Read with [`TapeKind::from_u8`] and the
    /// [`TapeRec::pack_kind_meta`] inverse.
    pub kinds: Vec<u8>,
    /// Bitfield byte: variant_idx (low 6), has_children (bit 6),
    /// meta_idx bit \[4\] (bit 7).
    pub flags: Vec<u8>,
    /// Packed per-record flags (`STRING_BORROW_BIT` today).
    pub extra: Vec<u16>,
    /// Source-byte start offset.
    pub span_lo: Vec<u32>,
    /// Source-byte end offset (`span_hi == span_lo` for epsilon).
    pub span_hi: Vec<u32>,
    /// Distance to the next-emission sibling within the parent's
    /// children run. `0` for the last sibling (and for the root).
    /// Computed at [`TapeBuilder::finish`](crate::TapeBuilder::finish).
    pub sib_skip: Vec<u32>,
    /// Polymorphic per-record pointer. For compounds: offset of the
    /// first child record (or `TapeOffset::NONE` on empty runs). For
    /// leaves carrying an inline scalar (`pay_narrow`) or wide
    /// scalar (`pay_wide`): the column rank — `pay_*[idx]` reads the
    /// payload. For leaves carrying an aggregate or byte-frame: the
    /// [`pay_agg`] byte offset. For leaves with no payload:
    /// `TapeOffset::NONE`.
    ///
    /// [`pay_agg`]: Self::pay_agg
    pub child_off: Vec<TapeOffset>,

    // ── Typed payload columns (dense-packed in push order) ─────────
    /// Inline scalars ≤ 4 B. The record's `child_off` holds the
    /// column rank.
    pub pay_narrow: Vec<u32>,
    /// 8-byte scalars (`f64` / `u64` / `i64` / packed `Span`). Stored
    /// as raw bits; readers reinterpret via `f64::from_bits` or the
    /// typed cast. The record's `child_off` holds the column rank.
    pub pay_wide: Vec<u64>,
    /// Unified payload arena — aggregate tuple bytes, decoded
    /// strings, and byte-string frames all land here. Continues to
    /// back [`Tape::arena`](crate::Tape::arena) for external
    /// consumers that slice raw bytes.
    pub pay_agg: Vec<u8>,
}

impl Columns {
    /// Construct an empty column set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct a column set sized for `expected` structural records.
    ///
    /// Only the six structural columns + `child_off` pre-allocate;
    /// typed-payload columns are lazy, since most grammars exercise
    /// only a subset.
    pub fn with_capacity(expected: usize) -> Self {
        Self {
            kinds: Vec::with_capacity(expected),
            flags: Vec::with_capacity(expected),
            extra: Vec::with_capacity(expected),
            span_lo: Vec::with_capacity(expected),
            span_hi: Vec::with_capacity(expected),
            sib_skip: Vec::with_capacity(expected),
            child_off: Vec::with_capacity(expected),
            pay_narrow: Vec::new(),
            pay_wide: Vec::new(),
            pay_agg: Vec::with_capacity(expected / 8 * 8),
        }
    }

    /// Number of structural records.
    #[inline]
    pub fn len(&self) -> usize {
        self.kinds.len()
    }

    /// True iff no structural records have been appended.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.kinds.is_empty()
    }

    /// Append one structural row across the six structural columns
    /// plus `child_off`, returning the row's position.
    ///
    /// Caller supplies pre-packed `kind_meta` / `flags` bytes via
    /// [`TapeRec::pack_kind_meta`]. The `sib_skip` column is stamped
    /// `0`; it gets back-filled at
    /// [`TapeBuilder::finish`](crate::TapeBuilder::finish).
    #[inline]
    pub(crate) fn push_structural(
        &mut self,
        kind_meta: u8,
        flags: u8,
        extra: u16,
        span_lo: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) -> u32 {
        let idx = self.kinds.len() as u32;
        self.kinds.push(kind_meta);
        self.flags.push(flags);
        self.extra.push(extra);
        self.span_lo.push(span_lo);
        self.span_hi.push(span_hi);
        self.sib_skip.push(0);
        self.child_off.push(child_off);
        idx
    }

    /// Materialise a 16-byte [`TapeRec`] view of row `i`.
    ///
    /// Reads the six structural columns + `child_off` at `i` and
    /// reconstructs the flat record. The returned value is the same
    /// 16-byte `Copy` struct pre-AV readers consumed — consumers that
    /// held `&TapeRec` migrated to owned `TapeRec` at the API
    /// boundary.
    #[inline]
    pub fn materialize(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.kinds.len(), "materialize: idx {} out of range", i);
        TapeRec {
            kind_meta: self.kinds[idx],
            flags: self.flags[idx],
            extra: self.extra[idx],
            span_lo: self.span_lo[idx],
            span_hi: self.span_hi[idx],
            child_off: self.child_off[idx],
        }
    }

    /// Unchecked variant of [`Self::materialize`].
    ///
    /// # Safety
    ///
    /// The caller must guarantee `i < self.len()`. The `TapeCursor`
    /// honours this via `TapeOffset`s produced by the builder.
    #[inline]
    pub unsafe fn materialize_unchecked(&self, i: u32) -> TapeRec {
        let idx = i as usize;
        debug_assert!(idx < self.kinds.len(), "materialize_unchecked: idx {} out of range", i);
        // SAFETY: caller guarantees idx < self.kinds.len(), and every
        // structural column is maintained at the same length by
        // `push_structural`.
        unsafe {
            TapeRec {
                kind_meta: *self.kinds.get_unchecked(idx),
                flags: *self.flags.get_unchecked(idx),
                extra: *self.extra.get_unchecked(idx),
                span_lo: *self.span_lo.get_unchecked(idx),
                span_hi: *self.span_hi.get_unchecked(idx),
                child_off: *self.child_off.get_unchecked(idx),
            }
        }
    }

    /// Read a record's `TapeKind` directly from the `kinds` column
    /// without materialising the whole 16-byte view.
    #[inline]
    pub fn kind_at(&self, i: u32) -> TapeKind {
        TapeKind::from_u8(self.kinds[i as usize] & 0x0F)
    }

    /// Read a record's `has_children` bit directly from the `flags`
    /// column.
    #[inline]
    pub fn has_children_at(&self, i: u32) -> bool {
        (self.flags[i as usize] & 0x40) != 0
    }

    /// Read a record's `child_off` directly.
    #[inline]
    pub fn child_off_at(&self, i: u32) -> TapeOffset {
        self.child_off[i as usize]
    }

    /// Read a record's `sib_skip` directly.
    #[inline]
    pub fn sib_skip_at(&self, i: u32) -> u32 {
        self.sib_skip[i as usize]
    }

    /// Read a record's `span_lo` / `span_hi` directly.
    #[inline]
    pub fn span_at(&self, i: u32) -> (u32, u32) {
        let idx = i as usize;
        (self.span_lo[idx], self.span_hi[idx])
    }

    /// Compute the `sib_skip` column for every non-root record,
    /// assuming the structural columns are fully populated in the
    /// current post-order layout.
    ///
    /// For every direct-child run reachable through the `child_off`
    /// pointers, sibling roots are enumerated and their `sib_skip`
    /// slots stamped with `next_root - this_root`; the last sibling
    /// in each run keeps its default `0`.
    ///
    /// O(N) across the whole tape — each record is visited once in
    /// the forward pass, and the backward-walk cost per compound is
    /// amortised by the fact that each child is visited by exactly
    /// one parent.
    pub(crate) fn compute_sibling_skip(&mut self) {
        // Forward scan: every record that advertises children kicks
        // off a backward enumeration to collect direct-child roots.
        for parent_idx in 0..self.kinds.len() as u32 {
            if !self.has_children_at(parent_idx) {
                continue;
            }
            let child_start = self.child_off_at(parent_idx);
            if child_start.is_none() {
                continue;
            }
            let start = child_start.0 as usize;
            let end = parent_idx as usize;
            if start >= end {
                continue;
            }
            // Collect child roots in reverse emission order via
            // backward walk, then stamp sib_skip forward.
            let mut roots: Vec<u32> = Vec::new();
            let mut pos = end;
            while pos > start {
                let co = pos - 1;
                roots.push(co as u32);
                let has_children = self.has_children_at(co as u32);
                let co_child_off = self.child_off_at(co as u32);
                pos = if has_children && !co_child_off.is_none() {
                    co_child_off.0 as usize
                } else {
                    co
                };
            }
            // `roots` is in reverse emission order; stamp sib_skip
            // between consecutive siblings in forward emission order.
            // The first iteration yields the largest offset (last
            // emitted, last sibling), which keeps the default `0`.
            for window in roots.windows(2) {
                let later = window[0];
                let earlier = window[1];
                self.sib_skip[earlier as usize] = later - earlier;
            }
        }
    }
}
