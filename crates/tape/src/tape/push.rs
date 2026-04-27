//! Write-side compound + leaf push surfaces for `Tape<R>`.
//!
//! Pre-order compound emission (`begin_compound` / `end_compound`),
//! post-order compound close (`end_compound_post_order`), and the
//! family of leaf-push entry points (`push_leaf`, `push_leaf_with`,
//! `push_leaf_with_arena_*`, `push_leaf_borrowed_string`,
//! `push_leaf_with_f64_direct`) live here. Arena-slot allocation
//! helpers (`alloc_aggregate_slot`, `alloc_large_aggregate_slot`,
//! `alloc_bytes_frame`) and the arena-mutation pair (`arena_mut`,
//! `arena_len`, `stamp_arena_len_prefix`) round out the write API.

use crate::kind::TapeKind;
use crate::value::PayloadTag;

use super::{leftmost_descendant_offset, PayloadData, Tape, TapeOffset, TapeRec};

impl<R> Tape<R> {
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

    /// Finalise a pre-order compound and stamp `child_off` to a
    /// caller-supplied override rather than the leftmost-descendant
    /// scan [`Self::end_compound`] performs.
    ///
    /// Pratt outers admit a non-trivial first-child semantics: their
    /// reduced operator tree's root (the final reducer) is the
    /// authoritative `child_off`, not the lexically-first operand the
    /// pre-order scan would name. B5.W4 collapses the historical
    /// pattern of `end_compound` + `set_child_off_at` post-call surgery
    /// into a single primitive — the override rides through the close
    /// natively, and the post-call surgery surface (`set_child_off_at`
    /// outside the substrate's own finalisation paths) is no longer
    /// reachable from emitted code.
    ///
    /// Symmetric with [`Self::end_compound_post_order`], which already
    /// accepts a caller-supplied `first_child` for tapes whose post-
    /// order layout makes the lexical first-child unrecoverable from
    /// `open_offset + 1`. Stamps `HAS_CHILDREN_BIT`; the value-arena
    /// frame closes identically to [`Self::end_compound`].
    #[inline(always)]
    pub fn end_compound_with_child_off(
        &mut self,
        open_offset: u32,
        span_hi: u32,
        child_off: TapeOffset,
    ) {
        self.columns.set_span_hi_at(open_offset, span_hi);
        self.columns.set_child_off_at(open_offset, child_off);
        self.columns
            .or_extra_at(open_offset, TapeRec::HAS_CHILDREN_BIT);
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
        // B5.W2.4 — both the f64-direct path and the generic wide
        // scalar path project through `pay_wide`; the
        // `PAYLOAD_F64_DIRECT_BIT` survives as the f64-interpretation
        // marker on the unified column.
        let rank = self.columns.pay_wide.len() as u32;
        self.columns.pay_wide.push(f64_bits);
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
}
