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

use super::{PayloadData, Tape, TapeOffset, TapeRec};

impl<R> Tape<R> {
    // ── Write surface — depth-bracket primitives (B5.W6) ─────────────

    /// Open a post-order shape's children scope.
    ///
    /// Bumps `current_depth` so children pushed by the body stamp
    /// `frame_depth` at the correct (parent + 1) depth at push time.
    /// Returns the current write position, which the caller threads
    /// to [`Self::end_compound_post_order`] as `first_child` after
    /// the body completes.
    ///
    /// Pairs with one of:
    /// - [`Self::end_compound_post_order`] — successful close, body
    ///   emitted ≥ 1 child + the post-order compound row;
    /// - [`Self::exit_post_order_children`] — failure / rollback path
    ///   where the body either emitted nothing or was rolled back
    ///   without a post-order compound row landing.
    ///
    /// # Architectural role (B5.W6 depth-stamp invariant inversion)
    ///
    /// Pre-W6 the substrate stamped `frame_depth` at push-time using
    /// the OUTER frame's depth, then retroactively bumped every byte
    /// in the closed compound's subtree by `+1` inside
    /// `end_compound_post_order` (the leftmost-descendant cascade).
    /// W6 inverts the invariant: depth bumps explicitly at the
    /// children-enter point, so children push at the correct depth
    /// without retroactive correction.
    ///
    /// `frame_depth` is now written exactly once per record — by
    /// `Columns::push_structural` at push time, reading the live
    /// `current_depth` the bracket has already advanced. The retroactive
    /// cascade and the auxiliary `leftmost_descendant_offset` walk both
    /// retire under this single-writer invariant.
    #[inline(always)]
    pub fn enter_post_order_children(&mut self) -> u32 {
        let pos = self.columns.records.len() as u32;
        self.columns.current_depth = self.columns.current_depth.saturating_add(1);
        pos
    }

    /// Cancel a post-order children scope without emitting the
    /// compound row.
    ///
    /// Decrements `current_depth` to undo the bump
    /// [`Self::enter_post_order_children`] applied. Called from
    /// retry-loop rollback sites where the failing branch's body
    /// rolled back to the bracket's open position (no post-order
    /// compound row will land, so no
    /// [`Self::end_compound_post_order`] will run to absorb the
    /// bump). The matching [`Self::rollback_to`] still rewinds the
    /// structural columns; this primitive owns the depth-counter
    /// half of the symmetric retreat.
    #[inline(always)]
    pub fn exit_post_order_children(&mut self) {
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
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
    /// [`Self::end_compound`]. Emitter retry paths rewind via
    /// [`Self::rollback_to`] with the returned offset; the next
    /// `begin_compound` reuses the same row.
    ///
    /// Post-order shapes use [`Self::begin_compound_post`] paired
    /// with a preceding [`Self::enter_post_order_children`].
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

    /// Begin a compound in post-order — the compound row lands
    /// AFTER its children have been emitted.
    ///
    /// Pairs with [`Self::enter_post_order_children`] / [`Self::end_compound_post_order`].
    ///
    /// # Depth stamp (B5.W6)
    ///
    /// The matching [`Self::enter_post_order_children`] already bumped
    /// `current_depth` from outer to outer+1 before the body emitted
    /// children. The compound row itself is conceptually at the OUTER
    /// frame's depth (it's a SIBLING of records that pushed before the
    /// bracket opened, not a child of them). Stamps the row at
    /// `current_depth - 1` directly, leaving the bracket-bumped
    /// `current_depth` in place for [`Self::end_compound_post_order`]
    /// to absorb on close.
    ///
    /// No `current_depth` bump happens here — unlike pre-order
    /// [`Self::begin_compound`], which stamps at `current_depth` then
    /// bumps so its post-allocation children push at the bumped depth.
    /// Post-order children already landed before this row, so the bump
    /// is unnecessary.
    #[inline(always)]
    pub fn begin_compound_post(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        variant_idx: u8,
        meta_idx: u8,
        extra_flags: u16,
    ) -> u32 {
        debug_assert!(
            kind.is_compound(),
            "begin_compound_post on leaf/annotation kind {:?}",
            kind
        );
        let (kind_meta, extra_meta_bit) = TapeRec::pack_kind_meta(kind, meta_idx);
        // Temporarily decrement so `push_structural` stamps the row at
        // the outer-frame depth; restore the bracket-bumped depth so
        // any further pushes between this row and the matching
        // `end_compound_post_order` (none in canonical post-order, but
        // the substrate must not assume) continue at the correct depth.
        let bumped = self.columns.current_depth;
        self.columns.current_depth = bumped.saturating_sub(1);
        let idx = self.columns.push_structural(
            kind_meta,
            variant_idx,
            extra_flags | extra_meta_bit,
            span_lo,
            span_lo,
            TapeOffset::NONE,
        );
        self.columns.current_depth = bumped;
        self.value_begin_compound(kind, span_lo, variant_idx, idx);
        idx
    }

    /// Finalise a compound opened via [`Self::begin_compound`] in
    /// pre-order — the caller emitted the compound row BEFORE its
    /// children, so the first child's root sits AT or AFTER `open_offset
    /// + 1` depending on whether the immediately-following record is a
    /// direct child or a deeper grandchild emitted by a post-order
    /// sub-shape.
    ///
    /// # First-child resolution (B5.W6)
    ///
    /// The forward scan walks `frame_depth` from `open_offset + 1`
    /// upward looking for the first record stamped at `open_depth + 1`.
    /// Under the bracket discipline the depth column is single-writer
    /// (stamped once at push time by `Columns::push_structural`); no
    /// retroactive cascade mutates it. The walk is structurally
    /// necessary — and only structurally necessary — because a
    /// post-order sub-shape emits its own body children FIRST at the
    /// post-order shape's bracket-bumped depth (`open_depth + 2` from
    /// this compound's perspective) and only then its outer compound
    /// row at `open_depth + 1`. The scan finds that outer row and
    /// stamps it as `child_off`.
    ///
    /// For purely-leaf children (no post-order sub-shape), the first
    /// record at `open_offset + 1` is already at `open_depth + 1`, so
    /// the scan terminates on the very first iteration. Pathological
    /// deep-nest inputs see scan length proportional to the leftmost
    /// post-order subtree's depth.
    #[inline(always)]
    pub fn end_compound(&mut self, open_offset: u32, span_hi: u32) {
        self.columns.set_span_hi_at(open_offset, span_hi);
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

    /// Finalise a compound opened via [`Self::begin_compound_post`]
    /// in post-order — the compound row was allocated AFTER its
    /// children, so `open_offset` is the LAST record and the first
    /// child's root is `first_child` (captured by the matching
    /// [`Self::enter_post_order_children`] before the body).
    ///
    /// # Single-writer invariant (B5.W6)
    ///
    /// `frame_depth` is written exactly once per record by
    /// `Columns::push_structural` at push time, reading the live
    /// `current_depth` the bracket has already advanced. This method
    /// no longer mutates `frame_depth`: the children's depths are
    /// already correct at push time, the compound row's depth was
    /// stamped at the outer-frame depth by [`Self::begin_compound_post`],
    /// and no retroactive correction is necessary.
    ///
    /// Decrements `current_depth` once to absorb the bump
    /// [`Self::enter_post_order_children`] applied; the matching
    /// bracket pair leaves the depth counter at its outer-frame value
    /// across the close.
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
        }
        self.columns.current_depth = self.columns.current_depth.saturating_sub(1);
        self.value_end_compound(span_hi);
    }

    /// Wrap a contiguous tail of already-emitted records as the
    /// children of a compound row that was opened pre-order via
    /// [`Self::begin_compound`] AFTER the children themselves landed.
    ///
    /// The Pratt reducer pattern is the sole consumer: operand /
    /// operator records were emitted into the tape by the operand-
    /// dispatch loop and the operator-leaf push, then the reducer
    /// fires and allocates a NEW compound row via
    /// [`Self::begin_compound`] at the post-position. The new row
    /// sits at the same depth as the operand records (both are
    /// children of the outer Pratt frame), but conceptually the
    /// operands are now children OF the reducer — one level deeper.
    /// Because the operand records were already pushed before the
    /// reducer compound existed, no bracket pair could anticipate
    /// them; the depth retrofit happens here as a flat-slice bump
    /// over `[first_child, open_offset)`.
    ///
    /// # B5.W6 distinction from `end_compound_post_order`
    ///
    /// Ordinary post-order shapes (Wrap, Seq, Repeat, Alt) bracket
    /// their children before emission via
    /// [`Self::enter_post_order_children`], so depth is correct at
    /// push time and `end_compound_post_order` only back-patches.
    /// The Pratt reducer pattern is uniquely retroactive — the
    /// "children" are emitted before the wrapping compound is even
    /// conceived — so the substrate offers a dedicated primitive
    /// that owns the flat-slice retrofit. The leftmost-descendant
    /// chain walk that pre-W6 `end_compound_post_order` performed is
    /// gone: under the new invariant the operand subtrees emitted
    /// inside `[first_child, open_offset)` are already self-
    /// consistent (their per-shape brackets stamped each interior
    /// at the correct relative depth), so a flat `+1` over the slice
    /// lifts the entire range uniformly.
    ///
    /// `open_offset` is the row produced by the matching
    /// [`Self::begin_compound`]; that call already bumped
    /// `current_depth` so the reducer row stamped one level above
    /// its eventual children. This method decrements `current_depth`
    /// once to restore the parent's frame, mirroring
    /// [`Self::end_compound_post_order`]'s discipline.
    ///
    /// # Leftmost-descendant retrofit (B5.W6)
    ///
    /// `first_child` names the operand's OUTER compound row (returned
    /// by the operand sub-shape call), not its leftmost descendant.
    /// Under the bracket discipline the operand's body children were
    /// pushed at depths inside the operand's own bracket — at indices
    /// strictly LESS than `first_child` (post-order layout). The
    /// reducer's `+1` lift must reach those descendants too, so the
    /// retrofit walks the leftmost-descendant chain from `first_child`
    /// down to the lowest interior offset, then lifts the contiguous
    /// `[leftmost, open_offset)` slice by `+1` uniformly.
    ///
    /// The walk follows `child_off` while it points strictly backward
    /// (canonical post-order subtree root). For pre-order children
    /// (`child_off >= self_idx`) and leaves the walk stops: pre-order
    /// child ranges live at offsets ABOVE the parent, so the parent's
    /// own offset is the leftmost in that subtree's prefix.
    #[inline(always)]
    pub fn wrap_existing_children_post_order(
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
            // Walk to the leftmost descendant of `first_child` so the
            // lift reaches every transitive descendant of the operand.
            let mut lmd = first_child.0;
            while self.columns.has_children_at(lmd) {
                let co = self.columns.child_off_at(lmd);
                if co.is_none() || co.0 >= lmd {
                    break;
                }
                lmd = co.0;
            }
            let lo = lmd as usize;
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
