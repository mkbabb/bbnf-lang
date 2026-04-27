//! Scalar-payload direct-write fused writers + reduce_column consumer.
//!
//! AW-V.W1.3 — the per-shape emitter's leaf-emission arms know the
//! scalar payload statically; the hot path writes the decoded value
//! directly into `pay_agg` / `pay_wide` alongside the structural slot,
//! bypassing PSI's Stage-A `push` / Stage-B rayon fan-out.
//!
//! AW-IV.W5.1 — [`Columns::reduce_column`] is the generic consumer
//! surface that walks a typed payload column via a compile-time
//! selector ([`super::ColumnTag`]) + reducer ([`super::Reducer`]).

use super::{ColumnTag, Columns, Reducer};
use crate::kind::TapeKind;
use crate::tape::{TapeOffset, TapeRec};

impl Columns {

    // ── AW-V.W1.3 — scalar-payload direct-write fused API ────────────
    //
    // Per AW-V.md §W1.3 + B2 §3: the per-shape emitter's leaf-emission
    // arms know the scalar payload statically; the hot path writes the
    // decoded value directly into `pay_agg` alongside the structural
    // slot, bypassing PSI's Stage-A `push` / Stage-B rayon fan-out.
    // Kept verbatim post-AY.W1.1 — the structural-row push internally
    // routes through `push_leaf_fused` (now flat-AoS) but the surface
    // is unchanged.

    /// Write a scalar `f64` payload into `pay_agg` at `child_off` (8 B
    /// little-endian via `f64::to_bits().to_le_bytes()`) and append the
    /// structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_f64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: f64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_f64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_bits().to_le_bytes();
        // SAFETY: the `debug_assert!` above (enforced in debug builds;
        // the emitter's monotonic arena-cursor pre-condition in release
        // builds) guarantees the 8-byte range `[dst_off, dst_off+8)` is
        // in-bounds of `pay_agg`'s initialised region.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `u8` payload into `pay_agg` at `child_off` (1 B)
    /// and append the structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_u8(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u8,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_u8: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `bool` payload into `pay_agg` at `child_off`
    /// (1 B — `0` for `false`, `1` for `true`) and append the
    /// structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_bool(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: bool,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 1 <= self.pay_agg.len(),
            "push_scalar_payload_bool: arena offset {} + 1 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            *self.pay_agg.as_mut_ptr().add(dst_off) = value as u8;
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar hex `u32` payload into `pay_agg` at `child_off`
    /// (4 B little-endian) and append the structural slot for the
    /// leaf record. CSS hex colours pass the pre-decoded `#rrggbbaa`
    /// u32 from the emitter's `parse_hex_u32` inline body.
    #[inline(always)]
    pub fn push_scalar_payload_hex_u32(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: u32,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 4 <= self.pay_agg.len(),
            "push_scalar_payload_hex_u32: arena offset {} + 4 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = value.to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                4,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// Write a scalar `i64` payload into `pay_agg` at `child_off`
    /// (8 B little-endian via `(value as u64).to_le_bytes()`) and
    /// append the structural slot for the leaf record.
    #[inline(always)]
    pub fn push_scalar_payload_i64(
        &mut self,
        kind: TapeKind,
        span_lo: u32,
        span_hi: u32,
        sib_skip: u32,
        child_off: u32,
        value: i64,
    ) -> u32 {
        let dst_off = child_off as usize;
        debug_assert!(
            dst_off + 8 <= self.pay_agg.len(),
            "push_scalar_payload_i64: arena offset {} + 8 exceeds pay_agg len {}",
            child_off,
            self.pay_agg.len(),
        );
        let bytes = (value as u64).to_le_bytes();
        // SAFETY: see `push_scalar_payload_f64`.
        unsafe {
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                self.pay_agg.as_mut_ptr().add(dst_off),
                8,
            );
        }
        let idx = self.push_leaf_fused(
            kind,
            0,
            TapeRec::PAYLOAD_IN_ARENA_BIT,
            span_lo,
            span_hi,
            TapeOffset(child_off),
        );
        if sib_skip != 0 {
            self.sib_skip[idx as usize] = sib_skip;
        }
        idx
    }

    /// AY.W1.1 — paired-column span write degenerates to a single
    /// AoS field write on the flat substrate.
    ///
    /// Pre-AY this routed through inline-asm-pinned adjacent stores
    /// targeting two distinct `Vec<u32>` allocations to coax LLVM /
    /// the M-series Firestorm front-end into macro-op fusion. Post-
    /// AY both span endpoints live in the same 16-byte `TapeRec`;
    /// the writer is one indexed AoS field-update + one
    /// Same semantics, simpler code.
    #[inline(always)]
    pub fn stp_span(&mut self, idx: usize, span_lo_val: u32, span_hi_val: u32) {
        debug_assert!(
            idx < self.records.len(),
            "stp_span: idx {} out of range (records len {})",
            idx,
            self.records.len(),
        );
        let rec = &mut self.records[idx];
        rec.span_lo = span_lo_val;
        rec.span_hi = span_hi_val;
    }

    // ── AW-IV.W5.1 — reduce_column<C, R> consumer API ────────────────
    //
    // The generic consumer surface over the three typed payload
    // columns (`pay_narrow`, `pay_wide`, `pay_agg`). Surface preserved
    // verbatim post-AY.W1.1 — the column tags + reducers are
    // orthogonal to the structural layout revert.

    /// Reduce a single payload column via a compile-time-selected
    /// fold.
    #[inline]
    pub fn reduce_column<C, R>(&self) -> R::Acc
    where
        C: ColumnTag,
        R: Reducer<C::Value>,
    {
        R::reduce_slice(C::column(self))
    }
}

