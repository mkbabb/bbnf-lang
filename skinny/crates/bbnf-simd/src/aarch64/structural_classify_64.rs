//! NEON intrinsics body for `STRUCTURAL_CLASSIFY_64` — the `vceqq` fan (scheme α).
//!
//! Contract (executable specification lives in
//! `src/scalar/structural_classify_64.rs`):
//!   Input  — 64 contiguous source bytes + a `KindPartition` (caller data: the
//!            structural byte-set partitioned into `open`/`close`/`quote`/
//!            `slash`/`stop` buckets).
//!   Output — a `StructuralClassify64`: `significant` u64 + one u64 per kind
//!            bucket, bit `i` ↔ `src[i]`.
//!
//! Why the fan, not a LUT (drives the kernel choice):
//!   The 11-byte significant set `' " / ( ) [ ] { } ; :` has lo-nibble
//!   collisions (`; [ {` ≡ 0xB; `] }` ≡ 0xD) AND a lo-6-bit collision
//!   (`;`=0x3B ≡ `{`=0x3B). So neither a single `vqtbl1q_u8` low-nibble LUT NOR
//!   the existing `vqtbl4q_u8` lo6-table can assign a DISTINCT kind per byte —
//!   exactly the lo6-collision that made §8 reject the table-classifier-unify.
//!   The `vceqq_u8` fan compares for EQUALITY, which is exact and collision-free
//!   by construction, so it is the correct aarch64 scheme for this set. (The
//!   simdjson lo×hi two-LUT AND-fold is the upgrade path IF a future grammar
//!   widens the set past ~16, where the fan's compare count would dominate; it
//!   is NOT built here.)
//!
//! Toolkit (REUSED verbatim from `byte_class_from_eq_set_64`):
//!   four `uint8x16` stripes, each `vceqq_u8`-fanned against each broadcast
//!   member and `vorrq_u8`-reduced into a per-kind mask-vector, then packed to
//!   u64 via the project-wide `movemask_u8x16` (shrn/narrow). The `significant`
//!   mask FALLS OUT of the kind fan for free: it is the OR of the five kind
//!   mask-vectors (one extra `vorrq` per stripe). No new movemask, no LUT build.
//!
//! Body status: the AArch64 admission body for the checkasm parity sweep; the
//! scalar reference is the parity anchor. aarch64 NEON ONLY (no x86, no SVE).

use crate::scalar::structural_classify_64::{KindPartition, StructuralClassify64};

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;

/// One 16-byte stripe's kind classification: OR-reduce the `vceqq_u8`
/// comparisons of each bucket's members into a per-kind mask-vector, and the
/// union into `significant`. Returns the five kind mask-vectors plus the
/// significant mask-vector (each lane 0xFF where the byte is a member, else
/// 0x00), ready for the `movemask_u8x16` pack.
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn classify_stripe(
    stripe: uint8x16_t,
    partition: &KindPartition<'_>,
) -> (
    uint8x16_t, // open
    uint8x16_t, // close
    uint8x16_t, // quote
    uint8x16_t, // slash
    uint8x16_t, // stop
    uint8x16_t, // significant (= OR of the five)
) {
    unsafe {
        let zero = vdupq_n_u8(0);

        // Each kind bucket OR-reduces the per-member equality compares.
        let mut open = zero;
        for &member in partition.open {
            open = vorrq_u8(open, vceqq_u8(stripe, vdupq_n_u8(member)));
        }
        let mut close = zero;
        for &member in partition.close {
            close = vorrq_u8(close, vceqq_u8(stripe, vdupq_n_u8(member)));
        }
        let mut quote = zero;
        for &member in partition.quote {
            quote = vorrq_u8(quote, vceqq_u8(stripe, vdupq_n_u8(member)));
        }
        let mut slash = zero;
        for &member in partition.slash {
            slash = vorrq_u8(slash, vceqq_u8(stripe, vdupq_n_u8(member)));
        }
        let mut stop = zero;
        for &member in partition.stop {
            stop = vorrq_u8(stop, vceqq_u8(stripe, vdupq_n_u8(member)));
        }

        // significant = the union of the kind buckets (free; one OR-tree).
        let significant = vorrq_u8(
            vorrq_u8(vorrq_u8(open, close), vorrq_u8(quote, slash)),
            stop,
        );

        (open, close, quote, slash, stop, significant)
    }
}

/// NEON body for `STRUCTURAL_CLASSIFY_64`.
///
/// Classifies the 64-byte block into a `significant` bitmask + one bitmask per
/// kind bucket via the `vceqq` fan (scheme α). Bit-exact with
/// `structural_classify_64_scalar`.
#[cfg(target_arch = "aarch64")]
#[inline]
pub fn structural_classify_64_neon(
    src: &[u8; 64],
    partition: &KindPartition<'_>,
) -> StructuralClassify64 {
    unsafe {
        // --- 1. Load the 64-byte source as four uint8x16_t stripes ----------
        let s0 = vld1q_u8(src.as_ptr());
        let s1 = vld1q_u8(src.as_ptr().add(16));
        let s2 = vld1q_u8(src.as_ptr().add(32));
        let s3 = vld1q_u8(src.as_ptr().add(48));

        // --- 2. Per-stripe kind fans ----------------------------------------
        let (o0, c0, q0, sl0, st0, sg0) = classify_stripe(s0, partition);
        let (o1, c1, q1, sl1, st1, sg1) = classify_stripe(s1, partition);
        let (o2, c2, q2, sl2, st2, sg2) = classify_stripe(s2, partition);
        let (o3, c3, q3, sl3, st3, sg3) = classify_stripe(s3, partition);

        // --- 3. Pack each kind's four 16-lane masks into a single u64 -------
        StructuralClassify64 {
            open: pack4(o0, o1, o2, o3),
            close: pack4(c0, c1, c2, c3),
            quote: pack4(q0, q1, q2, q3),
            slash: pack4(sl0, sl1, sl2, sl3),
            stop: pack4(st0, st1, st2, st3),
            significant: pack4(sg0, sg1, sg2, sg3),
        }
    }
}

/// Pack four 16-lane comparison vectors (stripes 0..4) into one 64-bit mask via
/// the project-wide `movemask_u8x16` convention (shrn/narrow), mirroring
/// `byte_class_from_eq_set_64_neon`.
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn pack4(m0: uint8x16_t, m1: uint8x16_t, m2: uint8x16_t, m3: uint8x16_t) -> u64 {
    unsafe {
        let lo0 = movemask_u8x16(m0) as u64;
        let lo1 = movemask_u8x16(m1) as u64;
        let lo2 = movemask_u8x16(m2) as u64;
        let lo3 = movemask_u8x16(m3) as u64;
        lo0 | (lo1 << 16) | (lo2 << 32) | (lo3 << 48)
    }
}

/// Pack a 16-lane comparison vector (each lane 0xFF or 0x00) into a 16-bit mask.
/// Mirrors the `movemask_u8x16` convention already in
/// `aarch64/byte_class_from_eq_set_64.rs` (multiply-by-bit-weight, sum halves).
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
    unsafe {
        let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
        let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
        let lo = vaddv_u8(vget_low_u8(bits)) as u16;
        let hi = vaddv_u8(vget_high_u8(bits)) as u16;
        lo | (hi << 8)
    }
}
