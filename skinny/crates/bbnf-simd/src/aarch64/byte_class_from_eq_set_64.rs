//! NEON intrinsics body for `BYTE_CLASS_FROM_EQ_SET_64`.
//!
//! Contract (executable specification lives in
//! `src/scalar/byte_class_from_eq_set_64.rs`):
//!   Input  — 64 contiguous source bytes + a set `S` of up to 8 byte values.
//!   Output — 64-bit mask where bit `i` is set iff `src[i] ∈ S`.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): the NEON
//!     translation of asmjson's `classify_chunk` is four 16-byte stripes;
//!     each stripe `vceqq_u8`-fans against each broadcast set member and
//!     `vorrq_u8`-reduces into a single mask-vector, then the four stripes
//!     are packed into a 64-bit result via the project-wide movemask
//!     convention (see `src/aarch64/movemask.rs` and the in-tree pattern in
//!     `src/lib.rs` `movemask_u8x16`).
//!   * Validark / dav1d "byte-class-fan" prior art: at NEON width the
//!     16-byte stripe collapses an arbitrary set membership check into
//!     `set_len`-many `vceqq_u8` ops fanned through `vorrq_u8`, with no
//!     scalar carry between stripes.
//!
//! Body status: this is the AArch64 admission body for the checkasm parity
//! sweep; the scalar reference is the parity anchor.

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;

/// NEON body for `BYTE_CLASS_FROM_EQ_SET_64`.
///
/// Returns a 64-bit mask where bit `i` is set iff `src[i] ∈ set`.
/// `set.len()` must be ≤ 8.
#[cfg(target_arch = "aarch64")]
#[inline]
pub fn byte_class_from_eq_set_64_neon(src: &[u8; 64], set: &[u8]) -> u64 {
    debug_assert!(set.len() <= 8, "BYTE_CLASS_FROM_EQ_SET_64 admits sets of size ≤ 8");
    unsafe {
        // --- 1. Load the 64-byte source as four uint8x16_t stripes ----------
        let s0 = vld1q_u8(src.as_ptr());
        let s1 = vld1q_u8(src.as_ptr().add(16));
        let s2 = vld1q_u8(src.as_ptr().add(32));
        let s3 = vld1q_u8(src.as_ptr().add(48));

        // --- 2. For each set member, OR-reduce equality across the stripes -
        let mut m0 = vdupq_n_u8(0);
        let mut m1 = vdupq_n_u8(0);
        let mut m2 = vdupq_n_u8(0);
        let mut m3 = vdupq_n_u8(0);

        for &member in set {
            let needle = vdupq_n_u8(member);
            m0 = vorrq_u8(m0, vceqq_u8(s0, needle));
            m1 = vorrq_u8(m1, vceqq_u8(s1, needle));
            m2 = vorrq_u8(m2, vceqq_u8(s2, needle));
            m3 = vorrq_u8(m3, vceqq_u8(s3, needle));
        }

        // --- 3. Pack four 16-lane masks into a single 64-bit result ---------
        //
        // We mirror the in-tree `movemask_u8x16` convention from `src/lib.rs`:
        // multiply each lane by 2^(lane_index mod 8), sum the low/high halves
        // separately, then concatenate.  This is the same shape Lemire +
        // Mula use for the AArch64 movemask spill in their JSON-parsing
        // paper, and it stays entirely in NEON registers.
        let lo0 = movemask_u8x16(m0) as u64;
        let lo1 = movemask_u8x16(m1) as u64;
        let lo2 = movemask_u8x16(m2) as u64;
        let lo3 = movemask_u8x16(m3) as u64;

        lo0 | (lo1 << 16) | (lo2 << 32) | (lo3 << 48)
    }
}

/// Pack a 16-lane comparison vector (each lane 0xFF or 0x00) into a 16-bit
/// mask.  Mirrors the `movemask_u8x16` convention already in `src/lib.rs`.
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
