//! NEON intrinsics body for `BRACKET_DEPTH_MASK_64`.
//!
//! Contract (executable specification lives in
//! `src/scalar/bracket_depth_mask_64.rs`):
//!   Input  — 64 source bytes + open/close byte sets + i32 depth carry.
//!   Output — a 64-bit bracket-interior (depth ≥ 1) mask + the next depth.
//!
//! Strategy (REDRESS-89 compliant — the running balance stays the DEFAULT
//! body): the per-byte open/close classification the scalar reference performs
//! one byte at a time is lifted to `vceqq_u8`-fans over the four 16-byte
//! stripes, producing 64-bit `opens`/`closes` event masks. The depth
//! accumulation is then the SAME scalar running balance the reference threads
//! (i32 `depth`, init from the carry) — NOT a CTZ-ranges body. The result is
//! bit-identical to the scalar reference, proven by the checkasm differential.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): NEON differential
//!     against the scalar parity anchor.
//!   * L6 binding condition (P2-F §4, SPEC §9 #4): the scalar running balance
//!     is the default body; CTZ is consumer-only + parity-gated. This NEON
//!     body keeps the running balance and only vectorizes the classification.

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;

const SET_CAP: usize = 4;

/// NEON body for `BRACKET_DEPTH_MASK_64`. Bit-identical to the scalar reference.
#[cfg(target_arch = "aarch64")]
#[inline]
pub fn bracket_depth_mask_64_neon(
    src: &[u8; 64],
    opens: &[u8; SET_CAP],
    open_len: usize,
    closes: &[u8; SET_CAP],
    close_len: usize,
    depth_in: i32,
) -> (u64, i32) {
    let open_mask = eq_set_mask_64(src, &opens[..open_len]);
    let close_mask = eq_set_mask_64(src, &closes[..close_len]);
    running_balance(open_mask, close_mask, depth_in)
}

/// 64-bit mask of lanes equal to ANY member of `set` (≤ SET_CAP members), via
/// `vceqq_u8` fans OR-reduced per stripe, packed through the in-tree movemask.
#[cfg(target_arch = "aarch64")]
#[inline]
fn eq_set_mask_64(src: &[u8; 64], set: &[u8]) -> u64 {
    unsafe {
        let s0 = vld1q_u8(src.as_ptr());
        let s1 = vld1q_u8(src.as_ptr().add(16));
        let s2 = vld1q_u8(src.as_ptr().add(32));
        let s3 = vld1q_u8(src.as_ptr().add(48));
        let mut m0 = vdupq_n_u8(0);
        let mut m1 = vdupq_n_u8(0);
        let mut m2 = vdupq_n_u8(0);
        let mut m3 = vdupq_n_u8(0);
        for &member in set {
            let n = vdupq_n_u8(member);
            m0 = vorrq_u8(m0, vceqq_u8(s0, n));
            m1 = vorrq_u8(m1, vceqq_u8(s1, n));
            m2 = vorrq_u8(m2, vceqq_u8(s2, n));
            m3 = vorrq_u8(m3, vceqq_u8(s3, n));
        }
        (movemask_u8x16(m0) as u64)
            | ((movemask_u8x16(m1) as u64) << 16)
            | ((movemask_u8x16(m2) as u64) << 32)
            | ((movemask_u8x16(m3) as u64) << 48)
    }
}

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

/// The scalar running balance over the precomputed open/close event masks —
/// the DEFAULT body (REDRESS-89: NOT CTZ). Bit-identical to the scalar twin.
#[inline]
fn running_balance(opens: u64, closes: u64, depth_in: i32) -> (u64, i32) {
    let mut mask: u64 = 0;
    let mut depth = depth_in;
    for i in 0..64 {
        let bit = 1u64 << i;
        if opens & bit != 0 {
            if depth > 0 {
                mask |= bit;
            }
            depth += 1;
        } else if closes & bit != 0 {
            if depth > 0 {
                mask |= bit;
                depth -= 1;
            }
        } else if depth > 0 {
            mask |= bit;
        }
    }
    (mask, depth)
}
