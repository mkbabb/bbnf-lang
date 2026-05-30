//! NEON intrinsics body for `COMMENT_BODY_MASK_64`.
//!
//! Contract (executable specification lives in
//! `src/scalar/comment_body_mask_64.rs`):
//!   Input  — 64 source bytes + a `CommentCarry` (in_comment + pending_half).
//!   Output — a 64-bit interior-of-comment mask + the next-block carry.
//!
//! Strategy: the per-lane byte comparisons that the scalar reference performs
//! one byte at a time (`src[i] == open[0]`, `== open[1]`, `== close[0]`,
//! `== close[1]`) are lifted to four `vceqq_u8`-fans over the four 16-byte
//! stripes, packed to 64-bit lane masks via the in-tree movemask convention.
//! The digraph pairing (`open0 & (open1>>1)` etc.) is then a pair of bitwise
//! shifts, and the comment-region fill is the one inherently-serial carry the
//! scalar twin also threads — resolved here over the 64-bit event masks rather
//! than re-loading bytes. The result is bit-identical to the scalar reference,
//! which the checkasm differential proves.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): the NEON body is
//!     a checkasm differential against the scalar parity anchor.
//!   * Lemire/Langdale comment suppression: the open/close event masks are the
//!     vectorized analogue of the scalar digraph tests; the region fill is the
//!     `overflowing_add`-style carry resolve over those masks.

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;

use crate::scalar::comment_body_mask_64::CommentCarry;

/// NEON body for `COMMENT_BODY_MASK_64`. Bit-identical to the scalar reference.
#[cfg(target_arch = "aarch64")]
#[inline]
pub fn comment_body_mask_64_neon(
    src: &[u8; 64],
    open: [u8; 2],
    close: [u8; 2],
    carry: CommentCarry,
) -> (u64, CommentCarry) {
    // Lane-equality masks for each digraph byte, computed with NEON.
    let open0 = eq_mask_64(src, open[0]);
    let open1 = eq_mask_64(src, open[1]);
    let close0 = eq_mask_64(src, close[0]);
    let close1 = eq_mask_64(src, close[1]);

    resolve_regions(open0, open1, close0, close1, carry)
}

/// 64-bit mask of lanes equal to `needle`, via four `vceqq_u8` stripes packed
/// through the in-tree movemask convention.
#[cfg(target_arch = "aarch64")]
#[inline]
fn eq_mask_64(src: &[u8; 64], needle: u8) -> u64 {
    unsafe {
        let n = vdupq_n_u8(needle);
        let s0 = vceqq_u8(vld1q_u8(src.as_ptr()), n);
        let s1 = vceqq_u8(vld1q_u8(src.as_ptr().add(16)), n);
        let s2 = vceqq_u8(vld1q_u8(src.as_ptr().add(32)), n);
        let s3 = vceqq_u8(vld1q_u8(src.as_ptr().add(48)), n);
        (movemask_u8x16(s0) as u64)
            | ((movemask_u8x16(s1) as u64) << 16)
            | ((movemask_u8x16(s2) as u64) << 32)
            | ((movemask_u8x16(s3) as u64) << 48)
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

/// Resolve the comment-interior mask from the four per-byte event masks. This
/// is the inherently-serial carry the scalar twin threads; it walks set bits
/// rather than every byte, so it is sparse on real CSS (few comment digraphs).
#[inline]
fn resolve_regions(
    open0: u64,
    open1: u64,
    close0: u64,
    close1: u64,
    carry: CommentCarry,
) -> (u64, CommentCarry) {
    let mut mask: u64 = 0;
    let mut in_comment = carry.in_comment;
    let mut pending_half = carry.pending_half;

    let mut i = 0usize;
    while i < 64 {
        let bit = 1u64 << i;
        if in_comment {
            mask |= bit;
            if pending_half {
                pending_half = false;
                if close1 & bit != 0 {
                    in_comment = false;
                    i += 1;
                    continue;
                }
            }
            if close0 & bit != 0 {
                if i + 1 < 64 {
                    if close1 & (bit << 1) != 0 {
                        mask |= bit << 1;
                        in_comment = false;
                        i += 2;
                        continue;
                    }
                } else {
                    pending_half = true;
                }
            }
            i += 1;
            continue;
        }

        if pending_half {
            pending_half = false;
            if open1 & bit != 0 {
                in_comment = true;
                i += 1;
                continue;
            }
        }
        if open0 & bit != 0 {
            if i + 1 < 64 {
                if open1 & (bit << 1) != 0 {
                    in_comment = true;
                    i += 2;
                    continue;
                }
            } else {
                pending_half = true;
            }
        }
        i += 1;
    }

    (mask, CommentCarry { in_comment, pending_half })
}
