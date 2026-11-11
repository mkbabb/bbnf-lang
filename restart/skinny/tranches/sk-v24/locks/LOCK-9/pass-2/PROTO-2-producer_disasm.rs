//! Standalone disasm probe: six movemask/classify producer forms, each an
//! #[inline(never)] #[no_mangle] symbol so otool -tv can isolate the
//! "instructions to a usable mask/count" per contract.
//!
//! Compile: rustc -O --edition 2021 producer_disasm.rs --crate-type bin -o pd
//! Then:    otool -tv pd | awk between the symbol labels.
#![feature(portable_simd)]
#![allow(internal_features)]
#![feature(core_intrinsics)]

use core::arch::aarch64::*;
use std::simd::prelude::*;

// ─────────────────────────────────────────────────────────────────────────
// (1) LEAN BASELINE — number_simd.rs:31-61 count_leading_digits_neon.
//     Double-vaddv movemask: AND-weights + two vaddv_u8 -> u16 -> tz.
//     Usable endpoint per LEAN contract: the leading-digit COUNT.
// ─────────────────────────────────────────────────────────────────────────
#[no_mangle]
#[inline(never)]
pub unsafe fn lean_baseline_double_vaddv(ptr: *const u8) -> u32 {
    let chunk = vld1q_u8(ptr);
    let adjusted = vsubq_u8(chunk, vdupq_n_u8(b'0'));
    let non_digit = vcgtq_u8(adjusted, vdupq_n_u8(9));

    let bit_weights: [u8; 8] = [1, 2, 4, 8, 16, 32, 64, 128];
    let weights = vld1_u8(bit_weights.as_ptr());
    let lo = vget_low_u8(non_digit);
    let hi = vget_high_u8(non_digit);
    let lo_masked = vand_u8(lo, weights);
    let hi_masked = vand_u8(hi, weights);
    let lo_sum = vaddv_u8(lo_masked) as u16;
    let hi_sum = vaddv_u8(hi_masked) as u16;
    let mask = lo_sum | (hi_sum << 8);
    if mask == 0 { 16 } else { mask.trailing_zeros() }
}

// ─────────────────────────────────────────────────────────────────────────
// (2) LEAN REBIRTH — cmeq/cgt -> vshrn_n_u16::<4> -> vget_lane_u64 -> tz>>2.
//     Nibble-stride mask; each lane -> 4 bits; count = tz>>2.
// ─────────────────────────────────────────────────────────────────────────
#[no_mangle]
#[inline(never)]
pub unsafe fn lean_rebirth_vshrn_nibble(ptr: *const u8) -> u32 {
    let chunk = vld1q_u8(ptr);
    let adjusted = vsubq_u8(chunk, vdupq_n_u8(b'0'));
    let non_digit = vcgtq_u8(adjusted, vdupq_n_u8(9)); // 0xFF per non-digit lane
    // nibble-stride narrow: 16xu8(0x00/0xFF) -> 8xu8 nibbles -> u64
    let narrowed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(non_digit));
    let mask = vget_lane_u64::<0>(vreinterpret_u64_u8(narrowed));
    if mask == 0 { 16 } else { mask.trailing_zeros() >> 2 }
}

// ─────────────────────────────────────────────────────────────────────────
// (3) DENSE BASELINE — quote_parity.rs classify_stripe_64.
//     4 x (portable_simd to_bitmask + shift + OR), for BOTH quote and bs.
//     Usable endpoint per DENSE contract: two 64-bit masks (quote, bs).
// ─────────────────────────────────────────────────────────────────────────
#[no_mangle]
#[inline(never)]
pub unsafe fn dense_baseline_to_bitmask(bytes: *const u8) -> u64 {
    let slice = core::slice::from_raw_parts(bytes, 64);
    let mut quote = 0u64;
    let mut bs = 0u64;
    for k in 0..4 {
        let chunk = u8x16::from_slice(slice.get_unchecked(k * 16..k * 16 + 16));
        let q = chunk.simd_eq(u8x16::splat(b'"')).to_bitmask() as u64;
        let b = chunk.simd_eq(u8x16::splat(b'\\')).to_bitmask() as u64;
        quote |= q << (k * 16);
        bs |= b << (k * 16);
    }
    // fold both usable masks into the return so neither is DCE'd
    quote ^ bs.rotate_left(1)
}

// ─────────────────────────────────────────────────────────────────────────
// (4) DENSE REBIRTH — simdjson dense-64 vpaddq bit-weight tree.
//     AND {0x01..0x80} + 3-stage vpaddq, per producer (quote, bs).
// ─────────────────────────────────────────────────────────────────────────
#[inline(always)]
unsafe fn movemask_bulk64(p0: uint8x16_t, p1: uint8x16_t, p2: uint8x16_t, p3: uint8x16_t) -> u64 {
    let bit_mask: [u8; 16] = [
        0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
        0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80,
    ];
    let bm = vld1q_u8(bit_mask.as_ptr());
    let t0 = vandq_u8(p0, bm);
    let t1 = vandq_u8(p1, bm);
    let t2 = vandq_u8(p2, bm);
    let t3 = vandq_u8(p3, bm);
    // stage 1
    let s0 = vpaddq_u8(t0, t1);
    let s1 = vpaddq_u8(t2, t3);
    // stage 2
    let s = vpaddq_u8(s0, s1);
    // stage 3
    let s = vpaddq_u8(s, s);
    vgetq_lane_u64::<0>(vreinterpretq_u64_u8(s))
}

#[no_mangle]
#[inline(never)]
pub unsafe fn dense_rebirth_vpaddq_tree(bytes: *const u8) -> u64 {
    let s0 = vld1q_u8(bytes);
    let s1 = vld1q_u8(bytes.add(16));
    let s2 = vld1q_u8(bytes.add(32));
    let s3 = vld1q_u8(bytes.add(48));
    let dq = vdupq_n_u8(b'"');
    let bsl = vdupq_n_u8(b'\\');
    let quote = movemask_bulk64(
        vceqq_u8(s0, dq), vceqq_u8(s1, dq), vceqq_u8(s2, dq), vceqq_u8(s3, dq),
    );
    let bs = movemask_bulk64(
        vceqq_u8(s0, bsl), vceqq_u8(s1, bsl), vceqq_u8(s2, bsl), vceqq_u8(s3, bsl),
    );
    quote ^ bs.rotate_left(1)
}

// ─────────────────────────────────────────────────────────────────────────
// (5) MOVEMASK REPACK — bbnf-simd/src/aarch64/movemask.rs movemask_u8x16.
//     vshrn narrow + scalar repack loop -> u16. Usable endpoint: u16 mask.
// ─────────────────────────────────────────────────────────────────────────
#[no_mangle]
#[inline(never)]
pub unsafe fn movemask_repack_bbnf(value_ptr: *const u8) -> u16 {
    let value = vld1q_u8(value_ptr);
    let pairs = vshrn_n_u16::<4>(vreinterpretq_u16_u8(value));
    let nibble_bits = vand_u8(pairs, vdup_n_u8(0x11));
    let lane_bits = vorr_u8(
        vand_u8(nibble_bits, vdup_n_u8(0x01)),
        vsri_n_u8::<3>(vdup_n_u8(0), nibble_bits),
    );
    let widened = vcombine_u8(lane_bits, vdup_n_u8(0));
    let interleaved = vzip1q_u8(widened, widened);
    let mut packed = [0u8; 16];
    vst1q_u8(packed.as_mut_ptr(), interleaved);
    let mut mask = 0u16;
    for pair in 0..8 {
        let bits = packed[pair * 2];
        mask |= u16::from(bits & 0x03) << (pair * 2);
    }
    mask
}

// ─────────────────────────────────────────────────────────────────────────
// (6) REFUTED DOUBLE-VADDV — byte_class_from_eq_set_64.rs:79-87 movemask_u8x16.
//     AND {1,2,4,..,128}x2 + two vaddv_u8 -> u16. Usable endpoint: u16 mask.
// ─────────────────────────────────────────────────────────────────────────
#[no_mangle]
#[inline(never)]
pub unsafe fn refuted_double_vaddv_bbnf(value_ptr: *const u8) -> u16 {
    let value = vld1q_u8(value_ptr);
    let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
    let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
    let lo = vaddv_u8(vget_low_u8(bits)) as u16;
    let hi = vaddv_u8(vget_high_u8(bits)) as u16;
    lo | (hi << 8)
}

fn main() {
    // Keep every symbol live: read 64 bytes from argv-independent static.
    static DATA: [u8; 64] = [b'1'; 64];
    let p = DATA.as_ptr();
    unsafe {
        let a = lean_baseline_double_vaddv(p);
        let b = lean_rebirth_vshrn_nibble(p);
        let c = dense_baseline_to_bitmask(p);
        let d = dense_rebirth_vpaddq_tree(p);
        let e = movemask_repack_bbnf(p);
        let f = refuted_double_vaddv_bbnf(p);
        std::process::exit((a ^ b ^ (c as u32) ^ (d as u32) ^ (e as u32) ^ (f as u32)) as i32 & 1);
    }
}
