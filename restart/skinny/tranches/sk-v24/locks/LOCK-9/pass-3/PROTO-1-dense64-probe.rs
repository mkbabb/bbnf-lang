//! LOCK-9 pass-3 PROTO-1 (Short Brief 1) — the canonical DENSE-64 producer vs the refuted double-vaddv.
//!
//! Reconciliation 3: the two lawful movemask producers are ABSENT from bbnf-simd
//! (`vpaddq` in zero files) and must be authored ONCE into any home regardless of ownership.
//! This probe authors the canonical DENSE-64 `vpaddq` bit-weight tree (simdjson neon_movemask_bulk),
//! places it beside the refuted local double-`vaddv` form (verbatim from bbnf-simd
//! structural_classify_64.rs:147-155 + pack4) and the LEAN `vshrn` form, and:
//!   (a) asserts BIT-IDENTICAL output DENSE-64 == scalar reference over black_box xorshift (differential oracle),
//!   (b) INJECT_BUG (flip one lane) MUST break parity,
//!   (c) three #[no_mangle] #[inline(never)] frames so otool can census reduction ops per producer.
//! Pure core::arch::aarch64, no nightly, no deps. aarch64 only.

#![allow(clippy::missing_safety_doc)]

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;
use std::hint::black_box;

// ---- Producer 1: the REFUTED double-vaddv (verbatim idiom from bbnf-simd) ----
// per-16-lane movemask_u8x16 via vand(bit-pattern) + TWO vaddv_u8 reductions,
// then shift-combine four 16-bit masks into a u64 (structural_classify_64 pack4).
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn movemask_u8x16_vaddv(value: uint8x16_t) -> u16 {
    let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
    let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
    let lo = vaddv_u8(vget_low_u8(bits)) as u16;
    let hi = vaddv_u8(vget_high_u8(bits)) as u16;
    lo | (hi << 8)
}

#[cfg(target_arch = "aarch64")]
#[no_mangle]
#[inline(never)]
pub unsafe fn pack64_double_vaddv(v: &[uint8x16_t; 4]) -> u64 {
    let lo0 = movemask_u8x16_vaddv(v[0]) as u64;
    let lo1 = movemask_u8x16_vaddv(v[1]) as u64;
    let lo2 = movemask_u8x16_vaddv(v[2]) as u64;
    let lo3 = movemask_u8x16_vaddv(v[3]) as u64;
    lo0 | (lo1 << 16) | (lo2 << 32) | (lo3 << 48)
}

// ---- Producer 2: the CANONICAL DENSE-64 vpaddq tree (simdjson neon_movemask_bulk) ----
#[cfg(target_arch = "aarch64")]
#[no_mangle]
#[inline(never)]
pub unsafe fn pack64_dense_vpaddq(v: &[uint8x16_t; 4]) -> u64 {
    let bit_mask: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
    let bm = vld1q_u8(bit_mask.as_ptr());
    let t0 = vandq_u8(v[0], bm);
    let t1 = vandq_u8(v[1], bm);
    let t2 = vandq_u8(v[2], bm);
    let t3 = vandq_u8(v[3], bm);
    // disjoint-bit pairwise-add tree; LLVM lowers to uzp/orr, zero horizontal reduction.
    let s0 = vpaddq_u8(t0, t1);
    let s1 = vpaddq_u8(t2, t3);
    let s = vpaddq_u8(s0, s1);
    let s = vpaddq_u8(s, s);
    vgetq_lane_u64(vreinterpretq_u64_u8(s), 0)
}

// ---- Producer 2b: single-stripe DENSE u16 via vpaddq fold (byte-order-preserving) ----
// Drop-in replacement for the four sites' local double-`vaddv` movemask_u8x16.
// 3 vpaddq collapses 16->8->4->2; byte0=lanes0-7 mask, byte1=lanes8-15 mask; NO horizontal reduction.
#[cfg(target_arch = "aarch64")]
#[no_mangle]
#[inline(never)]
pub unsafe fn movemask_u8x16_vpaddq(value: uint8x16_t) -> u16 {
    let pattern: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
    let bits = vandq_u8(value, vld1q_u8(pattern.as_ptr()));
    let p = vpaddq_u8(bits, bits);
    let p = vpaddq_u8(p, p);
    let p = vpaddq_u8(p, p);
    let lo = vgetq_lane_u8::<0>(p) as u16;
    let hi = vgetq_lane_u8::<1>(p) as u16;
    lo | (hi << 8)
}

// ---- Producer 3: the LEAN vshrn nibble first-set (single vector) ----
#[cfg(target_arch = "aarch64")]
#[no_mangle]
#[inline(never)]
pub unsafe fn firstset_lean_vshrn(v: uint8x16_t) -> u32 {
    let narrow = vshrn_n_u16::<4>(vreinterpretq_u16_u8(v));
    let m = vget_lane_u64(vreinterpret_u64_u8(narrow), 0);
    m.trailing_zeros() >> 2
}

// ---- scalar reference (parity anchor) ----
// lanes are 0x00 or 0xFF (compare results); bit i set iff lane i != 0.
fn pack64_scalar(bytes: &[u8; 64]) -> u64 {
    let mut m = 0u64;
    for (i, &b) in bytes.iter().enumerate() {
        if b != 0 {
            m |= 1u64 << i;
        }
    }
    m
}

#[cfg(target_arch = "aarch64")]
unsafe fn load4(bytes: &[u8; 64]) -> [uint8x16_t; 4] {
    [
        vld1q_u8(bytes.as_ptr()),
        vld1q_u8(bytes.as_ptr().add(16)),
        vld1q_u8(bytes.as_ptr().add(32)),
        vld1q_u8(bytes.as_ptr().add(48)),
    ]
}

fn main() {
    #[cfg(not(target_arch = "aarch64"))]
    {
        eprintln!("NON-AARCH64 — SPEC_ONLY");
        std::process::exit(2);
    }
    #[cfg(target_arch = "aarch64")]
    unsafe {
        // xorshift64 PRNG; each of 64 lanes independently 0x00 or 0xFF.
        let mut state: u64 = 0x9E3779B97F4A7C15;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            state
        };

        const N: usize = 200_000;
        let mut mism_vaddv = 0u64;
        let mut mism_dense = 0u64;
        let mut mism_ss = 0u64;
        let mut acc = 0u64; // keep results live

        for _ in 0..N {
            let mut bytes = [0u8; 64];
            let r = next();
            for (i, b) in bytes.iter_mut().enumerate() {
                *b = if (r >> (i & 63)) & 1 == 1 { 0xFF } else { 0x00 };
            }
            // decorrelate lanes 32..64 with a second draw
            let r2 = next();
            for i in 32..64 {
                bytes[i] = if (r2 >> (i & 63)) & 1 == 1 { 0xFF } else { 0x00 };
            }

            let bytes = black_box(bytes);
            let want = pack64_scalar(&bytes);
            let v = load4(&bytes);

            let got_vaddv = pack64_double_vaddv(&v);
            let got_dense = pack64_dense_vpaddq(&v);
            // single-stripe vpaddq drop-in must match the per-stripe scalar slices.
            let ss = (movemask_u8x16_vpaddq(v[0]) as u64)
                | ((movemask_u8x16_vpaddq(v[1]) as u64) << 16)
                | ((movemask_u8x16_vpaddq(v[2]) as u64) << 32)
                | ((movemask_u8x16_vpaddq(v[3]) as u64) << 48);

            if got_vaddv != want {
                mism_vaddv += 1;
            }
            if got_dense != want {
                mism_dense += 1;
            }
            if ss != want {
                mism_ss += 1;
            }
            acc ^= got_dense ^ got_vaddv ^ ss;
        }

        // INJECT_BUG: flip one lane (lane 5) to 0xFF in an all-zero block; the DENSE producer
        // must now DIFFER from the (unflipped) scalar expectation of zero — proving the oracle bites.
        let mut buggy = [0u8; 64];
        let clean_want = pack64_scalar(&buggy); // 0
        buggy[5] = 0xFF;
        let v = load4(&black_box(buggy));
        let buggy_dense = pack64_dense_vpaddq(&v);
        let inject_bites = buggy_dense != clean_want;

        // LEAN first-set spot check: lane 5 set -> first-set position 5.
        let mut lean_bytes = [0u8; 16];
        lean_bytes[5] = 0xFF;
        let lv = vld1q_u8(black_box(lean_bytes).as_ptr());
        let lean_pos = firstset_lean_vshrn(lv);

        println!("N={N}");
        println!("dense_vpaddq mismatches vs scalar = {mism_dense}");
        println!("double_vaddv mismatches vs scalar = {mism_vaddv}");
        println!("single_stripe_vpaddq mismatches vs scalar = {mism_ss}");
        println!("INJECT_BUG (flip lane5) bites dense = {inject_bites}");
        println!("LEAN vshrn first-set of lane5 = {lean_pos} (want 5)");
        println!("acc(guard) = {acc}");

        let pass = mism_dense == 0 && mism_vaddv == 0 && mism_ss == 0 && inject_bites && lean_pos == 5;
        println!("PARITY_PASS = {pass}");
        std::process::exit(if pass { 0 } else { 1 });
    }
}
