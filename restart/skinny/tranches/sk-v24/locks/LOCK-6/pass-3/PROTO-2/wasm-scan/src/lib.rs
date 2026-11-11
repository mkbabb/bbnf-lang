//! v128 (wasm SIMD128) structural-byte scanner emitting structural
//! offsets. Structural set = { } [ ] : ,  (6 targets, unique-bit
//! nibble-LUT → exact classify, no false positives).
//!
//! Build: RUSTFLAGS="-C target-feature=+simd128" cargo build --release
//!        --target wasm32-unknown-unknown
//! then:  wasm-opt -O3 --enable-simd
//!
//! Raw C ABI exports (no wasm-bindgen) so the UTF-16->UTF-8 encode +
//! copy-into-linear-memory marshalling tax is driven explicitly from
//! JS and counted INSIDE the wasm lane.

#![no_std]

use core::arch::wasm32::*;

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

// canada.json is 2.25 MB; cap at 3 MB input + 64 B trailing pad.
const CAP: usize = 3_000_000;
const PAD: usize = 64;

// bss (zero-init) — does not bloat the .wasm binary.
static mut INPUT: [u8; CAP + PAD] = [0; CAP + PAD];
static mut OUTPUT: [i32; CAP] = [0; CAP];

#[no_mangle]
pub extern "C" fn input_ptr() -> *mut u8 {
    unsafe { INPUT.as_mut_ptr() }
}

#[no_mangle]
pub extern "C" fn output_ptr() -> *mut i32 {
    unsafe { OUTPUT.as_mut_ptr() }
}

#[no_mangle]
pub extern "C" fn input_cap() -> usize {
    CAP
}

// Nibble LUTs for { } [ ] : ,  — each target gets a unique bit, so
// lo_r & hi_r != 0  iff  the byte is a member (exact, no aliasing).
//   ',' 0x2C bit0 | ':' 0x3A bit1 | '[' 0x5B bit2
//   ']' 0x5D bit3 | '{' 0x7B bit4 | '}' 0x7D bit5
#[rustfmt::skip]
const LO: [u8; 16] = [
    0,0,0,0, 0,0,0,0, 0,0, 0x02, 0x14, 0x01, 0x28, 0,0,
];
#[rustfmt::skip]
const HI: [u8; 16] = [
    0,0, 0x01, 0x02, 0, 0x0C, 0,0x30, 0,0,0,0, 0,0,0,0,
];

#[inline(always)]
unsafe fn classify16(ptr: *const u8, lo: v128, hi: v128) -> u32 {
    let v = v128_load(ptr as *const v128);
    let lo_n = v128_and(v, u8x16_splat(0x0F));
    let hi_n = u8x16_shr(v, 4);
    let lo_r = u8x16_swizzle(lo, lo_n);
    let hi_r = u8x16_swizzle(hi, hi_n);
    let matched = v128_and(lo_r, hi_r);
    let nz = u8x16_ne(matched, u8x16_splat(0));
    u8x16_bitmask(nz) as u32
}

/// Scan INPUT[0..len], emit each structural byte offset into OUTPUT.
/// Returns the count of structural bytes found.
#[no_mangle]
pub extern "C" fn scan(len: usize) -> usize {
    unsafe {
        let base = INPUT.as_ptr();
        let out = OUTPUT.as_mut_ptr();
        let lo = v128_load(LO.as_ptr() as *const v128);
        let hi = v128_load(HI.as_ptr() as *const v128);
        let mut n: usize = 0;
        let mut i: usize = 0;

        // Full 64-byte stripes: 4x classify16 ORed into a u64 mask,
        // CTZ-emit each hit.
        while i + 64 <= len {
            let p = base.add(i);
            let m0 = classify16(p, lo, hi) as u64;
            let m1 = classify16(p.add(16), lo, hi) as u64;
            let m2 = classify16(p.add(32), lo, hi) as u64;
            let m3 = classify16(p.add(48), lo, hi) as u64;
            let mut mask = m0 | (m1 << 16) | (m2 << 32) | (m3 << 48);
            while mask != 0 {
                let pos = i + mask.trailing_zeros() as usize;
                *out.add(n) = pos as i32;
                n += 1;
                mask &= mask - 1;
            }
            i += 64;
        }

        // Tail (< 64 bytes): the 64-byte zero pad lets us classify the
        // final stripe wholesale, then clamp bits >= len.
        if i < len {
            let p = base.add(i);
            let m0 = classify16(p, lo, hi) as u64;
            let m1 = classify16(p.add(16), lo, hi) as u64;
            let m2 = classify16(p.add(32), lo, hi) as u64;
            let m3 = classify16(p.add(48), lo, hi) as u64;
            let raw = m0 | (m1 << 16) | (m2 << 32) | (m3 << 48);
            let valid = len - i;
            let keep = if valid >= 64 { u64::MAX } else { (1u64 << valid) - 1 };
            let mut mask = raw & keep;
            while mask != 0 {
                let pos = i + mask.trailing_zeros() as usize;
                *out.add(n) = pos as i32;
                n += 1;
                mask &= mask - 1;
            }
        }
        n
    }
}
