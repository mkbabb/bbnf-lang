//! WASM SIMD128 kernel.
//!
//! 16-byte stripes (one `v128` per stripe). Same NEON-shape lowering:
//! `i8x16.swizzle` for the nibble-LUT collapse, `i8x16.eq` for
//! singleton/quote/digraph compare, `i8x16.bitmask` for mask
//! extraction. Quote parity uses the arch-neutral
//! `parity::prefix_xor_64` (shift-XOR fallback on WASM since CLMUL
//! isn't available without dedicated proposals).
//!
//! Targeted at WASM execution environments (browser, wasi). Verified
//! functional via cross-compilation; SIMD128 is the WebAssembly
//! baseline at proposal stage 4.

#![cfg(target_arch = "wasm32")]

use crate::alphabet::{KernelShape, NibbleLut, StructuralAlphabet};
use crate::compaction;
use crate::parity;
use bbnf_tape::stage1::StructuralIndex;

use core::arch::wasm32::*;

const STRIPE: usize = 64;
const CHUNK: usize = 16;

/// WASM entry point.
pub fn scan(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let shape = KernelShape::select(alphabet);
    match shape {
        KernelShape::Empty => StructuralIndex::new(),
        KernelShape::NibbleLut => scan_nibble(input, alphabet),
        KernelShape::WideLut | KernelShape::MultiCmp => scan_multi(input, alphabet),
    }
}

fn scan_nibble(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let lut = NibbleLut::from_singletons(alphabet.singletons);
    let byte_lut = lut.expand();
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut carry = false;

    let lo_v = u8x16_from(lut.lo);
    let hi_v = u8x16_from(lut.hi);
    let lo_mask = u8x16_splat(0x0F);

    let mut i = 0usize;
    while i + STRIPE <= input.len() {
        let mut struct_mask = 0u64;
        for k in 0..4 {
            let m =
                classify_chunk_nibble(unsafe { input.as_ptr().add(i + k * CHUNK) }, lo_v, hi_v, lo_mask)
                    as u64;
            struct_mask |= m << (k * CHUNK);
        }

        if has_digraphs {
            struct_mask |= digraph_stripe(unsafe { input.as_ptr().add(i) }, &alphabet.digraph_pairs);
        }
        if has_quotes {
            let qmask = quote_stripe(unsafe { input.as_ptr().add(i) }, alphabet.quote_classes);
            let prefix = parity::prefix_xor_64(qmask, carry);
            struct_mask &= !prefix;
            carry = (prefix >> 63) & 1 == 1;
        }

        if struct_mask != 0 {
            compaction::compact_stripe_tzcnt(
                struct_mask,
                i as u32,
                input,
                &mut idx.positions,
                &mut idx.kinds,
            );
        }
        i += STRIPE;
    }

    while i < input.len() {
        let b = unsafe { *input.get_unchecked(i) };
        let inside = if has_quotes { carry } else { false };
        if !inside && byte_lut[b as usize] {
            idx.push(i as u32, b);
        }
        if has_quotes && quote_lut[b as usize] {
            carry = !carry;
        }
        i += 1;
    }

    idx
}

fn scan_multi(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let mut byte_lut = [false; 256];
    for &b in alphabet.singletons {
        byte_lut[b as usize] = true;
    }
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut carry = false;

    let mut i = 0usize;
    while i + STRIPE <= input.len() {
        let mut struct_mask = 0u64;
        for k in 0..4 {
            let m = classify_chunk_multi(
                unsafe { input.as_ptr().add(i + k * CHUNK) },
                alphabet.singletons,
            ) as u64;
            struct_mask |= m << (k * CHUNK);
        }

        if has_digraphs {
            struct_mask |= digraph_stripe(unsafe { input.as_ptr().add(i) }, &alphabet.digraph_pairs);
        }
        if has_quotes {
            let qmask = quote_stripe(unsafe { input.as_ptr().add(i) }, alphabet.quote_classes);
            let prefix = parity::prefix_xor_64(qmask, carry);
            struct_mask &= !prefix;
            carry = (prefix >> 63) & 1 == 1;
        }

        if struct_mask != 0 {
            compaction::compact_stripe_tzcnt(
                struct_mask,
                i as u32,
                input,
                &mut idx.positions,
                &mut idx.kinds,
            );
        }
        i += STRIPE;
    }

    while i < input.len() {
        let b = unsafe { *input.get_unchecked(i) };
        let inside = if has_quotes { carry } else { false };
        if !inside && byte_lut[b as usize] {
            idx.push(i as u32, b);
        }
        if has_quotes && quote_lut[b as usize] {
            carry = !carry;
        }
        i += 1;
    }

    idx
}

#[inline(always)]
fn classify_chunk_nibble(ptr: *const u8, lo_v: v128, hi_v: v128, lo_mask: v128) -> u16 {
    // SAFETY: caller ensures ptr..ptr+16 in bounds.
    unsafe {
        let chunk = v128_load(ptr as *const v128);
        let lo_n = v128_and(chunk, lo_mask);
        let hi_n = u8x16_shr(chunk, 4);
        let lo_r = u8x16_swizzle(lo_v, lo_n);
        let hi_r = u8x16_swizzle(hi_v, hi_n);
        let matched = v128_and(lo_r, hi_r);
        let nz = u8x16_ne(matched, u8x16_splat(0));
        u8x16_bitmask(nz)
    }
}

#[inline(always)]
fn classify_chunk_multi(ptr: *const u8, singletons: &[u8]) -> u16 {
    // SAFETY: caller ensures ptr..ptr+16 in bounds.
    unsafe {
        let chunk = v128_load(ptr as *const v128);
        let mut acc = u8x16_splat(0);
        for &target in singletons {
            let cmp = u8x16_eq(chunk, u8x16_splat(target));
            acc = v128_or(acc, cmp);
        }
        u8x16_bitmask(acc)
    }
}

#[inline(always)]
fn digraph_stripe(ptr: *const u8, pairs: &[(u8, u8)]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller ensures ptr..ptr+65 in bounds (stripe-bound).
    unsafe {
        for &(first, second) in pairs {
            for k in 0..4 {
                let chunk = v128_load(ptr.add(k * CHUNK) as *const v128);
                let next = v128_load(ptr.add(k * CHUNK + 1) as *const v128);
                let f_eq = u8x16_eq(chunk, u8x16_splat(first));
                let s_eq = u8x16_eq(next, u8x16_splat(second));
                let both = v128_and(f_eq, s_eq);
                let m = u8x16_bitmask(both) as u64;
                mask |= m << (k * CHUNK);
            }
        }
    }
    mask
}

#[inline(always)]
fn quote_stripe(ptr: *const u8, quotes: &[u8]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller ensures ptr..ptr+64 in bounds.
    unsafe {
        for k in 0..4 {
            let chunk = v128_load(ptr.add(k * CHUNK) as *const v128);
            let mut acc = u8x16_splat(0);
            for &q in quotes {
                let cmp = u8x16_eq(chunk, u8x16_splat(q));
                acc = v128_or(acc, cmp);
            }
            // Strip escaped quotes.
            let prev = if k == 0 {
                // Lane 0 has no in-stripe predecessor; carry-in handled
                // via prefix-XOR.
                shift_right_one_v128(chunk)
            } else {
                v128_load(ptr.add(k * CHUNK - 1) as *const v128)
            };
            let bs = u8x16_eq(prev, u8x16_splat(b'\\'));
            let unescaped = v128_andnot(acc, bs);
            let m = u8x16_bitmask(unescaped) as u64;
            mask |= m << (k * CHUNK);
        }
    }
    mask
}

#[inline(always)]
fn shift_right_one_v128(v: v128) -> v128 {
    // 16-byte shift right by 1: [_, c[0], c[1], ..., c[14]].
    // WASM SIMD has `i8x16_shuffle` for arbitrary lane permutations.
    i8x16_shuffle::<16, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14>(u8x16_splat(0), v)
}

#[inline(always)]
fn u8x16_from(arr: [u8; 16]) -> v128 {
    u8x16(
        arr[0], arr[1], arr[2], arr[3], arr[4], arr[5], arr[6], arr[7], arr[8], arr[9], arr[10],
        arr[11], arr[12], arr[13], arr[14], arr[15],
    )
}

#[inline(always)]
fn build_quote_byte_lut(quotes: &[u8]) -> [bool; 256] {
    let mut lut = [false; 256];
    for &q in quotes {
        lut[q as usize] = true;
    }
    lut
}
