//! AVX-512 VBMI2 kernel.
//!
//! Opt-in path (cargo feature `avx512` + `RUSTFLAGS="-C target-feature=
//! +avx512vbmi2,+avx512f"`). The headline win:
//! `_mm512_mask_compressstoreu_epi8` compacts up to 64 byte indices in
//! one operation — the entire stripe-mask → position-list conversion
//! becomes a single store rather than a CTZ pop loop.
//!
//! # Why opt-in
//!
//! AVX-512 isn't broadly available in CI (Apple Silicon, GitHub
//! Actions Linux runners). Gating by `target_feature` plus the
//! `avx512` cargo feature lets the path build under x86_64 cross-
//! compilation with `RUSTFLAGS="-C target-feature=+avx512vbmi2"` while
//! never adding compile-time cost on hosts that can't run it.
//!
//! Per AW-III §W5: "the path compiles and tests under x86_64 cross-
//! compilation with `RUSTFLAGS="-C target-feature=+avx512vbmi2"`."

#![cfg(all(target_arch = "x86_64", target_feature = "avx512vbmi2"))]

use crate::StructuralIndex;
use crate::alphabet::{KernelShape, NibbleLut, StructuralAlphabet};
use crate::parity;

use core::arch::x86_64::*;

const STRIPE: usize = 64;

/// AVX-512 entry point.
pub fn scan(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let shape = KernelShape::select(alphabet);
    match shape {
        KernelShape::Empty => StructuralIndex::new(),
        _ => scan_compressstore(input, alphabet),
    }
}

fn scan_compressstore(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let mut byte_lut = [false; 256];
    for &b in alphabet.singletons {
        byte_lut[b as usize] = true;
    }
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut carry = false;

    // Pre-stage: position-base register `[0u8, 1u8, ..., 63u8]` for
    // the compressstore-style index emission.
    let positions_pattern: [u8; 64] = {
        let mut a = [0u8; 64];
        let mut k = 0;
        while k < 64 {
            a[k] = k as u8;
            k += 1;
        }
        a
    };

    // SAFETY: AVX-512 module gated by `target_feature = "avx512vbmi2"`.
    unsafe {
        let pos_v = _mm512_loadu_si512(positions_pattern.as_ptr() as *const __m512i);

        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let chunk = _mm512_loadu_si512(input.as_ptr().add(i) as *const __m512i);
            let mut mask: u64 = 0;
            for &target in alphabet.singletons {
                let cmp = _mm512_cmpeq_epi8_mask(chunk, _mm512_set1_epi8(target as i8));
                mask |= cmp;
            }

            if has_digraphs {
                for &(first, second) in alphabet.digraph_pairs {
                    let f_eq = _mm512_cmpeq_epi8_mask(chunk, _mm512_set1_epi8(first as i8));
                    let next = _mm512_loadu_si512(input.as_ptr().add(i + 1) as *const __m512i);
                    let s_eq = _mm512_cmpeq_epi8_mask(next, _mm512_set1_epi8(second as i8));
                    mask |= f_eq & s_eq;
                }
            }

            if has_quotes {
                let mut qmask: u64 = 0;
                for &q in alphabet.quote_classes {
                    let cmp = _mm512_cmpeq_epi8_mask(chunk, _mm512_set1_epi8(q as i8));
                    qmask |= cmp;
                }
                let prefix = parity::prefix_xor_64(qmask, carry);
                mask &= !prefix;
                carry = (prefix >> 63) & 1 == 1;
            }

            let count = mask.count_ones() as usize;
            if count > 0 {
                idx.positions.reserve(count);
                idx.kinds.reserve(count);

                // _mm512_mask_compressstoreu_epi8 needs i8 storage; we
                // extract per-bit indices via the position-base pattern.
                // Step 1: compress the byte-position pattern using mask.
                let compressed_pos = _mm512_maskz_compress_epi8(mask, pos_v);
                let mut pos_buf = [0u8; 64];
                _mm512_storeu_si512(pos_buf.as_mut_ptr() as *mut __m512i, compressed_pos);
                let compressed_kinds = _mm512_maskz_compress_epi8(mask, chunk);
                let mut kind_buf = [0u8; 64];
                _mm512_storeu_si512(kind_buf.as_mut_ptr() as *mut __m512i, compressed_kinds);

                for k in 0..count {
                    idx.positions.push(i as u32 + pos_buf[k] as u32);
                    idx.kinds.push(kind_buf[k]);
                }
            }
            i += STRIPE;
        }

        // Scalar tail.
        while i < input.len() {
            let b = *input.get_unchecked(i);
            let inside = if has_quotes { carry } else { false };
            if !inside && byte_lut[b as usize] {
                idx.push(i as u32, b);
            }
            if has_quotes && quote_lut[b as usize] {
                carry = !carry;
            }
            i += 1;
        }
    }

    // `NibbleLut` is currently unused in this path because we
    // unconditionally use `cmpeq_mask` (cheap on AVX-512) — the
    // import is preserved for future shape-dispatch differentiation
    // when the alphabet's wide-LUT case becomes worth specialising.
    let _ = NibbleLut::from_singletons(alphabet.singletons);

    idx
}

#[inline(always)]
fn build_quote_byte_lut(quotes: &[u8]) -> [bool; 256] {
    let mut lut = [false; 256];
    for &q in quotes {
        lut[q as usize] = true;
    }
    lut
}
