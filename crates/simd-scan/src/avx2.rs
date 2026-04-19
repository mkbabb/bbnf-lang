//! AVX2 kernel.
//!
//! 32-byte stripes (one `__m256i` chunk per stripe). Two stripes
//! make a 64-byte block matching the NEON-side stripe; we keep the
//! 32-byte primitive and OR pairs into 64-bit masks for output
//! parity with the NEON shape.
//!
//! Per chunk:
//! - **Singletons** (any cardinality): per-singleton `_mm256_cmpeq_epi8`
//!   + OR-reduce. `<= 8` enables a `_mm256_shuffle_epi8` nibble-LUT
//!   path that pays one shuffle per chunk instead of N comparisons.
//! - **Movemask**: `_mm256_movemask_epi8` → `u32` chunk mask.
//!
//! Per pair-of-chunks (== 64 B stripe):
//! - **Digraphs**: per-pair shifted-compare via unaligned next-chunk
//!   load, AND with first-byte mask.
//! - **Quote parity**: `pclmulqdq` (CLMUL) prefix-XOR via
//!   `parity::prefix_xor_64`. The CLMUL gating in `parity.rs` selects
//!   the intrinsic when `pclmulqdq` is available; the 6-op fallback
//!   covers AVX2-without-CLMUL hosts (rare; included for correctness).
//!
//! Compaction: `tzcnt`-loop default; `_pext_u64`-flavoured path under
//! `bmi2` cfg (see `compaction::compact_stripe_pext`).

#![cfg(all(target_arch = "x86_64", target_feature = "avx2"))]

use crate::alphabet::{KernelShape, NibbleLut, StructuralAlphabet};
use crate::compaction;
use crate::parity;
use tape::stage1::StructuralIndex;

use core::arch::x86_64::*;

const STRIPE: usize = 64;
const CHUNK: usize = 32;

/// AVX2 entry point.
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

    // SAFETY: AVX2 module gated by `target_feature = "avx2"`.
    unsafe {
        // The nibble-LUT shuffle on AVX2 uses `_mm256_shuffle_epi8`,
        // which lane-broadcasts per 16-byte half. We supply the same
        // 16-byte LUT in both halves of the 32-byte register.
        let lo_v = broadcast_16(lut.lo);
        let hi_v = broadcast_16(lut.hi);
        let lo_mask = _mm256_set1_epi8(0x0F);

        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let m_lo = classify_chunk_nibble(input.as_ptr().add(i), lo_v, hi_v, lo_mask) as u64;
            let m_hi =
                classify_chunk_nibble(input.as_ptr().add(i + CHUNK), lo_v, hi_v, lo_mask) as u64;
            let mut struct_mask = m_lo | (m_hi << 32);

            if has_digraphs {
                struct_mask |= digraph_stripe(input.as_ptr().add(i), &alphabet.digraph_pairs);
            }
            if has_quotes {
                let qmask = quote_stripe(input.as_ptr().add(i), alphabet.quote_classes);
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

        // 32-byte chunk tail: handles 32..63 residual bytes in one
        // `__m256i` operation, matching the wider AVX2 lane width.
        // Mirrors NEON's 16-byte tail loop shape one level up.
        while i + CHUNK <= input.len() {
            let mut m = classify_chunk_nibble(input.as_ptr().add(i), lo_v, hi_v, lo_mask) as u64;
            if has_digraphs {
                m |= digraph_chunk(input.as_ptr().add(i), &alphabet.digraph_pairs) as u64;
            }
            if has_quotes {
                let qmask = quote_chunk(input.as_ptr().add(i), alphabet.quote_classes) as u64;
                let prefix = parity::prefix_xor_64(qmask, carry);
                m &= !prefix;
                carry = (prefix >> 31) & 1 == 1;
            }
            if m != 0 {
                compaction::compact_stripe_tzcnt(
                    m,
                    i as u32,
                    input,
                    &mut idx.positions,
                    &mut idx.kinds,
                );
            }
            i += CHUNK;
        }

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

    // SAFETY: see scan_nibble.
    unsafe {
        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let m_lo = classify_chunk_multi(input.as_ptr().add(i), alphabet.singletons) as u64;
            let m_hi =
                classify_chunk_multi(input.as_ptr().add(i + CHUNK), alphabet.singletons) as u64;
            let mut struct_mask = m_lo | (m_hi << 32);

            if has_digraphs {
                struct_mask |= digraph_stripe(input.as_ptr().add(i), &alphabet.digraph_pairs);
            }
            if has_quotes {
                let qmask = quote_stripe(input.as_ptr().add(i), alphabet.quote_classes);
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

        // 32-byte chunk tail: handles 32..63 residual bytes in one
        // `__m256i` operation, matching the wider AVX2 lane width.
        while i + CHUNK <= input.len() {
            let mut m = classify_chunk_multi(input.as_ptr().add(i), alphabet.singletons) as u64;
            if has_digraphs {
                m |= digraph_chunk(input.as_ptr().add(i), &alphabet.digraph_pairs) as u64;
            }
            if has_quotes {
                let qmask = quote_chunk(input.as_ptr().add(i), alphabet.quote_classes) as u64;
                let prefix = parity::prefix_xor_64(qmask, carry);
                m &= !prefix;
                carry = (prefix >> 31) & 1 == 1;
            }
            if m != 0 {
                compaction::compact_stripe_tzcnt(
                    m,
                    i as u32,
                    input,
                    &mut idx.positions,
                    &mut idx.kinds,
                );
            }
            i += CHUNK;
        }

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

    idx
}

#[inline(always)]
unsafe fn classify_chunk_nibble(
    ptr: *const u8,
    lo_v: __m256i,
    hi_v: __m256i,
    lo_mask: __m256i,
) -> u32 {
    // SAFETY: caller ensures ptr..ptr+32 in bounds.
    unsafe {
        let chunk = _mm256_loadu_si256(ptr as *const __m256i);
        let lo_n = _mm256_and_si256(chunk, lo_mask);
        let hi_n = _mm256_and_si256(_mm256_srli_epi16(chunk, 4), lo_mask);
        let lo_r = _mm256_shuffle_epi8(lo_v, lo_n);
        let hi_r = _mm256_shuffle_epi8(hi_v, hi_n);
        let matched = _mm256_and_si256(lo_r, hi_r);
        let nz = _mm256_cmpgt_epi8(matched, _mm256_setzero_si256());
        _mm256_movemask_epi8(nz) as u32
    }
}

#[inline(always)]
unsafe fn classify_chunk_multi(ptr: *const u8, singletons: &[u8]) -> u32 {
    // SAFETY: caller ensures ptr..ptr+32 in bounds.
    unsafe {
        let chunk = _mm256_loadu_si256(ptr as *const __m256i);
        let mut acc = _mm256_setzero_si256();
        for &target in singletons {
            let cmp = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(target as i8));
            acc = _mm256_or_si256(acc, cmp);
        }
        _mm256_movemask_epi8(acc) as u32
    }
}

#[inline(always)]
unsafe fn digraph_stripe(ptr: *const u8, pairs: &[(u8, u8)]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller ensures ptr..ptr+65 in bounds (stripe-bound).
    unsafe {
        for &(first, second) in pairs {
            for k in 0..2 {
                let chunk = _mm256_loadu_si256(ptr.add(k * CHUNK) as *const __m256i);
                let next = _mm256_loadu_si256(ptr.add(k * CHUNK + 1) as *const __m256i);
                let f_eq = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(first as i8));
                let s_eq = _mm256_cmpeq_epi8(next, _mm256_set1_epi8(second as i8));
                let both = _mm256_and_si256(f_eq, s_eq);
                let m = _mm256_movemask_epi8(both) as u32 as u64;
                mask |= m << (k * 32);
            }
        }
    }
    mask
}

#[inline(always)]
unsafe fn digraph_chunk(ptr: *const u8, pairs: &[(u8, u8)]) -> u32 {
    // SAFETY: caller ensures ptr..ptr+33 in bounds (chunk-bound).
    let mut mask = 0u32;
    unsafe {
        for &(first, second) in pairs {
            let chunk = _mm256_loadu_si256(ptr as *const __m256i);
            let next = _mm256_loadu_si256(ptr.add(1) as *const __m256i);
            let f_eq = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(first as i8));
            let s_eq = _mm256_cmpeq_epi8(next, _mm256_set1_epi8(second as i8));
            let both = _mm256_and_si256(f_eq, s_eq);
            mask |= _mm256_movemask_epi8(both) as u32;
        }
    }
    mask
}

#[inline(always)]
unsafe fn quote_stripe(ptr: *const u8, quotes: &[u8]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller ensures ptr..ptr+64 in bounds.
    unsafe {
        for k in 0..2 {
            let chunk = _mm256_loadu_si256(ptr.add(k * CHUNK) as *const __m256i);
            let mut acc = _mm256_setzero_si256();
            for &q in quotes {
                let cmp = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(q as i8));
                acc = _mm256_or_si256(acc, cmp);
            }
            // Strip escaped quotes via shifted-compare against `\\`.
            // For k=0 (first chunk of stripe), there is no in-memory
            // predecessor byte within the stripe — we lane-shift the
            // current chunk right by 1, filling lane 0 with 0 (no
            // backslash). Stripe-carry backslash parity is handled by
            // the caller's `carry` bit through `parity::prefix_xor_64`.
            // For k=1, the predecessor byte is at ptr+31 (last byte
            // of chunk 0).
            let prev = if k == 0 {
                shift_right_one_avx2(chunk)
            } else {
                _mm256_loadu_si256(ptr.add(k * CHUNK - 1) as *const __m256i)
            };
            let bs = _mm256_cmpeq_epi8(prev, _mm256_set1_epi8(b'\\' as i8));
            let unescaped = _mm256_andnot_si256(bs, acc);
            let m = _mm256_movemask_epi8(unescaped) as u32 as u64;
            mask |= m << (k * 32);
        }
    }
    mask
}

#[inline(always)]
unsafe fn quote_chunk(ptr: *const u8, quotes: &[u8]) -> u32 {
    // SAFETY: caller ensures ptr..ptr+32 in bounds.
    unsafe {
        let chunk = _mm256_loadu_si256(ptr as *const __m256i);
        let mut acc = _mm256_setzero_si256();
        for &q in quotes {
            let cmp = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(q as i8));
            acc = _mm256_or_si256(acc, cmp);
        }
        // Lane-shift; cross-stripe backslash carry is folded via
        // prefix_xor_64 at the call site.
        let prev = shift_right_one_avx2(chunk);
        let bs = _mm256_cmpeq_epi8(prev, _mm256_set1_epi8(b'\\' as i8));
        let unescaped = _mm256_andnot_si256(bs, acc);
        _mm256_movemask_epi8(unescaped) as u32
    }
}

#[inline(always)]
unsafe fn shift_right_one_avx2(v: __m256i) -> __m256i {
    // Shift the entire 32-byte register right by 1 byte, filling
    // lane 0 with 0. AVX2 has no full-register byte shift; we cross
    // the 16-byte halves with `_mm256_permute2x128_si256`.
    // SAFETY: pure SIMD ops on a register; no memory access.
    unsafe {
        let lo = _mm256_permute2x128_si256(v, _mm256_setzero_si256(), 0x03);
        _mm256_alignr_epi8(v, lo, 15)
    }
}

#[inline(always)]
unsafe fn broadcast_16(lut: [u8; 16]) -> __m256i {
    // SAFETY: aligned construction from a stack array.
    unsafe { _mm256_broadcastsi128_si256(_mm_loadu_si128(lut.as_ptr() as *const __m128i)) }
}

#[inline(always)]
fn build_quote_byte_lut(quotes: &[u8]) -> [bool; 256] {
    let mut lut = [false; 256];
    for &q in quotes {
        lut[q as usize] = true;
    }
    lut
}
