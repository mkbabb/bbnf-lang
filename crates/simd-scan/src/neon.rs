//! NEON kernel.
//!
//! Targeted at Apple M-class P-cores (the canonical `cargo bench`
//! host for bbnf-lang on macOS). Verified functional on E-cores and
//! generic aarch64 — no Apple-specific intrinsics, NEON baseline only.
//!
//! # Stripe model
//!
//! 64-byte stripes; four `u8x16` chunks per stripe.
//!
//! Per chunk:
//! - **Nibble-LUT collapse** (`<= 8` singletons): low/high nibble
//!   `vqtbl1q_u8` against the precomputed `(lo, hi)` LUTs, AND, then
//!   `vshrn_n_u16 #4` movemask to a 64-bit per-chunk lane (16 bits
//!   used per chunk; ORed across 4 chunks per stripe).
//! - **Wide-LUT** (`9..=16`): two paired-table lookups
//!   (`vqtbl2q_u8` over `[lo_lo, hi_lo]` + `[lo_hi, hi_hi]`),
//!   ORed before the bitmask.
//! - **MultiCmp** (`> 16`): per-singleton `vceqq_u8` + OR.
//!
//! Per stripe:
//! - **Digraph compare** (`|digraph_pairs| > 0`): `vextq_u8`
//!   shift-by-one + `vceqq_u8` against the per-pair second-byte
//!   splat; AND with shifted first-byte mask; OR into the stripe
//!   mask.
//! - **Quote parity** (`|quote_classes| > 0`): per-quote `vceqq_u8`
//!   reduced to a 64-bit unescaped-quote mask; prefix-XOR via the
//!   shared `parity::prefix_xor_64` (CLMUL/PMULL or 6-op shift-XOR);
//!   AND-NOT against the structural mask before compaction.
//!
//! After classification, the stripe mask is dropped through
//! `compaction::compact_stripe_tzcnt` to produce dense `(positions,
//! kinds)` columns.

#![cfg(target_arch = "aarch64")]

use crate::StructuralIndex;
use crate::alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};
use crate::compaction;
use crate::parity;

use core::arch::aarch64::*;

const STRIPE: usize = 64;

/// NEON entry point. Mirrors `scan_structural` for aarch64.
pub fn scan(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let shape = KernelShape::select(alphabet);
    match shape {
        KernelShape::Empty => StructuralIndex::new(),
        KernelShape::NibbleLut => scan_nibble(input, alphabet),
        KernelShape::WideLut => scan_wide(input, alphabet),
        KernelShape::MultiCmp => scan_multi(input, alphabet),
    }
}

// ───────────────────────── nibble-LUT path ────────────────────────────

fn scan_nibble(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let lut = NibbleLut::from_singletons(alphabet.singletons);
    let byte_lut = lut.expand();
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    // Estimate dense capacity: ~1 structural per 8 bytes. Avoids ~5
    // grow-and-copies on a 1 MB input.
    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut quote_carries = [false; 4];
    let mut bs_carry = false;

    // SAFETY: NEON intrinsics require aarch64 + neon, both of which
    // are gated by the `#[cfg(target_arch = "aarch64")]` on the
    // module + `target_feature = "neon"` baseline of aarch64-*-* targets.
    unsafe {
        let lo_v = vld1q_u8(lut.lo.as_ptr());
        let hi_v = vld1q_u8(lut.hi.as_ptr());
        let lo_mask = vdupq_n_u8(0x0F);

        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let mut struct_mask =
                classify_stripe_nibble(input.as_ptr().add(i), lo_v, hi_v, lo_mask);

            if has_digraphs {
                let dmask = digraph_stripe(input.as_ptr().add(i), &alphabet.digraph_pairs);
                struct_mask |= dmask;
            }

            if has_quotes {
                let body = quote_stripe_masked(
                    input.as_ptr().add(i),
                    alphabet.quote_classes,
                    &mut bs_carry,
                    &mut quote_carries[..alphabet.quote_classes.len()],
                );
                struct_mask &= !body;
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

        // 16-byte tail loop.
        while i + 16 <= input.len() {
            let mut m = classify_chunk_nibble(input.as_ptr().add(i), lo_v, hi_v, lo_mask) as u64;
            if has_digraphs {
                m |= digraph_chunk(input.as_ptr().add(i), &alphabet.digraph_pairs) as u64;
            }
            if has_quotes {
                let body = quote_chunk_masked(
                    input.as_ptr().add(i),
                    alphabet.quote_classes,
                    &mut bs_carry,
                    &mut quote_carries[..alphabet.quote_classes.len()],
                );
                m &= !body;
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
            i += 16;
        }

        // Scalar epilogue for the last < 16 bytes. Per-class state
        // machine matches scalar.rs::compute_inside_string_mask, plus
        // digraph first-byte emission.
        let active_class_in: Option<usize> = quote_carries[..alphabet.quote_classes.len()]
            .iter()
            .position(|&b| b);
        let mut active = active_class_in;
        // Carry the backslash parity from the previous stripe: if
        // `bs_carry` is true at entry, the byte we're about to look at
        // is escaped by a hanging backslash run.
        let mut prev_was_bs = bs_carry;
        while i < input.len() {
            let b = *input.get_unchecked(i);
            let was_inside = active.is_some();
            // Singleton OR digraph-first-byte → structural.
            let is_singleton = byte_lut[b as usize];
            let is_digraph_first = if i + 1 < input.len() {
                let next = *input.get_unchecked(i + 1);
                alphabet
                    .digraph_pairs
                    .iter()
                    .any(|&(f, s)| f == b && s == next)
            } else {
                false
            };
            if !was_inside && (is_singleton || is_digraph_first) {
                idx.push(i as u32, b);
            }
            // Apply quote toggle.
            if has_quotes && quote_lut[b as usize] && !prev_was_bs {
                if let Some(act) = active {
                    if alphabet.quote_classes[act] == b {
                        active = None;
                    }
                } else if let Some(k) = alphabet.quote_classes.iter().position(|&q| q == b) {
                    active = Some(k);
                }
            }
            prev_was_bs = b == b'\\' && !prev_was_bs;
            i += 1;
        }
        for (k, c) in quote_carries[..alphabet.quote_classes.len()]
            .iter_mut()
            .enumerate()
        {
            *c = active == Some(k);
        }
    }

    idx
}

#[inline(always)]
unsafe fn classify_stripe_nibble(
    ptr: *const u8,
    lo_v: uint8x16_t,
    hi_v: uint8x16_t,
    lo_mask: uint8x16_t,
) -> u64 {
    let mut mask = 0u64;
    for k in 0..4 {
        let m = unsafe { classify_chunk_nibble(ptr.add(k * 16), lo_v, hi_v, lo_mask) } as u64;
        mask |= m << (k * 16);
    }
    mask
}

#[inline(always)]
unsafe fn classify_chunk_nibble(
    ptr: *const u8,
    lo_v: uint8x16_t,
    hi_v: uint8x16_t,
    lo_mask: uint8x16_t,
) -> u16 {
    // SAFETY: caller guarantees `ptr..ptr+16` is in bounds.
    unsafe {
        let chunk = vld1q_u8(ptr);
        let lo_n = vandq_u8(chunk, lo_mask);
        let hi_n = vshrq_n_u8(chunk, 4);
        let lo_r = vqtbl1q_u8(lo_v, lo_n);
        let hi_r = vqtbl1q_u8(hi_v, hi_n);
        // matched lane is non-zero iff the byte is in the alphabet; the
        // exact bit pattern encodes which singleton matched. We don't
        // need that here — the movemask only cares about `set/clear`,
        // so collapse non-zero → 0xFF via cmp-against-zero.
        let matched = vandq_u8(lo_r, hi_r);
        let any = vtstq_u8(matched, matched);
        movemask_u8x16(any)
    }
}

// ─────────────────────────── wide-LUT path ────────────────────────────

fn scan_wide(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let lut = WideLut::from_singletons(alphabet.singletons);
    let byte_lut = lut.expand();
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut quote_carries = [false; 4];
    let mut bs_carry = false;

    // SAFETY: see scan_nibble for cfg justification.
    unsafe {
        let lo_lo_v = vld1q_u8(lut.lo_lo.as_ptr());
        let hi_lo_v = vld1q_u8(lut.hi_lo.as_ptr());
        let lo_hi_v = vld1q_u8(lut.lo_hi.as_ptr());
        let hi_hi_v = vld1q_u8(lut.hi_hi.as_ptr());
        let lo_mask = vdupq_n_u8(0x0F);

        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let mut struct_mask = classify_stripe_wide(
                input.as_ptr().add(i),
                lo_lo_v,
                hi_lo_v,
                lo_hi_v,
                hi_hi_v,
                lo_mask,
            );

            if has_digraphs {
                struct_mask |= digraph_stripe(input.as_ptr().add(i), &alphabet.digraph_pairs);
            }

            if has_quotes {
                let body = quote_stripe_masked(
                    input.as_ptr().add(i),
                    alphabet.quote_classes,
                    &mut bs_carry,
                    &mut quote_carries[..alphabet.quote_classes.len()],
                );
                struct_mask &= !body;
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
            let b = *input.get_unchecked(i);
            let inside = if has_quotes {
                quote_carries[..alphabet.quote_classes.len()]
                    .iter()
                    .any(|&b| b)
            } else {
                false
            };
            if !inside && byte_lut[b as usize] {
                idx.push(i as u32, b);
            }
            if has_quotes && quote_lut[b as usize] {
                for c in quote_carries[..alphabet.quote_classes.len()].iter_mut() {
                    if quote_lut[b as usize] {
                        *c = !*c;
                        break;
                    }
                }
            }
            i += 1;
        }
    }

    idx
}

#[inline(always)]
unsafe fn classify_stripe_wide(
    ptr: *const u8,
    lo_lo: uint8x16_t,
    hi_lo: uint8x16_t,
    lo_hi: uint8x16_t,
    hi_hi: uint8x16_t,
    lo_mask: uint8x16_t,
) -> u64 {
    let mut mask = 0u64;
    for k in 0..4 {
        let m = unsafe { classify_chunk_wide(ptr.add(k * 16), lo_lo, hi_lo, lo_hi, hi_hi, lo_mask) }
            as u64;
        mask |= m << (k * 16);
    }
    mask
}

#[inline(always)]
unsafe fn classify_chunk_wide(
    ptr: *const u8,
    lo_lo: uint8x16_t,
    hi_lo: uint8x16_t,
    lo_hi: uint8x16_t,
    hi_hi: uint8x16_t,
    lo_mask: uint8x16_t,
) -> u16 {
    // SAFETY: caller guarantees ptr..ptr+16 in bounds.
    unsafe {
        let chunk = vld1q_u8(ptr);
        let lo_n = vandq_u8(chunk, lo_mask);
        let hi_n = vshrq_n_u8(chunk, 4);
        let r_lo_lo = vqtbl1q_u8(lo_lo, lo_n);
        let r_hi_lo = vqtbl1q_u8(hi_lo, hi_n);
        let r_lo_hi = vqtbl1q_u8(lo_hi, lo_n);
        let r_hi_hi = vqtbl1q_u8(hi_hi, hi_n);
        let matched_lo = vandq_u8(r_lo_lo, r_hi_lo);
        let matched_hi = vandq_u8(r_lo_hi, r_hi_hi);
        let matched = vorrq_u8(matched_lo, matched_hi);
        let any = vtstq_u8(matched, matched);
        movemask_u8x16(any)
    }
}

// ─────────────────────────── multi-cmp path ───────────────────────────

fn scan_multi(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    let mut byte_lut = [false; 256];
    for &b in alphabet.singletons {
        byte_lut[b as usize] = true;
    }
    let quote_lut = build_quote_byte_lut(alphabet.quote_classes);
    let has_quotes = !alphabet.quote_classes.is_empty();
    let has_digraphs = !alphabet.digraph_pairs.is_empty();

    let mut idx = StructuralIndex::with_capacity(input.len() / 8 + 1);
    let mut quote_carries = [false; 4];
    let mut bs_carry = false;

    // SAFETY: see scan_nibble.
    unsafe {
        let mut i = 0usize;
        while i + STRIPE <= input.len() {
            let mut struct_mask = classify_stripe_multi(input.as_ptr().add(i), alphabet.singletons);

            if has_digraphs {
                struct_mask |= digraph_stripe(input.as_ptr().add(i), &alphabet.digraph_pairs);
            }

            if has_quotes {
                let body = quote_stripe_masked(
                    input.as_ptr().add(i),
                    alphabet.quote_classes,
                    &mut bs_carry,
                    &mut quote_carries[..alphabet.quote_classes.len()],
                );
                struct_mask &= !body;
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
            let b = *input.get_unchecked(i);
            let inside = if has_quotes {
                quote_carries[..alphabet.quote_classes.len()]
                    .iter()
                    .any(|&b| b)
            } else {
                false
            };
            if !inside && byte_lut[b as usize] {
                idx.push(i as u32, b);
            }
            if has_quotes && quote_lut[b as usize] {
                for c in quote_carries[..alphabet.quote_classes.len()].iter_mut() {
                    if quote_lut[b as usize] {
                        *c = !*c;
                        break;
                    }
                }
            }
            i += 1;
        }
    }

    idx
}

#[inline(always)]
unsafe fn classify_stripe_multi(ptr: *const u8, singletons: &[u8]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller bounds ptr..ptr+64.
    unsafe {
        for k in 0..4 {
            let chunk = vld1q_u8(ptr.add(k * 16));
            let mut acc = vdupq_n_u8(0);
            for &target in singletons {
                let cmp = vceqq_u8(chunk, vdupq_n_u8(target));
                acc = vorrq_u8(acc, cmp);
            }
            let m = movemask_u8x16(acc) as u64;
            mask |= m << (k * 16);
        }
    }
    mask
}

// ───────────────────────── digraph helpers ────────────────────────────

#[inline(always)]
unsafe fn digraph_stripe(ptr: *const u8, pairs: &[(u8, u8)]) -> u64 {
    let mut mask = 0u64;
    // SAFETY: caller ensures ptr..ptr+64 in bounds; we read up to
    // ptr+64 via the shifted compare (we read 64 bytes worth of input
    // bytes; the second-byte test for stripe-end byte (idx 63) reads
    // ptr+64 which is allowed because the stripe-loop bounds the
    // outer call by `i + STRIPE <= input.len()`).
    unsafe {
        for &(first, second) in pairs {
            for k in 0..4 {
                let chunk = vld1q_u8(ptr.add(k * 16));
                // Load the next chunk (or beyond) for the shifted compare.
                let next = vld1q_u8(ptr.add(k * 16 + 1));
                let f_eq = vceqq_u8(chunk, vdupq_n_u8(first));
                let s_eq = vceqq_u8(next, vdupq_n_u8(second));
                let both = vandq_u8(f_eq, s_eq);
                let m = movemask_u8x16(both) as u64;
                mask |= m << (k * 16);
            }
        }
    }
    mask
}

#[inline(always)]
unsafe fn digraph_chunk(ptr: *const u8, pairs: &[(u8, u8)]) -> u16 {
    let mut mask = 0u16;
    // SAFETY: caller ensures ptr..ptr+17 in bounds (stripe-bound).
    unsafe {
        for &(first, second) in pairs {
            let chunk = vld1q_u8(ptr);
            let next = vld1q_u8(ptr.add(1));
            let f_eq = vceqq_u8(chunk, vdupq_n_u8(first));
            let s_eq = vceqq_u8(next, vdupq_n_u8(second));
            let both = vandq_u8(f_eq, s_eq);
            mask |= movemask_u8x16(both);
        }
    }
    mask
}

// ────────────────────────── quote helpers ─────────────────────────────

/// Build per-class quote masks + the backslash mask for a stripe.
/// Output is written into `out_qmasks` (length must equal `quotes.len()`).
#[inline(always)]
unsafe fn raw_quotes_and_bs_stripe(ptr: *const u8, quotes: &[u8], out_qmasks: &mut [u64]) -> u64 {
    debug_assert_eq!(out_qmasks.len(), quotes.len());
    for slot in out_qmasks.iter_mut() {
        *slot = 0;
    }
    let mut bsmask = 0u64;
    // SAFETY: caller ensures ptr..ptr+64 in bounds.
    unsafe {
        for k in 0..4 {
            let chunk = vld1q_u8(ptr.add(k * 16));
            for (qi, &q) in quotes.iter().enumerate() {
                let cmp = vceqq_u8(chunk, vdupq_n_u8(q));
                let m = movemask_u8x16(cmp) as u64;
                out_qmasks[qi] |= m << (k * 16);
            }
            let bs = vceqq_u8(chunk, vdupq_n_u8(b'\\'));
            bsmask |= (movemask_u8x16(bs) as u64) << (k * 16);
        }
    }
    bsmask
}

/// Quote-stripe: returns the per-byte "in-string body" mask (bits set
/// where the byte is INSIDE a string literal, excluding the opening
/// quote byte but INCLUDING the closing quote byte). Updates per-class
/// `quote_carries` and `bs_carry` for the next stripe.
///
/// **Multi-class handling.** When `quotes.len() > 1` (CSS uses both `"`
/// and `'`), strings of one class don't toggle the other class's
/// parity — a `'` inside `"..."` is opaque content, not a string
/// boundary. We track per-class state independently via a serial
/// stripe walker. This matches `scalar::compute_inside_string_mask`.
///
/// Convention follows the project scalar reference (`scalar::scan`):
/// opening quote is structural, closing quote is masked out, body
/// bytes are masked out. See AW-III.W5 §5.5.
#[inline(always)]
unsafe fn quote_stripe_masked(
    ptr: *const u8,
    quotes: &[u8],
    bs_carry: &mut bool,
    quote_carries: &mut [bool],
) -> u64 {
    debug_assert_eq!(quote_carries.len(), quotes.len());
    let mut qmasks = [0u64; 4];
    let bsmask = unsafe { raw_quotes_and_bs_stripe(ptr, quotes, &mut qmasks[..quotes.len()]) };
    let (escape, new_bs_carry) = parity::escape_mask_64(bsmask, *bs_carry);
    *bs_carry = new_bs_carry;

    // Strip escapes from each class.
    for qi in 0..quotes.len() {
        qmasks[qi] &= !escape;
    }

    if quotes.len() == 1 {
        // Single-class fast path — pure parallel parity.
        let real = qmasks[0];
        let prefix = parity::prefix_xor_64(real, quote_carries[0]);
        quote_carries[0] = (prefix >> 63) & 1 == 1;
        return prefix ^ real;
    }

    multi_class_inside(qmasks, quotes.len(), quote_carries, 64)
}

/// 16-byte-chunk variant; same contract as `quote_stripe_masked`.
#[inline(always)]
unsafe fn quote_chunk_masked(
    ptr: *const u8,
    quotes: &[u8],
    bs_carry: &mut bool,
    quote_carries: &mut [bool],
) -> u64 {
    debug_assert_eq!(quote_carries.len(), quotes.len());
    // SAFETY: caller ensures ptr..ptr+16 in bounds.
    unsafe {
        let chunk = vld1q_u8(ptr);
        let mut qmasks = [0u64; 4];
        for (qi, &q) in quotes.iter().enumerate() {
            let cmp = vceqq_u8(chunk, vdupq_n_u8(q));
            qmasks[qi] = movemask_u8x16(cmp) as u64;
        }
        let bs = vceqq_u8(chunk, vdupq_n_u8(b'\\'));
        let bsmask = movemask_u8x16(bs) as u64;
        let bs16 = bsmask & 0xFFFF;
        let (escape, _) = parity::escape_mask_64(bs16, *bs_carry);
        // Carry-out: count trailing 1-bits of the 16-bit region from
        // bit 15 down. If the entire 16-bit region is `\`, the new
        // carry depends on input carry; otherwise it's the local
        // parity.
        let high_bit_set = (bs16 >> 15) & 1 == 1;
        if high_bit_set {
            let mut count = 0u32;
            let mut bit = 15i32;
            while bit >= 0 && ((bs16 >> bit) & 1) == 1 {
                count += 1;
                bit -= 1;
            }
            *bs_carry = if count == 16 {
                *bs_carry // even-length contribution; carry passes through
            } else {
                (count % 2) == 1
            };
        } else {
            *bs_carry = false;
        }

        for qi in 0..quotes.len() {
            qmasks[qi] = (qmasks[qi] & !escape) & 0xFFFF;
        }

        if quotes.len() == 1 {
            let real = qmasks[0];
            let prefix = parity::prefix_xor_64(real, quote_carries[0]);
            quote_carries[0] = (prefix >> 15) & 1 == 1;
            return (prefix ^ real) & 0xFFFF;
        }

        multi_class_inside(qmasks, quotes.len(), quote_carries, 16)
    }
}

/// Compute the body-mask for a stripe with multiple quote classes via
/// the per-byte state machine. Bits 0..bits_in_stripe are processed.
#[inline]
fn multi_class_inside(
    real: [u64; 4],
    n_classes: usize,
    quote_carries: &mut [bool],
    bits_in_stripe: u32,
) -> u64 {
    // Determine starting state from per-class carries. At most one
    // class should be inside at a stripe boundary (well-formed inputs).
    let mut state: usize = usize::MAX;
    for k in 0..n_classes {
        if quote_carries[k] {
            state = k;
            break;
        }
    }

    let mut body = 0u64;
    for bit in 0..bits_in_stripe {
        let mask = 1u64 << bit;
        // Find the toggling class at this bit (if any). At most one
        // because the quote bytes themselves are distinct.
        let mut toggle_class: Option<usize> = None;
        for k in 0..n_classes {
            if (real[k] & mask) != 0 {
                toggle_class = Some(k);
                break;
            }
        }
        let was_inside = state != usize::MAX;
        if let Some(k) = toggle_class {
            if state == usize::MAX {
                state = k; // open
            } else if state == k {
                state = usize::MAX; // close
            }
            // else: inert toggle inside another class
        }
        // Body bit set iff the byte is inside a string OR is a closing
        // quote of the active class — both cases caught by `was_inside`.
        if was_inside {
            body |= mask;
        }
    }

    for k in 0..n_classes {
        quote_carries[k] = state == k;
    }
    body
}

// ─────────────────────────── movemask ─────────────────────────────────

/// Construct a 16-bit mask from a `uint8x16_t` whose bytes are
/// `0xFF` for "set" and `0x00` for "clear" (the natural output of
/// NEON `vceqq_u8`).
///
/// Standard NEON movemask via `vshrn_n_u16 #4` + read as u64 + bit-
/// stitch: shift-right-narrow on a 16-byte vector treating it as 8
/// `u16` lanes produces an 8-byte output where each nibble carries
/// the "set/clear" of the corresponding source byte. We then extract
/// the 16 nibbles as a u64 and apply a parallel-bits-extract to
/// reconstruct the 16-bit per-byte mask.
#[inline(always)]
unsafe fn movemask_u8x16(v: uint8x16_t) -> u16 {
    // SAFETY: pure SIMD intrinsics on a register; no memory access.
    unsafe {
        // Reinterpret the 16-byte vector as 8 u16 lanes, then narrow
        // right by 4 to produce 8 bytes whose low 4 bits each encode
        // 4 source bytes' set/clear via the 0xF/0x0 pattern.
        // Actually, vshrn_n_u16 #4 on a vec of [0xFFFF, 0x0000, ...]
        // yields per-byte [0xFF, 0x00, ...] where each byte represents
        // a pair of input bytes. We re-pack via a per-bit pattern.
        let pat: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
        let bits = vandq_u8(v, vld1q_u8(pat.as_ptr()));
        // Sum the 8 low-half bytes into one u8 (they're disjoint bits),
        // similarly for the 8 high-half bytes. addv reduces a 16-byte
        // vector to a single u8 by horizontal addition; we want
        // separate sums for the two 8-byte halves, so we use addv on
        // each half via vget_low/high.
        let lo_u8 = vaddv_u8(vget_low_u8(bits)) as u16;
        let hi_u8 = vaddv_u8(vget_high_u8(bits)) as u16;
        lo_u8 | (hi_u8 << 8)
    }
}

#[inline(always)]
fn build_quote_byte_lut(quotes: &[u8]) -> [bool; 256] {
    let mut lut = [false; 256];
    for &q in quotes {
        lut[q as usize] = true;
    }
    lut
}
