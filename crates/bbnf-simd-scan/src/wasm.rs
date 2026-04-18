//! WASM SIMD128 kernel.
//!
//! 64-byte stripes (4 × `v128` chunks per stripe, matching the NEON
//! shape). NEON-parity lowering:
//!
//! - `i8x16.swizzle` for the nibble-LUT collapse (one shuffle per
//!   16-byte chunk against the low/high nibble LUT);
//! - `i8x16.eq` for singleton / quote / digraph compare;
//! - `i8x16.bitmask` for mask extraction;
//! - `parity::escape_mask_64` (shared with NEON) computes the
//!   backslash-parity escape mask; `parity::prefix_xor_64` computes
//!   the in-string prefix (shift-XOR fallback on WASM since CLMUL
//!   isn't available without the dedicated `relaxed-simd` + CLMUL
//!   proposals).
//!
//! # Quote parity parity with NEON
//!
//! For browser-side parse parity with the native NEON kernel, the
//! backslash-parity escape logic is factored out into the shared
//! `parity::escape_mask_64` helper (same one the NEON and scalar
//! paths use). The WASM kernel extracts raw quote-byte and
//! backslash-byte 64-bit masks per stripe via `i8x16.eq` +
//! `i8x16.bitmask`, then funnels them through the arch-neutral
//! escape + prefix-XOR pipeline — identical algorithm, identical
//! output.
//!
//! Multi-quote-class CSS (`"` + `'`) routes through the same
//! `multi_class_inside` state machine the NEON path invokes; the
//! state machine is arch-neutral scalar code so WASM reuses it
//! verbatim.
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
    let mut quote_carries = [false; 4];
    let mut bs_carry = false;

    let lo_v = u8x16_from(lut.lo);
    let hi_v = u8x16_from(lut.hi);
    let lo_mask = u8x16_splat(0x0F);

    let mut i = 0usize;
    while i + STRIPE <= input.len() {
        let mut struct_mask = 0u64;
        for k in 0..4 {
            let m = classify_chunk_nibble(
                unsafe { input.as_ptr().add(i + k * CHUNK) },
                lo_v,
                hi_v,
                lo_mask,
            ) as u64;
            struct_mask |= m << (k * CHUNK);
        }

        if has_digraphs {
            struct_mask |=
                digraph_stripe(unsafe { input.as_ptr().add(i) }, &alphabet.digraph_pairs);
        }
        if has_quotes {
            let body = quote_stripe_masked(
                unsafe { input.as_ptr().add(i) },
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

    // Scalar epilogue for < 64 residual bytes. Mirrors NEON's tail
    // state machine: track per-class quote parity, backslash parity,
    // and digraph first-byte emission.
    let mut active_class: Option<usize> =
        quote_carries[..alphabet.quote_classes.len()]
            .iter()
            .position(|&b| b);
    let mut prev_was_bs = bs_carry;
    while i < input.len() {
        let b = unsafe { *input.get_unchecked(i) };
        let was_inside = active_class.is_some();
        let is_singleton = byte_lut[b as usize];
        let is_digraph_first = if i + 1 < input.len() {
            let next = unsafe { *input.get_unchecked(i + 1) };
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
        if has_quotes && quote_lut[b as usize] && !prev_was_bs {
            if let Some(act) = active_class {
                if alphabet.quote_classes[act] == b {
                    active_class = None;
                }
            } else if let Some(k) = alphabet.quote_classes.iter().position(|&q| q == b) {
                active_class = Some(k);
            }
        }
        prev_was_bs = b == b'\\' && !prev_was_bs;
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
    let mut quote_carries = [false; 4];
    let mut bs_carry = false;

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
            struct_mask |=
                digraph_stripe(unsafe { input.as_ptr().add(i) }, &alphabet.digraph_pairs);
        }
        if has_quotes {
            let body = quote_stripe_masked(
                unsafe { input.as_ptr().add(i) },
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

    let mut active_class: Option<usize> =
        quote_carries[..alphabet.quote_classes.len()]
            .iter()
            .position(|&b| b);
    let mut prev_was_bs = bs_carry;
    while i < input.len() {
        let b = unsafe { *input.get_unchecked(i) };
        let was_inside = active_class.is_some();
        let is_singleton = byte_lut[b as usize];
        let is_digraph_first = if i + 1 < input.len() {
            let next = unsafe { *input.get_unchecked(i + 1) };
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
        if has_quotes && quote_lut[b as usize] && !prev_was_bs {
            if let Some(act) = active_class {
                if alphabet.quote_classes[act] == b {
                    active_class = None;
                }
            } else if let Some(k) = alphabet.quote_classes.iter().position(|&q| q == b) {
                active_class = Some(k);
            }
        }
        prev_was_bs = b == b'\\' && !prev_was_bs;
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

/// Build per-class quote masks + the backslash mask for a stripe.
/// Mirrors NEON's `raw_quotes_and_bs_stripe`.
///
/// Output `out_qmasks[qi]` carries the 64-bit raw quote-byte mask for
/// class `qi` (bit j set iff `input[stripe_base+j]` equals
/// `quotes[qi]`). Return value is the 64-bit raw backslash mask.
#[inline(always)]
fn raw_quotes_and_bs_stripe(
    ptr: *const u8,
    quotes: &[u8],
    out_qmasks: &mut [u64],
) -> u64 {
    debug_assert_eq!(out_qmasks.len(), quotes.len());
    for slot in out_qmasks.iter_mut() {
        *slot = 0;
    }
    let mut bsmask = 0u64;
    // SAFETY: caller ensures ptr..ptr+64 in bounds.
    unsafe {
        for k in 0..4 {
            let chunk = v128_load(ptr.add(k * CHUNK) as *const v128);
            for (qi, &q) in quotes.iter().enumerate() {
                let cmp = u8x16_eq(chunk, u8x16_splat(q));
                let m = u8x16_bitmask(cmp) as u64;
                out_qmasks[qi] |= m << (k * CHUNK);
            }
            let bs = u8x16_eq(chunk, u8x16_splat(b'\\'));
            bsmask |= (u8x16_bitmask(bs) as u64) << (k * CHUNK);
        }
    }
    bsmask
}

/// Per-stripe in-string body mask. Mirrors NEON's `quote_stripe_masked`.
///
/// For single-class grammars, collapses to `parity::prefix_xor_64`
/// over the unescaped quote mask. For multi-class (CSS's `"` + `'`),
/// routes through the shared `multi_class_inside` state machine so
/// a `'` inside `"..."` is treated as opaque content (not a string
/// boundary), matching the NEON semantics exactly.
///
/// Convention: opening quote is structural (mask bit 0), body bytes
/// are masked out (bit 1), closing quote is masked out (bit 1). See
/// AW-III.W5 §5.5 and `scalar::compute_inside_string_mask`.
#[inline(always)]
fn quote_stripe_masked(
    ptr: *const u8,
    quotes: &[u8],
    bs_carry: &mut bool,
    quote_carries: &mut [bool],
) -> u64 {
    debug_assert_eq!(quote_carries.len(), quotes.len());
    let mut qmasks = [0u64; 4];
    let bsmask = raw_quotes_and_bs_stripe(ptr, quotes, &mut qmasks[..quotes.len()]);
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

/// Compute the body-mask for a stripe with multiple quote classes via
/// the per-byte state machine. Arch-neutral scalar code shared with
/// NEON's `multi_class_inside`.
///
/// `real` carries the per-class raw quote masks (bit j set iff
/// `input[stripe_base+j]` is an unescaped quote of that class);
/// `quote_carries` carries per-class open/close state across stripes;
/// `bits_in_stripe` is typically 64.
///
/// Returns a 64-bit mask with bit j set iff byte j is inside a string
/// literal (mask excludes the opening quote, includes the closing
/// quote and all body bytes).
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
