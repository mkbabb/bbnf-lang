//! SIMD structural bitmap emitters for scanner codegen (AU.2.7 v2).
//!
//! The v1 attempt (AO.0.1 `compute_structural_bytes` → AP.1
//! `structural_cursor` → AQ.5 deletion) tried a whole-input
//! pre-scan with a `structural_cursor` field on `ParserState` and
//! a scalar `filter_quote_parity` pass. The deletion commit message
//! is explicit: "pre-scan was a ~190µs net-regression on citm"
//! because AP.3.1's SIMD WS bitmap had already captured the savings
//! the pre-scan was supposed to provide.
//!
//! v2 keeps the bitmap primitive but collapses its scope: each
//! emitted scanner runs a stripe-local SIMD byte-class mask over
//! its input window, CTZ-iterates the mask within its own call
//! frame, and returns. No cursor on `ParserState`, no checkpoint
//! coupling, no hybrid dispatch, no fallback. The bitmap IS the
//! scanner; every needle set that fits in a nibble-LUT window
//! (≤ 8 bytes) routes through this single emitter.
//!
//! ## Integration surface
//!
//! `emit_structural_bitmap_kernel(targets, kind)` returns a
//! `TokenStream` that invokes
//! `parse_that::find_next_structural_from` with a pair of
//! precomputed nibble-LUT tables. Callers wrap the result in a
//! quantifier shell (Plus = "fail on zero-length match"; Star =
//! "always succeed with possibly-empty span").
//!
//! The old `emit_memchr1/2/3` + `emit_nibble_lut_scan` emitters
//! are deleted in the same commit where this module lands. Their
//! call sites in `emit/mod.rs` route through this kernel
//! unconditionally.
//!
//! ## Grammar alphabet derivation
//!
//! The per-grammar structural alphabet (`S`) is computed by the IR
//! pass `compute_structural_alphabet` (see
//! `crates/ir/src/passes/structural_alphabet.rs`). When a negated
//! char-class regex's excluded-byte set is a subset of `S`, the
//! emitter uses the grammar-wide alphabet so one precomputed LUT
//! serves every scan site. When the excluded set is a strict
//! subset, a per-site LUT is emitted; both paths share this
//! kernel.

use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Quantifier shape for a bitmap-driven negated scan.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScanQuantifier {
    /// `[^…]+` — requires at least one matching byte; returns
    /// `None` on zero-length match.
    Plus,
    /// `[^…]*` — always succeeds, possibly with an empty span.
    Star,
}

/// Compute the nibble-LUT tables for a set of target bytes.
///
/// Returns `(lo_lut, hi_lut)`. A byte `b` matches if
/// `lo_lut[b & 0x0F] & hi_lut[b >> 4] != 0`.
pub fn build_nibble_luts(targets: &[u8]) -> ([u8; 16], [u8; 16]) {
    let mut lo_lut = [0u8; 16];
    let mut hi_lut = [0u8; 16];

    for (i, &b) in targets.iter().enumerate() {
        if i >= 8 {
            break;
        }
        let bit = 1u8 << i;
        lo_lut[(b & 0x0F) as usize] |= bit;
        hi_lut[(b >> 4) as usize] |= bit;
    }

    (lo_lut, hi_lut)
}

/// Emit a bitmap-driven structural scan over `state.src_bytes`
/// starting at `state.offset`.
///
/// The scan consumes bytes until the first occurrence of any byte
/// in `targets`; the caller receives an `Option<usize>` carrying
/// the relative offset from the scan start (i.e. the number of
/// bytes consumed, not the absolute position).
///
/// ## Arch-gated widening
///
/// On `#[cfg(target_feature = "avx2")]` targets, the emitted scan uses
/// `_mm256_loadu_si256` + `_mm256_shuffle_epi8` (nibble-LUT) +
/// `_mm256_movemask_epi8` — 32 bytes classified per SIMD iteration.
/// Tail loop falls through the same nibble-LUT LUT expansion as the
/// u8x16 path for ≤ 31 residual bytes.
///
/// Otherwise (aarch64, wasm32, non-AVX2 x86_64), the emitted scan
/// routes through `parse_that::find_next_structural_from` — one
/// `std::simd::u8x16` nibble-LUT classification per 16 bytes.
///
/// The gate is compile-time (`target_feature`), not runtime: end-user
/// binaries that enable AVX2 via cargo profile / RUSTFLAGS get the
/// wider scan; cross-compilation to x86_64 without AVX2 falls back
/// without a runtime dispatch cost.
///
/// Both paths produce byte-identical output (the correctness tests in
/// `simd-scan::tests::correctness` enforce cross-arch parity for
/// the scanner side; this emitter side shares the nibble-LUT
/// expansion semantics with the u8x16 path via `build_nibble_luts`).
///
/// `targets` must contain 1..=8 bytes (the nibble-LUT window).
/// Empty or >8-byte sets return `None`.
pub fn emit_structural_bitmap_kernel(targets: &[u8]) -> Option<TokenStream> {
    if targets.is_empty() || targets.len() > 8 {
        return None;
    }

    let (lo, hi) = build_nibble_luts(targets);
    let lo_bytes: Vec<Literal> = lo.iter().map(|b| Literal::u8_unsuffixed(*b)).collect();
    let hi_bytes: Vec<Literal> = hi.iter().map(|b| Literal::u8_unsuffixed(*b)).collect();

    // The emitted block evaluates to `Option<usize>` — the relative
    // offset from `__start`, or `None` if no target byte is found
    // before end-of-input. Both arch arms use the same nibble-LUT
    // static tables, so per-grammar LUT data is shared across paths.
    //
    // AVX2 path: 32-byte chunks via `_mm256_shuffle_epi8` nibble-LUT.
    // The 128-bit LUT is broadcast into both halves of a `__m256i`
    // via `_mm256_broadcastsi128_si256`; `_mm256_shuffle_epi8`
    // operates lane-locally (each 16-byte half shuffles independently
    // against the same LUT), so one broadcast suffices for both
    // halves. The stripe loop classifies 32 bytes per iteration; tail
    // falls through to a scalar byte-LUT loop built from the same
    // nibble tables (the 256-byte LUT expansion mirrors
    // `parse_that::scan::structural_bitmap::expand_byte_lut`).
    //
    // The gate is compile-time; the non-AVX2 arm is the
    // `#[cfg(not(...))]` complement that routes through
    // `parse_that::find_next_structural_from` (portable u8x16).
    Some(quote! {
        {
            static __LO_LUT: [u8; 16] = [#(#lo_bytes),*];
            static __HI_LUT: [u8; 16] = [#(#hi_bytes),*];
            #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
            let __result: ::core::option::Option<usize> = 'avx2_scan: {
                use ::core::arch::x86_64::*;
                // SAFETY: gated by `target_feature = "avx2"`; every load
                // is bounded by the `__i + 32 <= __len` loop guard or
                // by the scalar tail's `__i < __len` check.
                unsafe {
                    let __bytes = state.src_bytes.as_slice();
                    let __len = __bytes.len();
                    let __ptr = __bytes.as_ptr();
                    // Broadcast the 16-byte LUT into both 128-bit halves
                    // of a 256-bit register. `_mm256_shuffle_epi8`
                    // operates lane-locally, so each half uses the same
                    // LUT against its 16 input bytes.
                    let __lo_v = _mm256_broadcastsi128_si256(
                        _mm_loadu_si128(__LO_LUT.as_ptr() as *const __m128i),
                    );
                    let __hi_v = _mm256_broadcastsi128_si256(
                        _mm_loadu_si128(__HI_LUT.as_ptr() as *const __m128i),
                    );
                    let __lo_mask = _mm256_set1_epi8(0x0F);
                    let __zero = _mm256_setzero_si256();
                    let mut __i = __start;
                    while __i + 32 <= __len {
                        let __chunk = _mm256_loadu_si256(
                            __ptr.add(__i) as *const __m256i,
                        );
                        let __lo_n = _mm256_and_si256(__chunk, __lo_mask);
                        let __hi_n = _mm256_and_si256(
                            _mm256_srli_epi16(__chunk, 4),
                            __lo_mask,
                        );
                        let __lo_r = _mm256_shuffle_epi8(__lo_v, __lo_n);
                        let __hi_r = _mm256_shuffle_epi8(__hi_v, __hi_n);
                        let __matched = _mm256_and_si256(__lo_r, __hi_r);
                        let __nz = _mm256_cmpgt_epi8(__matched, __zero);
                        let __mask = _mm256_movemask_epi8(__nz) as u32;
                        if __mask != 0 {
                            let __rel = __mask.trailing_zeros() as usize;
                            break 'avx2_scan {
                                ::core::option::Option::Some(
                                    (__i + __rel) - __start,
                                )
                            };
                        }
                        __i += 32;
                    }
                    // Scalar tail: rebuild the 256-byte LUT from the
                    // nibble tables and walk the residual byte-by-byte.
                    let mut __byte_lut = [false; 256];
                    {
                        let mut __b: u16 = 0;
                        while __b < 256 {
                            let __blo = __LO_LUT[(__b & 0x0F) as usize];
                            let __bhi = __HI_LUT[(__b >> 4) as usize];
                            __byte_lut[__b as usize] = (__blo & __bhi) != 0;
                            __b += 1;
                        }
                    }
                    while __i < __len {
                        let __b = *__ptr.add(__i);
                        if __byte_lut[__b as usize] {
                            break 'avx2_scan {
                                ::core::option::Option::Some(__i - __start)
                            };
                        }
                        __i += 1;
                    }
                    ::core::option::Option::None
                }
            };
            // AW-IV.W4.2.a — PaddedView pass-through on the non-AVX2
            // fallback. `state.padded()` carries the 64-byte trailing
            // zero pad at the type level so the SIMD stripe loop drops
            // the per-stripe tail guard and the scalar tail epilogue
            // that the unpadded entry previously required.
            #[cfg(not(all(target_arch = "x86_64", target_feature = "avx2")))]
            let __result: ::core::option::Option<usize> =
                ::parse_that::find_next_structural_from(
                    state.padded(),
                    __start,
                    &__LO_LUT,
                    &__HI_LUT,
                )
                .map(|(pos, _)| pos - __start);
            __result
        }
    })
}

/// Emit a negated-class scan with the requested quantifier.
///
/// `targets` is the excluded-byte set (i.e., the bytes whose
/// occurrence terminates the scan). Produces a self-contained
/// `TokenStream` that evaluates to `Option<Span>`.
pub fn emit_negated_scan(targets: &[u8], quantifier: ScanQuantifier) -> Option<TokenStream> {
    let kernel = emit_structural_bitmap_kernel(targets)?;
    let body = match quantifier {
        ScanQuantifier::Plus => quote! {
            {
                let __start = state.offset;
                if __start >= state.src_bytes.len() { None } else {
                    let __scan = (#kernel).unwrap_or(state.src_bytes.len() - __start);
                    if __scan == 0 { None } else {
                        state.offset = __start + __scan;
                        Some(::parse_that::Span::new(__start, state.offset, state.src))
                    }
                }
            }
        },
        ScanQuantifier::Star => quote! {
            {
                let __start = state.offset;
                let __scan = if __start >= state.src_bytes.len() { 0 } else {
                    (#kernel).unwrap_or(state.src_bytes.len() - __start)
                };
                state.offset = __start + __scan;
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            }
        },
    };
    Some(body)
}

/// Plus-quantifier wrapper for backwards call-site compatibility.
/// Routes through `emit_structural_bitmap_kernel` unconditionally.
pub fn emit_negated_scan_plus(targets: &[u8]) -> Option<TokenStream> {
    emit_negated_scan(targets, ScanQuantifier::Plus)
}

/// Star-quantifier wrapper for backwards call-site compatibility.
/// Routes through `emit_structural_bitmap_kernel` unconditionally.
pub fn emit_negated_scan_star(targets: &[u8]) -> Option<TokenStream> {
    emit_negated_scan(targets, ScanQuantifier::Star)
}

/// Emit a ws-interleaved negated-class scan (Star quantifier).
///
/// When the grammar has `@ws` with block-comment-aware whitespace,
/// the structural bitmap cannot see `/* ... */` comments embedded
/// inside a `[^;!}]*` span without consulting the ws scanner at
/// every position. This emitter produces a byte-at-a-time loop
/// that calls `scan_ws_block_comments` before each byte check,
/// transparently consuming block comments at every position.
///
/// Always succeeds (Star allows zero-length match).
pub fn emit_ws_interleaved_negated_scan_star(needles: &[u8]) -> TokenStream {
    let checks: Vec<TokenStream> = needles
        .iter()
        .map(|&b| {
            let lit = Literal::byte_character(b);
            quote! { __b == #lit }
        })
        .collect();

    quote! {
        {
            let __start = state.offset;
            loop {
                ::parse_that::scan_ws_block_comments(state);
                if state.offset >= state.src_bytes.len() { break; }
                let __b = state.src_bytes[state.offset];
                if #(#checks)||* { break; }
                state.offset += 1;
            }
            Some(::parse_that::Span::new(__start, state.offset, state.src))
        }
    }
}

/// Emit a ws-interleaved negated-class scan (Plus quantifier).
///
/// Same as the Star variant but returns `None` when zero bytes match.
pub fn emit_ws_interleaved_negated_scan_plus(needles: &[u8]) -> TokenStream {
    let checks: Vec<TokenStream> = needles
        .iter()
        .map(|&b| {
            let lit = Literal::byte_character(b);
            quote! { __b == #lit }
        })
        .collect();

    quote! {
        {
            let __start = state.offset;
            loop {
                ::parse_that::scan_ws_block_comments(state);
                if state.offset >= state.src_bytes.len() { break; }
                let __b = state.src_bytes[state.offset];
                if #(#checks)||* { break; }
                state.offset += 1;
            }
            if state.offset > __start {
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            } else {
                None
            }
        }
    }
}

/// Compute the set of ASCII bytes NOT in the given inclusive ranges.
///
/// Used to convert a positive character class into its excluded-byte
/// set for bitmap scanning.
pub fn compute_excluded_bytes(included_ranges: &[(u8, u8)]) -> Vec<u8> {
    let mut included = [false; 128];
    for &(lo, hi) in included_ranges {
        for b in lo..=hi.min(127) {
            included[b as usize] = true;
        }
    }
    (0u8..128).filter(|b| !included[*b as usize]).collect()
}
