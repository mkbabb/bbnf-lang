//! Body source for the nibble-LUT structural-scan kernel.
//!
//! # Paired runtime
//!
//! aarch64: [`crate::neon::scan`] nibble-LUT arm at `neon.rs:59` +
//! `classify_stripe_nibble` at `neon.rs:186` + `classify_chunk_nibble`
//! at `neon.rs:201`.
//!
//! x86_64: [`crate::avx2::scan`] nibble-LUT arm at `avx2.rs:47` +
//! `classify_chunk_nibble` at `avx2.rs:222`.
//!
//! The fragment is the **per-stripe classification body** — it classifies
//! a 64-byte stripe into a single `u64` where bit `i` is set iff byte
//! `i` of the stripe matches the nibble-LUT alphabet. The enclosing
//! scan loop (stripe-walker, digraph fold, quote-parity AND-NOT,
//! compaction) is owned by the per-shape emitter that splices this
//! body.
//!
//! # Splice-target type
//!
//! Per-arch [`syn::Block`]. Each block is an expression block whose
//! trailing expression is the stripe mask (`u64`).
//!
//! # Prebound names
//!
//! - `ptr: *const u8` — stripe base pointer.
//! - `lo_v`, `hi_v` (NEON `uint8x16_t` / AVX2 `__m256i`) — the two
//!   nibble-LUT registers.
//! - `lo_mask` — the `0x0F` splat constant of the same register type.
//!
//! The NEON variant expects `lo_v` / `hi_v` as 128-bit vectors
//! (single-register LUTs); the AVX2 variant expects them broadcast into
//! the low + high halves of each 256-bit register (one 16-byte LUT
//! replicated into both halves so `_mm256_shuffle_epi8`'s per-half
//! addressing sees the same LUT in each lane-group).
//!
//! # Shape — aarch64
//!
//! ```text
//! // unsafe { … }  // caller-owned outer unsafe block
//! {
//!     let mut mask = 0u64;
//!     for k in 0..4 {
//!         let chunk = vld1q_u8(ptr.add(k * 16));
//!         let lo_n = vandq_u8(chunk, lo_mask);
//!         let hi_n = vshrq_n_u8(chunk, 4);
//!         let lo_r = vqtbl1q_u8(lo_v, lo_n);
//!         let hi_r = vqtbl1q_u8(hi_v, hi_n);
//!         let matched = vandq_u8(lo_r, hi_r);
//!         let any = vtstq_u8(matched, matched);
//!         let m = movemask_u8x16(any) as u64;
//!         mask |= m << (k * 16);
//!     }
//!     mask
//! }
//! ```
//!
//! `movemask_u8x16` is the helper re-exported from
//! [`crate::neon`] (the portable NEON movemask via `vshrn_n_u16` + bit
//! weighting); when the per-shape emitter splices this body it must
//! also splice [`crate::emit::nospace64_scan`]'s helper exports or
//! call `crate::neon::movemask_u8x16` directly. The runtime kernel
//! keeps the helper; emitter-lifted callers inline it.
//!
//! # Shape — x86_64 (AVX2)
//!
//! ```text
//! {
//!     let mut mask = 0u64;
//!     for k in 0..2 {
//!         let chunk = _mm256_loadu_si256(ptr.add(k * 32) as *const __m256i);
//!         let lo_n = _mm256_and_si256(chunk, lo_mask);
//!         let hi_n = _mm256_and_si256(_mm256_srli_epi16(chunk, 4), lo_mask);
//!         let lo_r = _mm256_shuffle_epi8(lo_v, lo_n);
//!         let hi_r = _mm256_shuffle_epi8(hi_v, hi_n);
//!         let matched = _mm256_and_si256(lo_r, hi_r);
//!         let nz = _mm256_cmpgt_epi8(matched, _mm256_setzero_si256());
//!         let m = _mm256_movemask_epi8(nz) as u32 as u64;
//!         mask |= m << (k * 32);
//!     }
//!     mask
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// aarch64 NEON stripe body. Requires `core::arch::aarch64::*` imports
/// in scope + `movemask_u8x16` in scope (or qualified).
pub const SOURCE_NEON: &str = r#"{
    let mut mask = 0u64;
    for k in 0..4 {
        let chunk = unsafe { vld1q_u8(ptr.add(k * 16)) };
        let lo_n = unsafe { vandq_u8(chunk, lo_mask) };
        let hi_n = unsafe { vshrq_n_u8::<4>(chunk) };
        let lo_r = unsafe { vqtbl1q_u8(lo_v, lo_n) };
        let hi_r = unsafe { vqtbl1q_u8(hi_v, hi_n) };
        let matched = unsafe { vandq_u8(lo_r, hi_r) };
        let any = unsafe { vtstq_u8(matched, matched) };
        let m = unsafe { movemask_u8x16(any) } as u64;
        mask |= m << (k * 16);
    }
    mask
}"#;

/// x86_64 AVX2 stripe body. Requires `core::arch::x86_64::*` imports
/// in scope + `target_feature = "avx2"` on the enclosing fn.
pub const SOURCE_AVX2: &str = r#"{
    let mut mask = 0u64;
    for k in 0..2 {
        let chunk = unsafe { _mm256_loadu_si256(ptr.add(k * 32) as *const __m256i) };
        let lo_n = unsafe { _mm256_and_si256(chunk, lo_mask) };
        let hi_n = unsafe { _mm256_and_si256(_mm256_srli_epi16::<4>(chunk), lo_mask) };
        let lo_r = unsafe { _mm256_shuffle_epi8(lo_v, lo_n) };
        let hi_r = unsafe { _mm256_shuffle_epi8(hi_v, hi_n) };
        let matched = unsafe { _mm256_and_si256(lo_r, hi_r) };
        let nz = unsafe { _mm256_cmpgt_epi8(matched, _mm256_setzero_si256()) };
        let m = unsafe { _mm256_movemask_epi8(nz) } as u32 as u64;
        mask |= m << (k * 32);
    }
    mask
}"#;

/// Parse the aarch64 NEON source into a [`TokenStream`].
pub fn fragment_neon() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_NEON)
        .expect("bbnf-simd-scan::emit::nibble_lut_scan: SOURCE_NEON must parse as syn::Block")
        .to_token_stream()
}

/// Parse the x86_64 AVX2 source into a [`TokenStream`].
pub fn fragment_avx2() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_AVX2)
        .expect("bbnf-simd-scan::emit::nibble_lut_scan: SOURCE_AVX2 must parse as syn::Block")
        .to_token_stream()
}
