//! Body source for the multi-byte compare-reduce structural-scan kernel.
//!
//! # Paired runtime
//!
//! aarch64: [`crate::neon::scan`] multi-cmp arm at `neon.rs:346` +
//! `classify_stripe_multi` at `neon.rs:410`.
//!
//! x86_64: [`crate::avx2::scan`] multi-cmp arm at `avx2.rs:137` +
//! `classify_chunk_multi` at `avx2.rs:242`.
//!
//! Selected when `|singletons| > 16` — when the nibble-LUT and wide-LUT
//! paths can no longer encode the alphabet distinctly. Each singleton
//! contributes one compare-splat-OR per chunk.
//!
//! # Splice-target type
//!
//! Per-arch [`syn::Block`]. Each block is an expression block whose
//! trailing expression is the stripe mask (`u64`).
//!
//! # Prebound names
//!
//! - `ptr: *const u8` — stripe base pointer.
//! - `singletons: &[u8]` — the per-singleton byte list the loop scans.
//!
//! The enclosing emitter-produced fn owns the outer `unsafe` wrapper
//! (per the runtime kernel's `unsafe fn` signature); the fragment
//! assumes unsafe intrinsic calls are permitted at splice time.
//!
//! # Shape — aarch64 NEON
//!
//! ```text
//! {
//!     let mut mask = 0u64;
//!     for k in 0..4 {
//!         let chunk = vld1q_u8(ptr.add(k * 16));
//!         let mut acc = vdupq_n_u8(0);
//!         for &target in singletons {
//!             let cmp = vceqq_u8(chunk, vdupq_n_u8(target));
//!             acc = vorrq_u8(acc, cmp);
//!         }
//!         let m = movemask_u8x16(acc) as u64;
//!         mask |= m << (k * 16);
//!     }
//!     mask
//! }
//! ```
//!
//! # Shape — x86_64 AVX2
//!
//! ```text
//! {
//!     let mut mask = 0u64;
//!     for k in 0..2 {
//!         let chunk = _mm256_loadu_si256(ptr.add(k * 32) as *const __m256i);
//!         let mut acc = _mm256_setzero_si256();
//!         for &target in singletons {
//!             let cmp = _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(target as i8));
//!             acc = _mm256_or_si256(acc, cmp);
//!         }
//!         let m = _mm256_movemask_epi8(acc) as u32 as u64;
//!         mask |= m << (k * 32);
//!     }
//!     mask
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// aarch64 NEON multi-cmp stripe body. Requires
/// `core::arch::aarch64::*` + `movemask_u8x16` in scope.
pub const SOURCE_NEON: &str = r#"{
    let mut mask = 0u64;
    for k in 0..4 {
        let chunk = unsafe { vld1q_u8(ptr.add(k * 16)) };
        let mut acc = unsafe { vdupq_n_u8(0) };
        for &target in singletons {
            let cmp = unsafe { vceqq_u8(chunk, vdupq_n_u8(target)) };
            acc = unsafe { vorrq_u8(acc, cmp) };
        }
        let m = unsafe { movemask_u8x16(acc) } as u64;
        mask |= m << (k * 16);
    }
    mask
}"#;

/// x86_64 AVX2 multi-cmp stripe body. Requires `core::arch::x86_64::*`
/// + `target_feature = "avx2"` on the enclosing fn.
pub const SOURCE_AVX2: &str = r#"{
    let mut mask = 0u64;
    for k in 0..2 {
        let chunk = unsafe { _mm256_loadu_si256(ptr.add(k * 32) as *const __m256i) };
        let mut acc = unsafe { _mm256_setzero_si256() };
        for &target in singletons {
            let cmp = unsafe { _mm256_cmpeq_epi8(chunk, _mm256_set1_epi8(target as i8)) };
            acc = unsafe { _mm256_or_si256(acc, cmp) };
        }
        let m = unsafe { _mm256_movemask_epi8(acc) } as u32 as u64;
        mask |= m << (k * 32);
    }
    mask
}"#;

/// Parse the aarch64 NEON source into a [`TokenStream`].
pub fn fragment_neon() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_NEON)
        .expect("bbnf-simd-scan::emit::multi_cmp_scan: SOURCE_NEON must parse as syn::Block")
        .to_token_stream()
}

/// Parse the x86_64 AVX2 source into a [`TokenStream`].
pub fn fragment_avx2() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_AVX2)
        .expect("bbnf-simd-scan::emit::multi_cmp_scan: SOURCE_AVX2 must parse as syn::Block")
        .to_token_stream()
}
