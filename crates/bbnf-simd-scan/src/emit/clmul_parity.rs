//! Body source for the CLMUL / PMULL prefix-XOR kernel.
//!
//! # Paired runtime
//!
//! [`crate::parity::clmul_prefix_xor_aarch64`] (sibling `parity.rs:179`)
//! and [`crate::parity::clmul_prefix_xor_x86`] (sibling `parity.rs:194`).
//!
//! # Splice-target type
//!
//! Per-arch [`syn::Block`] splices. The CLMUL / PMULL intrinsics require
//! `unsafe` context; the fragment carries the `unsafe` block as its
//! outer wrapper so the emitter pastes it directly inside a `fn body`
//! without adding a fresh `unsafe` arm.
//!
//! The aarch64 path requires `target_feature = "aes"`; the x86_64 path
//! requires `target_feature = "pclmulqdq"`. The enclosing emitter gates
//! each splice with the corresponding `cfg` arm. The fragment assumes
//! the caller has pre-bound `mask: u64` in the enclosing scope and
//! consumes the block's trailing expression as the u64 result.
//!
//! # Shape
//!
//! ```text
//! // aarch64:
//! unsafe {
//!     let m = vmull_p64(mask, !0u64);
//!     core::mem::transmute::<u128, [u64; 2]>(m)[0]
//! }
//!
//! // x86_64:
//! unsafe {
//!     let a = _mm_set_epi64x(0, mask as i64);
//!     let b = _mm_set1_epi8(-1i8);
//!     let prod = _mm_clmulepi64_si128::<0>(a, b);
//!     _mm_cvtsi128_si64(prod) as u64
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// aarch64 PMULL body: `vmull_p64(mask, !0u64)` → low 64 of the 128-bit
/// product is the prefix-XOR.
///
/// Requires: `use core::arch::aarch64::*;` visible in the enclosing scope.
pub const SOURCE_AARCH64: &str = r#"{
    let m = unsafe { vmull_p64(mask, !0u64) };
    unsafe { core::mem::transmute::<u128, [u64; 2]>(m)[0] }
}"#;

/// x86_64 PCLMULQDQ body.
///
/// Requires: `use core::arch::x86_64::*;` visible in the enclosing scope.
pub const SOURCE_X86_64: &str = r#"{
    let a = unsafe { _mm_set_epi64x(0, mask as i64) };
    let b = unsafe { _mm_set1_epi8(-1i8) };
    let prod = unsafe { _mm_clmulepi64_si128::<0>(a, b) };
    let lo = unsafe { _mm_cvtsi128_si64(prod) };
    lo as u64
}"#;

/// Parse [`SOURCE_AARCH64`] into a [`TokenStream`].
pub fn fragment_aarch64() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_AARCH64)
        .expect("bbnf-simd-scan::emit::clmul_parity: SOURCE_AARCH64 must parse as syn::Block")
        .to_token_stream()
}

/// Parse [`SOURCE_X86_64`] into a [`TokenStream`].
pub fn fragment_x86_64() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_X86_64)
        .expect("bbnf-simd-scan::emit::clmul_parity: SOURCE_X86_64 must parse as syn::Block")
        .to_token_stream()
}
