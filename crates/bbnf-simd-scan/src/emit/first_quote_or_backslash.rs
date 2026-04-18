//! Body source for the first-quote-or-backslash locator kernel.
//!
//! # Paired runtime
//!
//! Prototype reference in
//! `crates/bbnf-json-prototype/src/simd.rs:286` (NEON at 303, AVX2 at
//! 347, scalar at 381). The W2.1 prototype already verified this body
//! inlines under workspace LTO + `#[inline(always)]`; lifting it into
//! the shape-emitter keeps the same verified inner loop but splices it
//! inline at the per-rule call site rather than relying on cross-crate
//! inlining.
//!
//! # Splice-target type
//!
//! Per-arch [`syn::Block`]. Each block is an expression block whose
//! trailing expression is `Option<(usize, u8)>` — `Some((offset, byte))`
//! on hit with `byte ∈ {b'"', b'\\'}`, `None` when neither appears in
//! the input.
//!
//! # Prebound names
//!
//! - `bytes: &[u8]` — the haystack; the kernel scans from offset 0.
//!
//! The NEON and AVX2 variants wrap their intrinsic loads in explicit
//! `unsafe {}` blocks so the splice is usable inside a safe fn on hosts
//! where `target_feature = "avx2"` (x86_64) / baseline NEON (aarch64)
//! is guaranteed by the enclosing emitter's cfg context.
//!
//! # Shape — aarch64 NEON
//!
//! ```text
//! {
//!     let quote = vdupq_n_u8(b'"');
//!     let backslash = vdupq_n_u8(b'\\');
//!     let ptr = bytes.as_ptr();
//!     let len = bytes.len();
//!     let mut i = 0usize;
//!     while i + 16 <= len {
//!         let v = vld1q_u8(ptr.add(i));
//!         let eq_q = vceqq_u8(v, quote);
//!         let eq_b = vceqq_u8(v, backslash);
//!         let hit = vorrq_u8(eq_q, eq_b);
//!         let packed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit));
//!         let bits = vget_lane_u64::<0>(vreinterpret_u64_u8(packed));
//!         if bits != 0 {
//!             let off = (bits.trailing_zeros() >> 2) as usize;
//!             let byte = *ptr.add(i + off);
//!             return Some((i + off, byte));  // caller wraps in labelled block
//!         }
//!         i += 16;
//!     }
//!     while i < len {
//!         let b = *ptr.add(i);
//!         if b == b'"' || b == b'\\' {
//!             return Some((i, b));
//!         }
//!         i += 1;
//!     }
//!     None
//! }
//! ```
//!
//! The prototype uses early `return` inside the scan; when lifted into
//! a shape emitter that inlines the body at a non-fn splice site, the
//! caller wraps the block in a labelled break block so the `return`
//! becomes `break 'scan`. The fragment below uses `break 'scan` for
//! that shape — the emitter is expected to wrap with
//! `let out: Option<(usize, u8)> = 'scan: { … };`.
//!
//! # Shape — x86_64 AVX2
//!
//! ```text
//! 'scan: {
//!     let quote = _mm256_set1_epi8(b'"' as i8);
//!     let backslash = _mm256_set1_epi8(b'\\' as i8);
//!     let ptr = bytes.as_ptr();
//!     let len = bytes.len();
//!     let mut i = 0usize;
//!     while i + 32 <= len {
//!         let v = _mm256_loadu_si256(ptr.add(i) as *const __m256i);
//!         let eq_q = _mm256_cmpeq_epi8(v, quote);
//!         let eq_b = _mm256_cmpeq_epi8(v, backslash);
//!         let hit = _mm256_or_si256(eq_q, eq_b);
//!         let mask = _mm256_movemask_epi8(hit) as u32;
//!         if mask != 0 {
//!             let off = mask.trailing_zeros() as usize;
//!             break 'scan Some((i + off, *ptr.add(i + off)));
//!         }
//!         i += 32;
//!     }
//!     while i < len {
//!         let b = *ptr.add(i);
//!         if b == b'"' || b == b'\\' { break 'scan Some((i, b)); }
//!         i += 1;
//!     }
//!     None
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// aarch64 NEON body. The fragment is an outer block containing a
/// single labelled `'scan` block — `break 'scan <value>` short-circuits
/// the scan with the hit tuple; the trailing `None` is the miss case.
/// The outer block's trailing expression IS the `Option<(usize, u8)>`
/// result; the per-shape emitter may capture it via
/// `let out = { <SOURCE_NEON> };` at the splice site.
pub const SOURCE_NEON: &str = r#"{
    'scan: {
        let quote = unsafe { vdupq_n_u8(b'"') };
        let backslash = unsafe { vdupq_n_u8(b'\\') };
        let ptr = bytes.as_ptr();
        let len = bytes.len();
        let mut i = 0usize;
        while i + 16 <= len {
            let v = unsafe { vld1q_u8(ptr.add(i)) };
            let eq_q = unsafe { vceqq_u8(v, quote) };
            let eq_b = unsafe { vceqq_u8(v, backslash) };
            let hit = unsafe { vorrq_u8(eq_q, eq_b) };
            let packed = unsafe { vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit)) };
            let bits = unsafe { vget_lane_u64::<0>(vreinterpret_u64_u8(packed)) };
            if bits != 0 {
                let off = (bits.trailing_zeros() >> 2) as usize;
                let byte = unsafe { *ptr.add(i + off) };
                break 'scan Some((i + off, byte));
            }
            i += 16;
        }
        while i < len {
            let b = unsafe { *ptr.add(i) };
            if b == b'"' || b == b'\\' {
                break 'scan Some((i, b));
            }
            i += 1;
        }
        None
    }
}"#;

/// x86_64 AVX2 body; same outer-block / labelled-`'scan` contract as
/// [`SOURCE_NEON`].
pub const SOURCE_AVX2: &str = r#"{
    'scan: {
        let quote = unsafe { _mm256_set1_epi8(b'"' as i8) };
        let backslash = unsafe { _mm256_set1_epi8(b'\\' as i8) };
        let ptr = bytes.as_ptr();
        let len = bytes.len();
        let mut i = 0usize;
        while i + 32 <= len {
            let v = unsafe { _mm256_loadu_si256(ptr.add(i) as *const __m256i) };
            let eq_q = unsafe { _mm256_cmpeq_epi8(v, quote) };
            let eq_b = unsafe { _mm256_cmpeq_epi8(v, backslash) };
            let hit = unsafe { _mm256_or_si256(eq_q, eq_b) };
            let mask = unsafe { _mm256_movemask_epi8(hit) } as u32;
            if mask != 0 {
                let off = mask.trailing_zeros() as usize;
                let byte = unsafe { *ptr.add(i + off) };
                break 'scan Some((i + off, byte));
            }
            i += 32;
        }
        while i < len {
            let b = unsafe { *ptr.add(i) };
            if b == b'"' || b == b'\\' {
                break 'scan Some((i, b));
            }
            i += 1;
        }
        None
    }
}"#;

/// Portable scalar fallback body. `bytes: &[u8]` must be in scope.
pub const SOURCE_SCALAR: &str = r#"{
    let mut result: Option<(usize, u8)> = None;
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if b == b'"' || b == b'\\' {
            result = Some((i, b));
            break;
        }
        i += 1;
    }
    result
}"#;

/// Parse the aarch64 NEON source into a [`TokenStream`].
pub fn fragment_neon() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_NEON)
        .expect(
            "bbnf-simd-scan::emit::first_quote_or_backslash: SOURCE_NEON must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse the x86_64 AVX2 source into a [`TokenStream`].
pub fn fragment_avx2() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_AVX2)
        .expect(
            "bbnf-simd-scan::emit::first_quote_or_backslash: SOURCE_AVX2 must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse the scalar-fallback source into a [`TokenStream`].
pub fn fragment_scalar() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_SCALAR)
        .expect(
            "bbnf-simd-scan::emit::first_quote_or_backslash: SOURCE_SCALAR must parse as syn::Block",
        )
        .to_token_stream()
}
