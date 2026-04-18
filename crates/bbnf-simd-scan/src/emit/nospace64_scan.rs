//! Body source for the 64-byte whitespace-bitmap scan kernel.
//!
//! # Paired runtime
//!
//! The prototype's `nospace_bitmap_64` family in
//! `crates/bbnf-json-prototype/src/simd.rs:132` (NEON at 151,
//! AVX2 at 236, scalar at 266). Ported from
//! `sonic-rs/src/util/simd/*::get_nonspace_bits`.
//!
//! The shape emitter's `skip_space` fast path consumes this bitmap to
//! locate the first non-whitespace byte in a 64-byte stripe via
//! `mask.trailing_zeros()`.
//!
//! # Splice-target type
//!
//! Per-arch [`syn::Block`]. Each block is an expression block whose
//! trailing expression is the non-whitespace bitmap (`u64`), where
//! bit `i` is set iff byte `i` of the stripe is NOT in
//! `{b' ', b'\t', b'\n', b'\r'}`.
//!
//! # Prebound names
//!
//! - `ptr: *const u8` — stripe base pointer (64 bytes addressable).
//!
//! The NEON and AVX2 variants are pre-`unsafe`-wrapped at the per-load
//! intrinsic level so the splice may appear inside a safe fn when
//! target features are guaranteed by the enclosing emitter's context.
//! (The aarch64 NEON baseline is unconditional on aarch64-*-*, so the
//! NEON fragment is always splice-safe on that arch.)
//!
//! # Shape — aarch64 NEON
//!
//! ```text
//! {
//!     let space = vdupq_n_u8(b' ');
//!     let tab = vdupq_n_u8(b'\t');
//!     let nl = vdupq_n_u8(b'\n');
//!     let cr = vdupq_n_u8(b'\r');
//!     let bits_lo: [u8; 16] = [1,2,4,8,16,32,64,128,1,2,4,8,16,32,64,128];
//!     let bit_vec = vld1q_u8(bits_lo.as_ptr());
//!     let masks: [u16; 4] = [
//!         chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec),
//!         chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec),
//!         chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec),
//!         chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec),
//!     ];
//!     (masks[0] as u64)
//!         | ((masks[1] as u64) << 16)
//!         | ((masks[2] as u64) << 32)
//!         | ((masks[3] as u64) << 48)
//! }
//! ```
//!
//! `chunk_ns_mask16` is the helper inlined from the prototype's
//! `src/simd.rs:205` — the per-16-byte-chunk compare+mask+horizontal-add
//! reduction. The fragment expects that helper to be re-emitted as a
//! sibling `fn` in the generated module, or to resolve to
//! `crate::neon::movemask`-adjacent code the per-shape emitter splices
//! alongside this fragment.
//!
//! # Shape — x86_64 AVX2
//!
//! ```text
//! {
//!     let space = _mm256_set1_epi8(b' ' as i8);
//!     let tab = _mm256_set1_epi8(b'\t' as i8);
//!     let nl = _mm256_set1_epi8(b'\n' as i8);
//!     let cr = _mm256_set1_epi8(b'\r' as i8);
//!     let mut out = 0u64;
//!     for i in 0..2 {
//!         let v = _mm256_loadu_si256(ptr.add(i * 32) as *const __m256i);
//!         let is_space = _mm256_cmpeq_epi8(v, space);
//!         let is_tab = _mm256_cmpeq_epi8(v, tab);
//!         let is_nl = _mm256_cmpeq_epi8(v, nl);
//!         let is_cr = _mm256_cmpeq_epi8(v, cr);
//!         let ws = _mm256_or_si256(
//!             _mm256_or_si256(is_space, is_tab),
//!             _mm256_or_si256(is_nl, is_cr),
//!         );
//!         let ws_mask = _mm256_movemask_epi8(ws) as u32;
//!         let ns_mask = !ws_mask as u64;
//!         out |= (ns_mask & 0xFFFF_FFFF) << (i * 32);
//!     }
//!     out
//! }
//! ```
//!
//! # Shape — scalar fallback
//!
//! ```text
//! {
//!     let stripe = core::slice::from_raw_parts(ptr, 64);
//!     let mut out = 0u64;
//!     for (i, &b) in stripe.iter().enumerate() {
//!         if !matches!(b, b' ' | b'\t' | b'\n' | b'\r') {
//!             out |= 1u64 << i;
//!         }
//!     }
//!     out
//! }
//! ```

use proc_macro2::TokenStream;
use quote::ToTokens;

/// aarch64 NEON nospace bitmap body.
///
/// Expects `chunk_ns_mask16` to be in scope. The per-shape emitter
/// emits that helper into the generated module alongside this splice
/// (or routes through `crate::emit::nospace64_scan::CHUNK_NS_MASK16_NEON`
/// which declares the helper fn body).
pub const SOURCE_NEON: &str = r#"{
    let space = unsafe { vdupq_n_u8(b' ') };
    let tab = unsafe { vdupq_n_u8(b'\t') };
    let nl = unsafe { vdupq_n_u8(b'\n') };
    let cr = unsafe { vdupq_n_u8(b'\r') };
    let bits_lo: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
    let bit_vec = unsafe { vld1q_u8(bits_lo.as_ptr()) };
    let mask0 = unsafe { chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec) };
    let mask1 = unsafe { chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec) };
    let mask2 = unsafe { chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec) };
    let mask3 = unsafe { chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec) };
    (mask0 as u64) | ((mask1 as u64) << 16) | ((mask2 as u64) << 32) | ((mask3 as u64) << 48)
}"#;

/// aarch64 `chunk_ns_mask16` helper body — splice sibling for
/// [`SOURCE_NEON`]. Parses as a [`syn::Block`] whose trailing expression
/// is `u16`. The per-shape emitter wraps this in an `unsafe fn chunk_ns_mask16(...)`
/// in the generated module.
pub const CHUNK_NS_MASK16_NEON: &str = r#"{
    let chunk = unsafe { vld1q_u8(ptr.add(off)) };
    let ws = unsafe {
        vorrq_u8(
            vorrq_u8(vceqq_u8(chunk, space), vceqq_u8(chunk, tab)),
            vorrq_u8(vceqq_u8(chunk, nl), vceqq_u8(chunk, cr)),
        )
    };
    let ns = unsafe { vmvnq_u8(ws) };
    let weighted = unsafe { vandq_u8(ns, bit_vec) };
    let low = unsafe { vaddv_u8(vget_low_u8(weighted)) } as u16;
    let high = unsafe { vaddv_u8(vget_high_u8(weighted)) } as u16;
    low | (high << 8)
}"#;

/// x86_64 AVX2 nospace bitmap body.
pub const SOURCE_AVX2: &str = r#"{
    let space = unsafe { _mm256_set1_epi8(b' ' as i8) };
    let tab = unsafe { _mm256_set1_epi8(b'\t' as i8) };
    let nl = unsafe { _mm256_set1_epi8(b'\n' as i8) };
    let cr = unsafe { _mm256_set1_epi8(b'\r' as i8) };
    let mut out = 0u64;
    let v0 = unsafe { _mm256_loadu_si256(ptr.add(0) as *const __m256i) };
    let ws0 = unsafe {
        _mm256_or_si256(
            _mm256_or_si256(_mm256_cmpeq_epi8(v0, space), _mm256_cmpeq_epi8(v0, tab)),
            _mm256_or_si256(_mm256_cmpeq_epi8(v0, nl), _mm256_cmpeq_epi8(v0, cr)),
        )
    };
    let ws_mask0 = unsafe { _mm256_movemask_epi8(ws0) } as u32;
    out |= (!ws_mask0 as u64) & 0xFFFF_FFFF;
    let v1 = unsafe { _mm256_loadu_si256(ptr.add(32) as *const __m256i) };
    let ws1 = unsafe {
        _mm256_or_si256(
            _mm256_or_si256(_mm256_cmpeq_epi8(v1, space), _mm256_cmpeq_epi8(v1, tab)),
            _mm256_or_si256(_mm256_cmpeq_epi8(v1, nl), _mm256_cmpeq_epi8(v1, cr)),
        )
    };
    let ws_mask1 = unsafe { _mm256_movemask_epi8(ws1) } as u32;
    out |= ((!ws_mask1 as u64) & 0xFFFF_FFFF) << 32;
    out
}"#;

/// Portable scalar fallback body. Requires `stripe: &[u8]` of length 64
/// in scope — the caller is expected to construct this from the raw
/// pointer.
pub const SOURCE_SCALAR: &str = r#"{
    let mut out = 0u64;
    let mut i = 0usize;
    while i < 64 {
        let b = stripe[i];
        if b != b' ' && b != b'\t' && b != b'\n' && b != b'\r' {
            out |= 1u64 << i;
        }
        i += 1;
    }
    out
}"#;

/// Parse the aarch64 NEON source into a [`TokenStream`].
pub fn fragment_neon() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_NEON)
        .expect("bbnf-simd-scan::emit::nospace64_scan: SOURCE_NEON must parse as syn::Block")
        .to_token_stream()
}

/// Parse the `chunk_ns_mask16` helper source into a [`TokenStream`].
pub fn fragment_chunk_ns_mask16_neon() -> TokenStream {
    syn::parse_str::<syn::Block>(CHUNK_NS_MASK16_NEON)
        .expect(
            "bbnf-simd-scan::emit::nospace64_scan: CHUNK_NS_MASK16_NEON must parse as syn::Block",
        )
        .to_token_stream()
}

/// Parse the x86_64 AVX2 source into a [`TokenStream`].
pub fn fragment_avx2() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_AVX2)
        .expect("bbnf-simd-scan::emit::nospace64_scan: SOURCE_AVX2 must parse as syn::Block")
        .to_token_stream()
}

/// Parse the scalar-fallback source into a [`TokenStream`].
pub fn fragment_scalar() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE_SCALAR)
        .expect("bbnf-simd-scan::emit::nospace64_scan: SOURCE_SCALAR must parse as syn::Block")
        .to_token_stream()
}
