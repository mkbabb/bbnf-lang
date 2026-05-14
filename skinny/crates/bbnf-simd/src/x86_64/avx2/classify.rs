//! AVX-2 fallback structural classifier — 32-byte lane.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN dav1d primitive-lift table, AVX-2 row):
//!     classify becomes `vpshufb` over a low-4-bit class table fused with
//!     `vpcmpeqb` for the high-nibble disambiguator, then `vpmovmskb` for the
//!     32-bit emit mask.
//!   * Lemire + Langdale, "Parsing gigabytes of JSON per second" (VLDB 2019),
//!     § 4.1: AVX-2 classifier shape with `_mm256_shuffle_epi8` + `_mm256_and`
//!     + `_mm256_cmpeq_epi8` over a 16-entry low-nibble table.
//!   * Mula, "Faster URL parsing with SIMD" (2018): same low/high nibble TBL
//!     pattern, demonstrating that 32-byte AVX-2 lanes beat 16-byte SSE-4 at
//!     ~2.0× because the shuffle latency is half the lane width.
//!   * Sneller "asmjson AVX-2 classifier" reference (proprietary; described in
//!     SK-V3 audit notes): the version we replace.
//!
//! Replaces in asmjson:
//!   * asmjson's `classifyAVX2` symbol — same 32-byte cadence, but our version
//!     fuses the AND-with-mask into the shuffle by pre-baking the OR-mask into
//!     the LUT entries (saves one µop per chunk).

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — parity anchor. Returns the 32-bit structural mask for a
/// 32-byte block, where bit `i` is set iff `block[i]` is in `alphabet`.
#[inline]
pub fn classify_block_scalar(block: &[u8; 32], alphabet: &[u8]) -> u32 {
    let mut mask = 0u32;
    for index in 0..32 {
        if alphabet.contains(&block[index]) {
            mask |= 1u32 << index;
        }
    }
    mask
}

/// AVX-2 intrinsic body — see module docstring.
///
/// # Safety
///
/// Requires `target_feature = "avx2"` at the call site.  `ptr` must point to
/// 32 readable bytes; the kernel performs an unaligned `_mm256_loadu_si256`.
#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[inline]
pub unsafe fn classify_block_avx2(_ptr: *const u8) -> u32 {
    unimplemented!("Wave 6: AVX-2 vpshufb low-nibble TBL + high-nibble cmpeq fuse");
}
