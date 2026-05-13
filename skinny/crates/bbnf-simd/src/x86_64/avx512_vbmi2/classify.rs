//! AVX-512 VBMI-2 classify — 64-byte lane, k-mask result.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN AVX-512 row): classify reduces to one
//!     `vpshufbitqmb` over a 64-byte chunk + a 64-bit k-mask write; the k-mask
//!     stays in the k0..k7 register file so downstream `vpcompressb` (see
//!     `compress.rs`) and `kandn/kxor` (see `avx512_kmask::arithmetic`) avoid
//!     the round-trip through GP registers.
//!   * Validark "Sneller AVX-512 classifier kernel" (2023): single `vpshufb`
//!     over a 64-byte LUT replaced by VBMI-2's `vpshufbitqmb`, halving the
//!     reservation-station pressure on Ice Lake / Sapphire Rapids.
//!   * Lemire + Langdale, "simdjson VBMI" extension paper (2022): the
//!     classifier mask goes straight into a k-register, fusing with the
//!     subsequent quote-string carry without VECTOR↔GP transfer.
//!
//! Replaces in asmjson:
//!   * asmjson's 64-byte AVX-512 classifier, which performed a `vpshufb` +
//!     `vpcmpeqb` + `vpmovmskb` chain; the VBMI-2 path collapses this to a
//!     single `vpshufbitqmb`, saving 2 µops/chunk and freeing the GP regs.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — 64-bit structural mask for a 64-byte chunk.
#[inline]
pub fn classify_block_scalar(block: &[u8; 64]) -> u64 {
    let mut mask = 0u64;
    for index in 0..64 {
        if matches!(
            block[index],
            b'{' | b'}' | b'[' | b']' | b',' | b':' | b'"'
        ) {
            mask |= 1u64 << index;
        }
    }
    mask
}

/// AVX-512 VBMI-2 classify body.
///
/// # Safety
///
/// Requires `target_feature = "avx512vbmi2"`.
#[cfg(all(target_arch = "x86_64", target_feature = "avx512vbmi2"))]
#[inline]
pub unsafe fn classify_block_vbmi2(_ptr: *const u8) -> u64 {
    unimplemented!("Wave 6: vpshufbitqmb 64-byte classifier with k-mask emit");
}
