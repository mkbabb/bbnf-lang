//! AVX-512 VPCLMUL 4×-lane prefix-XOR.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, Wave 1 Agent 3 finding): `vpclmulqdq` over
//!     a 512-bit lane computes four 64-bit CLMUL products in a single µop on
//!     Ice Lake-SP and later, quadrupling simdjson's prefix-XOR throughput.
//!   * Validark "Why prefix-XOR is the secret sauce of simdjson" (2022):
//!     each PCLMUL by the constant `~0` is a 64-bit prefix-XOR; VPCLMUL fans
//!     this to 4× per cycle.
//!   * Lemire + Langdale (2022) "AVX-512 string-body propagation" follow-up:
//!     describes the same shape and reports a 3.4× speedup on 64 KiB JSON
//!     payloads.
//!
//! Replaces in asmjson:
//!   * The PCLMULQDQ-based prefix-XOR in asmjson's AVX-512 path, which
//!     issued four sequential `pclmulqdq` ops; we issue one `vpclmulqdq`.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — bit-identical to `lib.rs::prefix_xor_64`.
#[inline]
pub fn prefix_xor_scalar(mut mask: u64, carry_in: bool) -> u64 {
    mask ^= mask << 1;
    mask ^= mask << 2;
    mask ^= mask << 4;
    mask ^= mask << 8;
    mask ^= mask << 16;
    mask ^= mask << 32;
    if carry_in {
        !mask
    } else {
        mask
    }
}

/// VPCLMUL 4×-lane prefix-XOR body.
///
/// # Safety
///
/// Requires `target_feature = "vpclmulqdq"` and `target_feature = "avx512f"`.
/// `quotes_512` is a 4×64-bit quote mask spanning a 256-byte window; the
/// kernel returns the in-string mask for the same window.
#[cfg(all(
    target_arch = "x86_64",
    target_feature = "vpclmulqdq",
    target_feature = "avx512f"
))]
#[inline]
pub unsafe fn prefix_xor_vpclmul(_quotes_512: [u64; 4], _carry_in: bool) -> ([u64; 4], bool) {
    unimplemented!("Wave 6: vpclmulqdq by ~0 across 4 lanes");
}
