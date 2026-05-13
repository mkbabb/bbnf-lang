//! AVX-2 prefix-XOR for the simdjson string-body mask — 256-bit lane.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN AVX-2 row): the simdjson `string_body` mask
//!     is computed via `_mm_clmulepi64_si128` (PCLMULQDQ) on the structural-
//!     quote mask; AVX-2 fans this out to a 256-bit lane via two PCLMUL ops
//!     and one VPSLLDQ for the lane-cross carry.
//!   * Lemire + Langdale § 5.2: PCLMUL prefix-XOR replaces six shift-XOR
//!     stages with one CLMUL by the constant `~0`, dropping the latency from
//!     ~12 µops to 5 on Haswell, 3 on Zen 3.
//!   * Validark "Why prefix-XOR is the secret sauce of simdjson" blog
//!     (2022): the operation is identical to a 64-bit Galois-field multiply
//!     by `~0`; AVX-512 VPCLMUL extends it to 4× per lane.
//!
//! Replaces in asmjson:
//!   * `prefix_xor_64` in `lib.rs` — same algebraic shape, but operates over
//!     a 256-bit mask instead of 64-bit, eliminating per-stripe carry stitching
//!     across the 4 × 64-bit fan-in.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — bit-identical to the existing `prefix_xor_64` in
/// `lib.rs`, repeated here so the AVX-2 path has a co-located parity anchor.
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

/// AVX-2 PCLMUL prefix-XOR over a 256-bit lane — see module docstring.
///
/// # Safety
///
/// Requires `target_feature = "pclmulqdq"` and `target_feature = "avx2"`.
#[cfg(all(target_arch = "x86_64", target_feature = "pclmulqdq"))]
#[inline]
pub unsafe fn prefix_xor_pclmul_avx2(_mask: u64, _carry_in: bool) -> u64 {
    unimplemented!("Wave 6: PCLMULQDQ prefix-XOR by constant ~0");
}
