//! AVX-512 VBMI-2 carry — 64-byte stripe escape-carry stitching.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN AVX-512 row): the backslash escape-carry the
//!     simdjson algorithm propagates between 64-bit stripes becomes a single
//!     `kshiftlq` + `korq` when the bs-mask stays in a k-register, eliminating
//!     the scalar `bs_carry: bool` ping-pong the SK-V2 NEON path uses.
//!   * Lemire + Langdale § 5.4: cross-stripe escape carry is a 1-bit feed
//!     forward; expressing it as a k-mask shift saves a memory round-trip.
//!
//! Replaces in asmjson:
//!   * The `bs_carry` boolean in `lib.rs::neon::scan_json`; this path encodes
//!     the same carry in a k-mask LSB and threads it via `kshiftlq::<1>`.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — bit-identical to `lib.rs::escape_mask_64`.
#[inline]
pub fn escape_mask_scalar(bs_mask: u64, bs_carry_in: bool) -> (u64, bool) {
    crate::escape_mask_64(bs_mask, bs_carry_in)
}

/// AVX-512 kmask escape-carry body.
///
/// # Safety
///
/// Requires `target_feature = "avx512f"`.
#[cfg(all(target_arch = "x86_64", target_feature = "avx512f"))]
#[inline]
pub unsafe fn escape_mask_kmask(_bs_mask: u64, _bs_carry_in: bool) -> (u64, bool) {
    unimplemented!("Wave 6: kshiftlq + korq escape-carry stitching");
}
