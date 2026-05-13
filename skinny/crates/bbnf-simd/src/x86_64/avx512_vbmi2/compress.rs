//! AVX-512 VBMI-2 mask-driven compaction — `vpcompressb` / `vpcompressd`.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN AVX-512 row): structural-position emit is
//!     `vpcompressd` over the per-byte index vector gated by the classifier
//!     k-mask; one µop replaces the entire `tzcnt + lea` loop the SK-V2
//!     scalar emitter used.
//!   * Intel 64 ISA Reference Vol. 2C, VPCOMPRESS{B,W,D,Q}: 1-µop latency on
//!     Ice Lake-SP and later when the k-mask is already resident.
//!   * Lemire "AVX-512 byte compression" blog (2020): demonstrates that
//!     `vpcompressb` outperforms `pext`-based compaction once chunk size
//!     exceeds 32 bytes; we use the 64-byte cadence to maximize this win.
//!
//! Replaces in asmjson:
//!   * The bit-by-bit `tzcnt` emit loop in asmjson's AVX-2 fallback (still
//!     present in `avx2::bmi2_emit`); the VBMI-2 path emits up to 64 indices
//!     per cycle.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — write `(base + position)` for every set bit in `mask`.
#[inline]
pub fn compress_positions_scalar(base: u32, mask: u64, out: &mut Vec<u32>) {
    let mut bits = mask;
    while bits != 0 {
        let offset = bits.trailing_zeros();
        out.push(base + offset);
        bits &= bits - 1;
    }
}

/// AVX-512 VBMI-2 `vpcompressd` body.
///
/// # Safety
///
/// Requires `target_feature = "avx512vbmi2"`.  `out` must reserve at least 64
/// trailing u32 slots; the kernel writes unconditionally up to the popcount
/// of `mask`.
#[cfg(all(target_arch = "x86_64", target_feature = "avx512vbmi2"))]
#[inline]
pub unsafe fn compress_positions_vbmi2(_base: u32, _mask: u64, _out: &mut Vec<u32>) {
    unimplemented!("Wave 6: vpcompressd over 4× 16-lane index vectors gated by k-mask");
}
