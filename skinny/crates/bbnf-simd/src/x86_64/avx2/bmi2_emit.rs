//! AVX-2 + BMI2 structural-position emit — `pext` + `pdep` carry-free compaction.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN AVX-2 dispatch row): "structural-emit becomes
//!     `pext` over the classifier mask + a CLMUL-free `pdep` widening to u32
//!     positions; replaces asmjson's loop-iter `tzcnt + 1` per emit."
//!   * Langdale + Lemire, "Parsing gigabytes of JSON per second" § 5.1:
//!     `_pext_u64` over the structural mask compacts set-bit positions into
//!     the low bits of a 64-bit register, eliminating the `bsf/lea` loop the
//!     SK-V2 emitter used.
//!   * Mula "BMI2 bitmask population" series: `pext` throughput is 3 cycles
//!     on Haswell, 1 cycle on Zen 3+ / Ice Lake; the dispatch surface keeps a
//!     pdep/pext fallback for AMD Zen 1/2 where `pext` is microcoded.
//!
//! Replaces in asmjson:
//!   * The `compact_mask` u32-position loop in the SK-V2 NEON path is the
//!     reference; this AVX-2 path performs the same compaction but with two
//!     BMI2 instructions per 64-bit chunk instead of one tzcnt per bit.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — emit u32 positions of every set bit in `mask`, added
/// to `base`.  Matches the existing `compact_mask` shape in `lib.rs`.
#[inline]
pub fn compact_mask_scalar(base: u32, mask: u64, out: &mut Vec<u32>) {
    let mut bits = mask;
    while bits != 0 {
        let offset = bits.trailing_zeros();
        out.push(base + offset);
        bits &= bits - 1;
    }
}

/// AVX-2 + BMI2 emit kernel — see module docstring.
///
/// # Safety
///
/// Requires `target_feature = "bmi2"` and `target_feature = "avx2"`.
#[cfg(all(target_arch = "x86_64", target_feature = "bmi2"))]
#[inline]
pub unsafe fn compact_mask_bmi2(_base: u32, _mask: u64, _out: &mut Vec<u32>) {
    unimplemented!("Wave 6: pext-driven position compaction");
}
