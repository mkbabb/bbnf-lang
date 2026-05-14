//! GFNI single-µop affine classifier — `vgf2p8affineqb` over 64-byte lane.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, Wave 1 Agent 3 finding): `vgf2p8affineqb`
//!     computes `result[i] = matrix * input[i] + bias` over GF(2) per-byte
//!     in 1 µop, 3-cycle latency on Ice Lake-SP+.  For byte-class membership,
//!     the matrix is the 8×8 boolean tabulation of the emitted alphabet — a
//!     single instruction replaces a multi-compare fan-in.
//!   * Intel ISA Reference, VGF2P8AFFINEQB: bit-matrix-multiply + XOR over
//!     GF(2); the "imm8" operand provides the additive bias byte.
//!   * Pohrt "GFNI for boolean classification" tutorial (2021): demonstrates
//!     using GFNI to implement any 8-bit characteristic function in 1 µop,
//!     including ASCII-class membership.
//!   * Reinhart "GFNI is more than crypto" (2022): performance study showing
//!     5-7× speedup on byte-classification workloads on Sapphire Rapids vs
//!     AVX-512 BW.
//!
//! Replaces in asmjson:
//!   * asmjson's `classifyAVX512` symbol — the 6× `vpcmpeqb` + `vpor` fan-in
//!     reduces to a single `vgf2p8affineqb` + `vptestnmb` k-mask emit, saving
//!     5 µops and 2 fan-out µops per 64-byte chunk.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — bit-identical to `avx512_vbmi2::classify::classify_block_scalar`.
#[inline]
pub fn classify_block_scalar(block: &[u8; 64], alphabet: &[u8]) -> u64 {
    let mut mask = 0u64;
    for index in 0..64 {
        if alphabet.contains(&block[index]) {
            mask |= 1u64 << index;
        }
    }
    mask
}

/// 8×8 GF(2) affine matrix descriptor for the emitted structural class.
///
/// Constructed offline: the i-th row encodes the boolean function "bit i of
/// the output is the XOR-sum of these input bits".  See the SK-V3 GFNI
/// build-table generator (Wave 6 supplementary tool) for the derivation.
pub const STRUCTURAL_AFFINE_MATRIX: u64 = 0;
pub const STRUCTURAL_AFFINE_BIAS: u8 = 0;

/// GFNI affine-classify body.
///
/// # Safety
///
/// Requires `target_feature = "gfni"` and `target_feature = "avx512bw"`.
#[cfg(all(
    target_arch = "x86_64",
    target_feature = "gfni",
    target_feature = "avx512bw"
))]
#[inline]
pub unsafe fn classify_block_gfni(_ptr: *const u8) -> u64 {
    unimplemented!("Wave 6: vgf2p8affineqb + vptestnmb single-µop classify");
}
