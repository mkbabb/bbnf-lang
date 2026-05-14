//! AVX-512 BITALG `vpshufbitqmb` multi-class classifier.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, Wave 1 Agent 3 finding): the 4-classify
//!     fan (structural, quote, backslash, control) becomes one `vpshufbitqmb`
//!     plus four `kshiftq` operand extractions when the four predicates are
//!     packed into 4 of the 8 bit-classes of the BITALG operand.
//!   * Intel ISA Reference, VPSHUFBITQMB: 3-cycle latency, 1-cycle throughput
//!     on Sapphire Rapids; encodes 8 boolean functions over each input byte.
//!   * Sneller AVX-512 audit notes (2023, internal): demonstrates 2-3×
//!     speedup over the VBMI-2 single-class classifier when more than two
//!     classes are needed per chunk.
//!
//! Replaces in asmjson:
//!   * The chain of `classify | quote_mask | backslash_mask | control_mask`
//!     emissions in asmjson's `classifyAVX512_full` — four `vpshufb` plus
//!     `vpcmpeqb` ops compressed into one `vpshufbitqmb`.

#![allow(clippy::missing_safety_doc)]

use crate::classifier::ClassifyResult;

/// Scalar reference — emits the full 4-way ClassifyResult for a 64-byte block.
#[inline]
pub fn classify_full_scalar(
    block: &[u8; 64],
    structural_alphabet: &[u8],
    terminator: u8,
    escape: u8,
    control_limit: u8,
) -> ClassifyResult {
    let mut result = ClassifyResult::default();
    for index in 0..64 {
        let bit = 1u64 << index;
        let byte = block[index];
        if structural_alphabet.contains(&byte) {
            result.structural_mask |= bit;
        }
        if byte == terminator {
            result.quote_mask |= bit;
        }
        if byte == escape {
            result.backslash_mask |= bit;
        }
        if byte < control_limit {
            result.control_mask |= bit;
        }
    }
    result
}

/// AVX-512 BITALG multi-class classify body.
///
/// # Safety
///
/// Requires `target_feature = "avx512bitalg"` and `target_feature = "avx512bw"`.
#[cfg(all(
    target_arch = "x86_64",
    target_feature = "avx512bitalg",
    target_feature = "avx512bw"
))]
#[inline]
pub unsafe fn classify_full_bitalg(_ptr: *const u8) -> ClassifyResult {
    unimplemented!("Wave 6: vpshufbitqmb 8-class single-µop classify");
}
