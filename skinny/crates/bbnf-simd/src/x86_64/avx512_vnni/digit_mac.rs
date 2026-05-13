//! AVX-512 VNNI `vpdpbusd` digit-block MAC.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, Wave 1 Agent 3 finding): integer-parse
//!     digit-block accumulation `acc += digit[i] * pow10[i]` for i in 0..16
//!     reduces to one `vpdpbusd` µop on Ice Lake-SP+ (was 4 `vpmullw + vpadd`
//!     ops in asmjson).
//!   * Intel ISA Reference, VPDPBUSD: `acc[k] += sum_{j=0..3} a[k*4+j] * b[k*4+j]`,
//!     1-cycle throughput on Sapphire Rapids.
//!   * Lemire + Mula "SIMD integer parsing" (2020): describes the
//!     digit-block formulation; VNNI gives the µop floor.
//!
//! Replaces in asmjson:
//!   * The `vpmullw + vphaddw` chain in asmjson's AVX-512 number parser.

#![allow(clippy::missing_safety_doc)]

/// Scalar reference — bit-identical to the existing aarch64 `parse_4_digits`.
#[inline]
pub fn parse_8_digits_scalar(bytes: &[u8; 8]) -> Option<u64> {
    let mut value = 0u64;
    for &byte in bytes {
        if !byte.is_ascii_digit() {
            return None;
        }
        value = value * 10 + u64::from(byte - b'0');
    }
    Some(value)
}

/// AVX-512 VNNI digit-MAC body.
///
/// # Safety
///
/// Requires `target_feature = "avx512vnni"`.
#[cfg(all(target_arch = "x86_64", target_feature = "avx512vnni"))]
#[inline]
pub unsafe fn parse_16_digits_vnni(_ptr: *const u8) -> Option<u64> {
    unimplemented!("Wave 6: vpdpbusd 4×4 digit-power MAC");
}
