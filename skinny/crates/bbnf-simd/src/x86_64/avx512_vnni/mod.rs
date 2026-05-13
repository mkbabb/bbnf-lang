//! AVX-512 VNNI dispatch — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, `vpdpbusd` provides a
//! fused multiply-accumulate over four byte products in a single µop, ideal
//! for the digit-block × power-of-10 weight MAC that integer parsing needs.

pub mod digit_mac;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avx512vnni"));
