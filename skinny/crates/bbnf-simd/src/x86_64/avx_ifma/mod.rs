//! AVX-IFMA dispatch — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, `vpmadd52luq` enables a
//! direct Eisel–Lemire 52-bit mantissa multiplication in one µop, which the
//! float-parsing fast path requires.  Available on Sapphire Rapids and later
//! Intel + Zen 4 AMD.

pub mod mantissa;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avxifma"));
