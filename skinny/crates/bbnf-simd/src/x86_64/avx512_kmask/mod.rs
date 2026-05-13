//! AVX-512 k-mask arithmetic dispatch — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, the `kandn / kxor /
//! kxnor / kor` family executes against the k0..k7 register file at 1-cycle
//! latency and 1/4 throughput, so the entire structural-fan can live in
//! k-registers without ever materializing a 64-bit GP value.

pub mod arithmetic;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avx512f"));
