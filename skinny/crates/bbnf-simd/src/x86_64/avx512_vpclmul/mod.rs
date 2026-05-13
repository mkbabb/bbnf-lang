//! AVX-512 VPCLMUL dispatch — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, `vpclmulqdq` operating
//! on a 512-bit lane (Ice Lake-SP+) provides 4× the simdjson prefix-XOR
//! throughput: one VPCLMUL replaces four scalar PCLMUL ops, dropping the
//! string-body propagation to ~1 ns/64 B.

pub mod prefix_xor;

pub const ENABLED: bool = cfg!(all(
    target_arch = "x86_64",
    target_feature = "vpclmulqdq"
));
