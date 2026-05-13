//! AVX-512 BITALG dispatch — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, `vpshufbitqmb` provides
//! a single-µop multi-class classifier: each byte of the 64-byte chunk is
//! classified against an 8-class membership predicate encoded as 64 bits of
//! the lookup operand.  Replaces the structural+quote+backslash+control
//! 4-classify fan with one instruction.

pub mod multiclass;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avx512bitalg"));
