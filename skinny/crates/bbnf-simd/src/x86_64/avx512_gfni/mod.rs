//! AVX-512 GFNI dispatch surface — Wave 1 Agent 3 new finding.
//!
//! Per SK-V3 § 5 + Wave 1 Agent 3 dispatch addendum, `vgf2p8affineqb` provides
//! a single-µop affine transform that replaces the 6× `vpcmpeqb` chain used by
//! the SK-V2 classifier.  The instruction was originally designed for Galois-
//! field cryptography (AES sub-byte transforms) but the affine form is general
//! enough to implement an arbitrary 8-bit boolean function in one µop.

pub mod classify_affine;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "gfni"));
