//! AVX-512 VBMI-2 dispatch surface — Wave 6 root.
//!
//! Per SK-V3 § 5, VBMI-2 unlocks `vpcompressb` / `vpcompressw` (byte-grained
//! mask-driven compaction) and `vpshufbitqmb` (the wider classify primitive
//! the BITALG layer doubles).  This module is the cluster head for the
//! intrinsic-bodied AVX-512 kernels routed via Wave 6 dispatch.

pub mod carry;
pub mod classify;
pub mod compress;
pub mod mask_fuse;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avx512vbmi2"));
