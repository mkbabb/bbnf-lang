//! AVX-2 fallback dispatch surface — the SK-V3 lowest-x86_64 tier.
//!
//! Per the SK-V3 packet § 5, AVX-2 is the universal x86_64 floor: every host
//! that runs the bbnf-simd dispatch surface in production has at least AVX-2
//! (Haswell, 2013 onward).  The submodules here implement the same
//! NEON-equivalent primitives the aarch64 tree provides, fan-folded over
//! AVX-2's 256-bit lane.
//!
//! Wave 6 of the SK-V3 packet routes higher-tier kernels (VBMI-2, GFNI,
//! AVX-512 mask arithmetic, VPCLMUL, IFMA, VNNI, BITALG) here as a fallback;
//! the dispatch table walks them in descending capability order.

pub mod bmi2_emit;
pub mod classify;
pub mod prefix_xor;

pub const ENABLED: bool = cfg!(all(target_arch = "x86_64", target_feature = "avx2"));
