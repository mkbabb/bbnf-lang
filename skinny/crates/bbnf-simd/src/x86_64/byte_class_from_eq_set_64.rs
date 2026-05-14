//! Rust FFI shim for `BYTE_CLASS_FROM_EQ_SET_64` — AVX-512 BW body lives in
//! `byte_class_from_eq_set_64.asm`.
//!
//! Citations:
//!   * asmjson `classify_chunk` inner loop: at 64-byte width on AVX-512 BW,
//!     this is the strict additive lift over asmjson's 32-byte AVX2 path —
//!     one zmm load, up-to-eight broadcast/compare pairs, kor reductions,
//!     one kmovq spill.
//!   * Lock 16 (SOTA-BEAT-DESIGN): the scalar reference in
//!     `src/scalar/byte_class_from_eq_set_64.rs` is the executable
//!     specification; on hosts without AVX-512 BW we fall back to it.
//!
//! The FFI symbol is provided by `byte_class_from_eq_set_64.asm`, assembled
//! and archived by `build.rs` on x86_64 hosts.

#[cfg(all(target_arch = "x86_64", target_feature = "avx512bw"))]
unsafe extern "C" {
    fn byte_class_from_eq_set_64_avx512(src: *const u8, set_ptr: *const u8, set_len: usize) -> u64;
}

/// Safe Rust wrapper around the AVX-512 BW kernel.
///
/// Returns a 64-bit mask where bit `i` is set iff `src[i] ∈ set`.
/// `set.len()` must be ≤ 8; longer sets are admitted via a different
/// primitive shape.
///
/// On hosts that do not enable `target_feature = "avx512bw"`, this
/// transparently falls back to the scalar reference (the executable
/// specification).
#[inline]
pub fn byte_class_from_eq_set_64(src: &[u8; 64], set: &[u8]) -> u64 {
    debug_assert!(
        set.len() <= 8,
        "BYTE_CLASS_FROM_EQ_SET_64 admits sets of size ≤ 8"
    );

    #[cfg(all(target_arch = "x86_64", target_feature = "avx512bw"))]
    {
        if !set.is_empty() {
            // SAFETY: src is a 64-byte array; set is non-empty with len <= 8.
            // The kernel reads exactly `set_len` bytes from `set_ptr` and 64
            // bytes from `src`.  Both are non-null and live for the call.
            return unsafe {
                byte_class_from_eq_set_64_avx512(src.as_ptr(), set.as_ptr(), set.len())
            };
        }
        return 0;
    }

    #[cfg(not(all(target_arch = "x86_64", target_feature = "avx512bw")))]
    {
        crate::scalar::byte_class_from_eq_set_64::byte_class_from_eq_set_64_scalar(src, set)
    }
}
