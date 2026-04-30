//! `simd-scan` — architecture-neutral SIMD structural-bitmap
//! kernel for the bbnf-lang DTA driver.
//!
//! # Role (Tranche AW-III §W5)
//!
//! The DTA driver consumes a `StructuralIndex` produced once per
//! parse: a dense list of `(position, kind)` pairs naming every byte
//! the parser may need to dispatch on. The walker stops reading
//! `src[pos]` byte-by-byte for every `ByteDispatch` arm; it reads
//! the precomputed kind at the next structural slot and jumps the
//! cursor in O(1).
//!
//! This crate owns both the kernel that builds the
//! [`StructuralIndex`] and the wire type consumed by generated parser
//! dispatch. It has no workspace dependencies.
//!
//! # Architecture
//!
//! Per-arch kernels are sibling modules:
//!
//! - [`neon`] — aarch64 NEON; nibble-LUT + wide-LUT + `vextq_u8`
//!   digraph compare + 6-op shift-XOR (or PMULL64) parity.
//! - [`avx2`] — x86_64 AVX2; `_mm256_cmpeq_epi8` + `_mm256_movemask_epi8`
//!   + PCLMULQDQ parity.
//! - [`avx512`] — x86_64 AVX-512 VBMI2; `_mm512_mask_compressstoreu_epi8`
//!   for index compaction. Opt-in via the `avx512` cargo feature.
//! - [`wasm`] — wasm32 SIMD; `i8x16.swizzle` + `i8x16.bitmask`.
//! - [`scalar`] — portable fallback; correctness reference for
//!   `tests/fuzz.rs`.
//!
//! Helpers:
//!
//! - [`alphabet`] — `StructuralAlphabet` config + `KernelShape`
//!   selector + nibble-LUT / wide-LUT helpers.
//! - [`compaction`] — bitmap → `Vec<u32>` index compaction;
//!   `tzcnt`-loop default + PEXT specialisation.
//! - [`parity`] — quote-state computation; CLMUL or shift-XOR.
//!
//! # Entry point
//!
//! [`scan_structural`] is the public API; it picks the optimal kernel
//! at runtime via `is_aarch64_feature_detected!` /
//! `is_x86_feature_detected!` and falls back to [`scalar::scan`] when
//! no SIMD path is available. Output is byte-identical across all
//! paths (verified by `tests/correctness.rs` + `tests/fuzz.rs`).

#![warn(missing_docs)]

pub mod alphabet;
pub mod compaction;
pub mod index;
pub mod parity;
pub mod scalar;

#[cfg(target_arch = "aarch64")]
pub mod neon;

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
pub mod avx2;

#[cfg(all(target_arch = "x86_64", target_feature = "avx512vbmi2"))]
pub mod avx512;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};
pub use index::{StructuralIndex, next_structural_at_or_after};

/// Build the [`StructuralIndex`] for `input` under `alphabet`.
///
/// Dispatches to the optimal per-arch kernel at runtime. The
/// `target_arch` cfg gates the *availability* of each module; the
/// runtime detection (`is_aarch64_feature_detected!` /
/// `is_x86_feature_detected!`) gates the *call*. Falls back to
/// [`scalar::scan`] when no SIMD path is available.
///
/// Output is byte-identical across all kernels (the fuzz harness
/// in `tests/fuzz.rs` enforces this).
pub fn scan_structural(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    // Empty alphabet → empty index. Skip the kernel call entirely.
    if alphabet.singletons.is_empty() && alphabet.digraph_pairs.is_empty() {
        return StructuralIndex::new();
    }

    #[cfg(target_arch = "aarch64")]
    {
        // NEON is in the aarch64 baseline (every supported aarch64
        // CPU has NEON), so no runtime detection needed.
        return neon::scan(input, alphabet);
    }

    #[cfg(all(target_arch = "x86_64", target_feature = "avx512vbmi2"))]
    {
        if std::is_x86_feature_detected!("avx512vbmi2") {
            return avx512::scan(input, alphabet);
        }
    }

    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    {
        if std::is_x86_feature_detected!("avx2") {
            return avx2::scan(input, alphabet);
        }
    }

    #[cfg(target_arch = "wasm32")]
    {
        return wasm::scan(input, alphabet);
    }

    #[allow(unreachable_code)]
    scalar::scan(input, alphabet)
}
