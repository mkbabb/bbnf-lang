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
//! - [`avx512`] — x86_64 AVX-512 F + BW + VBMI2; `_mm512_maskz_compress_epi8`
//!   for index compaction.
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
//! [`scan_structural`] is the public API; it runs the best tier the CPU
//! supports ([`dispatch::selected_tier`], detected once). Every ISA tier
//! of the target architecture is compiled into a default build; kernel
//! bodies carry `#[target_feature]`, never `cfg(target_feature)`.
//! Output is byte-identical across all tiers (`tests/dispatch_tiers.rs`
//! holds every tier the host can run to [`scalar::scan`];
//! `tests/guard_page.rs` proves no tier reads past its input).

#![warn(missing_docs)]

pub mod alphabet;
pub mod compaction;
pub mod dispatch;
pub mod index;
pub mod parity;
pub mod scalar;
// The wasm32 kernel predates the shared stripe state (LW2.PERF-1 ports it).
#[cfg(any(target_arch = "aarch64", target_arch = "x86_64"))]
mod stripe;

#[cfg(target_arch = "aarch64")]
pub mod neon;

#[cfg(target_arch = "x86_64")]
pub mod avx2;

#[cfg(target_arch = "x86_64")]
pub mod avx512;

#[cfg(target_arch = "wasm32")]
pub mod wasm;

pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};
pub use dispatch::{Tier, compiled_tiers, scan_with, selected_tier};
pub use index::{StructuralIndex, next_structural_at_or_after};

/// Build the [`StructuralIndex`] for `input` under `alphabet`.
///
/// Runs the best tier the running CPU supports
/// ([`dispatch::selected_tier`]). Output is byte-identical across all
/// tiers.
pub fn scan_structural(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    dispatch::scan_selected(input, alphabet)
}
