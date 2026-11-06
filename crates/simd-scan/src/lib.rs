//! `simd-scan` — structural-bitmap kernels for the bbnf-lang DTA
//! driver on Apple AArch64 and wasm32 browsers.
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
//! The retained kernels are sibling modules:
//!
//! - [`neon`] — Apple AArch64 NEON; nibble-LUT + wide-LUT + `vextq_u8`
//!   digraph compare + 6-op shift-XOR (or PMULL64) parity.
//! - [`wasm`] — wasm32 SIMD; `i8x16.swizzle` + `i8x16.bitmask`.
//! - [`scalar`] — portable correctness reference for tests and
//!   differential validation; it is not a production dispatch target.
//!
//! Helpers:
//!
//! - [`alphabet`] — `StructuralAlphabet` config + `KernelShape`
//!   selector + nibble-LUT / wide-LUT helpers.
//! - [`compaction`] — bitmap → `Vec<u32>` index compaction via a
//!   trailing-zero loop.
//! - [`parity`] — quote-state computation; Apple PMULL or shift-XOR.
//!
//! # Entry point
//!
//! [`scan_structural`] is the public production API. It uses NEON on
//! Apple AArch64 and WebAssembly SIMD on wasm32. Other production targets
//! are rejected at compile time. Output is byte-identical to the
//! [`scalar`] correctness reference (verified by
//! `tests/correctness.rs` + `tests/fuzz.rs`).

#![warn(missing_docs)]

#[cfg(not(any(
    all(target_arch = "aarch64", target_os = "macos"),
    all(target_arch = "wasm32", target_os = "unknown")
)))]
compile_error!(
    "simd-scan production kernels support only Apple AArch64 and wasm32; scalar is a correctness reference, not a production target"
);

pub mod alphabet;
pub mod compaction;
pub mod index;
pub mod parity;
pub mod scalar;

#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
pub mod neon;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub mod wasm;

pub use alphabet::{KernelShape, NibbleLut, StructuralAlphabet, WideLut};
pub use index::{StructuralIndex, next_structural_at_or_after};

/// Build the [`StructuralIndex`] for `input` under `alphabet`.
///
/// Dispatches at compile time to NEON on Apple AArch64 or WebAssembly SIMD
/// on wasm32. Other production targets fail to compile.
///
/// Output is byte-identical across all kernels (the fuzz harness
/// in `tests/fuzz.rs` enforces this).
#[cfg(any(
    all(target_arch = "aarch64", target_os = "macos"),
    all(target_arch = "wasm32", target_os = "unknown")
))]
pub fn scan_structural(input: &[u8], alphabet: &StructuralAlphabet) -> StructuralIndex {
    // Empty alphabet → empty index. Skip the kernel call entirely.
    if alphabet.singletons.is_empty() && alphabet.digraph_pairs.is_empty() {
        return StructuralIndex::new();
    }

    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    // NEON is in the aarch64 baseline (every supported aarch64 CPU has
    // NEON), so no runtime detection needed.
    let index = neon::scan(input, alphabet);

    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    let index = wasm::scan(input, alphabet);

    index
}
