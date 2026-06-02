//! Scalar reference for `BYTE_CLASS_FROM_EQ_SET_64` — the executable specification.
//!
//! Contract:
//!   Input  — 64 contiguous source bytes + a set `S` of up to 8 byte values.
//!   Output — 64-bit mask where bit `i` is set iff `src[i] ∈ S`.
//!
//! Citations:
//!   * Lock 16 (SOTA-BEAT-DESIGN, dav1d primitive-lift row): the scalar
//!     reference is the parity anchor for the checkasm admission gate; the
//!     NEON body must agree with this implementation byte-for-byte on every
//!     (src, set) pair the harness sweeps.
//!   * asmjson (Lemire et al.) `classify_chunk` inner loop: the NEON body fans
//!     a per-member compare against each broadcast member and reduces into a
//!     single 64-bit mask.  At 64-byte width this is the strict additive lift
//!     over asmjson's narrower-block path — no esoterica.
//!
//! Body status: this is the source-of-truth implementation.  The vector
//! bodies are strictly correctness-equivalent fan-outs.

/// Scalar reference for `BYTE_CLASS_FROM_EQ_SET_64`.
///
/// Returns a 64-bit mask where bit `i` is set iff `src[i] ∈ set`.
/// `set.len()` must be ≤ 8 (the contract upper bound; longer sets must be
/// admitted via a different primitive shape).
#[inline]
pub fn byte_class_from_eq_set_64_scalar(src: &[u8; 64], set: &[u8]) -> u64 {
    debug_assert!(
        set.len() <= 8,
        "BYTE_CLASS_FROM_EQ_SET_64 admits sets of size ≤ 8"
    );
    let mut mask: u64 = 0;
    for i in 0..64 {
        if set.contains(&src[i]) {
            mask |= 1u64 << i;
        }
    }
    mask
}
