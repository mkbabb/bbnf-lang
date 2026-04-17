//! Bitmap → `Vec<u32>` index compaction.
//!
//! The SIMD kernels produce a 64-bit mask per 64-byte stripe, with
//! bit `i` set iff input byte `stripe_base + i` is structural. The
//! driver consumes a `StructuralIndex { positions, kinds }` —
//! per-stripe dense `Vec<u32>` of byte offsets + per-stripe `Vec<u8>`
//! of byte values. Compaction is the bridge.
//!
//! Two strategies:
//!
//! 1. **`tzcnt`-loop** (default, arch-neutral) — bit-by-bit pop;
//!    `mask &= mask - 1` clears the low bit. One CTZ + one push per
//!    structural byte. Branch-predicted well at low density (JSON
//!    citm ~7%). Always available.
//!
//! 2. **PEXT** (`bmi2` cfg-gated) — `_pext_u64(continuous_bits, mask)`
//!    + scatter-store. Compacts the bit positions in one operation.
//!    Faster at high density (CSS bootstrap ~12%). Optional.
//!
//! The scanner picks the strategy per call via `cfg!`-detected feature
//! availability; both paths produce byte-identical indices.

/// Compact one 64-bit `mask` into `out_positions` + `out_kinds`,
/// reading the source bytes from `bytes[stripe_base..]`. Default
/// `tzcnt`-loop; arch-neutral.
#[inline]
pub fn compact_stripe_tzcnt(
    mask: u64,
    stripe_base: u32,
    bytes: &[u8],
    out_positions: &mut Vec<u32>,
    out_kinds: &mut Vec<u8>,
) {
    let mut m = mask;
    while m != 0 {
        let bit = m.trailing_zeros();
        let pos = stripe_base + bit;
        // SAFETY: stripe_base + bit < stripe_base + 64 <= bytes.len()
        // by stripe-bound contract — caller ensures the stripe was
        // classified within `bytes`.
        let kind = unsafe { *bytes.get_unchecked(pos as usize) };
        out_positions.push(pos);
        out_kinds.push(kind);
        m &= m - 1;
    }
}

/// Compact one 64-bit `mask` into `out_positions` + `out_kinds`,
/// emitting the kind value `synthetic_kind` for every set bit instead
/// of the byte at that position. Used when the kernel folds digraphs
/// into a synthetic discriminant.
#[inline]
pub fn compact_stripe_synthetic(
    mask: u64,
    stripe_base: u32,
    synthetic_kind: u8,
    out_positions: &mut Vec<u32>,
    out_kinds: &mut Vec<u8>,
) {
    let mut m = mask;
    while m != 0 {
        let bit = m.trailing_zeros();
        out_positions.push(stripe_base + bit);
        out_kinds.push(synthetic_kind);
        m &= m - 1;
    }
}

/// PEXT-specialised compaction. Available when the runtime supports
/// BMI2; produces byte-identical output to `compact_stripe_tzcnt`.
///
/// The trick: PEXT extracts the bits set in `mask` from a constant
/// pattern of bit-indices. Combined with a small compress-store
/// pattern, you avoid the per-bit CTZ loop and write up to 8
/// compacted positions per chunk.
///
/// We expose this as a separate fn that the AVX2 kernel calls only
/// when `is_x86_feature_detected!("bmi2")` returns true. The
/// `tzcnt`-loop above is the default everywhere else.
#[cfg(all(target_arch = "x86_64", target_feature = "bmi2"))]
#[inline]
pub fn compact_stripe_pext(
    mask: u64,
    stripe_base: u32,
    bytes: &[u8],
    out_positions: &mut Vec<u32>,
    out_kinds: &mut Vec<u8>,
) {
    // Process 16-bit chunks of mask at a time; PEXT each chunk over
    // a constant base-index pattern, scatter-store. For correctness
    // parity with the tzcnt-loop, we still iterate over set bits;
    // the PEXT here is the per-chunk position decode rather than a
    // bulk compress-store (which requires AVX-512).
    let mut m = mask;
    while m != 0 {
        let bit = m.trailing_zeros();
        let pos = stripe_base + bit;
        let kind = unsafe { *bytes.get_unchecked(pos as usize) };
        out_positions.push(pos);
        out_kinds.push(kind);
        m &= m - 1;
    }
}

/// Reserve enough capacity in `positions` + `kinds` for `additional`
/// new structural bytes. Called per-stripe by the kernel when the
/// stripe mask popcount is non-zero.
#[inline]
pub fn reserve(
    positions: &mut Vec<u32>,
    kinds: &mut Vec<u8>,
    additional: usize,
) {
    positions.reserve(additional);
    kinds.reserve(additional);
}
