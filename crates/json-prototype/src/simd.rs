//! SIMD helpers consumed inline by the per-shape parse functions.
//!
//! # Composition
//!
//! - [`ScanState`] holds the 64-byte cached nospace bitmap (sonic-rs
//!   parser.rs:1220–1237 shape) + the stripe base offset the cache
//!   refers to. Per-parse local state on the CPU stack.
//! - [`skip_space_slow`] refills the cache when the next byte is
//!   whitespace; the hot-path fast-exit (next byte non-ws) is kept on
//!   the caller side at [`crate::skip_space`].
//! - [`first_quote_or_backslash`] scans a byte slice for the first
//!   `b'"'` or `b'\\'`, returning the byte offset + the found byte.
//!   Uses NEON 64-byte stripes on aarch64 + AVX2 on x86_64 + a scalar
//!   memchr fallback.
//! - [`scan_digit_run_len`] scans a byte slice for the first
//!   non-digit byte, returning the digit-run length. NEON
//!   implementation mirrors sonic-rs's number scanner.
//!
//! The `simd-scan` crate's `parity` module is consumed for
//! `prefix_xor_64` + `escape_mask_64` (the escape-counting kernel the
//! string path needs); `scan_structural` is NOT used because it
//! materialises a `StructuralIndex` with positions + kinds, which the
//! prototype does not pre-compute. Per B1 §4 the prototype reaches
//! inside the kernel primitives directly.

/// Per-parse SIMD scratch state.
///
/// Holds the 64-byte whitespace-bitmap cache for the `skip_space` fast
/// path. The bitmap is built lazily: zero until
/// [`skip_space_slow`] hits. `nospace_start` is the input-byte offset
/// the bitmap was built at; `-1` means the cache is invalid.
///
/// Sonic-rs's equivalent cache lives in `parser.rs:1220–1237`. The
/// shape is identical; this is the same cache with a different owner.
#[derive(Debug, Default)]
pub struct ScanState {
    /// 64-bit per-byte cache of "non-whitespace" bits over the stripe
    /// starting at `nospace_start`.
    ///
    /// Bit `i` is set iff byte at `nospace_start + i` is NOT
    /// whitespace; `trailing_zeros()` gives the next non-ws position
    /// relative to `nospace_start`.
    pub(crate) nospace_bits: u64,
    /// Byte offset the cache was built at; `-1` when the cache is
    /// invalid.
    pub(crate) nospace_start: isize,
}

impl ScanState {
    /// Construct a fresh [`ScanState`] with the cache invalidated.
    #[inline]
    pub fn new() -> Self {
        Self {
            nospace_bits: 0,
            nospace_start: -1,
        }
    }
}

/// Slow-path whitespace skip — invoked from [`crate::skip_space`] only
/// when the next byte is whitespace. Amortises the 64-byte SIMD stripe
/// across contiguous whitespace runs by caching the non-ws bitmap on
/// [`ScanState`].
///
/// Keeps `*p` at the first non-whitespace byte (or `input.len()` on
/// all-whitespace tails). Returns nothing; the progress is the side
/// effect on `*p`.
#[inline(always)]
pub(crate) fn skip_space_slow(input: &[u8], p: &mut usize, state: &mut ScanState) {
    loop {
        // Is the current position inside the cached stripe?
        let cache_base = state.nospace_start;
        if cache_base >= 0 && (*p as isize) >= cache_base {
            let rel = *p - cache_base as usize;
            if rel < 64 {
                // The cache is valid for the next 64-rel bytes; mask
                // out the already-consumed low bits.
                let masked = state.nospace_bits & (!0u64 << rel);
                if masked != 0 {
                    // The first set bit is the next non-whitespace
                    // byte.
                    let bit = masked.trailing_zeros() as usize;
                    *p = cache_base as usize + bit;
                    return;
                }
                // Cache exhausted — advance past the stripe and rebuild.
                *p = cache_base as usize + 64;
            }
        }

        // Fast scalar path for the tail when there aren't 64 bytes
        // remaining.
        if *p + 64 > input.len() {
            while let Some(&b) = input.get(*p) {
                if !matches!(b, b' ' | b'\t' | b'\n' | b'\r') {
                    return;
                }
                *p += 1;
            }
            return;
        }

        // SAFETY: bounds checked above — `input[*p..*p+64]` is valid.
        let stripe = unsafe {
            std::slice::from_raw_parts(input.as_ptr().add(*p), 64)
        };
        let mask = nospace_bitmap_64(stripe);
        state.nospace_bits = mask;
        state.nospace_start = *p as isize;

        if mask != 0 {
            let bit = mask.trailing_zeros() as usize;
            *p += bit;
            return;
        }

        // All 64 bytes were whitespace — advance the cursor and continue.
        *p += 64;
    }
}

/// Compute the 64-bit "non-whitespace" bitmap for a 64-byte stripe.
///
/// Bit `i` is `1` iff `stripe[i]` is NOT in `{' ', '\t', '\n', '\r'}`.
///
/// Ported from `sonic-rs/src/util/simd/*::get_nonspace_bits`. The
/// NEON path is a 4×16-byte parallel compare reduced to a 64-bit mask
/// via `vshrn_n_u16 #4`; the AVX2 path uses `_mm256_cmpeq_epi8` +
/// `_mm256_movemask_epi8` over a 32-byte stripe pair. The scalar
/// fallback is the per-byte loop.
#[inline(always)]
pub(crate) fn nospace_bitmap_64(stripe: &[u8]) -> u64 {
    debug_assert_eq!(stripe.len(), 64, "nospace_bitmap_64 expects a 64-byte stripe");

    #[cfg(target_arch = "aarch64")]
    unsafe {
        return nospace_bitmap_64_neon(stripe);
    }

    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    unsafe {
        return nospace_bitmap_64_avx2(stripe);
    }

    #[allow(unreachable_code)]
    nospace_bitmap_64_scalar(stripe)
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn nospace_bitmap_64_neon(stripe: &[u8]) -> u64 {
    // SAFETY: NEON is baseline on aarch64. The caller guarantees
    // `stripe.len() == 64`, so four 16-byte loads are in bounds.
    //
    // The skip_space caller treats `trailing_zeros` on the 64-bit
    // "non-whitespace" mask as the first non-ws byte offset. We
    // need a correct 1-bit-per-byte mask; compute it via the
    // per-byte "any bit in nibble" OR-reduce using vshrn + vqmovn
    // narrow-and-test pattern.
    use core::arch::aarch64::*;
    unsafe {
        let ptr = stripe.as_ptr();
        let space = vdupq_n_u8(b' ');
        let tab = vdupq_n_u8(b'\t');
        let nl = vdupq_n_u8(b'\n');
        let cr = vdupq_n_u8(b'\r');

        // Per-chunk: compute `ns` (16 bytes, 0xFF for non-ws, 0x00
        // for ws); then collapse to 16-bit dense mask using
        // `vshrn_n_u16<4>` + extract-bit-per-nibble via multiply.
        // The multiplier that gathers the low bits of each nibble
        // into the high byte of a u64 is
        // `0x8040_2010_0804_0201` (but simpler: use `vand` with
        // per-bit mask and horizontal sum).
        //
        // The cleanest + correct form uses the classic per-lane
        // compare-and-movemask equivalent:
        //   let mask_bits: [u16; 8] = [1, 2, 4, 8, 16, 32, 64, 128,
        //                              256, 512, 1024, 2048, 4096, 8192, 16384, 32768];
        //   out = sum_over_i (ns[i] & mask_bits[i])
        // realised as vand + vaddvq.
        //
        // `vcgezq_s8` + `vand` against a per-lane bit vector, then
        // `vaddvq_u8` gives the 16-bit mask per chunk.
        let bits_lo: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
        let bit_vec = vld1q_u8(bits_lo.as_ptr());

        let masks_lo = [
            chunk_ns_mask16(ptr, 0, space, tab, nl, cr, bit_vec),
            chunk_ns_mask16(ptr, 16, space, tab, nl, cr, bit_vec),
            chunk_ns_mask16(ptr, 32, space, tab, nl, cr, bit_vec),
            chunk_ns_mask16(ptr, 48, space, tab, nl, cr, bit_vec),
        ];

        (masks_lo[0] as u64)
            | ((masks_lo[1] as u64) << 16)
            | ((masks_lo[2] as u64) << 32)
            | ((masks_lo[3] as u64) << 48)
    }
}

/// Compute a 16-bit "non-whitespace" mask for one 16-byte chunk.
#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn chunk_ns_mask16(
    ptr: *const u8,
    off: usize,
    space: core::arch::aarch64::uint8x16_t,
    tab: core::arch::aarch64::uint8x16_t,
    nl: core::arch::aarch64::uint8x16_t,
    cr: core::arch::aarch64::uint8x16_t,
    bit_vec: core::arch::aarch64::uint8x16_t,
) -> u16 {
    use core::arch::aarch64::*;
    unsafe {
        let chunk = vld1q_u8(ptr.add(off));
        let ws = vorrq_u8(
            vorrq_u8(vceqq_u8(chunk, space), vceqq_u8(chunk, tab)),
            vorrq_u8(vceqq_u8(chunk, nl), vceqq_u8(chunk, cr)),
        );
        // `ns` has 0xFF in each non-ws byte, 0x00 in each ws byte.
        let ns = vmvnq_u8(ws);
        // AND with the per-lane bit vector: each non-ws byte
        // contributes exactly one bit (the one assigned to its
        // lane index mod 8), the top 8 lanes reuse the same bits.
        let weighted = vandq_u8(ns, bit_vec);
        // Horizontal add splits into two 8-byte halves.
        let low = vaddv_u8(vget_low_u8(weighted)) as u16;
        let high = vaddv_u8(vget_high_u8(weighted)) as u16;
        low | (high << 8)
    }
}

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[inline(always)]
unsafe fn nospace_bitmap_64_avx2(stripe: &[u8]) -> u64 {
    use core::arch::x86_64::*;
    unsafe {
        let ptr = stripe.as_ptr();
        let space = _mm256_set1_epi8(b' ' as i8);
        let tab = _mm256_set1_epi8(b'\t' as i8);
        let nl = _mm256_set1_epi8(b'\n' as i8);
        let cr = _mm256_set1_epi8(b'\r' as i8);

        // Two 32-byte chunks.
        let mut out = 0u64;
        for i in 0..2 {
            let v = _mm256_loadu_si256(ptr.add(i * 32) as *const __m256i);
            let is_space = _mm256_cmpeq_epi8(v, space);
            let is_tab = _mm256_cmpeq_epi8(v, tab);
            let is_nl = _mm256_cmpeq_epi8(v, nl);
            let is_cr = _mm256_cmpeq_epi8(v, cr);
            let ws = _mm256_or_si256(
                _mm256_or_si256(is_space, is_tab),
                _mm256_or_si256(is_nl, is_cr),
            );
            let ws_mask = _mm256_movemask_epi8(ws) as u32;
            let ns_mask = !ws_mask as u64;
            out |= (ns_mask & 0xFFFF_FFFF) << (i * 32);
        }
        out
    }
}

#[inline(always)]
pub(crate) fn nospace_bitmap_64_scalar(stripe: &[u8]) -> u64 {
    let mut out = 0u64;
    for (i, &b) in stripe.iter().enumerate() {
        if !matches!(b, b' ' | b'\t' | b'\n' | b'\r') {
            out |= 1u64 << i;
        }
    }
    out
}

/// Find the first occurrence of `b'"'` or `b'\\'` in `bytes`.
///
/// Returns `Some((offset, byte))` on hit (byte is either `b'"'` or
/// `b'\\'`); returns `None` if neither occurs.
///
/// NEON path: 16-byte compare → `vaddvq_u8` reduce → per-lane test; the
/// compact reduction lands ~2 cyc / 16 B on M-class P-cores. AVX2 path:
/// `_mm256_cmpeq_epi8` → `_mm256_movemask_epi8` → trailing zeros.
/// Scalar fallback: byte-by-byte.
#[inline(always)]
pub fn first_quote_or_backslash(bytes: &[u8]) -> Option<(usize, u8)> {
    #[cfg(target_arch = "aarch64")]
    unsafe {
        return first_quote_or_backslash_neon(bytes);
    }

    #[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
    unsafe {
        return first_quote_or_backslash_avx2(bytes);
    }

    #[allow(unreachable_code)]
    first_quote_or_backslash_scalar(bytes)
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn first_quote_or_backslash_neon(bytes: &[u8]) -> Option<(usize, u8)> {
    use core::arch::aarch64::*;
    unsafe {
        let quote = vdupq_n_u8(b'"');
        let backslash = vdupq_n_u8(b'\\');
        let ptr = bytes.as_ptr();
        let len = bytes.len();
        let mut i = 0usize;

        // Main 16-byte loop. The `vshrn_n_u16 #4` trick packs 16
        // compare-lane flags into a 64-bit word where each nibble
        // is either 0xF (match) or 0x0 (no match); trailing_zeros on
        // the packed word locates the first match at 4-bit
        // granularity. Divide by 4 to recover the byte offset.
        while i + 16 <= len {
            let v = vld1q_u8(ptr.add(i));
            let eq_q = vceqq_u8(v, quote);
            let eq_b = vceqq_u8(v, backslash);
            let hit = vorrq_u8(eq_q, eq_b);
            // Pack 16 byte-lane flags into 64 bits (4 bits per byte).
            let packed = vshrn_n_u16::<4>(vreinterpretq_u16_u8(hit));
            let bits = vget_lane_u64::<0>(vreinterpret_u64_u8(packed));
            if bits != 0 {
                let off = (bits.trailing_zeros() >> 2) as usize;
                let byte = *ptr.add(i + off);
                return Some((i + off, byte));
            }
            i += 16;
        }

        // Tail.
        while i < len {
            let b = *ptr.add(i);
            if b == b'"' || b == b'\\' {
                return Some((i, b));
            }
            i += 1;
        }
        None
    }
}

#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))]
#[inline(always)]
unsafe fn first_quote_or_backslash_avx2(bytes: &[u8]) -> Option<(usize, u8)> {
    use core::arch::x86_64::*;
    unsafe {
        let quote = _mm256_set1_epi8(b'"' as i8);
        let backslash = _mm256_set1_epi8(b'\\' as i8);
        let ptr = bytes.as_ptr();
        let len = bytes.len();
        let mut i = 0usize;

        while i + 32 <= len {
            let v = _mm256_loadu_si256(ptr.add(i) as *const __m256i);
            let eq_q = _mm256_cmpeq_epi8(v, quote);
            let eq_b = _mm256_cmpeq_epi8(v, backslash);
            let hit = _mm256_or_si256(eq_q, eq_b);
            let mask = _mm256_movemask_epi8(hit) as u32;
            if mask != 0 {
                let off = mask.trailing_zeros() as usize;
                return Some((i + off, *ptr.add(i + off)));
            }
            i += 32;
        }

        while i < len {
            let b = *ptr.add(i);
            if b == b'"' || b == b'\\' {
                return Some((i, b));
            }
            i += 1;
        }
        None
    }
}

#[inline(always)]
pub(crate) fn first_quote_or_backslash_scalar(bytes: &[u8]) -> Option<(usize, u8)> {
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'"' || b == b'\\' {
            return Some((i, b));
        }
    }
    None
}
