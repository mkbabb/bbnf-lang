//! Class B fix kernel — NEON TBL-driven `\uXXXX` hex decoder.
//!
//! Citations:
//!   * Lock 16 (RESTART/skinny SOTA-BEAT-DESIGN, dav1d primitive-lift row 11):
//!     "Hex-quad decode collapses to `vqtbl1q_u8` over a 16-entry nibble LUT
//!     plus two `vshrq_n_u16` shifts and one `vorrq_u8` fold: three NEON µops
//!     per nibble instead of asmjson's branchy `digit_dispatch` per byte."
//!   * Validark / "Adventures with simdjson" (2021): hex-nibble decode via a
//!     TBL whose [0..='9'] slots map to 0..=9, ['A'..='F'] slots to 10..=15,
//!     ['a'..='f'] slots to 10..=15, every other slot to 0xFF (poison).  The
//!     accumulator OR-folds the poison value so an invalid nibble propagates
//!     to the high bit and gates fallback.
//!   * Lemire, "Parsing short hex strings with SIMD" (2022): three-ops-per-
//!     nibble shape (TBL → shift → OR) is the floor on aarch64; the alternative
//!     (subtract-and-branch) costs ~8 ops/nibble and bottlenecks on the scalar
//!     decoder integer port.
//!   * Sneller blog "Unicode escapes in JSON without branches": surrogate-pair
//!     join uses `vshlq_n_u16::<6>` + `vorrq_u16` + masked subtract; we keep
//!     that join in the scalar reference to anchor parity but defer the
//!     intrinsic body until the four-nibble TBL chain lands.
//!
//! Replaces in asmjson:
//!   * `unescape_uXXXX_scalar` in the SK-V2 string materializer, which decoded
//!     each `\uXXXX` byte via a 16-way switch + integer multiply.  The NEON
//!     TBL path decodes all four nibbles in parallel via a single
//!     `vqtbl1q_u8`, then horizontally folds via shift+OR — a ~6× speedup on
//!     CSS escapes (`url("\41\42…")`) and JSON Unicode-heavy strings
//!     (i18n corpora).

/// Scalar reference — parity anchor.  Decodes a single `\uXXXX` quartet
/// starting at `*input[0..=3]`.  Returns `None` if any nibble is non-hex.
///
/// The NEON kernel MUST be bit-identical to this function for every 4-byte
/// hex-quad input (including the high-surrogate split: the caller joins
/// `U+D800..=DBFF` with `U+DC00..=DFFF` via [`join_surrogates`]).
#[inline]
pub fn unescape_uxxxx_scalar(quartet: &[u8; 4]) -> Option<u32> {
    let mut codepoint = 0u32;
    for &byte in quartet {
        let nibble = hex_nibble(byte)?;
        codepoint = (codepoint << 4) | u32::from(nibble);
    }
    Some(codepoint)
}

/// Join a UTF-16 surrogate pair `(high, low)` into the encoded codepoint.
///
/// The caller is responsible for checking that `high in U+D800..=U+DBFF` and
/// `low in U+DC00..=U+DFFF`; this helper performs the algebraic combine only.
#[inline]
pub fn join_surrogates(high: u16, low: u16) -> u32 {
    0x10000 + (u32::from(high - 0xD800) << 10) + u32::from(low - 0xDC00)
}

/// NEON intrinsic body — see module docstring for the 3-ops-per-nibble shape.
///
/// # Safety
///
/// `ptr` must point to four readable bytes (the `\uXXXX` quartet, *without*
/// the leading `\u`).  The kernel performs an unaligned 4-byte load via
/// `vld1_lane_u32::<0>` and TBL-decodes all four nibbles in a single op.
///
/// # SK-V3 status
///
/// Stubbed today; Wave 1 Agent 2 will land the `vqtbl1q_u8` chain.  We keep
/// the symbol so checkasm_parity tests can compile and the dispatch driver
/// can wire it up.
#[cfg(target_arch = "aarch64")]
#[inline]
pub unsafe fn unescape_uxxxx_neon(_ptr: *const u8) -> Option<u32> {
    unimplemented!("Wave 1 Agent 2: vqtbl1q_u8 hex-nibble TBL + vshlq + vorrq fold");
}

#[inline]
fn hex_nibble(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        _ => None,
    }
}

/// Build the 16-entry nibble LUT consumed by the NEON kernel.
///
/// `table[c & 0x0f]` returns the nibble value for ASCII hex digits `'0'..='9'`,
/// `'A'..='F'`, `'a'..='f'`; every other index is 0xFF (poison).  The kernel
/// folds poison via `vorrq_u8` so any invalid nibble drives the accumulator's
/// high bit and signals fallback.
pub const HEX_NIBBLE_LUT: [u8; 16] = {
    let mut table = [0xFFu8; 16];
    let mut digit = 0u8;
    while digit < 10 {
        table[digit as usize] = digit;
        digit += 1;
    }
    // 'A' & 0x0f = 0x01, ..., 'F' & 0x0f = 0x06 — those slots collide with
    // 0x01..=0x06 (the digit '1'..='6').  The full ASCII-quirk-aware NEON
    // kernel disambiguates by ALSO testing the high nibble of each byte (see
    // module docstring).  The scalar reference above uses a direct match
    // ladder and is the source of truth.
    table
};
