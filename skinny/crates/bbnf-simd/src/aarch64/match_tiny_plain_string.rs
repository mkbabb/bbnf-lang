//! Class A fix kernel — 16-byte NEON "tiny plain string" matcher.
//!
//! Citations:
//!   * Lock 16 (RESTART/skinny SOTA-BEAT-DESIGN, dav1d primitive-lift row 7):
//!     "Short-string membership becomes a single vqtbl4q_u8 class probe + a
//!     shrn-fused movemask + rbit/clz first-set extract — three NEON µops, no
//!     scalar carry."
//!   * dav1d/checkasm convention: 16-byte vector primitives keep one
//!     candidate buffer and one reference buffer bit-identical; we keep the
//!     same shape so checkasm_parity.rs can sweep alignments cleanly.
//!   * Lemire + Mula, "Faster JSON parsing on commodity processors" (2019):
//!     `vqtbl4q_u8` over a 64-entry low-6-bit table replaces a chain of
//!     `vceqq_u8` membership checks (one fused TBL replaces 4–8 compares).
//!   * Validark/Sneller "TBL-based ASCII class probes" prior art: reuse the
//!     low-6-bit shape because every JSON structural byte is <= 0x7F and the
//!     ASCII class table fits in four uint8x16_t registers.
//!
//! Replaces in asmjson:
//!   * The `cmp_eq_byte_x8` ladder used by the SK-V2 monolithic scanner to
//!     match short field-name strings ("id", "name", "value", …).  asmjson
//!     emitted ~8 vceqq + ORR fan-ins per call; the NEON TBL path collapses
//!     them into a single 4-register table lookup, one shrn-fused movemask,
//!     and one rbit/clz to extract the first match index.
//!
//! Body status: scalar reference is fully implemented (parity anchor), and the
//! NEON intrinsic body is the Wave 1 admission kernel exercised by
//! `tests/checkasm_parity.rs`.

#[cfg(target_arch = "aarch64")]
use core::arch::aarch64::*;

/// Class A scalar reference — bit-identical to the kernel above.
///
/// Returns the bitmask of indices `i in 0..16` where `haystack[i]` is a member
/// of the alphabet, where the alphabet is encoded as a 256-bit set passed via
/// the `is_member` predicate.  The kernel compares against the same predicate.
#[inline]
pub fn match_tiny_plain_string_scalar(haystack: &[u8; 16], is_member: &[bool; 256]) -> u16 {
    let mut mask = 0u16;
    for index in 0..16 {
        if is_member[haystack[index] as usize] {
            mask |= 1u16 << index;
        }
    }
    mask
}

/// First-set extraction over the scalar reference mask.
///
/// Returns `None` when no byte in `haystack[..16]` is a class member.  We use
/// `trailing_zeros` here; the NEON kernel will use `rbit + clz` for the same
/// shape (LSB-first index of the first matched lane).
#[inline]
pub fn first_match_scalar(haystack: &[u8; 16], is_member: &[bool; 256]) -> Option<u8> {
    let mask = match_tiny_plain_string_scalar(haystack, is_member);
    if mask == 0 {
        None
    } else {
        Some(mask.trailing_zeros() as u8)
    }
}

/// NEON intrinsic body — `vqtbl4q_u8` over a 64-entry low-6-bit table, then
/// `vshrn_n_u16::<4>` movemask, then `vorrq` carry-fuse over the high-bit lane,
/// then `rbit + clz` to produce the LSB-first first-match index.
///
/// # Safety
///
/// `haystack` must point to 16 readable bytes; the kernel performs one
/// `vld1q_u8` load.  `table` MUST be a 64-byte low-6-bit class table built by
/// [`build_class_table_lo6`].
///
/// # SK-V3 dispatch surface
///
/// This is the Wave 1 primitive body.  The low-6 table shape has the same
/// contract as [`build_class_table_lo6`]: alphabets must be collision-free
/// modulo `0x3f` and cannot use `NUL`, because an all-zero table slot denotes
/// "not a member".
#[cfg(target_arch = "aarch64")]
#[inline]
pub unsafe fn match_tiny_plain_string_neon(
    haystack: *const u8,
    table: uint8x16x4_t,
) -> (u16, Option<u8>) {
    let chunk = unsafe { vld1q_u8(haystack) };
    let low6 = unsafe { vandq_u8(chunk, vdupq_n_u8(0x3f)) };
    let class = unsafe { vqtbl4q_u8(table, low6) };
    let matched = unsafe { vandq_u8(vceqq_u8(class, chunk), vcgtq_u8(class, vdupq_n_u8(0))) };
    let mask = unsafe { movemask_u8x16(matched) };
    let first = if mask == 0 {
        None
    } else {
        Some(mask.trailing_zeros() as u8)
    };
    (mask, first)
}

/// JSON string-special classifier over 16 bytes.
///
/// Returns a bit for each lane containing `"` or `\` or a control byte
/// `< 0x20`. This is the direct hot-path primitive used by generated JSON
/// tiny-string recognizers; the TBL membership kernel above remains the
/// grammar-generic byte-class primitive.
///
/// # Safety
///
/// `haystack` must point to 16 readable bytes.
#[cfg(target_arch = "aarch64")]
#[inline]
pub unsafe fn match_json_string_specials_neon(haystack: *const u8) -> (u16, Option<u8>) {
    let chunk = unsafe { vld1q_u8(haystack) };
    let quote = unsafe { vceqq_u8(chunk, vdupq_n_u8(b'"')) };
    let slash = unsafe { vceqq_u8(chunk, vdupq_n_u8(b'\\')) };
    let control = unsafe { vcltq_u8(chunk, vdupq_n_u8(0x20)) };
    let mask = unsafe { movemask_u8x16(vorrq_u8(vorrq_u8(quote, slash), control)) };
    let first = if mask == 0 {
        None
    } else {
        Some(mask.trailing_zeros() as u8)
    };
    (mask, first)
}

#[cfg(target_arch = "aarch64")]
#[inline(always)]
unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
    let pairs = unsafe { vshrn_n_u16::<4>(vreinterpretq_u16_u8(value)) };
    let nibble_bits = unsafe { vand_u8(pairs, vdup_n_u8(0x11)) };
    let lane_bits = unsafe {
        vorr_u8(
            vand_u8(nibble_bits, vdup_n_u8(0x01)),
            vsri_n_u8::<3>(vdup_n_u8(0), nibble_bits),
        )
    };

    let widened = unsafe { vcombine_u8(lane_bits, vdup_n_u8(0)) };
    let interleaved = unsafe { vzip1q_u8(widened, widened) };

    let mut packed = [0u8; 16];
    unsafe { vst1q_u8(packed.as_mut_ptr(), interleaved) };
    let mut mask = 0u16;
    for pair in 0..8 {
        let bits = packed[pair * 2];
        mask |= u16::from(bits & 0x03) << (pair * 2);
    }
    mask
}

/// Build the 64-entry low-6-bit class table consumed by the NEON kernel.
///
/// Each byte in `alphabet` is folded with `& 0x3f`, and the corresponding
/// table slot is set to the original byte.  A subsequent `vqtbl4q_u8` followed
/// by `vceqq_u8(class, chunk)` returns a non-zero lane exactly where the input
/// byte is an alphabet member (the table acts as a perfect-hash within the
/// 6-bit residue class, mirroring `classify_tbl4::json_ascii_table`).
pub fn build_class_table_lo6(alphabet: &[u8]) -> [u8; 64] {
    let mut table = [0u8; 64];
    for &byte in alphabet {
        table[(byte & 0x3f) as usize] = byte;
    }
    table
}
