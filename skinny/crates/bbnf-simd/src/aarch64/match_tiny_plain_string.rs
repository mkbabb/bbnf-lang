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
//! Body status: scalar reference is fully implemented (parity anchor).  The
//! NEON intrinsic body is `unimplemented!()` until Wave 1 Agent 2's kernel
//! lands; it exists today so `tests/checkasm_parity.rs` can compile against a
//! stable symbol surface.

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
/// `haystack` must point to 16 readable bytes; the kernel performs an aligned
/// `vld1q_u8`.  `table` MUST be a 64-byte low-6-bit class table built by
/// [`build_class_table_lo6`].
///
/// # SK-V3 dispatch surface
///
/// This body is stubbed today so the file compiles against the rest of the
/// crate; Wave 1 Agent 2 will fill in the three-µop TBL/shrn/rbit-clz chain.
#[inline]
pub unsafe fn match_tiny_plain_string_neon(
    _haystack: *const u8,
    _table: uint8x16x4_t,
) -> (u16, Option<u8>) {
    unimplemented!("Wave 1 Agent 2: vqtbl4q_u8 + shrn + rbit/clz tiny-string TBL path");
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
