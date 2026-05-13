use core::arch::aarch64::*;

use super::movemask::movemask_u8x16;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct StringBlock {
    pub quote_mask: u16,
    pub backslash_mask: u16,
    pub control_mask: u16,
    pub has_escape: bool,
}

#[inline(always)]
pub unsafe fn quote_escape_control_masks(ptr: *const u8) -> (u16, u16, u16) {
    let chunk = unsafe { vld1q_u8(ptr) };
    unsafe {
        (
            movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(b'"'))),
            movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(b'\\'))),
            movemask_u8x16(vcltq_u8(chunk, vdupq_n_u8(0x20))),
        )
    }
}

#[inline(always)]
pub unsafe fn scan_string_block(ptr: *const u8) -> StringBlock {
    let (quote_mask, backslash_mask, control_mask) = unsafe { quote_escape_control_masks(ptr) };
    StringBlock {
        quote_mask,
        backslash_mask,
        control_mask,
        has_escape: backslash_mask != 0 || control_mask != 0,
    }
}
