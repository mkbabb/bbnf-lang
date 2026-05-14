use core::arch::aarch64::*;

use super::movemask::movemask_u8x16;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct StringSpecialBlock {
    pub terminator_mask: u16,
    pub escape_mask: u16,
    pub control_mask: u16,
    pub non_ascii_mask: u16,
}

impl StringSpecialBlock {
    #[inline(always)]
    pub fn interesting_mask(self) -> u16 {
        self.terminator_mask | self.escape_mask | self.control_mask | self.non_ascii_mask
    }

    #[inline(always)]
    pub fn first_interesting(self) -> Option<u8> {
        let mask = self.interesting_mask();
        if mask == 0 {
            None
        } else {
            Some(mask.trailing_zeros() as u8)
        }
    }
}

#[inline]
pub fn scan_string_special_block_scalar(
    bytes: &[u8; 16],
    terminator: u8,
    escape: u8,
    control_limit: u8,
) -> StringSpecialBlock {
    let mut block = StringSpecialBlock::default();
    for (index, byte) in bytes.iter().copied().enumerate() {
        let bit = 1u16 << index;
        if byte == terminator {
            block.terminator_mask |= bit;
        }
        if byte == escape {
            block.escape_mask |= bit;
        }
        if byte < control_limit {
            block.control_mask |= bit;
        }
        if byte >= 0x80 {
            block.non_ascii_mask |= bit;
        }
    }
    block
}

#[inline(always)]
pub unsafe fn scan_string_special_block(
    ptr: *const u8,
    terminator: u8,
    escape: u8,
    control_limit: u8,
) -> StringSpecialBlock {
    let chunk = unsafe { vld1q_u8(ptr) };
    unsafe {
        StringSpecialBlock {
            terminator_mask: movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(terminator))),
            escape_mask: movemask_u8x16(vceqq_u8(chunk, vdupq_n_u8(escape))),
            control_mask: movemask_u8x16(vcltq_u8(chunk, vdupq_n_u8(control_limit))),
            non_ascii_mask: movemask_u8x16(vcgeq_u8(chunk, vdupq_n_u8(0x80))),
        }
    }
}
