use core::arch::aarch64::*;

use crate::aarch64::movemask::movemask_u8x16;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValidateStatus {
    is_valid: bool,
    complete_bytes: u8,
    bad_byte_offset: u8,
    continues: bool,
}

impl ValidateStatus {
    pub const BAD_OFFSET_NONE: u8 = u8::MAX;

    #[inline]
    pub const fn valid_complete() -> Self {
        Self {
            is_valid: true,
            complete_bytes: 16,
            bad_byte_offset: Self::BAD_OFFSET_NONE,
            continues: false,
        }
    }

    #[inline]
    pub const fn valid_prefix(complete_bytes: u8) -> Self {
        Self {
            is_valid: true,
            complete_bytes,
            bad_byte_offset: Self::BAD_OFFSET_NONE,
            continues: true,
        }
    }

    #[inline]
    pub const fn invalid(bad_byte_offset: u8) -> Self {
        Self {
            is_valid: false,
            complete_bytes: bad_byte_offset,
            bad_byte_offset,
            continues: false,
        }
    }

    #[inline]
    pub const fn is_valid(self) -> bool {
        self.is_valid
    }

    #[inline]
    pub const fn is_valid_and_complete(self) -> bool {
        self.is_valid && !self.continues && self.complete_bytes == 16
    }

    #[inline]
    pub const fn is_valid_but_continues(self) -> bool {
        self.is_valid && self.continues
    }

    #[inline]
    pub const fn complete_bytes(self) -> u8 {
        self.complete_bytes
    }

    #[inline]
    pub const fn bad_byte_offset(self) -> Option<u8> {
        if self.bad_byte_offset == Self::BAD_OFFSET_NONE {
            None
        } else {
            Some(self.bad_byte_offset)
        }
    }
}

#[inline(always)]
pub unsafe fn validate_block(ptr: *const u8) -> ValidateStatus {
    let chunk = unsafe { vld1q_u8(ptr) };
    let high_mask = unsafe { movemask_u8x16(vcgeq_u8(chunk, vdupq_n_u8(0x80))) };
    if high_mask == 0 {
        return ValidateStatus::valid_complete();
    }

    let mut bytes = [0u8; 16];
    unsafe { vst1q_u8(bytes.as_mut_ptr(), chunk) };
    validate_block_scalar(&bytes)
}

#[inline]
pub fn validate_block_scalar(bytes: &[u8; 16]) -> ValidateStatus {
    let mut cursor = 0usize;
    while cursor < bytes.len() {
        let first = bytes[cursor];
        if first < 0x80 {
            cursor += 1;
            continue;
        }

        let Some(width) = utf8_width(first) else {
            return ValidateStatus::invalid(cursor as u8);
        };
        if cursor + width > bytes.len() {
            return ValidateStatus::valid_prefix(cursor as u8);
        }
        if valid_sequence(bytes, cursor, width) {
            cursor += width;
        } else {
            return ValidateStatus::invalid(cursor as u8);
        }
    }
    ValidateStatus::valid_complete()
}

#[inline]
fn utf8_width(first: u8) -> Option<usize> {
    match first {
        0xc2..=0xdf => Some(2),
        0xe0..=0xef => Some(3),
        0xf0..=0xf4 => Some(4),
        _ => None,
    }
}

#[inline]
fn valid_sequence(bytes: &[u8; 16], cursor: usize, width: usize) -> bool {
    let first = bytes[cursor];
    match width {
        2 => is_cont(bytes[cursor + 1]),
        3 => match first {
            0xe0 => (0xa0..=0xbf).contains(&bytes[cursor + 1]) && is_cont(bytes[cursor + 2]),
            0xed => (0x80..=0x9f).contains(&bytes[cursor + 1]) && is_cont(bytes[cursor + 2]),
            _ => is_cont(bytes[cursor + 1]) && is_cont(bytes[cursor + 2]),
        },
        4 => match first {
            0xf0 => {
                (0x90..=0xbf).contains(&bytes[cursor + 1])
                    && is_cont(bytes[cursor + 2])
                    && is_cont(bytes[cursor + 3])
            }
            0xf4 => {
                (0x80..=0x8f).contains(&bytes[cursor + 1])
                    && is_cont(bytes[cursor + 2])
                    && is_cont(bytes[cursor + 3])
            }
            _ => {
                is_cont(bytes[cursor + 1])
                    && is_cont(bytes[cursor + 2])
                    && is_cont(bytes[cursor + 3])
            }
        },
        _ => false,
    }
}

#[inline]
fn is_cont(byte: u8) -> bool {
    (0x80..=0xbf).contains(&byte)
}
