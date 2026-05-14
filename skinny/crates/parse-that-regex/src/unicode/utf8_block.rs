#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValidateStatus {
    pub is_valid: bool,
    pub complete_bytes: u8,
    pub bad_byte_offset: Option<u8>,
    pub continues: bool,
}

impl ValidateStatus {
    #[inline]
    pub const fn is_valid_and_complete(self) -> bool {
        self.is_valid && !self.continues && self.complete_bytes == 16
    }

    #[inline]
    pub const fn is_valid_but_continues(self) -> bool {
        self.is_valid && self.continues
    }
}

#[inline]
pub fn validate_block(input: &[u8; 16]) -> ValidateStatus {
    #[cfg(target_arch = "aarch64")]
    {
        let status = unsafe { bbnf_simd::aarch64::utf8::validate_block(input.as_ptr()) };
        return ValidateStatus {
            is_valid: status.is_valid(),
            complete_bytes: status.complete_bytes(),
            bad_byte_offset: status.bad_byte_offset(),
            continues: status.is_valid_but_continues(),
        };
    }

    #[allow(unreachable_code)]
    crate::unicode::utf8_hoehrmann::validate_block(input)
}
