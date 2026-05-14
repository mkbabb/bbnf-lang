#[inline]
pub fn bitmap_prefix_xor_64_neon(mask: u64, carry_in: bool) -> u64 {
    crate::scalar::bitmap_prefix_xor_64::bitmap_prefix_xor_64_scalar(mask, carry_in)
}
