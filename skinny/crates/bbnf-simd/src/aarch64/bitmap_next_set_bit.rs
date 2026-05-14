#[inline]
pub fn bitmap_next_set_bit_neon(mask: u64, cursor: u8) -> u8 {
    crate::scalar::bitmap_next_set_bit::bitmap_next_set_bit_scalar(mask, cursor)
}
