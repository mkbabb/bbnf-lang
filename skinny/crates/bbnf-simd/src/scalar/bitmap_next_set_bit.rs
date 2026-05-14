#[inline]
pub fn bitmap_next_set_bit_scalar(mask: u64, cursor: u8) -> u8 {
    debug_assert!(cursor <= 64);
    if cursor == 64 {
        return 64;
    }
    let shifted = mask >> cursor;
    if shifted == 0 {
        64
    } else {
        cursor + shifted.trailing_zeros() as u8
    }
}
