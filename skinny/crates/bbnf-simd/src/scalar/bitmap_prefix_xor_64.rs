#[inline]
pub fn bitmap_prefix_xor_64_scalar(mut mask: u64, carry_in: bool) -> u64 {
    mask ^= mask << 1;
    mask ^= mask << 2;
    mask ^= mask << 4;
    mask ^= mask << 8;
    mask ^= mask << 16;
    mask ^= mask << 32;
    if carry_in {
        !mask
    } else {
        mask
    }
}
