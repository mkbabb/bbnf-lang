#[inline]
pub fn byte_class_from_table_64_scalar(src: &[u8; 64], table: &[u8; 256]) -> u64 {
    let mut mask = 0u64;
    for index in 0..64 {
        if table[src[index] as usize] != 0 {
            mask |= 1u64 << index;
        }
    }
    mask
}
