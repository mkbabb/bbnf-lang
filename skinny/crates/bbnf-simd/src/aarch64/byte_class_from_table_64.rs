#[inline]
pub fn byte_class_from_table_64_neon(src: &[u8; 64], table: &[u8; 256]) -> u64 {
    crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar(src, table)
}
