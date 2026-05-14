#[inline]
pub unsafe fn bulk_emit_positions_64_neon(base: u32, mask: u64, dst: *mut u32) -> usize {
    unsafe { crate::scalar::bulk_emit_positions_64::bulk_emit_positions_64_scalar(base, mask, dst) }
}
