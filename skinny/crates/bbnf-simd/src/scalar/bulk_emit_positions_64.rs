#[inline]
pub unsafe fn bulk_emit_positions_64_scalar(base: u32, mut mask: u64, dst: *mut u32) -> usize {
    let mut written = 0usize;
    while mask != 0 {
        let bit = mask.trailing_zeros();
        unsafe {
            dst.add(written).write(base + bit);
        }
        mask &= mask - 1;
        written += 1;
    }
    written
}
