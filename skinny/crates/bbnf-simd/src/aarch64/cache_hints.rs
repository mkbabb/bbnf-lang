use core::arch::asm;

#[inline(always)]
pub fn prefetch_read(ptr: *const u8) {
    unsafe {
        asm!("prfm pldl2strm, [{ptr}]", ptr = in(reg) ptr, options(nostack, preserves_flags, readonly));
    }
}

#[inline(always)]
pub fn prefetch_write_stream(ptr: *mut u8) {
    unsafe {
        asm!("prfm pstl1strm, [{ptr}]", ptr = in(reg) ptr, options(nostack, preserves_flags, readonly));
    }
}

#[inline(always)]
pub unsafe fn store_pair_streaming_u64(ptr: *mut u64, first: u64, second: u64) {
    unsafe {
        asm!(
            "stnp {first}, {second}, [{ptr}]",
            ptr = in(reg) ptr,
            first = in(reg) first,
            second = in(reg) second,
            options(nostack, preserves_flags)
        );
    }
}

#[inline(always)]
pub fn streaming_pair_store_hint(ptr: *mut u8) {
    prefetch_write_stream(ptr);
}
