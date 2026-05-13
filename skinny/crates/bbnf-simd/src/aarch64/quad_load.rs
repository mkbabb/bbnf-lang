use core::arch::aarch64::*;

#[inline(always)]
pub unsafe fn load4(ptr: *const u8) -> uint8x16x4_t {
    unsafe { vld1q_u8_x4(ptr) }
}
