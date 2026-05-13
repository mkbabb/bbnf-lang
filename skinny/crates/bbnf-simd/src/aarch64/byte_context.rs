use core::arch::aarch64::*;

#[inline(always)]
pub unsafe fn shift_right_one(previous: uint8x16_t, current: uint8x16_t) -> uint8x16_t {
    unsafe { vextq_u8(previous, current, 15) }
}

#[inline(always)]
pub unsafe fn shift_left_one(current: uint8x16_t, next: uint8x16_t) -> uint8x16_t {
    unsafe { vextq_u8(current, next, 1) }
}
