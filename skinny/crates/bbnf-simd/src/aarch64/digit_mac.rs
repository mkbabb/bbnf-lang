use core::arch::aarch64::*;
use core::arch::asm;

#[inline(always)]
pub fn parse_4_digits(bytes: [u8; 4]) -> Option<u32> {
    if !bytes.iter().all(u8::is_ascii_digit) {
        return None;
    }

    #[cfg(target_feature = "dotprod")]
    unsafe {
        return Some(parse_4_digits_dotprod(bytes));
    }

    #[cfg(not(target_feature = "dotprod"))]
    {
        let mut value = 0u32;
        for byte in bytes {
            value = value * 10 + u32::from(byte - b'0');
        }
        Some(value)
    }
}

#[target_feature(enable = "dotprod")]
#[inline]
pub unsafe fn parse_4_digits_dotprod(bytes: [u8; 4]) -> u32 {
    let mut digits = [0u8; 16];
    digits[..4].copy_from_slice(&bytes);
    for byte in &mut digits[..4] {
        *byte -= b'0';
    }

    let weights = [100u8, 10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut acc = vdupq_n_u32(0);
    let digits_vector = unsafe { vld1q_u8(digits.as_ptr()) };
    let weights_vector = unsafe { vld1q_u8(weights.as_ptr()) };
    unsafe {
        asm!(
            "udot {acc:v}.4s, {digits:v}.16b, {weights:v}.16b",
            acc = inout(vreg) acc,
            digits = in(vreg) digits_vector,
            weights = in(vreg) weights_vector,
            options(nostack, preserves_flags)
        );
    }
    let dot = vgetq_lane_u32::<0>(acc);
    dot * 10 + u32::from(digits[3])
}

#[target_feature(enable = "dotprod")]
#[inline]
pub unsafe fn dot4_i8(lhs: [i8; 4], rhs: [i8; 4]) -> i32 {
    let mut left = [0i8; 16];
    let mut right = [0i8; 16];
    left[..4].copy_from_slice(&lhs);
    right[..4].copy_from_slice(&rhs);
    let mut acc = vdupq_n_s32(0);
    let left_vector = unsafe { vld1q_s8(left.as_ptr()) };
    let right_vector = unsafe { vld1q_s8(right.as_ptr()) };
    unsafe {
        asm!(
            "sdot {acc:v}.4s, {left:v}.16b, {right:v}.16b",
            acc = inout(vreg) acc,
            left = in(vreg) left_vector,
            right = in(vreg) right_vector,
            options(nostack, preserves_flags)
        );
        vgetq_lane_s32::<0>(acc)
    }
}
