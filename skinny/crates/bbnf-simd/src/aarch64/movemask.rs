use core::arch::aarch64::*;

#[inline(always)]
pub unsafe fn movemask_u8x16(value: uint8x16_t) -> u16 {
    let pairs = unsafe { vshrn_n_u16::<4>(vreinterpretq_u16_u8(value)) };
    let nibble_bits = unsafe { vand_u8(pairs, vdup_n_u8(0x11)) };
    let lane_bits = unsafe {
        vorr_u8(
            vand_u8(nibble_bits, vdup_n_u8(0x01)),
            vsri_n_u8::<3>(vdup_n_u8(0), nibble_bits),
        )
    };

    let widened = unsafe { vcombine_u8(lane_bits, vdup_n_u8(0)) };
    let interleaved = unsafe { vzip1q_u8(widened, widened) };

    let mut packed = [0u8; 16];
    unsafe { vst1q_u8(packed.as_mut_ptr(), interleaved) };
    let mut mask = 0u16;
    for pair in 0..8 {
        let bits = packed[pair * 2];
        mask |= u16::from(bits & 0x03) << (pair * 2);
    }
    mask
}
