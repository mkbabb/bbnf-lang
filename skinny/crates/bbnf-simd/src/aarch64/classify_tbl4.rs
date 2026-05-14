use core::arch::aarch64::*;

use crate::classifier::ClassifyResult;

use super::movemask::movemask_u8x16;

#[inline]
pub fn build_lo6_table(alphabet: &[u8]) -> [u8; 64] {
    let mut table = [0u8; 64];
    for byte in alphabet.iter().copied().filter(|byte| *byte != 0) {
        table[(byte & 0x3f) as usize] = byte;
    }
    table
}

#[inline(always)]
pub unsafe fn load_lo6_table(table: &[u8; 64]) -> uint8x16x4_t {
    unsafe { vld1q_u8_x4(table.as_ptr()) }
}

#[inline(always)]
pub unsafe fn classify_chunk_from_table(
    ptr: *const u8,
    table: uint8x16x4_t,
    terminator: u8,
    escape: u8,
    control_limit: u8,
) -> (u16, u16, u16, u16) {
    let chunk = unsafe { vld1q_u8(ptr) };
    let low6 = unsafe { vandq_u8(chunk, vdupq_n_u8(0x3f)) };
    let class = unsafe { vqtbl4q_u8(table, low6) };
    let structural = unsafe { vandq_u8(vceqq_u8(class, chunk), vcgtq_u8(class, vdupq_n_u8(0))) };
    let terminators = unsafe { vceqq_u8(chunk, vdupq_n_u8(terminator)) };
    let escapes = unsafe { vceqq_u8(chunk, vdupq_n_u8(escape)) };
    let controls = unsafe { vcltq_u8(chunk, vdupq_n_u8(control_limit)) };
    unsafe {
        (
            movemask_u8x16(structural),
            movemask_u8x16(terminators),
            movemask_u8x16(escapes),
            movemask_u8x16(controls),
        )
    }
}

#[inline(always)]
pub unsafe fn classify_block_from_table(
    ptr: *const u8,
    table: uint8x16x4_t,
    terminator: u8,
    escape: u8,
    control_limit: u8,
) -> ClassifyResult {
    let mut result = ClassifyResult::default();
    for lane in 0..4 {
        let (structural, terminators, escapes, controls) = unsafe {
            classify_chunk_from_table(ptr.add(lane * 16), table, terminator, escape, control_limit)
        };
        let shift = lane * 16;
        result.structural_mask |= u64::from(structural) << shift;
        result.quote_mask |= u64::from(terminators) << shift;
        result.backslash_mask |= u64::from(escapes) << shift;
        result.control_mask |= u64::from(controls) << shift;
    }
    result
}
