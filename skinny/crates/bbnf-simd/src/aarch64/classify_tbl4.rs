use core::arch::aarch64::*;

use crate::classifier::ClassifyResult;

use super::movemask::movemask_u8x16;

#[inline(always)]
pub unsafe fn classify_json_chunk(ptr: *const u8) -> (u16, u16, u16, u16) {
    let chunk = unsafe { vld1q_u8(ptr) };
    let (punctuation, quotes, backslashes) = unsafe { classify_json_ascii(chunk) };
    let controls = unsafe { vcltq_u8(chunk, vdupq_n_u8(0x20)) };
    unsafe {
        (
            movemask_u8x16(punctuation),
            movemask_u8x16(quotes),
            movemask_u8x16(backslashes),
            movemask_u8x16(controls),
        )
    }
}

#[inline(always)]
pub unsafe fn classify_json_block(ptr: *const u8) -> ClassifyResult {
    let mut result = ClassifyResult::default();
    let lanes = unsafe { vld1q_u8_x4(ptr) };
    for lane in 0..4 {
        let chunk = match lane {
            0 => lanes.0,
            1 => lanes.1,
            2 => lanes.2,
            _ => lanes.3,
        };
        let (punctuation, quotes, backslashes) = unsafe { classify_json_ascii(chunk) };
        let controls = unsafe { vcltq_u8(chunk, vdupq_n_u8(0x20)) };
        let shift = lane * 16;
        unsafe {
            let punctuation_mask = u64::from(movemask_u8x16(punctuation));
            let quote_mask = u64::from(movemask_u8x16(quotes));
            result.structural_mask |= (punctuation_mask | quote_mask) << shift;
            result.quote_mask |= quote_mask << shift;
            result.backslash_mask |= u64::from(movemask_u8x16(backslashes)) << shift;
            result.control_mask |= u64::from(movemask_u8x16(controls)) << shift;
        }
    }
    result
}

#[inline(always)]
unsafe fn classify_json_ascii(chunk: uint8x16_t) -> (uint8x16_t, uint8x16_t, uint8x16_t) {
    let low6 = unsafe { vandq_u8(chunk, vdupq_n_u8(0x3f)) };
    let table = unsafe { json_ascii_table() };
    let class = unsafe { vqtbl4q_u8(table, low6) };
    let matched = unsafe { vandq_u8(vceqq_u8(class, chunk), vcgtq_u8(class, vdupq_n_u8(0))) };
    let quotes = unsafe { vandq_u8(matched, vceqq_u8(class, vdupq_n_u8(b'"'))) };
    let backslashes = unsafe { vandq_u8(matched, vceqq_u8(class, vdupq_n_u8(b'\\'))) };
    let punctuation = unsafe { vbicq_u8(matched, vorrq_u8(quotes, backslashes)) };
    (punctuation, quotes, backslashes)
}

#[inline(always)]
unsafe fn json_ascii_table() -> uint8x16x4_t {
    const TABLE: [u8; 64] = {
        let mut table = [0u8; 64];
        table[(b'"' & 0x3f) as usize] = b'"';
        table[(b',' & 0x3f) as usize] = b',';
        table[(b':' & 0x3f) as usize] = b':';
        table[(b'[' & 0x3f) as usize] = b'[';
        table[(b'\\' & 0x3f) as usize] = b'\\';
        table[(b']' & 0x3f) as usize] = b']';
        table[(b'{' & 0x3f) as usize] = b'{';
        table[(b'}' & 0x3f) as usize] = b'}';
        table
    };

    unsafe { vld1q_u8_x4(TABLE.as_ptr()) }
}
