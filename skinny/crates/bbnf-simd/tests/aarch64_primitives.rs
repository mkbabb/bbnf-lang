#![cfg(target_arch = "aarch64")]

use bbnf_simd::{ClassifyResult, SimdClassifier};
use core::arch::aarch64::*;

static TEST_ALPHABET: [u8; 64] = {
    let mut alphabet = [0u8; 64];
    alphabet[0] = b'{';
    alphabet[1] = b'}';
    alphabet[2] = b'[';
    alphabet[3] = b']';
    alphabet[4] = b',';
    alphabet[5] = b':';
    alphabet[6] = b'"';
    alphabet
};

fn scalar_classify(bytes: &[u8; 64]) -> ClassifyResult {
    bbnf_simd::scalar::classify_chunk(bytes, &TEST_ALPHABET)
}

#[test]
fn movemask_extracts_compare_lanes() {
    let lanes = [
        0xff, 0, 0xff, 0xff, 0, 0, 0xff, 0, 0xff, 0xff, 0, 0, 0xff, 0, 0xff, 0xff,
    ];
    let vector = unsafe { vld1q_u8(lanes.as_ptr()) };
    let mask = unsafe { bbnf_simd::aarch64::movemask::movemask_u8x16(vector) };
    assert_eq!(mask, 0b1101_0011_0100_1101);
}

#[test]
fn vqtbl_classifier_matches_scalar_block() {
    let mut bytes = [b'a'; 64];
    for (index, byte) in b"{\"a\":[1,2],\"b\":\"c\\\\d\"}\n"
        .iter()
        .copied()
        .enumerate()
    {
        bytes[index] = byte;
    }
    bytes[40] = 0x1f;

    let table = bbnf_simd::aarch64::classify_tbl4::build_lo6_table(&TEST_ALPHABET);
    let table = unsafe { bbnf_simd::aarch64::classify_tbl4::load_lo6_table(&table) };
    let neon = unsafe {
        bbnf_simd::aarch64::classify_tbl4::classify_block_from_table(
            bytes.as_ptr(),
            table,
            b'"',
            b'\\',
            0x20,
        )
    };
    assert_eq!(neon, scalar_classify(&bytes));
}

#[test]
fn selected_classifier_uses_neon_equivalent_masks() {
    let mut bytes = [b'z'; 64];
    bytes[0] = b'{';
    bytes[7] = b'"';
    bytes[15] = b'\\';
    bytes[31] = b']';
    bytes[63] = 0x00;

    let classifier = bbnf_simd::select_classifier(&TEST_ALPHABET);
    assert_eq!(classifier.classify_chunk(&bytes), scalar_classify(&bytes));
}

#[test]
fn quad_load_returns_four_contiguous_vectors() {
    let mut bytes = [0u8; 64];
    for (index, byte) in bytes.iter_mut().enumerate() {
        *byte = index as u8;
    }

    let loaded = unsafe { bbnf_simd::aarch64::quad_load::load4(bytes.as_ptr()) };
    let mut lane = [0u8; 16];
    unsafe { vst1q_u8(lane.as_mut_ptr(), loaded.2) };
    assert_eq!(lane, bytes[32..48]);
}

#[test]
fn byte_context_shifts_across_chunk_boundaries() {
    let previous_bytes = [0u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let current_bytes = [
        16u8, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
    ];
    let next_bytes = [
        32u8, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
    ];
    let previous = unsafe { vld1q_u8(previous_bytes.as_ptr()) };
    let current = unsafe { vld1q_u8(current_bytes.as_ptr()) };
    let next = unsafe { vld1q_u8(next_bytes.as_ptr()) };

    let right = unsafe { bbnf_simd::aarch64::byte_context::shift_right_one(previous, current) };
    let left = unsafe { bbnf_simd::aarch64::byte_context::shift_left_one(current, next) };

    let mut right_bytes = [0u8; 16];
    let mut left_bytes = [0u8; 16];
    unsafe {
        vst1q_u8(right_bytes.as_mut_ptr(), right);
        vst1q_u8(left_bytes.as_mut_ptr(), left);
    }

    assert_eq!(
        right_bytes,
        [15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
    );
    assert_eq!(
        left_bytes,
        [17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32]
    );
}

#[test]
fn string_block_reports_escape_and_control_masks() {
    let mut bytes = [b'a'; 16];
    bytes[1] = b'"';
    bytes[3] = b'\\';
    bytes[9] = 0x1f;

    let block = unsafe {
        bbnf_simd::aarch64::string_block::scan_string_special_block(
            bytes.as_ptr(),
            b'"',
            b'\\',
            0x20,
        )
    };
    assert_eq!(block.terminator_mask, 1 << 1);
    assert_eq!(block.escape_mask, 1 << 3);
    assert_eq!(block.control_mask, 1 << 9);
    assert_ne!(block.interesting_mask(), 0);
}

#[test]
fn string_special_block_matches_scalar_reference() {
    let mut bytes = [b'a'; 16];
    bytes[0] = b'\'';
    bytes[2] = b'\\';
    bytes[5] = 0x1f;
    bytes[7] = 0x80;
    bytes[15] = b'\'';

    let scalar = bbnf_simd::aarch64::string_block::scan_string_special_block_scalar(
        &bytes, b'\'', b'\\', 0x20,
    );
    let neon = unsafe {
        bbnf_simd::aarch64::string_block::scan_string_special_block(
            bytes.as_ptr(),
            b'\'',
            b'\\',
            0x20,
        )
    };

    assert_eq!(neon, scalar);
    assert_eq!(neon.terminator_mask, (1 << 0) | (1 << 15));
    assert_eq!(neon.escape_mask, 1 << 2);
    assert_eq!(neon.control_mask, 1 << 5);
    assert_eq!(neon.non_ascii_mask, 1 << 7);
    assert_eq!(neon.first_interesting(), Some(0));
}

#[test]
fn digit_mac_parses_four_digit_blocks() {
    assert_eq!(
        bbnf_simd::aarch64::digit_mac::parse_4_digits(*b"2026"),
        Some(2026)
    );
    assert_eq!(
        bbnf_simd::aarch64::digit_mac::parse_4_digits(*b"20x6"),
        None
    );
}

#[cfg(target_feature = "dotprod")]
#[test]
fn signed_dot_product_primitive_accumulates_four_lanes() {
    let dot = unsafe { bbnf_simd::aarch64::digit_mac::dot4_i8([1, -2, 3, -4], [5, 6, 7, 8]) };
    assert_eq!(dot, -18);
}

#[test]
fn streaming_pair_store_writes_two_words() {
    let mut words = [0u64; 2];
    unsafe {
        bbnf_simd::aarch64::cache_hints::store_pair_streaming_u64(
            words.as_mut_ptr(),
            0x1122_3344_5566_7788,
            0x99aa_bbcc_ddee_ff00,
        );
    }
    assert_eq!(words, [0x1122_3344_5566_7788, 0x99aa_bbcc_ddee_ff00]);
}
