#![cfg(target_arch = "aarch64")]

fn scalar_status(input: &[u8; 16]) -> bbnf_simd::aarch64::utf8::ValidateStatus {
    bbnf_simd::aarch64::utf8::validate_block_scalar(input)
}

fn neon_status(input: &[u8; 16]) -> bbnf_simd::aarch64::utf8::ValidateStatus {
    unsafe { bbnf_simd::aarch64::utf8::validate_block(input.as_ptr()) }
}

#[test]
fn utf8_block_accepts_ascii() {
    let input = *b"abcdefghijklmnop";
    let status = neon_status(&input);
    assert!(status.is_valid_and_complete());
    assert_eq!(status, scalar_status(&input));
}

#[test]
fn utf8_block_accepts_multibyte_complete() {
    let mut block = [b' '; 16];
    let text = "éé水水".as_bytes();
    block[..text.len()].copy_from_slice(text);
    let status = neon_status(&block);
    assert!(status.is_valid_and_complete());
    assert_eq!(status, scalar_status(&block));
}

#[test]
fn utf8_block_reports_continuation_across_boundary() {
    let mut block = [b'a'; 16];
    block[14] = 0xf0;
    block[15] = 0x9f;
    let status = neon_status(&block);
    assert!(status.is_valid_but_continues());
    assert_eq!(status.complete_bytes(), 14);
    assert_eq!(status, scalar_status(&block));
}

#[test]
fn utf8_block_rejects_overlong_and_surrogates() {
    for bad in [
        [0xc0, 0x80],
        [0xe0, 0x80],
        [0xed, 0xa0],
        [0xf4, 0x90],
        [0x80, 0x80],
    ] {
        let mut block = [b'a'; 16];
        block[..bad.len()].copy_from_slice(&bad);
        let status = neon_status(&block);
        assert!(!status.is_valid());
        assert_eq!(status.bad_byte_offset(), Some(0));
        assert_eq!(status, scalar_status(&block));
    }
}

#[test]
fn unescape_uxxxx_x4_matches_scalar() {
    let packed = *b"0041d83dde0000e9";
    let units = unsafe { bbnf_simd::aarch64::unescape_uxxxx::unescape_uxxxx_x4_neon(&packed) }
        .expect("valid packed quartets");
    assert_eq!(units, [0x0041, 0xd83d, 0xde00, 0x00e9]);
    assert_eq!(
        bbnf_simd::aarch64::unescape_uxxxx::join_surrogate_pair_neon(units[1], units[2]),
        Some(0x1f600)
    );
}
