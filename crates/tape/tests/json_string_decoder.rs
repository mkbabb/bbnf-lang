//! AW-III.W1.A — JSON string-escape decoder kernel tests.
//!
//! Per-kernel coverage isolated from the PSI dispatch wiring. The
//! `crates/core/tests/json_decode.rs` integration tests exercise the
//! full lift + walk pipeline; this file pin-points the kernel itself.

use tape::decoders::json_string::decode_into;

fn decode(input: &[u8]) -> String {
    let mut buf = vec![0u8; 4 + input.len()];
    // SAFETY: buf.len() == 4 + input.len() ≥ 4 + body.len(); the
    // kernel never writes past that bound.
    let len = unsafe { decode_into(input, buf.as_mut_ptr(), 0, buf.len()) };
    let stamped = u32::from_le_bytes(buf[0..4].try_into().unwrap()) as usize;
    assert_eq!(stamped, len, "length prefix matches return value");
    String::from_utf8(buf[4..4 + len].to_vec()).unwrap()
}

#[test]
fn decode_plain_round_trips() {
    assert_eq!(decode(b"\"hello\""), "hello");
}

#[test]
fn decode_simple_escapes() {
    assert_eq!(decode(b"\"a\\nb\\tc\\\"d\\\\e\\/f\""), "a\nb\tc\"d\\e/f");
}

#[test]
fn decode_u_escape_bmp() {
    assert_eq!(decode(b"\"\\u0041\""), "A");
    assert_eq!(decode(b"\"\\u00e9\""), "\u{00E9}");
    assert_eq!(decode(b"\"\\u4e2d\""), "\u{4E2D}");
}

#[test]
fn decode_surrogate_pair_yields_4_byte_utf8() {
    assert_eq!(decode(b"\"\\uD83D\\uDE00\""), "\u{1F600}");
    assert_eq!(decode(b"\"\\uD83C\\uDF89\""), "\u{1F389}");
}

#[test]
fn decode_lone_high_surrogate_yields_replacement() {
    assert_eq!(decode(b"\"\\uD83D\""), "\u{FFFD}");
}

#[test]
fn decode_lone_low_surrogate_yields_replacement() {
    assert_eq!(decode(b"\"\\uDC00\""), "\u{FFFD}");
}

#[test]
fn decode_empty_string_round_trips() {
    assert_eq!(decode(b"\"\""), "");
}
