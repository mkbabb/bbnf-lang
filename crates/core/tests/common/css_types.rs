//! Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map.
//!
//! The CSS L4 grammar references `css_types::parse_hex_color` from
//! its `hex` rule's map annotation. Tests that derive-instantiate
//! the CSS L4 parser must provide this function in a local scope
//! visible to the derive expansion. Previously each test file
//! carried its own inline copy (see `css_l4_color_view.rs`,
//! `lightningcss_parity.rs`, etc.); factoring the logic here keeps
//! the tests hermetic without duplication.

#![allow(dead_code)]

pub fn parse_hex_color(s: &str) -> u32 {
    let hex = s.as_bytes();
    match hex.len() {
        3 => {
            let r = hex_digit(hex[0]);
            let g = hex_digit(hex[1]);
            let b = hex_digit(hex[2]);
            ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
        }
        4 => {
            let r = hex_digit(hex[0]);
            let g = hex_digit(hex[1]);
            let b = hex_digit(hex[2]);
            let a = hex_digit(hex[3]);
            ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
        }
        6 => {
            let r = hex_byte(hex[0], hex[1]);
            let g = hex_byte(hex[2], hex[3]);
            let b = hex_byte(hex[4], hex[5]);
            (r << 24) | (g << 16) | (b << 8) | 0xFF
        }
        8 => {
            let r = hex_byte(hex[0], hex[1]);
            let g = hex_byte(hex[2], hex[3]);
            let b = hex_byte(hex[4], hex[5]);
            let a = hex_byte(hex[6], hex[7]);
            (r << 24) | (g << 16) | (b << 8) | a
        }
        _ => 0,
    }
}

#[inline(always)]
fn hex_digit(b: u8) -> u32 {
    match b {
        b'0'..=b'9' => (b - b'0') as u32,
        b'a'..=b'f' => (b - b'a' + 10) as u32,
        b'A'..=b'F' => (b - b'A' + 10) as u32,
        _ => 0,
    }
}

#[inline(always)]
fn hex_byte(hi: u8, lo: u8) -> u32 {
    (hex_digit(hi) << 4) | hex_digit(lo)
}
