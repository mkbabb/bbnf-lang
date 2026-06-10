//! Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map.
//!
//! The CSS L4 grammar references this host function from its `hex`
//! rule's map annotation (`grammar/css/l4/color.bbnf`). The shim lives
//! here, next to the CSS L4 runtime that consumes it
//! (`crate::runtime::css_l4::builder`), rather than in the generic
//! crate root — the generic substrate carries zero grammar-specific
//! code (Lock 14). The generated `builder.rs` references
//! `crate::runtime::css_l4::host::parse_hex_color`; the emitter
//! (`xtask/src/regen_css.rs`) splices that path.
//!
//! Single source of truth (per feedback_no_workarounds): one host
//! shim, one resolution path, no test-side duplicate copies.

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
