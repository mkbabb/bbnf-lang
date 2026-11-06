//! Host shim for the CSS L4 grammar's fallible hex-color map.
//!
//! The CSS L4 grammar references this host function from its `hex`
//! rule's map annotation (`grammar/css/l4/color.bbnf`). The shim lives
//! here, next to the CSS L4 runtime that consumes it
//! (`crate::runtime::css_l4::builder`), rather than in the generic
//! crate root — the generic substrate carries zero grammar-specific
//! code (Lock 14).
//!
//! Single source of truth (per feedback_no_workarounds): one host
//! conversion, one resolution path, no test-side duplicate copies.
//! Keyword/value tables remain in BBNF; this module contains only the
//! algorithmic and total hexadecimal decoder.

#![allow(dead_code)]

pub(crate) fn try_parse_hex_color(s: &str) -> Option<u32> {
    let hex = s.strip_prefix('#').unwrap_or(s).as_bytes();
    Some(match hex.len() {
        3 => {
            let r = hex_digit(hex[0])?;
            let g = hex_digit(hex[1])?;
            let b = hex_digit(hex[2])?;
            ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
        }
        4 => {
            let r = hex_digit(hex[0])?;
            let g = hex_digit(hex[1])?;
            let b = hex_digit(hex[2])?;
            let a = hex_digit(hex[3])?;
            ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
        }
        6 => {
            let r = hex_byte(hex[0], hex[1])?;
            let g = hex_byte(hex[2], hex[3])?;
            let b = hex_byte(hex[4], hex[5])?;
            (r << 24) | (g << 16) | (b << 8) | 0xFF
        }
        8 => {
            let r = hex_byte(hex[0], hex[1])?;
            let g = hex_byte(hex[2], hex[3])?;
            let b = hex_byte(hex[4], hex[5])?;
            let a = hex_byte(hex[6], hex[7])?;
            (r << 24) | (g << 16) | (b << 8) | a
        }
        _ => return None,
    })
}

#[inline(always)]
fn hex_digit(b: u8) -> Option<u32> {
    match b {
        b'0'..=b'9' => Some((b - b'0') as u32),
        b'a'..=b'f' => Some((b - b'a' + 10) as u32),
        b'A'..=b'F' => Some((b - b'A' + 10) as u32),
        _ => None,
    }
}

#[inline(always)]
fn hex_byte(hi: u8, lo: u8) -> Option<u32> {
    Some((hex_digit(hi)? << 4) | hex_digit(lo)?)
}
