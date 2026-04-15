//! AU.2.5 round-trip tests for the seven typed CSS dimension rules.
//!
//! W3-E wired `valueUnit` into `properties.bbnf`'s `dimension` rule so
//! `length`, `angle`, `time`, `frequency`, `resolution`, `flex`, and
//! `percentage` reach the compiled parser. Each typed dimension rule is
//! `(number, unit)` — when `number -> f64` re-activates, every site
//! materialises a `(f64, u8)` aggregate via `push_leaf_with` +
//! `PayloadData::Aggregate`.
//!
//! These tests cover the grammar-level reachability invariant (the seven
//! dimension types must all parse via the typed Alt) and serve as the
//! W3-E regression bed for tailwind's leading-dot literals.

use bbnf_derive::Parser;

/// Host functions referenced by the CSS L4 grammar's typed leaves.
#[allow(dead_code)]
mod css_types {
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
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

fn parses(input: &str) -> bool {
    CssL4Parser::parse(input).is_ok()
}

// ── Each typed dimension type ────────────────────────────────────────────

#[test]
fn length_px_parses() {
    assert!(parses("a { x: 100px; }"), "100px should parse via length");
}

#[test]
fn length_em_parses() {
    assert!(parses("a { x: 1.5em; }"), "1.5em should parse via length");
}

#[test]
fn length_rem_parses() {
    assert!(parses("a { x: 0.25rem; }"), "0.25rem should parse via length");
}

#[test]
fn angle_deg_parses() {
    assert!(parses("a { transform: rotate(45deg); }"), "45deg should parse via angle");
}

#[test]
fn angle_turn_parses() {
    assert!(parses("a { transform: rotate(0.25turn); }"), "0.25turn should parse via angle");
}

#[test]
fn time_ms_parses() {
    assert!(parses("a { transition-duration: 250ms; }"), "250ms should parse via time");
}

#[test]
fn time_s_parses() {
    assert!(parses("a { transition-duration: 1.5s; }"), "1.5s should parse via time");
}

#[test]
fn frequency_hz_parses() {
    assert!(parses("a { x: 440Hz; }"), "440Hz should parse via frequency");
}

#[test]
fn frequency_khz_parses() {
    assert!(parses("a { x: 1.5kHz; }"), "1.5kHz should parse via frequency");
}

#[test]
fn resolution_dpi_parses() {
    assert!(parses("a { x: 96dpi; }"), "96dpi should parse via resolution");
}

#[test]
fn resolution_dppx_parses() {
    assert!(parses("a { x: 2dppx; }"), "2dppx should parse via resolution");
}

#[test]
fn flex_fr_parses() {
    assert!(parses("a { x: 1fr; }"), "1fr should parse via flex");
}

#[test]
fn percentage_parses() {
    assert!(parses("a { width: 50%; }"), "50% should parse via percentage");
}

#[test]
fn unitless_int_parses() {
    assert!(parses("a { z-index: 100; }"), "100 should parse via unitless number");
}

#[test]
fn unitless_decimal_parses() {
    assert!(parses("a { line-height: 1.5; }"), "1.5 should parse via unitless number");
}

// ── Tailwind-style leading-dot regression bed ────────────────────────────

#[test]
fn opacity_leading_dot_parses() {
    // Pre-W3 regression: `.5` failed because `number -> f64` routed
    // through `scan_number_strict_f64` which rejects leading dots.
    // Held back to `scan_number_f64` (generic) until agent F lands the
    // emitter dispatch fix that respects `allow_leading_dot`.
    assert!(parses("a { opacity: .5; }"), "opacity: .5 should parse");
}

#[test]
fn keyframes_with_leading_dot_value() {
    let input = "@keyframes pulse { 50% { opacity: .5; } }";
    assert!(parses(input), "tailwind-style @keyframes with `.5` should parse");
}

#[test]
fn consecutive_keyframes_after_leading_dot() {
    // Direct regression for the tailwind.css offset 111798 failure:
    // two `@keyframes` rules separated by blank line, second carries
    // a leading-dot value.
    let input = "@keyframes ping {\n  75%, 100% {\n    transform: scale(2);\n    opacity: 0;\n  }\n}\n\n@keyframes pulse {\n  50% {\n    opacity: .5;\n  }\n}\n";
    assert!(parses(input), "tailwind keyframes regression at offset 111798");
}

// ── Negative numbers ─────────────────────────────────────────────────────

#[test]
fn length_negative_parses() {
    assert!(parses("a { margin-left: -10px; }"), "negative length should parse");
}

#[test]
fn unitless_negative_parses() {
    assert!(parses("a { z-index: -1; }"), "negative integer should parse");
}

// ── Decimal edge cases ───────────────────────────────────────────────────

#[test]
fn length_pure_decimal_parses() {
    assert!(parses("a { x: 0.5px; }"), "0.5px should parse");
}

#[test]
fn length_leading_dot_parses() {
    assert!(parses("a { x: .5px; }"), ".5px should parse via length");
}

#[test]
fn percentage_leading_dot_parses() {
    assert!(parses("a { width: .5%; }"), ".5% should parse via percentage");
}
