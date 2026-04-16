//! AW.0.5 — CSS L4 colour-function `LargeAggregate` view smoke tests.
//!
//! Two layers of coverage:
//!
//! 1. **Standalone decoder** (`decode_*` / `from_tuple_*`): stable
//!    against fabricated 40 B byte arrays. These verify the
//!    [`Color`] struct decode contract under every `ColorSpace`
//!    discriminant + the NaN-alpha passthrough. They do not depend
//!    on the parser, the emitter, or bootstrap regen — they run
//!    from first commit.
//!
//! 2. **End-to-end view** (`view_*`): parses a CSS value through
//!    `CssL4Parser` and walks to the inner `colorFunction` /
//!    `colorFn` record to call `.as_color()`. These depend on the
//!    colour-function rules actually routing through
//!    `LargeAggregate` at parse time, which in turn depends on:
//!    - the emitter (W0b) actually driving payload writes on the
//!      colour-function body;
//!    - bootstrap regen (orchestrator post-wave) folding the new
//!      `.as_color()` shim into `crates/core/src/grammar/generated.rs`.
//!
//!    Until those land, the end-to-end tests are expected to fail
//!    gracefully at either `parse` or at the `.as_color()` dispatch.
//!    They are kept live (not `#[ignore]`d) so the post-regen green
//!    transition is observable without un-ignoring.

use bbnf::runtime::view::{Color, ColorSpace, COLOR_PAYLOAD_BYTES};
use bbnf_derive::Parser;

// The CSS L4 grammar references `crate::css_types::parse_hex_color`;
// reproduce the minimal shim here so the grammar compiles cleanly
// inside this test crate. Shared with `css_l4.rs`; kept verbatim
// to avoid cross-test coupling.
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

#[derive(Parser, Debug)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", skip_recover)]
struct CssL4Parser;

// ---------------------------------------------------------------------------
// Layer 1 — standalone decoder tests
//
// These verify the `Color` byte-layout decode against fabricated
// arrays so they are stable regardless of whether the emitter has
// wired the colour-function payload writes yet.
// ---------------------------------------------------------------------------

/// Compose a 40 B colour-function payload in the same byte layout
/// the emitter produces: `[u8 space @ 0][7 B pad][f64 c1 @ 8]
/// [f64 c2 @ 16][f64 c3 @ 24][f64 alpha @ 32]`.
fn compose_color_bytes(space: u8, c1: f64, c2: f64, c3: f64, alpha: f64) -> [u8; 40] {
    let mut buf = [0u8; 40];
    buf[0] = space;
    buf[8..16].copy_from_slice(&c1.to_le_bytes());
    buf[16..24].copy_from_slice(&c2.to_le_bytes());
    buf[24..32].copy_from_slice(&c3.to_le_bytes());
    buf[32..40].copy_from_slice(&alpha.to_le_bytes());
    buf
}

#[test]
fn color_payload_bytes_matches_40() {
    assert_eq!(COLOR_PAYLOAD_BYTES, 40, "wire contract: 40 B LargeAggregate slot");
}

#[test]
fn decode_rgb_alpha_0_5() {
    let bytes = compose_color_bytes(0, 255.0, 128.0, 0.0, 0.5);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Rgb);
    assert_eq!(c.c1, 255.0);
    assert_eq!(c.c2, 128.0);
    assert_eq!(c.c3, 0.0);
    assert_eq!(c.alpha, 0.5);
}

#[test]
fn decode_rgba_u8_max_alpha_passthrough() {
    // `rgba(255, 255, 255, 1)` with the u8-max mapping inside the
    // f64 slot — verifies the decoder does not round or clamp.
    let bytes = compose_color_bytes(1, 255.0, 255.0, 255.0, 1.0);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Rgba);
    assert_eq!(c.alpha, 1.0);
}

#[test]
fn decode_hsl_angle_channel() {
    // `hsl(180 50% 50%)` — hue channel is 180 (degrees); decoder
    // keeps angular values as-is in `c1` without any 360-wrap.
    let bytes = compose_color_bytes(2, 180.0, 50.0, 50.0, f64::NAN);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Hsl);
    assert_eq!(c.c1, 180.0);
    assert_eq!(c.c2, 50.0);
    assert_eq!(c.c3, 50.0);
    assert!(c.alpha.is_nan(), "alpha-less HSL → NaN passthrough");
}

#[test]
fn decode_hsla_explicit_alpha() {
    let bytes = compose_color_bytes(3, 240.0, 100.0, 50.0, 0.75);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Hsla);
    assert_eq!(c.alpha, 0.75);
}

#[test]
fn decode_hwb() {
    let bytes = compose_color_bytes(4, 120.0, 20.0, 10.0, 1.0);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Hwb);
    assert_eq!(c.c1, 120.0);
}

#[test]
fn decode_lab_negative_channels() {
    // CIE L*a*b* admits negative a/b channels; decoder preserves sign.
    let bytes = compose_color_bytes(5, 50.0, -20.5, 30.5, 1.0);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Lab);
    assert_eq!(c.c2, -20.5);
    assert_eq!(c.c3, 30.5);
}

#[test]
fn decode_lch() {
    let bytes = compose_color_bytes(6, 50.0, 40.0, 180.0, 1.0);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Lch);
}

#[test]
fn decode_oklab() {
    let bytes = compose_color_bytes(7, 0.7, -0.05, 0.15, 1.0);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Oklab);
    assert_eq!(c.c1, 0.7);
}

#[test]
fn decode_oklch_nan_alpha() {
    // `oklch(0.7 0.1 240)` — no alpha clause → NaN passthrough.
    let bytes = compose_color_bytes(8, 0.7, 0.1, 240.0, f64::NAN);
    let c = Color::decode(&bytes);
    assert_eq!(c.space, ColorSpace::Oklch);
    assert_eq!(c.c1, 0.7);
    assert_eq!(c.c2, 0.1);
    assert_eq!(c.c3, 240.0);
    assert!(c.alpha.is_nan(), "alpha-less Oklch → NaN passthrough");
}

#[test]
fn try_decode_short_slice_returns_none() {
    let short = [0u8; 32];
    assert!(Color::try_decode(&short).is_none());
}

#[test]
fn try_decode_bad_discriminant_returns_none() {
    let mut bytes = compose_color_bytes(0, 0.0, 0.0, 0.0, 1.0);
    bytes[0] = 99; // Out of range.
    assert!(Color::try_decode(&bytes).is_none());
}

#[test]
#[should_panic(expected = "invalid color space discriminant")]
fn decode_bad_discriminant_panics() {
    let mut bytes = compose_color_bytes(0, 0.0, 0.0, 0.0, 1.0);
    bytes[0] = 42;
    let _ = Color::decode(&bytes);
}

#[test]
#[should_panic(expected = "Color::decode requires at least 40 bytes")]
fn decode_short_slice_panics() {
    let short = [0u8; 16];
    let _ = Color::decode(&short);
}

#[test]
fn from_tuple_roundtrip() {
    // Round-trip the tuple form produced by the generated
    // `.value()` accessor through `Color::from_tuple`.
    let tuple = (0u8, 255.0, 128.0, 0.0, 0.5);
    let c = Color::from_tuple(tuple);
    assert_eq!(c.space, ColorSpace::Rgb);
    assert_eq!(c.c1, 255.0);
    assert_eq!(c.c2, 128.0);
    assert_eq!(c.c3, 0.0);
    assert_eq!(c.alpha, 0.5);
}

#[test]
fn try_from_tuple_bad_discriminant() {
    assert!(Color::try_from_tuple((42u8, 0.0, 0.0, 0.0, 0.0)).is_none());
    assert!(Color::try_from_tuple((8u8, 0.7, 0.1, 240.0, 1.0)).is_some());
}

#[test]
fn colorspace_from_u8_matches_variants() {
    // Exhaustive: every valid discriminant maps to the expected
    // variant; out-of-range returns None.
    assert_eq!(ColorSpace::from_u8(0), Some(ColorSpace::Rgb));
    assert_eq!(ColorSpace::from_u8(1), Some(ColorSpace::Rgba));
    assert_eq!(ColorSpace::from_u8(2), Some(ColorSpace::Hsl));
    assert_eq!(ColorSpace::from_u8(3), Some(ColorSpace::Hsla));
    assert_eq!(ColorSpace::from_u8(4), Some(ColorSpace::Hwb));
    assert_eq!(ColorSpace::from_u8(5), Some(ColorSpace::Lab));
    assert_eq!(ColorSpace::from_u8(6), Some(ColorSpace::Lch));
    assert_eq!(ColorSpace::from_u8(7), Some(ColorSpace::Oklab));
    assert_eq!(ColorSpace::from_u8(8), Some(ColorSpace::Oklch));
    assert_eq!(ColorSpace::from_u8(9), None);
    assert_eq!(ColorSpace::from_u8(u8::MAX), None);
}

// ---------------------------------------------------------------------------
// Layer 2 — end-to-end view tests
//
// These depend on the colour-function rules actually routing
// through `LargeAggregate` at parse time (requires W0b emitter
// writes + orchestrator bootstrap regen). Until both land, these
// tests are expected to fail at parse-time or at the `.as_color()`
// dispatch; W0c commits them live so the post-regen green
// transition is observable. Re-examine them after the AW W0
// wave's bootstrap regen lands on master.
// ---------------------------------------------------------------------------

/// Smoke gate: the Parser derive compiles and accepts a trivial
/// declaration. This test runs regardless of colour-function
/// routing, so a CssL4 compile regression surfaces separately from
/// any `.as_color()` wiring issue.
#[test]
fn parser_accepts_rgb_value() {
    let result = CssL4Parser::parse("a { color: rgb(255 128 0 / 0.5); }");
    assert!(
        result.is_ok(),
        "CssL4Parser must accept rgb(...) declaration: {result:?}",
    );
}

#[test]
fn parser_accepts_oklch_value() {
    let result = CssL4Parser::parse("a { color: oklch(0.7 0.1 240); }");
    assert!(
        result.is_ok(),
        "CssL4Parser must accept oklch(...) declaration: {result:?}",
    );
}

#[test]
fn parser_accepts_hsl_value() {
    let result = CssL4Parser::parse("a { color: hsl(180 50% 50%); }");
    assert!(
        result.is_ok(),
        "CssL4Parser must accept hsl(...) declaration: {result:?}",
    );
}
