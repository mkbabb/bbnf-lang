//! Test-support decoder for the legacy 40-byte CSS L4 colour-function
//! payload.
//!
//! Production CSS L4 carries colours through
//! [`crate::runtime::css_l4::CssColor`] and its sibling typed enums;
//! the typed graph is the parity reality. This module preserves the
//! byte-blob decoder that pre-typed-CSS tests still use to drive the
//! colour-channel parity comparator against lightningcss.
//!
//! Two decoder consumers survive:
//!
//! 1. `tests/css_l4_color_view.rs` — the AW.0.5 layer-1 standalone
//!    decoder smoke tests + the layer-2 parser-acceptance tests that
//!    project against [`Color`] / [`ColorSpace`].
//! 2. `tests/lightningcss_parity.rs` — the colour-channel field-for-
//!    field parity comparator that projects bbnf [`CssColor`] variants
//!    into the `(space, c1, c2, c3, alpha)` shape lightningcss's
//!    `CssColor::RGBA` consumes.
//!
//! ## Wire format
//!
//! ```text
//! offset  field    width  dtype
//! 0       space    1 B    u8
//! 1..8    pad      7 B    —
//! 8       c1       8 B    f64 (LE bytes)
//! 16      c2       8 B    f64 (LE bytes)
//! 24      c3       8 B    f64 (LE bytes)
//! 32      alpha    8 B    f64 (LE bytes; NaN = absent)
//! —       —        —      —     40 B total, 8-byte aligned
//! ```

use core::convert::TryInto;

/// Discriminant tag for a [`Color`]'s colour space.
///
/// Variants 0–8 correspond to the CSS L4 `colorType` Alt branches
/// (`rgb/rgba/hsl/hsla/hwb/lab/lch/oklab/oklch`).
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ColorSpace {
    /// `rgb(...)` — legacy comma-separated or modern space-separated.
    Rgb = 0,
    /// `rgba(...)` — legacy with explicit alpha.
    Rgba = 1,
    /// `hsl(...)` — legacy hue/saturation/lightness.
    Hsl = 2,
    /// `hsla(...)` — legacy HSL with alpha.
    Hsla = 3,
    /// `hwb(...)` — hue/whiteness/blackness.
    Hwb = 4,
    /// `lab(...)` — CIE L\*a\*b\*.
    Lab = 5,
    /// `lch(...)` — CIE LCH.
    Lch = 6,
    /// `oklab(...)` — OKLab.
    Oklab = 7,
    /// `oklch(...)` — OKLCh.
    Oklch = 8,
}

impl ColorSpace {
    /// Decode a u8 discriminant to a [`ColorSpace`]. Returns `None`
    /// for values outside the `colorType` Alt range (0..=8).
    #[inline]
    pub fn from_u8(b: u8) -> Option<ColorSpace> {
        Some(match b {
            0 => ColorSpace::Rgb,
            1 => ColorSpace::Rgba,
            2 => ColorSpace::Hsl,
            3 => ColorSpace::Hsla,
            4 => ColorSpace::Hwb,
            5 => ColorSpace::Lab,
            6 => ColorSpace::Lch,
            7 => ColorSpace::Oklab,
            8 => ColorSpace::Oklch,
            _ => return None,
        })
    }

    /// Non-fallible decode; panics on out-of-range discriminants.
    #[inline]
    pub fn from_u8_unchecked(b: u8) -> ColorSpace {
        ColorSpace::from_u8(b).unwrap_or_else(|| panic!("invalid color space discriminant {b}"))
    }
}

/// 40 B aggregate-payload byte width pin.
pub const COLOR_PAYLOAD_BYTES: usize = 40;

/// Typed projection of a CSS L4 colour-function record's
/// `LargeAggregate` payload.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Color {
    /// Colour-space discriminant.
    pub space: ColorSpace,
    /// First colour channel (R, H, L, etc.).
    pub c1: f64,
    /// Second colour channel (G, S, a/A, etc.).
    pub c2: f64,
    /// Third colour channel (B, L, b/B, etc.).
    pub c3: f64,
    /// Alpha / opacity. `NaN` when the input carried no alpha clause.
    pub alpha: f64,
}

impl Color {
    /// Decode a 40 B `PayloadData::LargeAggregate` byte blob into a
    /// typed [`Color`]. Panics on short slices or out-of-range
    /// discriminants — malformed wire data is an emitter bug.
    #[inline]
    pub fn decode(bytes: &[u8]) -> Color {
        assert!(
            bytes.len() >= COLOR_PAYLOAD_BYTES,
            "Color::decode requires at least {COLOR_PAYLOAD_BYTES} bytes, got {}",
            bytes.len(),
        );
        let space = ColorSpace::from_u8_unchecked(bytes[0]);
        let c1 = f64::from_le_bytes(
            bytes[8..16]
                .try_into()
                .expect("Color::decode c1 slice is 8 bytes"),
        );
        let c2 = f64::from_le_bytes(
            bytes[16..24]
                .try_into()
                .expect("Color::decode c2 slice is 8 bytes"),
        );
        let c3 = f64::from_le_bytes(
            bytes[24..32]
                .try_into()
                .expect("Color::decode c3 slice is 8 bytes"),
        );
        let alpha = f64::from_le_bytes(
            bytes[32..40]
                .try_into()
                .expect("Color::decode alpha slice is 8 bytes"),
        );
        Color {
            space,
            c1,
            c2,
            c3,
            alpha,
        }
    }

    /// Decode a 40 B blob, returning `None` on short slice or
    /// out-of-range discriminant.
    #[inline]
    pub fn try_decode(bytes: &[u8]) -> Option<Color> {
        if bytes.len() < COLOR_PAYLOAD_BYTES {
            return None;
        }
        let space = ColorSpace::from_u8(bytes[0])?;
        let c1 = f64::from_le_bytes(bytes[8..16].try_into().ok()?);
        let c2 = f64::from_le_bytes(bytes[16..24].try_into().ok()?);
        let c3 = f64::from_le_bytes(bytes[24..32].try_into().ok()?);
        let alpha = f64::from_le_bytes(bytes[32..40].try_into().ok()?);
        Some(Color {
            space,
            c1,
            c2,
            c3,
            alpha,
        })
    }

    /// Project the aggregate-view tuple `(u8 space, f64 c1, f64 c2,
    /// f64 c3, f64 alpha)` into a typed [`Color`]. Panics on
    /// out-of-range `space`.
    #[inline]
    pub fn from_tuple(tuple: (u8, f64, f64, f64, f64)) -> Color {
        let (space, c1, c2, c3, alpha) = tuple;
        Color {
            space: ColorSpace::from_u8_unchecked(space),
            c1,
            c2,
            c3,
            alpha,
        }
    }

    /// Fallible tuple projection; returns `None` if `space` is out
    /// of range.
    #[inline]
    pub fn try_from_tuple(tuple: (u8, f64, f64, f64, f64)) -> Option<Color> {
        let (space, c1, c2, c3, alpha) = tuple;
        Some(Color {
            space: ColorSpace::from_u8(space)?,
            c1,
            c2,
            c3,
            alpha,
        })
    }
}
