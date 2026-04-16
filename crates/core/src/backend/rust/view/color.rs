//! View-layer `Color` projection for CSS L4 colour-function records.
//!
//! AW.0.5: typed Rust-side projection of the `LargeAggregate` 40 B
//! payload emitted by `colorFunction`, `colorFn`, and `colorMix`
//! rules. The grammar declares these rules with `-> input : Color`;
//! [`super::named_types::RustNamedTypes`] resolves the name to a
//! `(U8, F64, F64, F64, F64)` scalar tuple; the IR layout pass
//! plans a 40 B layout at the `LARGE_PAYLOAD_MAX` cap; the emitter
//! routes through `PayloadData::LargeAggregate`; this module owns
//! the decoder that reads the blob back into a typed struct.
//!
//! ## Wire format
//!
//! ```text
//! offset  field    width  dtype  source
//! 0       space    1 B    u8     colorType / colorSpace / mixSpace
//! 1..8    pad      7 B    —      zero-init (plan_layout align 1→8)
//! 8       c1       8 B    f64    first colour channel (LE bytes)
//! 16      c2       8 B    f64    second colour channel (LE bytes)
//! 24      c3       8 B    f64    third colour channel (LE bytes)
//! 32      alpha    8 B    f64    alpha / percentage (LE bytes; NaN = absent)
//! —       —        —      —     40 B total, 8-byte aligned
//! ```
//!
//! The 7 B alignment pad follows automatically from `plan_layout`'s
//! natural-alignment arithmetic — the u8 at offset 0 bumps the next
//! field to `(0 + 1 + 7) & !7 = 8`. No hand-packing arm; the payload
//! layout is the same algebraic shape as every other aggregate.
//!
//! ## NaN alpha discipline
//!
//! When a colour input carries no alpha channel — e.g.
//! `rgb(255 128 0)` (no `/ alpha` clause), `oklch(0.7 0.1 240)` —
//! the emitter's alpha-less epilogue arm writes
//! `f64::NAN.to_le_bytes()` into the alpha slot. Downstream
//! consumers check [`Color::alpha`] via
//! [`f64::is_nan`](core::primitive::f64::is_nan) and either pick a
//! default (CSS's usual `1.0` opaque) or propagate the absence.
//! [`Color::decode`] itself passes the bytes through verbatim; NaN
//! recognition is the consumer's policy.
//!
//! Until the emitter's alpha-less arm lands (W0b), the alpha slot
//! may carry zero bytes for alpha-less inputs — `Color::decode`
//! returns whatever is there. The decoder is stable; only the emit
//! side migrates.

use core::convert::TryInto;

/// Discriminant tag for a `Color`'s colour space.
///
/// Discriminants pin to the values emitted by the grammar's
/// `colorType` / `colorSpace` / `mixSpace` Alt branches — lightningcss
/// compatibility is a projection function above this enum, not a
/// mapping baked into the enum ordering.
///
/// - Variants 0–8 correspond to `colorType` branches
///   (`rgb/rgba/hsl/hsla/hwb/lab/lch/oklab/oklch`).
/// - Variants 10+ are reserved for `colorFn`'s
///   `colorSpace` discriminants shifted by +10 at decode time so a
///   single [`ColorSpace`] enum covers both grammar rules. (The
///   full shift projection lands alongside the `.as_color()` Alt
///   dispatch in a follow-up wave; this module's
///   [`Color::decode`] reads whatever u8 the emitter wrote.)
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
    /// `oklab(...)` — OKLab (better-perceptual L\*a\*b\*).
    Oklab = 7,
    /// `oklch(...)` — OKLCh (better-perceptual LCH).
    Oklch = 8,
}

impl ColorSpace {
    /// Decode a u8 discriminant to a [`ColorSpace`].
    ///
    /// Returns `None` for values outside the `colorType` Alt range
    /// (0..=8). Callers typically want to propagate the malformed
    /// discriminant as a parse error rather than panic.
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
    ///
    /// Use in contexts where the u8 provably came from the emitter's
    /// `colorType` Alt range (i.e. the rule is `colorFunction`'s
    /// field decode). Unit tests that fabricate byte buffers should
    /// prefer [`Self::from_u8`] + `.expect(...)` for clearer panic
    /// messages.
    #[inline]
    pub fn from_u8_unchecked(b: u8) -> ColorSpace {
        ColorSpace::from_u8(b)
            .unwrap_or_else(|| panic!("invalid color space discriminant {b}"))
    }
}

/// Typed projection of a CSS L4 colour-function record's
/// `LargeAggregate` payload.
///
/// Produced by [`Color::decode`] on the 40 B byte blob the emitter
/// wrote via `PayloadData::LargeAggregate`. All four channels use
/// `f64` — CSS Color Level 4 admits `<number>` / `<percentage>` /
/// `<angle>` in every slot (e.g. `hsl(180deg 50% 50%)`'s hue does
/// not fit `u8`), and one struct covers all nine colour-function
/// families without boxing.
///
/// ## lightningcss equivalence
///
/// `lightningcss::values::color::CssColor` uses a tagged enum with
/// per-family structs (`RGBA` = all-u8, `Predefined` / `LAB` = f32
/// channels, etc.). Equivalence against lightningcss is a
/// projection function applied at the W5 parity harness, not baked
/// into this struct. For `RGBA` the projection is:
///
/// ```text
/// bbnf.space == ColorSpace::Rgb | ColorSpace::Rgba
/// bbnf.c1/c2/c3 == red/green/blue as f64
/// bbnf.alpha == (alpha as f64) / 255.0   // lightningcss stores u8 alpha
/// ```
///
/// f32↔f64 precision mismatches in the Predefined/LAB families are
/// resolved at parity-compare time via `(f32 as f64)` promotion,
/// not by changing bbnf's internal storage.
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
    /// Alpha / opacity. `NaN` when the input carried no alpha
    /// channel (see module docs §NaN alpha discipline). Consumers
    /// check via [`f64::is_nan`](core::primitive::f64::is_nan) and
    /// pick their default policy.
    pub alpha: f64,
}

/// Byte width of the colour-function aggregate payload. Pins the
/// [`Color::decode`] contract against the grammar's 40 B layout.
pub const COLOR_PAYLOAD_BYTES: usize = 40;

impl Color {
    /// Decode a 40 B `PayloadData::LargeAggregate` byte blob into a
    /// typed [`Color`].
    ///
    /// Wire format (see module docs for the offset table):
    ///
    /// - `bytes[0]` → `space` (u8 discriminant, panics if out of range).
    /// - `bytes[8..16]` → `c1` (f64 LE bytes).
    /// - `bytes[16..24]` → `c2` (f64 LE bytes).
    /// - `bytes[24..32]` → `c3` (f64 LE bytes).
    /// - `bytes[32..40]` → `alpha` (f64 LE bytes; may be `NaN`).
    ///
    /// # Panics
    ///
    /// Panics if `bytes.len() < COLOR_PAYLOAD_BYTES` — the caller is
    /// responsible for sourcing the slice from
    /// `Tape::payload_bytes(rec, 40)`, which returns `None` rather
    /// than a short slice. Also panics on an out-of-range `space`
    /// discriminant; malformed wire data is an emitter bug, not a
    /// runtime possibility on correct parses.
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

    /// Decode a 40 B blob, returning `None` when the slice is too
    /// short or the `space` discriminant is out of range.
    ///
    /// Preferred by consumers that want to propagate a decode
    /// failure rather than abort. The `Tape::payload_bytes` reader
    /// path already returns `Option<&[u8]>`, so the natural chain is
    /// `tape.payload_bytes(rec, 40).map(Color::decode)` when the
    /// caller trusts the parse OR
    /// `tape.payload_bytes(rec, 40).and_then(Color::try_decode)`
    /// when it does not.
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
    /// f64 c3, f64 alpha)` — the return type of
    /// `<ColorFunctionView>::value()` — into a typed [`Color`].
    ///
    /// Complements [`Self::decode`] for callers that already hold
    /// the tuple form (the codegen's `.value()` accessor returns
    /// the tuple directly from the byte slice without exposing the
    /// blob). Panics on an out-of-range `space` discriminant; use
    /// [`Self::try_from_tuple`] for the fallible variant.
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
