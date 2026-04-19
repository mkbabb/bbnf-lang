//! CSS value-type AST — `bbnf::css::values` surface.
//!
//! AX.W1.B: typed value representation covering the shapes the CSS L4
//! grammar can project from its tape. Leans on the existing
//! [`super::super::color::Color`] view decoder for colour-function
//! records so the 40 B `LargeAggregate` payload already produced by
//! the grammar flows into the Value API without double-decode.
//!
//! Isomorphism is field-complete per invariant 18. Divergences against
//! `lightningcss::values` are logged in
//! `docs/tranches/AX/parity/css_divergence.md` §values.

use std::borrow::Cow;

use crate::backend::rust::view::color::{Color, ColorSpace};

/// A CSS value — the right-hand side of a `property: value` pair.
///
/// A declaration's value list is a `Vec<Value>`; multi-value
/// shorthands (`margin: 10px 20px`, `font: bold 14px/1.5 sans-serif`)
/// round-trip by holding each token as a separate `Value` in source
/// order.
#[derive(Clone, Debug, PartialEq)]
pub enum Value<'i> {
    /// A CSS-wide keyword: `initial`, `inherit`, `unset`, `revert`,
    /// `revert-layer`.
    Global(CssGlobalKeyword),
    /// A numeric-with-unit dimension (`10px`, `1.5em`, `45deg`).
    Dimension(Dimension),
    /// A percentage (`50%`).
    Percentage(f64),
    /// A unitless number (`0`, `1.5`, `-0.75`).
    Number(f64),
    /// An integer (`42`, `-5`). Distinct from [`Self::Number`] to
    /// carry parse-time provenance when the grammar sees no decimal
    /// point; downstream consumers can still promote to f64.
    Integer(i64),
    /// A CSS string literal (`"foo"`, `'bar'`).
    String(Cow<'i, str>),
    /// A raw identifier (property-value keyword not covered by the
    /// typed keyword tables).
    Ident(Cow<'i, str>),
    /// A dashed identifier (`-webkit-box`, `-moz-initial`).
    DashedIdent(Cow<'i, str>),
    /// A hex colour (`#abc`, `#1234`, `#f0f8ff`, `#f0f8ffff`).
    ///
    /// Stored as 0xRRGGBBAA for round-trip fidelity with the grammar
    /// payload `hex -> parse_hex_color(input) : u32`.
    Hex(u32),
    /// A named CSS colour (`red`, `aliceblue`).
    ///
    /// Stored as 0xRRGGBBAA matching the `namedColor -> 0xRRGGBBAAu32`
    /// grammar payload.
    NamedColor {
        /// The source-literal name (lowercase, as CSS keyword).
        name: Cow<'i, str>,
        /// The resolved 0xRRGGBBAA value.
        rgba: u32,
    },
    /// A parameterised colour: `rgb(...)`, `hsl(...)`, `oklch(...)`,
    /// `color(srgb ...)`, `color-mix(in lch, ...)`, etc.
    ///
    /// Payload mirrors the existing [`Color`] decoder so the 40 B
    /// `LargeAggregate` produced by `colorFunction`/`colorFn`/`colorMix`
    /// rules projects without re-parsing.
    Color(ColorValue),
    /// `var(--name, fallback)` custom-property reference.
    Var {
        /// The `--name` identifier (including leading `--`).
        name: Cow<'i, str>,
        /// The comma-separated fallback tokens, verbatim.
        fallback: Option<Cow<'i, str>>,
    },
    /// `calc(expr)` / `min(...)` / `max(...)` / `clamp(...)`.
    ///
    /// Stores the raw token text; structural expression analysis is
    /// out-of-scope for the initial Value API (bbnf's grammar captures
    /// the balanced parens but not the expression tree).
    Calc {
        /// Which flavour: `calc`, `min`, `max`, `clamp`.
        kind: CalcKind,
        /// The raw expression body (between outer parens, excluding
        /// the function name and outer delimiters).
        body: Cow<'i, str>,
    },
    /// `url(...)` reference.
    Url {
        /// The URL value (unescaped, quotes stripped).
        url: Cow<'i, str>,
    },
    /// Arbitrary function call `func(...)` that doesn't match a typed
    /// grammar branch (e.g. `translateX(...)`, `hsv(...)`,
    /// `scroll(...)`).
    Function {
        /// The function name (before `(`).
        name: Cow<'i, str>,
        /// The raw argument list, verbatim (no commas interpreted).
        args: Cow<'i, str>,
    },
    /// A property-list separator `,` (kept as a token so value-list
    /// round-trip preserves comma placement).
    Comma,
    /// A property-list separator `/` (background shorthand).
    Slash,
    /// Any token the grammar recognised but the Value API doesn't
    /// structurally project yet (the grammar's catch-all branch).
    ///
    /// Holds the verbatim source substring so consumers that need
    /// the token text still have it.
    Raw(Cow<'i, str>),
}

/// CSS-wide global keyword.
///
/// Discriminant order matches `css_l4/keywords.bbnf` globalKeyword:
/// Inherit=2, Initial=3, Unset=4, Revert=1, RevertLayer=0.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CssGlobalKeyword {
    /// `revert-layer`.
    RevertLayer = 0,
    /// `revert`.
    Revert = 1,
    /// `inherit`.
    Inherit = 2,
    /// `initial`.
    Initial = 3,
    /// `unset`.
    Unset = 4,
}

/// Numeric-with-unit dimension (`10px`, `1.5em`, `45deg`).
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Dimension {
    /// The numeric part.
    pub value: f64,
    /// The unit.
    pub unit: Unit,
}

/// CSS length / angle / time / frequency / resolution / flex unit.
///
/// Discriminant order matches `css_l4/value-unit.bbnf`; lightningcss
/// separates these across `LengthValue`, `Angle`, `Time`, `Frequency`,
/// `Resolution`, `CSSNumber`. bbnf unifies them into one enum and the
/// consumer dispatches by [`Unit::kind`].
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Unit {
    // --- Absolute length ---
    /// `px` — CSS pixel.
    Px = 0,
    /// `cm` — centimetre.
    Cm = 1,
    /// `mm` — millimetre.
    Mm = 2,
    /// `in` — inch.
    In = 3,
    /// `pt` — point.
    Pt = 4,
    /// `pc` — pica.
    Pc = 5,
    /// `Q` — quarter-millimetre.
    Q = 6,
    // --- Font-relative length ---
    /// `em`.
    Em = 10,
    /// `rem`.
    Rem = 11,
    /// `ex`.
    Ex = 12,
    /// `ch`.
    Ch = 13,
    /// `cap`.
    Cap = 14,
    /// `ic`.
    Ic = 15,
    /// `lh`.
    Lh = 16,
    /// `rlh`.
    Rlh = 17,
    // --- Viewport-relative length ---
    /// `vh`.
    Vh = 20,
    /// `vw`.
    Vw = 21,
    /// `vmin`.
    Vmin = 22,
    /// `vmax`.
    Vmax = 23,
    /// `vb`.
    Vb = 24,
    /// `vi`.
    Vi = 25,
    /// `svh`.
    Svh = 26,
    /// `svw`.
    Svw = 27,
    /// `lvh`.
    Lvh = 28,
    /// `lvw`.
    Lvw = 29,
    /// `dvh`.
    Dvh = 30,
    /// `dvw`.
    Dvw = 31,
    // --- Container-relative length ---
    /// `cqw`.
    Cqw = 40,
    /// `cqh`.
    Cqh = 41,
    /// `cqi`.
    Cqi = 42,
    /// `cqb`.
    Cqb = 43,
    /// `cqmin`.
    Cqmin = 44,
    /// `cqmax`.
    Cqmax = 45,
    // --- Angle ---
    /// `deg`.
    Deg = 60,
    /// `rad`.
    Rad = 61,
    /// `grad`.
    Grad = 62,
    /// `turn`.
    Turn = 63,
    // --- Time ---
    /// `s` — seconds.
    S = 70,
    /// `ms` — milliseconds.
    Ms = 71,
    // --- Frequency ---
    /// `Hz`.
    Hz = 80,
    /// `kHz`.
    Khz = 81,
    // --- Resolution ---
    /// `dpi`.
    Dpi = 90,
    /// `dpcm`.
    Dpcm = 91,
    /// `dppx` / `x`.
    Dppx = 92,
    // --- Flex ---
    /// `fr` — flex fraction.
    Fr = 100,
    // --- Percentage (used when Dimension holds a percent; kept for
    // unit-round-trip, though `Value::Percentage(..)` is the usual
    // storage) ---
    /// `%`.
    Percent = 255,
}

/// Coarse categorisation of [`Unit`].
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnitKind {
    /// Absolute length (`px`, `cm`, ...).
    AbsoluteLength = 0,
    /// Font-relative length (`em`, `rem`, ...).
    FontRelative = 1,
    /// Viewport-relative length (`vh`, `vw`, ...).
    ViewportRelative = 2,
    /// Container-relative length (`cqw`, ...).
    ContainerRelative = 3,
    /// Angle (`deg`, `rad`, ...).
    Angle = 4,
    /// Time (`s`, `ms`).
    Time = 5,
    /// Frequency (`Hz`, `kHz`).
    Frequency = 6,
    /// Resolution (`dpi`, `dpcm`, `dppx`).
    Resolution = 7,
    /// Flex fraction (`fr`).
    Flex = 8,
    /// Percent (`%`).
    Percent = 9,
}

impl Unit {
    /// Coarse category.
    pub const fn kind(self) -> UnitKind {
        match self {
            Self::Px | Self::Cm | Self::Mm | Self::In | Self::Pt | Self::Pc | Self::Q => {
                UnitKind::AbsoluteLength
            }
            Self::Em
            | Self::Rem
            | Self::Ex
            | Self::Ch
            | Self::Cap
            | Self::Ic
            | Self::Lh
            | Self::Rlh => UnitKind::FontRelative,
            Self::Vh
            | Self::Vw
            | Self::Vmin
            | Self::Vmax
            | Self::Vb
            | Self::Vi
            | Self::Svh
            | Self::Svw
            | Self::Lvh
            | Self::Lvw
            | Self::Dvh
            | Self::Dvw => UnitKind::ViewportRelative,
            Self::Cqw | Self::Cqh | Self::Cqi | Self::Cqb | Self::Cqmin | Self::Cqmax => {
                UnitKind::ContainerRelative
            }
            Self::Deg | Self::Rad | Self::Grad | Self::Turn => UnitKind::Angle,
            Self::S | Self::Ms => UnitKind::Time,
            Self::Hz | Self::Khz => UnitKind::Frequency,
            Self::Dpi | Self::Dpcm | Self::Dppx => UnitKind::Resolution,
            Self::Fr => UnitKind::Flex,
            Self::Percent => UnitKind::Percent,
        }
    }
}

/// A parameterised colour value.
///
/// Isomorphic to lightningcss's `CssColor` enum head. bbnf projects
/// the existing [`Color`] view blob into [`Self::Function`] variants
/// when the grammar records a `LargeAggregate` payload; hex colours
/// land in [`Self::Hex`]; named colours land in [`Self::Named`].
#[derive(Clone, Debug, PartialEq)]
pub enum ColorValue {
    /// Hex colour `#rrggbbaa` — 0xRRGGBBAA packed.
    Hex(u32),
    /// Named colour keyword — resolved to 0xRRGGBBAA.
    Named {
        /// The keyword (lowercase).
        name: String,
        /// The resolved packed RGBA.
        rgba: u32,
    },
    /// `rgb(...)` / `rgba(...)` / `hsl(...)` / `lab(...)` / `oklch(...)`
    /// / `color(space c1 c2 c3 / alpha)` / `color-mix(...)`.
    Function(Color),
    /// CSS-wide `currentColor` keyword.
    CurrentColor,
    /// `transparent` keyword.
    Transparent,
    /// `system-ui`-style system colour keyword (e.g. `Canvas`,
    /// `ButtonText`).
    System(String),
}

impl ColorValue {
    /// Colour-space discriminant for the wrapped [`Color`], if any.
    #[inline]
    pub fn space(&self) -> Option<ColorSpace> {
        match self {
            Self::Function(c) => Some(c.space),
            _ => None,
        }
    }
}

/// Calc-family function flavour.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CalcKind {
    /// `calc(...)`.
    Calc = 0,
    /// `min(...)`.
    Min = 1,
    /// `max(...)`.
    Max = 2,
    /// `clamp(...)`.
    Clamp = 3,
    /// `mod(...)`.
    Mod = 4,
    /// `rem(...)`.
    Rem = 5,
    /// `round(...)`.
    Round = 6,
    /// `sign(...)`.
    Sign = 7,
    /// `abs(...)`.
    Abs = 8,
    /// `sin(...)`, `cos(...)`, `tan(...)`, `asin(...)`, `acos(...)`,
    /// `atan(...)`, `atan2(...)` — all grouped as trig flavours;
    /// specific function name lives in the `Function` wrapper when
    /// round-trip fidelity is required.
    Trig = 9,
    /// `log(...)` / `exp(...)` / `sqrt(...)` / `pow(...)` / `hypot(...)`.
    Exponential = 10,
}

/// Parse a unit token (`"px"`, `"em"`, ...) into a [`Unit`].
///
/// Case-insensitive for units that are conventionally lowercase
/// (`Hz` / `kHz` are preserved case-sensitively to distinguish from
/// `hz`). Returns `None` for unrecognised tokens.
pub fn parse_unit(token: &str) -> Option<Unit> {
    let lower = token.to_ascii_lowercase();
    let unit = match lower.as_str() {
        "px" => Unit::Px,
        "cm" => Unit::Cm,
        "mm" => Unit::Mm,
        "in" => Unit::In,
        "pt" => Unit::Pt,
        "pc" => Unit::Pc,
        "q" => Unit::Q,
        "em" => Unit::Em,
        "rem" => Unit::Rem,
        "ex" => Unit::Ex,
        "ch" => Unit::Ch,
        "cap" => Unit::Cap,
        "ic" => Unit::Ic,
        "lh" => Unit::Lh,
        "rlh" => Unit::Rlh,
        "vh" => Unit::Vh,
        "vw" => Unit::Vw,
        "vmin" => Unit::Vmin,
        "vmax" => Unit::Vmax,
        "vb" => Unit::Vb,
        "vi" => Unit::Vi,
        "svh" => Unit::Svh,
        "svw" => Unit::Svw,
        "lvh" => Unit::Lvh,
        "lvw" => Unit::Lvw,
        "dvh" => Unit::Dvh,
        "dvw" => Unit::Dvw,
        "cqw" => Unit::Cqw,
        "cqh" => Unit::Cqh,
        "cqi" => Unit::Cqi,
        "cqb" => Unit::Cqb,
        "cqmin" => Unit::Cqmin,
        "cqmax" => Unit::Cqmax,
        "deg" => Unit::Deg,
        "rad" => Unit::Rad,
        "grad" => Unit::Grad,
        "turn" => Unit::Turn,
        "s" => Unit::S,
        "ms" => Unit::Ms,
        "hz" => Unit::Hz,
        "khz" => Unit::Khz,
        "dpi" => Unit::Dpi,
        "dpcm" => Unit::Dpcm,
        "dppx" | "x" => Unit::Dppx,
        "fr" => Unit::Fr,
        "%" => Unit::Percent,
        _ => return None,
    };
    Some(unit)
}
