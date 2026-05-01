//! AZ-I.W2-act.B3 — CSS L4 typed value sum.
//!
//! The parse output is rooted at a
//! [`crate::runtime::css_l4::document::CssDocument`] holding a
//! [`crate::runtime::css_l4::arena::CssArena`] (the owner of every
//! compound's child slice) and a root [`StyleSheet<'p>`]. The `'p`
//! lifetime ties every borrowed string slice and every
//! arena-allocated child slice to the parse call site.
//!
//! Shape derivation comes directly from the modular grammar at
//! `grammar/css/l4/*.bbnf` — the typed enums on this module mirror
//! the alternation structure of each `value`-class rule, preserving
//! lightningcss-equivalent fidelity per
//! `feedback_preserve-rich-ast`. No alternation flattens to a string
//! for parse speed; every branch becomes a typed variant whose
//! payload carries the rule's `->` projection.
//!
//! # Module surface
//!
//! - Numeric primitives: [`CssLength`], [`CssAngle`], [`CssTime`],
//!   [`CssFrequency`], [`CssResolution`], [`CssFlex`],
//!   [`CssPercentage`], [`CssDimension`].
//! - Color: [`CssColor`], [`CssColorSpace`], [`CssColorMixSpace`],
//!   [`CssHueMethod`], [`CssColorType`].
//! - Function families: [`CssFunction`].
//! - Selector / declaration / aggregate: [`Selector`], [`Declaration`],
//!   [`StyleRule`], [`MediaRule`], [`KeyframesRule`], [`KeyframeBlock`],
//!   [`CssRule`], [`StyleSheet`], [`CssTypedValue`].
//!
//! Every typed value enum carries a discriminant matching the
//! grammar's `-> Nu8` projection; the f64 / u32 payloads land
//! through `push_leaf_with_*` calls.

use crate::runtime::css_l4::arena::{
    CssDeclListId, CssRuleListId, CssSelectorListId, CssValueListId,
};

// ---------------------------------------------------------------------
// §1 — Length / dimension / numeric units
// ---------------------------------------------------------------------

/// Length unit discriminant per `value-unit.bbnf::lengthUnit`.
///
/// Keep in sync with the grammar's `-> Nu8` projections: each variant's
/// numeric value is the u8 the grammar emits. The variant ordering is
/// arbitrary but every grammar-named unit is represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssLengthUnit {
    /// `px` — absolute pixel.
    Px = 0,
    /// `em` — font-relative.
    Em = 1,
    /// `rem` — root-em font-relative.
    Rem = 2,
    /// `vh` — viewport-height percentage.
    Vh = 3,
    /// `vw` — viewport-width percentage.
    Vw = 4,
    /// `vmin` — viewport-minimum percentage.
    Vmin = 5,
    /// `vmax` — viewport-maximum percentage.
    Vmax = 6,
    /// `ch` — character-relative.
    Ch = 7,
    /// `ex` — x-height-relative.
    Ex = 8,
    /// `cm` — centimetre.
    Cm = 9,
    /// `mm` — millimetre.
    Mm = 10,
    /// `in` — inch.
    In = 11,
    /// `pt` — point.
    Pt = 12,
    /// `pc` — pica.
    Pc = 13,
    /// `lh` — line-height-relative.
    Lh = 14,
    /// `rlh` — root-line-height-relative.
    Rlh = 15,
    /// `svw` — small viewport-width.
    Svw = 16,
    /// `svh` — small viewport-height.
    Svh = 17,
    /// `dvw` — dynamic viewport-width.
    Dvw = 18,
    /// `dvh` — dynamic viewport-height.
    Dvh = 19,
    /// `lvw` — large viewport-width.
    Lvw = 20,
    /// `lvh` — large viewport-height.
    Lvh = 21,
    /// `cqw` — container-query-width.
    Cqw = 22,
    /// `cqh` — container-query-height.
    Cqh = 23,
    /// `cqi` — container-query-inline.
    Cqi = 24,
    /// `cqb` — container-query-block.
    Cqb = 25,
    /// `Q` — quarter-millimetre.
    Q = 26,
    /// Catch-all sentinel for vb / vi / svb / svi / lvb / lvi / dvb /
    /// dvi / cqmin / cqmax / cap / ic / rcap / rex / rch / ric. The
    /// grammar projects each to a distinct u8; this variant holds the
    /// raw discriminant so structural shape is preserved without
    /// enumerating every boundary unit individually.
    Other(u8) = 255,
}

impl CssLengthUnit {
    /// Map the grammar's u8 discriminant to the matching enum variant.
    /// Unknown discriminants land in [`Self::Other`] preserving the raw
    /// byte for diagnostic round-trip.
    #[inline]
    pub fn from_discriminant(d: u8) -> Self {
        match d {
            0 => Self::Px,
            1 => Self::Em,
            2 => Self::Rem,
            3 => Self::Vh,
            4 => Self::Vw,
            5 => Self::Vmin,
            6 => Self::Vmax,
            7 => Self::Ch,
            8 => Self::Ex,
            9 => Self::Cm,
            10 => Self::Mm,
            11 => Self::In,
            12 => Self::Pt,
            13 => Self::Pc,
            14 => Self::Lh,
            15 => Self::Rlh,
            16 => Self::Svw,
            17 => Self::Svh,
            18 => Self::Dvw,
            19 => Self::Dvh,
            20 => Self::Lvw,
            21 => Self::Lvh,
            22 => Self::Cqw,
            23 => Self::Cqh,
            24 => Self::Cqi,
            25 => Self::Cqb,
            26 => Self::Q,
            other => Self::Other(other),
        }
    }
}

/// CSS length value — `length = number , lengthUnit` per the grammar.
///
/// Mirrors lightningcss's `LengthValue` typed shape: every grammar-
/// declared unit becomes a variant carrying the f64 magnitude. The
/// rich-AST invariant per `feedback_preserve-rich-ast` requires the
/// unit to be typed, not flattened to a `(f64, &str)` pair.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssLength {
    /// The magnitude — projected through the grammar's `number -> f64`.
    pub value: f64,
    /// The unit discriminant — projected through one of
    /// `absoluteLengthUnit | relativeLengthUnit` `-> Nu8`.
    pub unit: CssLengthUnit,
}

/// Angle unit discriminant per `value-unit.bbnf::angleUnit`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssAngleUnit {
    /// `deg` — degrees.
    Deg = 0,
    /// `rad` — radians.
    Rad = 1,
    /// `grad` — gradians.
    Grad = 2,
    /// `turn` — full turns.
    Turn = 3,
}

impl CssAngleUnit {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Deg),
            1 => Some(Self::Rad),
            2 => Some(Self::Grad),
            3 => Some(Self::Turn),
            _ => None,
        }
    }
}

/// CSS angle value — `angle = number , angleUnit`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssAngle {
    /// The angular magnitude.
    pub value: f64,
    /// The unit discriminant.
    pub unit: CssAngleUnit,
}

/// Time unit discriminant per `value-unit.bbnf::timeUnit`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssTimeUnit {
    /// `ms` — milliseconds.
    Ms = 0,
    /// `s` — seconds.
    S = 1,
}

impl CssTimeUnit {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Ms),
            1 => Some(Self::S),
            _ => None,
        }
    }
}

/// CSS time value — `time = number , timeUnit`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssTime {
    /// The temporal magnitude.
    pub value: f64,
    /// The unit discriminant.
    pub unit: CssTimeUnit,
}

/// Frequency unit discriminant per `value-unit.bbnf::frequencyUnit`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssFrequencyUnit {
    /// `Hz` — hertz.
    Hz = 0,
    /// `kHz` — kilohertz.
    KHz = 1,
}

impl CssFrequencyUnit {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Hz),
            1 => Some(Self::KHz),
            _ => None,
        }
    }
}

/// CSS frequency value — `frequency = number , frequencyUnit`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssFrequency {
    /// The frequency magnitude.
    pub value: f64,
    /// The unit discriminant.
    pub unit: CssFrequencyUnit,
}

/// Resolution unit discriminant per `value-unit.bbnf::resolutionUnit`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssResolutionUnit {
    /// `dpi` — dots-per-inch.
    Dpi = 0,
    /// `dpcm` — dots-per-centimetre.
    Dpcm = 1,
    /// `dppx` — dots-per-px.
    Dppx = 2,
    /// `x` — alias for dppx.
    X = 3,
}

impl CssResolutionUnit {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Dpi),
            1 => Some(Self::Dpcm),
            2 => Some(Self::Dppx),
            3 => Some(Self::X),
            _ => None,
        }
    }
}

/// CSS resolution value — `resolution = number , resolutionUnit`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssResolution {
    /// The resolution magnitude.
    pub value: f64,
    /// The unit discriminant.
    pub unit: CssResolutionUnit,
}

/// CSS flex value — `flex = number , flexUnit` (the grammar admits
/// the lone `fr` unit per Grid L1).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssFlex {
    /// Flex magnitude (in fr).
    pub value: f64,
}

/// CSS percentage value — `percentage = number , percentageUnit`.
///
/// The grammar projects `%` to the discriminant `255u8`; the
/// percentage's typed shape carries the f64 magnitude only because
/// the unit is structurally fixed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssPercentage {
    /// The percentage magnitude.
    pub value: f64,
}

/// Composite dimension type — `dimension = length | angle | time |
/// frequency | resolution | flex | percentage`.
///
/// Materialises every grammar branch as a typed variant per
/// `feedback_preserve-rich-ast`. Each variant carries the matching
/// inner typed value, never collapsing to a `(f64, &str)` pair.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssDimension {
    /// `length`.
    Length(CssLength),
    /// `angle`.
    Angle(CssAngle),
    /// `time`.
    Time(CssTime),
    /// `frequency`.
    Frequency(CssFrequency),
    /// `resolution`.
    Resolution(CssResolution),
    /// `flex`.
    Flex(CssFlex),
    /// `percentage`.
    Percentage(CssPercentage),
    /// `unitless = number` — the grammar's catch-all numeric branch,
    /// stored as a bare f64 magnitude.
    Unitless(f64),
}

// ---------------------------------------------------------------------
// §2 — Color
// ---------------------------------------------------------------------

/// `colorType` discriminant per `color.bbnf::colorType`. Identifies the
/// functional-color family (rgb / rgba / hsl / hsla / hwb / lab / lch
/// / oklab / oklch).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssColorType {
    /// `rgb`.
    Rgb = 0,
    /// `rgba`.
    Rgba = 1,
    /// `hsl`.
    Hsl = 2,
    /// `hsla`.
    Hsla = 3,
    /// `hwb`.
    Hwb = 4,
    /// `lab`.
    Lab = 5,
    /// `lch`.
    Lch = 6,
    /// `oklab`.
    Oklab = 7,
    /// `oklch`.
    Oklch = 8,
}

impl CssColorType {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Rgb),
            1 => Some(Self::Rgba),
            2 => Some(Self::Hsl),
            3 => Some(Self::Hsla),
            4 => Some(Self::Hwb),
            5 => Some(Self::Lab),
            6 => Some(Self::Lch),
            7 => Some(Self::Oklab),
            8 => Some(Self::Oklch),
            _ => None,
        }
    }
}

/// `colorSpace` / `mixSpace` discriminant per `color.bbnf::colorSpace`
/// and `color.bbnf::mixSpace`. The two share variants — `mixSpace`
/// extends with `oklab`/`oklch`/`lab`/`lch`/`hsl`/`hwb` overlaps —
/// stored on a single typed enum to preserve the alternation structure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssColorSpace {
    /// `srgb`.
    Srgb = 0,
    /// `srgb-linear`.
    SrgbLinear = 1,
    /// `display-p3`.
    DisplayP3 = 2,
    /// `a98-rgb`.
    A98Rgb = 3,
    /// `prophoto-rgb`.
    ProphotoRgb = 4,
    /// `rec2020`.
    Rec2020 = 5,
    /// `xyz-d50`.
    XyzD50 = 6,
    /// `xyz-d65`.
    XyzD65 = 7,
    /// `xyz`.
    Xyz = 8,
    /// `lch` (mixSpace only).
    Lch = 9,
    /// `lab` (mixSpace only).
    Lab = 10,
    /// `oklab` (mixSpace only).
    Oklab = 11,
    /// `oklch` (mixSpace only).
    Oklch = 12,
    /// `hsl` (mixSpace only).
    Hsl = 13,
    /// `hwb` (mixSpace only).
    Hwb = 14,
}

impl CssColorSpace {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Srgb),
            1 => Some(Self::SrgbLinear),
            2 => Some(Self::DisplayP3),
            3 => Some(Self::A98Rgb),
            4 => Some(Self::ProphotoRgb),
            5 => Some(Self::Rec2020),
            6 => Some(Self::XyzD50),
            7 => Some(Self::XyzD65),
            8 => Some(Self::Xyz),
            9 => Some(Self::Lch),
            10 => Some(Self::Lab),
            11 => Some(Self::Oklab),
            12 => Some(Self::Oklch),
            13 => Some(Self::Hsl),
            14 => Some(Self::Hwb),
            _ => None,
        }
    }
}

/// `hueMethodKeyword` discriminant per `color.bbnf::hueMethodKeyword`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssHueMethod {
    /// `shorter`.
    Shorter = 0,
    /// `longer`.
    Longer = 1,
    /// `increasing`.
    Increasing = 2,
    /// `decreasing`.
    Decreasing = 3,
}

impl CssHueMethod {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Shorter),
            1 => Some(Self::Longer),
            2 => Some(Self::Increasing),
            3 => Some(Self::Decreasing),
            _ => None,
        }
    }
}

/// CSS color — `color = colorMix | colorFn | hex | colorFunction |
/// namedColor` per the grammar.
///
/// Every branch becomes a typed variant:
///
/// - [`Self::Hex`] / [`Self::Named`] — InlineScalar projection
///   producing the packed 0xRRGGBBAA u32.
/// - [`Self::Function`] — `colorFunction = colorType , "(" >> … << ")"`
///   carrying the typed `CssColorType` discriminant, three colour
///   components, and the optional alpha.
/// - [`Self::Predefined`] — `colorFn = "color" , "(" >> colorSpace , …`
///   carrying the typed `CssColorSpace` discriminant.
/// - [`Self::Mix`] — `color-mix(...)` — recursive: each nested colour
///   becomes a `&'p CssColor<'p>` reference into the arena.
///
/// `feedback_preserve-rich-ast` is in force: no flattening of the
/// alternation to a packed-u32 representation; the AST is preserved
/// node-for-node.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssColor<'p> {
    /// `hex = "#" , /[0-9a-fA-F]{3,8}/` — packed 0xRRGGBBAA u32.
    Hex(u32),
    /// `namedColor = "aliceblue" -> 0xF0F8FFFFu32 | …` — packed u32.
    /// The borrowed slice carries the input span for diagnostic
    /// round-trip parity with lightningcss.
    Named { name: &'p str, packed: u32 },
    /// `colorFunction = colorType , "(" >> colorValue , … << ")"`.
    Function(CssColorFunction),
    /// `colorFn = "color" , "(" >> colorSpace , … << ")"`.
    Predefined(CssColorPredefined),
    /// `colorMix = "color-mix" , "(" >> "in" , mixSpace , … << ")"`.
    Mix(CssColorMix<'p>),
}

/// `colorFunction` payload — typed-aggregate per `color.bbnf` aggregate
/// layout (LargeAggregate, 40-byte arena slot).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssColorFunction {
    /// The colour family (rgb / rgba / hsl / etc.).
    pub kind: CssColorType,
    /// Component 1 — colour-space-specific axis.
    pub c1: f64,
    /// Component 2.
    pub c2: f64,
    /// Component 3.
    pub c3: f64,
    /// Optional alpha; `None` when the alpha clause is absent.
    pub alpha: Option<f64>,
}

/// `colorFn` payload — typed-aggregate matching `colorFunction` shape
/// but tagged with [`CssColorSpace`] instead of [`CssColorType`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssColorPredefined {
    /// The colour space (srgb / display-p3 / xyz-d65 / etc.).
    pub space: CssColorSpace,
    /// Component 1.
    pub c1: f64,
    /// Component 2.
    pub c2: f64,
    /// Component 3.
    pub c3: f64,
    /// Optional alpha; `None` when the alpha clause is absent.
    pub alpha: Option<f64>,
}

/// `colorMix` payload — `color-mix(in <mixSpace> <hueMethod>?, c1 p1?,
/// c2 p2?)`. Recursive: each nested colour reaches the arena via a
/// borrowed reference.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CssColorMix<'p> {
    /// `in <mixSpace>`.
    pub mix_space: CssColorSpace,
    /// Optional `<hueMethodKeyword> hue` modifier.
    pub hue_method: Option<CssHueMethod>,
    /// Left colour reference.
    pub left: &'p CssColor<'p>,
    /// Optional left percentage.
    pub left_pct: Option<f64>,
    /// Right colour reference.
    pub right: &'p CssColor<'p>,
    /// Optional right percentage.
    pub right_pct: Option<f64>,
}

// ---------------------------------------------------------------------
// §3 — Math / function families
// ---------------------------------------------------------------------

/// Math operator discriminant per `values.bbnf::mathOperator`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssMathOperator {
    /// `+`.
    Add = 0,
    /// `-`.
    Sub = 1,
    /// `*`.
    Mul = 2,
    /// `/`.
    Div = 3,
}

impl CssMathOperator {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::Add),
            1 => Some(Self::Sub),
            2 => Some(Self::Mul),
            3 => Some(Self::Div),
            _ => None,
        }
    }
}

/// `globalKeyword` discriminant per `values.bbnf::globalKeyword`.
///
/// Tracks the CSS Cascading-and-Inheritance L5 keyword set
/// (`revert-layer | inherit | initial | unset | revert`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum CssGlobalKeyword {
    /// `revert-layer`.
    RevertLayer = 0,
    /// `inherit`.
    Inherit = 1,
    /// `initial`.
    Initial = 2,
    /// `unset`.
    Unset = 3,
    /// `revert`.
    Revert = 4,
}

impl CssGlobalKeyword {
    /// Map the grammar's u8 discriminant to the matching variant.
    #[inline]
    pub fn from_discriminant(d: u8) -> Option<Self> {
        match d {
            0 => Some(Self::RevertLayer),
            1 => Some(Self::Inherit),
            2 => Some(Self::Initial),
            3 => Some(Self::Unset),
            4 => Some(Self::Revert),
            _ => None,
        }
    }
}

/// CSS function family — calc / min / max / clamp / var / env / url /
/// gradient / transform / filter / easing / generic.
///
/// Every grammar `*Function` rule projects through this enum. The
/// payload carries an arena-borrowed argument list and (where needed)
/// the function name span for round-trip parity. Per
/// `feedback_preserve-rich-ast` no function-family flattens to a string
/// — each is typed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssFunction<'p> {
    /// `calc(...)` — arithmetic expression.
    Calc { args: CssValueListId },
    /// `min(...)` — minimum of comma-separated arguments.
    Min { args: CssValueListId },
    /// `max(...)`.
    Max { args: CssValueListId },
    /// `clamp(min, val, max)`.
    Clamp { args: CssValueListId },
    /// `var(--name [, fallback])`.
    Var {
        name: &'p str,
        fallback: CssValueListId,
    },
    /// `env(name [, fallback])`.
    Env {
        name: &'p str,
        fallback: CssValueListId,
    },
    /// `url(...)` — span captured for source-map round-trip.
    Url { raw: &'p str },
    /// `gradient(...)` — every gradient family folds here; the name
    /// span discriminates `linear-gradient` / `radial-gradient` etc.
    Gradient { name: &'p str, args: CssValueListId },
    /// Transform / filter / easing / generic — the arena-borrowed
    /// argument list carries the typed payload.
    Generic { name: &'p str, args: CssValueListId },
}

// ---------------------------------------------------------------------
// §4 — Selector / declaration / aggregate
// ---------------------------------------------------------------------

/// CSS selector — `selector` / `compoundSelector` / `complexSelector` /
/// `selectorList` per `selectors.bbnf`.
///
/// The grammar admits a rich, recursive selector syntax; this enum
/// preserves the alternation structure per
/// `feedback_preserve-rich-ast`. The aggregate `SelectorList` lives
/// behind an arena handle on [`StyleRule::selectors`].
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Selector<'p> {
    /// `*` — universal selector.
    Universal,
    /// Type selector — element name.
    Type(&'p str),
    /// Class selector — `.name`.
    Class(&'p str),
    /// ID selector — `#name`.
    Id(&'p str),
    /// Attribute selector — `[attr=val]` — full match expression
    /// captured as a span for source-map fidelity.
    Attribute(&'p str),
    /// Pseudo-class selector — `:hover` / `:nth-child(n)` etc.
    PseudoClass(&'p str),
    /// Pseudo-element selector — `::before` / `::placeholder`.
    PseudoElement(&'p str),
    /// Combinator — `>` / `+` / `~` / ` ` (descendant). The variant's
    /// span captures the literal combinator byte for diagnostic
    /// round-trip.
    Combinator(&'p str),
    /// Span fallback when the selector branches above don't admit the
    /// shape — preserves source for round-trip without flattening
    /// declared variants.
    Span(&'p str),
}

/// CSS declaration — `<typedDecl>` per `properties.bbnf`.
///
/// Holds the property name (borrowed from input), the typed value, and
/// the `!important` flag.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Declaration<'p> {
    /// The property name span — `color`, `background`, `--my-var`, etc.
    pub property: &'p str,
    /// The typed value — every grammar branch on `value` resolves here.
    pub value: CssTypedValue<'p>,
    /// `true` when the declaration carries `!important`.
    pub important: bool,
}

/// CSS style rule — `qualifiedRule = selectorList , ruleBlock`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StyleRule<'p> {
    /// Selector list — arena-backed slice of [`Selector`].
    pub selectors: CssSelectorListId,
    /// Declaration list — arena-backed slice of [`Declaration`].
    pub declarations: CssDeclListId,
    /// Source span over the entire rule for diagnostic round-trip.
    pub span: &'p str,
}

/// CSS `@media` rule — `mediaRule = "@media" , mediaQueryList , ruleBlock`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MediaRule<'p> {
    /// Media query span — captured raw for round-trip fidelity; a
    /// future tranche (per `feedback_preserve-rich-ast`) refines this
    /// into a typed `MediaQueryList` sub-graph.
    pub query: &'p str,
    /// Inner declarations / nested rules.
    pub rules: CssRuleListId,
}

/// One keyframe block — `keyframeBlock = keyframeSel , "{" >>
/// declarations << "}"`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct KeyframeBlock<'p> {
    /// Selector span — `from`, `to`, `0%`, etc.
    pub selector: &'p str,
    /// Declaration list inside the block.
    pub declarations: CssDeclListId,
}

/// CSS `@keyframes` rule — `keyframesRule = "@keyframes" , ident ,
/// keyframeBlock*`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct KeyframesRule<'p> {
    /// The animation name.
    pub name: &'p str,
    /// The keyframe blocks — arena-backed slice of [`KeyframeBlock`].
    pub blocks: CssKeyframeListId,
}

/// Generic `@-rule` — captured when neither media nor keyframes match.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GenericAtRule<'p> {
    /// The at-rule keyword (`@font-face`, `@supports`, etc.).
    pub name: &'p str,
    /// The prelude span (between the keyword and the body).
    pub prelude: &'p str,
    /// Body span — block content or `;`-terminated.
    pub body: &'p str,
}

/// CSS rule alternation — `ruleItem = qualifiedRule | atRule`.
///
/// Each grammar branch becomes a typed variant per
/// `feedback_preserve-rich-ast`.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssRule<'p> {
    /// `qualifiedRule` — selector + declaration block.
    Style(StyleRule<'p>),
    /// `mediaRule` — `@media` block.
    Media(MediaRule<'p>),
    /// `keyframesRule` — `@keyframes` block.
    Keyframes(KeyframesRule<'p>),
    /// `genericAtRule` — fallback for unrecognised `@-rule` keywords.
    GenericAt(GenericAtRule<'p>),
}

/// CSS stylesheet — `stylesheet = ruleList ?w` per the grammar's entry
/// rule. Returns the arena-backed rule list.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StyleSheet {
    /// Top-level rule list — arena-backed slice of [`CssRule`].
    pub rules: CssRuleListId,
}

// ---------------------------------------------------------------------
// §5 — CssTypedValue alternation
// ---------------------------------------------------------------------

/// CSS typed value — the `value` rule's alternation closure.
///
/// `value = calcFunction | minFunction | maxFunction | clampFunction |
/// varFunction | envFunction | urlFunction | gradient |
/// transformFunction | filterFunction | easingFunction | color |
/// dimension | number | string | globalKeyword | ident | /[^\s;!}]+/`.
///
/// Every branch becomes a typed variant per
/// `feedback_preserve-rich-ast`; the alternation never flattens to a
/// single span for parse speed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CssTypedValue<'p> {
    /// `dimension` / typed-numeric branch.
    Dimension(CssDimension),
    /// `number` (bare numeric, when the grammar admits it without a
    /// unit).
    Number(f64),
    /// `integer` (bare integer projection).
    Integer(i64),
    /// `string`.
    String(&'p str),
    /// `ident` — bare identifier.
    Ident(&'p str),
    /// `globalKeyword` (inherit / initial / unset / revert / revert-
    /// layer).
    GlobalKeyword(CssGlobalKeyword),
    /// `color` — every typed colour family.
    Color(CssColor<'p>),
    /// `*Function` — calc / var / url / etc.
    Function(CssFunction<'p>),
    /// Multi-token list — comma- or whitespace-separated values
    /// captured under a single declaration.
    List(CssValueListId),
    /// Span fallback — raw token sequence the typed alternation didn't
    /// capture (preserves source for round-trip without losing
    /// fidelity per `feedback_preserve-rich-ast`).
    Span(&'p str),
}

// Re-export the keyframe arena handle from the arena module so this
// module's struct shapes stay self-contained.
pub use crate::runtime::css_l4::arena::CssKeyframeListId;
