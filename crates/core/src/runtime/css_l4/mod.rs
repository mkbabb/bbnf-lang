//! AZ-I.W2-act.B3 — CSS L4 struct-direct runtime.
//!
//! `crates/core/src/runtime/css_l4/` is the typed-struct runtime for
//! the CSS Selectors / Values / Color / Animations Level 4 grammar.
//! The generated parser writes directly into a [`CssDocument`] graph
//! via the [`CssStructBuilder`] consumer of the
//! [`crate::runtime::builder::StructBuilder`] trait — the tape
//! substrate is severed on the CSS L4 parse path; no `TapeBuilder` /
//! `TapeRec` / `TapeCursor` symbol appears in this module's transitive
//! code.
//!
//! # Module layout
//!
//! - [`value`]    — typed-value enum family: numeric primitives
//!   ([`CssLength`], [`CssAngle`], [`CssTime`], [`CssFrequency`],
//!   [`CssResolution`], [`CssFlex`], [`CssPercentage`],
//!   [`CssDimension`]); colour ([`CssColor`], [`CssColorSpace`],
//!   [`CssColorMix`]); function family ([`CssFunction`]); selector /
//!   declaration / aggregate ([`Selector`], [`Declaration`],
//!   [`StyleRule`], [`MediaRule`], [`KeyframesRule`], [`KeyframeBlock`],
//!   [`CssRule`], [`StyleSheet`]); aggregate union [`CssTypedValue`].
//! - [`arena`]    — the [`CssArena`] owning slab for compound child
//!   slices (rule lists, declaration lists, selector lists, value
//!   lists, keyframe lists) plus the recursive colour DAG referenced
//!   by [`CssColorMix`].
//! - [`document`] — the [`CssDocument`] root + the [`CssView`] newtype
//!   + [`CssPathQuery`] trait. Mirrors the JSON document API surface
//!   per AZ-I.W2-act.A.
//! - [`builder`]  — the [`CssStructBuilder`] concrete `StructBuilder`
//!   impl that the generated parse function targets.
//!
//! # Wire contract
//!
//! `bbnf::grammar::generated::css_l4::CssL4Parser::parse(src)` returns
//! a [`CssDocument<'_>`] borrowing from `src`'s lifetime. The grammar's
//! typed `->` annotations close as follows:
//!
//! - `length = number , lengthUnit` → [`CssDimension::Length`]
//! - `angle = number , angleUnit` → [`CssDimension::Angle`]
//! - `time = number , timeUnit` → [`CssDimension::Time`]
//! - `frequency` / `resolution` / `flex` / `percentage` → matching
//!   [`CssDimension`] variant
//! - `hex -> u32` → [`CssColor::Hex`]
//! - `namedColor -> 0xRRGGBBAAu32` → [`CssColor::Named`]
//! - `colorFunction = colorType , "(" >> … << ")"` →
//!   [`CssColor::Function`]
//! - `colorFn = "color" , "(" >> colorSpace , … << ")"` →
//!   [`CssColor::Predefined`]
//! - `colorMix = "color-mix" , "(" >> "in" , mixSpace , …` →
//!   [`CssColor::Mix`]
//! - `calcFunction` / `minFunction` / `maxFunction` / `clampFunction`
//!   / `varFunction` / `envFunction` / `urlFunction` → matching
//!   [`CssFunction`] variant
//! - `qualifiedRule = selectorList , ruleBlock` → [`StyleRule`]
//! - `mediaRule` / `keyframesRule` / `genericAtRule` → matching
//!   [`CssRule`] variant
//! - `stylesheet = ruleList ?w` → [`StyleSheet`]

pub mod arena;
pub mod builder;
pub mod document;
pub mod value;

pub use arena::{
    CssArena, CssDeclListId, CssKeyframeListId, CssRuleListId, CssSelectorListId,
    CssValueListId,
};
pub use builder::CssStructBuilder;
pub use document::{CssDocument, CssDocumentKind, CssPathQuery, CssView};
pub use value::{
    CssAngle, CssAngleUnit, CssColor, CssColorFunction, CssColorMix, CssColorPredefined,
    CssColorSpace, CssColorType, CssDimension, CssFlex, CssFrequency, CssFrequencyUnit,
    CssFunction, CssGlobalKeyword, CssHueMethod, CssLength, CssLengthUnit, CssMathOperator,
    CssPercentage, CssResolution, CssResolutionUnit, CssTime, CssTimeUnit, CssTypedValue,
    Declaration, GenericAtRule, KeyframeBlock, KeyframesRule, MediaRule, Selector,
    StyleRule, StyleSheet,
};
