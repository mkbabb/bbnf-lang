//! `runtime` — types emitted directly into generated parser code.
//!
//! Tranche AB.2a introduced `Parsed<View>`; Tranche AC.1 adds
//! `ParseErr`. Together they form the public surface area of
//! every generated `Grammar::parse(input)` entry point:
//!
//! ```ignore
//! impl Grammar {
//!     pub fn parse(
//!         input: &str,
//!     ) -> Result<crate::runtime::Parsed<Self>, ParseErr> { ... }
//! }
//! ```
//!
//! `Parsed<Grammar>` is marker-typed over the grammar struct itself;
//! the root view lifetime is lent by `&self` via the `Root` trait's
//! GAT `type View<'tape>` (landing in AC.2 alongside the emitter
//! rewrite).

pub mod builder;
pub mod css_l4;
pub mod error;
pub mod google_sheets;
pub mod handle;
pub mod json;
pub mod parsed;
pub mod path;

pub use builder::StructBuilder;
pub use css_l4::{
    CssAngle, CssAngleUnit, CssArena, CssColor, CssColorFunction, CssColorMix,
    CssColorPredefined, CssColorSpace, CssColorType, CssDeclListId, CssDimension,
    CssDocument, CssDocumentKind, CssFlex, CssFrequency, CssFrequencyUnit, CssFunction,
    CssGlobalKeyword, CssHueMethod, CssKeyframeListId, CssLength, CssLengthUnit,
    CssMathOperator, CssPathQuery, CssPercentage, CssResolution, CssResolutionUnit,
    CssRuleListId, CssSelectorListId, CssStructBuilder, CssTime, CssTimeUnit,
    CssTypedValue, CssValueListId, CssView, GenericAtRule, KeyframeBlock,
    KeyframesRule, MediaRule, StyleRule, StyleSheet,
};
pub use css_l4::value::{Declaration as CssDeclaration, Selector as CssSelector};
pub use css_l4::value::CssRule;
pub use error::ParseErr;
pub use google_sheets::{
    SheetsArena, SheetsCompound, SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
    SheetsDocument, SheetsKind, SheetsPathQuery, SheetsStructBuilder, SheetsValue, SheetsView,
};
pub use handle::{CompoundHandle, StringHandle};
pub use json::{
    JsonArena, JsonArray, JsonArrayId, JsonDocument, JsonKind, JsonNumber, JsonObject,
    JsonObjectId, JsonPair, JsonPathQuery, JsonStructBuilder, JsonValue, JsonView,
};
pub use parsed::{Parsed, PathQuery, Root, ValueRoot};
pub use path::{IntoPathSegment, Path, PathSegment};

// B5.W1 — the unified [`tape::Tape<R>`] substrate is the sole write
// + read + projection surface the grammar-emitted parse entry uses.
// The pre-B5.W1 builder/output/value-output triumvirate retired
// alongside the welded boundary.
pub use tape::{PayloadTag, PayloadValue, ValueChildren, ValueFrame};

/// Re-export the full `tape` public surface from `bbnf::runtime`.
///
/// Generated parsers reference `crate::runtime::tape::*` for tape
/// types ([`Tape`], [`TapeOffset`], [`TapeCursor`], [`TapeKind`],
/// [`TapeBuildError`]) so downstream consumers do not need a direct
/// `tape` dependency — `bbnf` already carries it as the substrate
/// for the generated code.
///
/// [`Tape`]: tape::Tape
/// [`TapeOffset`]: tape::TapeOffset
/// [`TapeCursor`]: tape::TapeCursor
/// [`TapeKind`]: tape::TapeKind
/// [`TapeBuildError`]: tape::TapeBuildError
pub use tape;

/// AW.0.5: typed view-layer projections the generated `.as_color()`
/// shims reference. The Rust-side `Color` struct + `ColorSpace`
/// enum live in the backend's `view/color.rs`; this re-export
/// surfaces them at the stable `crate::runtime::view::*` path so
/// generated `the proc-macro derive (retired B2)` output reaches the types without
/// depending on crate-internal `backend::rust::view::*` paths.
pub mod view {
    pub use crate::backend::rust::view::color::{Color, ColorSpace, COLOR_PAYLOAD_BYTES};
}
