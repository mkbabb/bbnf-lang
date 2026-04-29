//! `runtime` — types emitted directly into generated parser code.
//!
//! Generated parse entry points return concrete grammar documents. The
//! runtime surface provides those document arenas, builders, typed
//! values, shared path/query traits, parse errors, and the remaining
//! substrate types consumed by generated code.

pub mod bbnf;
pub mod bnf;
pub mod builder;
pub mod css_l4;
pub mod css_pretty;
pub mod csv;
pub mod ebnf;
pub mod error;
pub mod google_sheets;
pub mod handle;
pub mod json;
pub mod math;
pub mod path;
pub mod root;
pub mod view;

pub use bbnf::{
    BbnfArena, BbnfCompound, BbnfCompoundId, BbnfCompoundKind, BbnfDocument, BbnfKind,
    BbnfPathQuery, BbnfStructBuilder, BbnfValue, BbnfView,
};
pub use builder::StructBuilder;
pub use csv::{
    CsvArena, CsvCompound, CsvCompoundId, CsvCompoundKind, CsvDocument, CsvKind,
    CsvPathQuery, CsvStructBuilder, CsvValue, CsvView,
};
pub use math::{
    MathArena, MathCompound, MathCompoundId, MathCompoundKind, MathDocument, MathKind,
    MathPathQuery, MathStructBuilder, MathValue, MathView,
};
pub use bnf::{
    BnfArena, BnfCompound, BnfCompoundId, BnfCompoundKind, BnfDocument, BnfKind,
    BnfPathQuery, BnfStructBuilder, BnfValue, BnfView,
};
pub use ebnf::{
    EbnfArena, EbnfCompound, EbnfCompoundId, EbnfCompoundKind, EbnfDocument, EbnfKind,
    EbnfPathQuery, EbnfStructBuilder, EbnfValue, EbnfView,
};
pub use css_pretty::{
    CssPrettyArena, CssPrettyCompound, CssPrettyCompoundId, CssPrettyCompoundKind,
    CssPrettyDocument, CssPrettyKind, CssPrettyPathQuery, CssPrettyStructBuilder,
    CssPrettyValue, CssPrettyView,
};
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
pub use path::{IntoPathSegment, Path, PathSegment};
pub use root::{PathQuery, Root, ValueRoot};

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

/// AZ-I.W2-act.close A.fix — re-export the grammar-agnostic
/// [`view::RuntimeView`] trait at the stable `crate::runtime` path.
pub use view::RuntimeView;
