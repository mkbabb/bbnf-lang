//! `runtime` — types emitted directly into generated parser code.
//!
//! Generated parse entry points return concrete grammar documents. The
//! runtime surface provides those document arenas, builders, typed
//! values, shared path/query traits, parse errors, and the remaining
//! substrate types consumed by generated code.

pub mod arena_template;
pub mod bbnf;
pub mod bnf;
pub mod builder;
pub mod builder_template;
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
pub mod view;

pub use bbnf::{
    BbnfArena, BbnfCompound, BbnfCompoundId, BbnfCompoundKind, BbnfDocument, BbnfKind,
    BbnfPathQuery, BbnfStructBuilder, BbnfValue, BbnfView,
};
pub use bnf::{
    BnfArena, BnfCompound, BnfCompoundId, BnfCompoundKind, BnfDocument, BnfKind, BnfPathQuery,
    BnfStructBuilder, BnfValue, BnfView,
};
pub use builder::StructBuilder;
pub use css_l4::value::CssRule;
pub use css_l4::value::{Declaration as CssDeclaration, Selector as CssSelector};
pub use css_l4::{
    CssAngle, CssAngleUnit, CssArena, CssColor, CssColorFunction, CssColorMix, CssColorPredefined,
    CssColorSpace, CssColorType, CssDeclListId, CssDimension, CssDocument, CssDocumentKind,
    CssFlex, CssFrequency, CssFrequencyUnit, CssFunction, CssGlobalKeyword, CssHueMethod,
    CssKeyframeListId, CssLength, CssLengthUnit, CssMathOperator, CssPathQuery, CssPercentage,
    CssResolution, CssResolutionUnit, CssRuleListId, CssSelectorListId, CssStructBuilder, CssTime,
    CssTimeUnit, CssTypedValue, CssValueListId, CssView, GenericAtRule, KeyframeBlock,
    KeyframesRule, MediaRule, StyleRule, StyleSheet,
};
pub use css_pretty::{
    CssPrettyArena, CssPrettyCompound, CssPrettyCompoundId, CssPrettyCompoundKind,
    CssPrettyDocument, CssPrettyKind, CssPrettyPathQuery, CssPrettyStructBuilder, CssPrettyValue,
    CssPrettyView,
};
pub use csv::{
    CsvArena, CsvCompound, CsvCompoundId, CsvCompoundKind, CsvDocument, CsvKind, CsvPathQuery,
    CsvStructBuilder, CsvValue, CsvView,
};
pub use ebnf::{
    EbnfArena, EbnfCompound, EbnfCompoundId, EbnfCompoundKind, EbnfDocument, EbnfKind,
    EbnfPathQuery, EbnfStructBuilder, EbnfValue, EbnfView,
};
pub use error::{DtaError, ParseErr};
pub use google_sheets::{
    SheetsArena, SheetsCompound, SheetsCompoundId, SheetsCompoundKind, SheetsCompoundView,
    SheetsDocument, SheetsKind, SheetsPathQuery, SheetsStructBuilder, SheetsValue, SheetsView,
};
pub use handle::{CompoundHandle, StringHandle};
pub use json::{
    JsonArena, JsonArray, JsonArrayId, JsonDocument, JsonKind, JsonNumber, JsonObject,
    JsonObjectId, JsonPair, JsonPathQuery, JsonStructBuilder, JsonValue, JsonView,
};
pub use math::{
    MathArena, MathCompound, MathCompoundId, MathCompoundKind, MathDocument, MathKind,
    MathPathQuery, MathStructBuilder, MathValue, MathView,
};
pub use path::{IntoPathSegment, Path, PathSegment};

/// AZ-I.W2-act.close A.fix — re-export the grammar-agnostic
/// [`view::RuntimeView`] trait at the stable `crate::runtime` path.
pub use view::RuntimeView;
