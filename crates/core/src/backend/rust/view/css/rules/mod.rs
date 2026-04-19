//! CSS rule AST — `bbnf::css::rules` surface.
//!
//! AX.W1.B: `CssRule` enum isomorphic to `lightningcss::rules::CssRule`.
//! Every variant lightningcss exposes has a field-complete
//! counterpart here per invariant 18. bbnf's grammar populates the
//! subset its `l4/stylesheet.bbnf` rules directly parse
//! (`StyleRule`, `MediaRule`, `KeyframesRule`, plus the `Unknown`
//! catch-all from `genericAtRule`). Variants outside bbnf's grammar
//! are populated by `From<lightningcss::rules::CssRule>` and
//! documented in `docs/tranches/AX/parity/css_divergence.md` §rules.

use std::borrow::Cow;

use super::declarations::DeclarationBlock;
use super::selectors::SelectorList;
use super::values::Value;

/// Top-level CSS rule.
///
/// Field-complete isomorphism against `lightningcss::rules::CssRule`.
/// The variant enumeration follows lightningcss's order so
/// projection is 1:1 where supported.
#[derive(Clone, Debug, PartialEq)]
pub enum CssRule<'i> {
    /// `@media` rule.
    Media(MediaRule<'i>),
    /// `@import` rule.
    Import(ImportRule<'i>),
    /// A qualified style rule (selector block).
    Style(StyleRule<'i>),
    /// `@keyframes` rule.
    Keyframes(KeyframesRule<'i>),
    /// `@font-face` rule.
    FontFace(FontFaceRule<'i>),
    /// `@font-palette-values` rule.
    FontPaletteValues(FontPaletteValuesRule<'i>),
    /// `@font-feature-values` rule.
    FontFeatureValues(FontFeatureValuesRule<'i>),
    /// `@page` rule.
    Page(PageRule<'i>),
    /// `@supports` rule.
    Supports(SupportsRule<'i>),
    /// `@counter-style` rule.
    CounterStyle(CounterStyleRule<'i>),
    /// `@namespace` rule.
    Namespace(NamespaceRule<'i>),
    /// `@-moz-document` rule.
    MozDocument(MozDocumentRule<'i>),
    /// `@nest` rule (CSS Nesting pre-standardisation).
    Nesting(NestingRule<'i>),
    /// A nested declarations rule (implicit wrapper inside a nested
    /// style rule).
    NestedDeclarations(NestedDeclarationsRule<'i>),
    /// `@viewport` rule (deprecated, but still in the grammar space).
    Viewport(ViewportRule<'i>),
    /// `@custom-media` rule.
    CustomMedia(CustomMediaRule<'i>),
    /// `@layer` statement rule (names without a block).
    LayerStatement(LayerStatementRule<'i>),
    /// `@layer` block rule.
    LayerBlock(LayerBlockRule<'i>),
    /// `@property` rule.
    Property(PropertyRule<'i>),
    /// `@container` rule.
    Container(ContainerRule<'i>),
    /// `@scope` rule.
    Scope(ScopeRule<'i>),
    /// `@starting-style` rule.
    StartingStyle(StartingStyleRule<'i>),
    /// `@view-transition` rule.
    ViewTransition(ViewTransitionRule<'i>),
    /// A rule that was removed during transforms (lightningcss marker).
    Ignored,
    /// Unknown at-rule — bbnf's `genericAtRule` fallback populates
    /// this when no typed rule matches.
    Unknown(UnknownAtRule<'i>),
    /// Custom at-rule (lightningcss extension slot; bbnf never
    /// populates via grammar, reachable only via `From<lightningcss>`).
    Custom(CustomAtRule<'i>),
}

/// Source location of a rule.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Location {
    /// Source file index within the parent stylesheet's `sources`.
    pub source_index: u32,
    /// Line number (0-based).
    pub line: u32,
    /// Column number (1-based, UTF-16 code units per CSS spec).
    pub column: u32,
    /// Byte offset into the source text (bbnf extension — lightningcss
    /// stores only line/column; byte offset is preserved for tape
    /// round-trip).
    pub byte_offset: u32,
}

// ─── StyleRule ───────────────────────────────────────────────────────

/// A qualified style rule.
#[derive(Clone, Debug, PartialEq)]
pub struct StyleRule<'i> {
    /// The selector list.
    pub selectors: SelectorList<'i>,
    /// The declaration block.
    pub declarations: DeclarationBlock<'i>,
    /// Nested rules (CSS Nesting).
    pub rules: Vec<CssRule<'i>>,
    /// Vendor prefix, or `None` if unprefixed.
    pub vendor_prefix: Option<super::selectors::VendorPrefix>,
    /// Source location.
    pub loc: Location,
}

// ─── MediaRule ───────────────────────────────────────────────────────

/// `@media` rule body.
#[derive(Clone, Debug, PartialEq)]
pub struct MediaRule<'i> {
    /// The media query list.
    pub query: MediaList<'i>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// A media query list — one or more queries separated by `,`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct MediaList<'i> {
    /// The individual media queries.
    pub media_queries: Vec<MediaQuery<'i>>,
}

/// A single media query.
#[derive(Clone, Debug, PartialEq)]
pub struct MediaQuery<'i> {
    /// The qualifier (`only` / `not`), if any.
    pub qualifier: Option<MediaQualifier>,
    /// The media type.
    pub media_type: MediaType<'i>,
    /// The condition (`(feature: value)` etc.), if any.
    pub condition: Option<MediaCondition<'i>>,
}

/// Media query qualifier.
///
/// Discriminant matches `media.bbnf` mediaQualifier: Not=0, Only=1.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum MediaQualifier {
    /// `not`.
    Not = 0,
    /// `only`.
    Only = 1,
}

/// Media type.
///
/// Discriminant matches `media.bbnf` mediaType: All=0, Print=1,
/// Screen=2, Speech=3, Custom=255.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MediaType<'i> {
    /// `all`.
    All,
    /// `print`.
    Print,
    /// `screen`.
    Screen,
    /// `speech` (deprecated).
    Speech,
    /// Custom media type (any other identifier).
    Custom(Cow<'i, str>),
}

/// A media condition — the boolean structure of a media query.
#[derive(Clone, Debug, PartialEq)]
pub enum MediaCondition<'i> {
    /// A single feature expression `(name: value)`.
    Feature(MediaFeature<'i>),
    /// `not (...)`.
    Not(Box<MediaCondition<'i>>),
    /// `(...) and (...) and ...`.
    And(Vec<MediaCondition<'i>>),
    /// `(...) or (...) or ...`.
    Or(Vec<MediaCondition<'i>>),
    /// `(expr)` parenthesised.
    InParens(Box<MediaCondition<'i>>),
}

/// A media feature expression `(name)` or `(name: value)`.
#[derive(Clone, Debug, PartialEq)]
pub struct MediaFeature<'i> {
    /// Feature name (`min-width`, `prefers-color-scheme`, etc.).
    pub name: Cow<'i, str>,
    /// Optional feature value (verbatim; bbnf doesn't further parse it).
    pub value: Option<Cow<'i, str>>,
}

// ─── ImportRule ──────────────────────────────────────────────────────

/// `@import` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct ImportRule<'i> {
    /// The imported URL (unescaped, quotes stripped).
    pub url: Cow<'i, str>,
    /// Optional `layer(...)` specifier.
    pub layer: Option<Option<LayerName<'i>>>,
    /// Optional `supports(...)` condition.
    pub supports: Option<SupportsCondition<'i>>,
    /// Media list.
    pub media: MediaList<'i>,
    /// Source location.
    pub loc: Location,
}

// ─── KeyframesRule ───────────────────────────────────────────────────

/// `@keyframes` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct KeyframesRule<'i> {
    /// The animation name.
    pub name: KeyframesName<'i>,
    /// The individual keyframes.
    pub keyframes: Vec<Keyframe<'i>>,
    /// Vendor prefix (`-webkit-keyframes`, etc.).
    pub vendor_prefix: Option<super::selectors::VendorPrefix>,
    /// Source location.
    pub loc: Location,
}

/// `@keyframes` name — custom ident or string.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KeyframesName<'i> {
    /// Plain identifier.
    Ident(Cow<'i, str>),
    /// Quoted string.
    Custom(Cow<'i, str>),
}

/// A single keyframe inside `@keyframes`.
#[derive(Clone, Debug, PartialEq)]
pub struct Keyframe<'i> {
    /// Selectors (`0%`, `from`, `to`).
    pub selectors: Vec<KeyframeSelector>,
    /// Declaration block.
    pub declarations: DeclarationBlock<'i>,
}

/// A keyframe selector.
///
/// Discriminant matches `stylesheet.bbnf` keyframeStop: Percentage=0,
/// From=1, To=2.
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum KeyframeSelector {
    /// Explicit percentage (`0%` ... `100%`).
    Percentage(f32),
    /// `from` (equivalent to `0%`).
    From,
    /// `to` (equivalent to `100%`).
    To,
    /// Named timeline-range selector (e.g. `entry 50%`) — lightningcss
    /// extension; populated via `From<lightningcss>`.
    TimelineRangePercentage {
        /// The name part (e.g. `entry`, `exit`, `contain`, `cover`).
        name: TimelineRangeName,
        /// The percentage.
        percentage: f32,
    },
}

/// Scroll-animations named timeline range.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TimelineRangeName {
    /// `entry`.
    Entry = 0,
    /// `exit`.
    Exit = 1,
    /// `contain`.
    Contain = 2,
    /// `cover`.
    Cover = 3,
    /// `entry-crossing`.
    EntryCrossing = 4,
    /// `exit-crossing`.
    ExitCrossing = 5,
}

// ─── FontFace / FontPalette / FontFeature ────────────────────────────

/// `@font-face` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct FontFaceRule<'i> {
    /// The declaration-like properties inside `@font-face`.
    pub properties: Vec<FontFaceProperty<'i>>,
    /// Source location.
    pub loc: Location,
}

/// A single `@font-face` declaration.
#[derive(Clone, Debug, PartialEq)]
pub struct FontFaceProperty<'i> {
    /// Property name (`font-family`, `src`, `font-style`, etc.).
    pub name: Cow<'i, str>,
    /// Raw value token list.
    pub value: Vec<Value<'i>>,
}

/// `@font-palette-values` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct FontPaletteValuesRule<'i> {
    /// The palette name (`--my-palette`).
    pub name: Cow<'i, str>,
    /// Inner property list.
    pub properties: Vec<FontFaceProperty<'i>>,
    /// Source location.
    pub loc: Location,
}

/// `@font-feature-values` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct FontFeatureValuesRule<'i> {
    /// Font-family name list.
    pub families: Vec<Cow<'i, str>>,
    /// Feature-values blocks (e.g. `@styleset { ... }`).
    pub features: Vec<FontFeatureValuesBlock<'i>>,
    /// Source location.
    pub loc: Location,
}

/// A single feature-values block like `@styleset { nice-style: 12; }`.
#[derive(Clone, Debug, PartialEq)]
pub struct FontFeatureValuesBlock<'i> {
    /// The feature name (`styleset`, `swash`, `annotation`, `ornaments`,
    /// `stylistic`, `character-variant`, `historical-forms`).
    pub name: Cow<'i, str>,
    /// Mapping from feature-name identifiers to integer values.
    pub values: Vec<(Cow<'i, str>, Vec<i32>)>,
}

// ─── PageRule ────────────────────────────────────────────────────────

/// `@page` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct PageRule<'i> {
    /// The page selectors (e.g. `:first`, `:left`).
    pub selectors: Vec<PageSelector<'i>>,
    /// The declaration block.
    pub declarations: DeclarationBlock<'i>,
    /// Nested page-margin rules (`@top-left-corner`, etc.).
    pub rules: Vec<PageMarginRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// A `@page` selector.
#[derive(Clone, Debug, PartialEq)]
pub struct PageSelector<'i> {
    /// Optional named-page ident.
    pub name: Option<Cow<'i, str>>,
    /// Page-pseudo-class list (`first`, `left`, `right`, `blank`).
    pub pseudo_classes: Vec<PagePseudoClass>,
}

/// Page pseudo-class.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PagePseudoClass {
    /// `:first`.
    First = 0,
    /// `:left`.
    Left = 1,
    /// `:right`.
    Right = 2,
    /// `:blank`.
    Blank = 3,
}

/// `@top-left-corner`, `@top-left`, etc. inside a `@page` block.
#[derive(Clone, Debug, PartialEq)]
pub struct PageMarginRule<'i> {
    /// Which page-margin box.
    pub margin_box: PageMarginBox,
    /// Declarations.
    pub declarations: DeclarationBlock<'i>,
    /// Source location.
    pub loc: Location,
}

/// CSS `@page` page-margin-box names.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PageMarginBox {
    /// `@top-left-corner`.
    TopLeftCorner = 0,
    /// `@top-left`.
    TopLeft = 1,
    /// `@top-center`.
    TopCenter = 2,
    /// `@top-right`.
    TopRight = 3,
    /// `@top-right-corner`.
    TopRightCorner = 4,
    /// `@bottom-left-corner`.
    BottomLeftCorner = 5,
    /// `@bottom-left`.
    BottomLeft = 6,
    /// `@bottom-center`.
    BottomCenter = 7,
    /// `@bottom-right`.
    BottomRight = 8,
    /// `@bottom-right-corner`.
    BottomRightCorner = 9,
    /// `@left-top`.
    LeftTop = 10,
    /// `@left-middle`.
    LeftMiddle = 11,
    /// `@left-bottom`.
    LeftBottom = 12,
    /// `@right-top`.
    RightTop = 13,
    /// `@right-middle`.
    RightMiddle = 14,
    /// `@right-bottom`.
    RightBottom = 15,
}

// ─── SupportsRule ────────────────────────────────────────────────────

/// `@supports` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct SupportsRule<'i> {
    /// The supports condition.
    pub condition: SupportsCondition<'i>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// `@supports` condition tree.
#[derive(Clone, Debug, PartialEq)]
pub enum SupportsCondition<'i> {
    /// A `not (...)` branch.
    Not(Box<SupportsCondition<'i>>),
    /// `(...) and (...) and ...`.
    And(Vec<SupportsCondition<'i>>),
    /// `(...) or (...) or ...`.
    Or(Vec<SupportsCondition<'i>>),
    /// A `(property: value)` declaration probe.
    Declaration {
        /// Property name.
        property_id: Cow<'i, str>,
        /// Raw value text.
        value: Cow<'i, str>,
    },
    /// `selector(...)` — tests for selector support.
    Selector(Cow<'i, str>),
    /// `font-format(...)` / `font-tech(...)` CSS feature function.
    FontFormat(Cow<'i, str>),
    /// Parenthesised wrapper.
    Parens(Box<SupportsCondition<'i>>),
    /// An unknown supports condition — preserved verbatim.
    Unknown(Cow<'i, str>),
}

// ─── CounterStyleRule ────────────────────────────────────────────────

/// `@counter-style` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct CounterStyleRule<'i> {
    /// The counter-style name.
    pub name: Cow<'i, str>,
    /// Declaration-like properties inside the block.
    pub properties: Vec<FontFaceProperty<'i>>,
    /// Source location.
    pub loc: Location,
}

// ─── NamespaceRule ───────────────────────────────────────────────────

/// `@namespace` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct NamespaceRule<'i> {
    /// Optional prefix (`p`).
    pub prefix: Option<Cow<'i, str>>,
    /// Namespace URL.
    pub url: Cow<'i, str>,
    /// Source location.
    pub loc: Location,
}

// ─── MozDocumentRule ─────────────────────────────────────────────────

/// `@-moz-document` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct MozDocumentRule<'i> {
    /// Match functions (e.g. `url-prefix("https://")`).
    pub matches: Vec<MozDocumentMatch<'i>>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// One match clause in an `@-moz-document` rule.
#[derive(Clone, Debug, PartialEq)]
pub enum MozDocumentMatch<'i> {
    /// `url(...)`.
    Url(Cow<'i, str>),
    /// `url-prefix(...)`.
    UrlPrefix(Cow<'i, str>),
    /// `domain(...)`.
    Domain(Cow<'i, str>),
    /// `regexp(...)`.
    Regexp(Cow<'i, str>),
    /// `media-document(all|image|video|plugin)`.
    MediaDocument(Cow<'i, str>),
}

// ─── NestingRule / NestedDeclarationsRule ────────────────────────────

/// `@nest` rule (pre-standardisation CSS Nesting).
#[derive(Clone, Debug, PartialEq)]
pub struct NestingRule<'i> {
    /// Inner style rule.
    pub style: StyleRule<'i>,
    /// Source location.
    pub loc: Location,
}

/// Implicit nested declarations block inside a nested context.
#[derive(Clone, Debug, PartialEq)]
pub struct NestedDeclarationsRule<'i> {
    /// The declarations.
    pub declarations: DeclarationBlock<'i>,
    /// Source location.
    pub loc: Location,
}

// ─── ViewportRule ────────────────────────────────────────────────────

/// `@viewport` rule (deprecated).
#[derive(Clone, Debug, PartialEq)]
pub struct ViewportRule<'i> {
    /// Vendor prefix (`@-ms-viewport`).
    pub vendor_prefix: Option<super::selectors::VendorPrefix>,
    /// Declaration block.
    pub declarations: DeclarationBlock<'i>,
    /// Source location.
    pub loc: Location,
}

// ─── CustomMediaRule ─────────────────────────────────────────────────

/// `@custom-media` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct CustomMediaRule<'i> {
    /// The custom-media name.
    pub name: Cow<'i, str>,
    /// The media query list.
    pub query: MediaList<'i>,
    /// Source location.
    pub loc: Location,
}

// ─── LayerStatement / LayerBlock ─────────────────────────────────────

/// `@layer name, name, ...;` statement.
#[derive(Clone, Debug, PartialEq)]
pub struct LayerStatementRule<'i> {
    /// Layer names.
    pub names: Vec<LayerName<'i>>,
    /// Source location.
    pub loc: Location,
}

/// `@layer name { ... }` block.
#[derive(Clone, Debug, PartialEq)]
pub struct LayerBlockRule<'i> {
    /// Optional layer name (anonymous when `None`).
    pub name: Option<LayerName<'i>>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// A layer name — dot-separated identifier path.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct LayerName<'i> {
    /// The parts (in source order).
    pub parts: Vec<Cow<'i, str>>,
}

// ─── PropertyRule ────────────────────────────────────────────────────

/// `@property` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct PropertyRule<'i> {
    /// Property name (must start with `--`).
    pub name: Cow<'i, str>,
    /// `syntax` descriptor (raw text).
    pub syntax: Option<Cow<'i, str>>,
    /// `inherits` boolean.
    pub inherits: Option<bool>,
    /// `initial-value` raw text.
    pub initial_value: Option<Cow<'i, str>>,
    /// Source location.
    pub loc: Location,
}

// ─── ContainerRule ───────────────────────────────────────────────────

/// `@container` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct ContainerRule<'i> {
    /// Optional container name.
    pub name: Option<Cow<'i, str>>,
    /// Container condition.
    pub condition: ContainerCondition<'i>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

/// `@container` condition — structurally parallel to media condition
/// but over container features.
#[derive(Clone, Debug, PartialEq)]
pub enum ContainerCondition<'i> {
    /// `(width: 100px)` style feature.
    Feature {
        /// Feature name.
        name: Cow<'i, str>,
        /// Feature value.
        value: Option<Cow<'i, str>>,
    },
    /// `not (...)`.
    Not(Box<ContainerCondition<'i>>),
    /// `... and ...`.
    And(Vec<ContainerCondition<'i>>),
    /// `... or ...`.
    Or(Vec<ContainerCondition<'i>>),
    /// `style(...)` condition.
    Style(Cow<'i, str>),
}

// ─── ScopeRule ───────────────────────────────────────────────────────

/// `@scope` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct ScopeRule<'i> {
    /// `scope-start` selector list (optional).
    pub scope_start: Option<SelectorList<'i>>,
    /// `scope-end` selector list (optional).
    pub scope_end: Option<SelectorList<'i>>,
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

// ─── StartingStyleRule ───────────────────────────────────────────────

/// `@starting-style` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct StartingStyleRule<'i> {
    /// Nested rules.
    pub rules: Vec<CssRule<'i>>,
    /// Source location.
    pub loc: Location,
}

// ─── ViewTransitionRule ──────────────────────────────────────────────

/// `@view-transition` rule.
#[derive(Clone, Debug, PartialEq)]
pub struct ViewTransitionRule<'i> {
    /// The declaration block (typically `navigation: auto|none;`).
    pub declarations: DeclarationBlock<'i>,
    /// Source location.
    pub loc: Location,
}

// ─── UnknownAtRule / CustomAtRule ────────────────────────────────────

/// Unknown at-rule (bbnf's `genericAtRule` fallback + lightningcss's
/// `Unknown` variant).
#[derive(Clone, Debug, PartialEq)]
pub struct UnknownAtRule<'i> {
    /// At-rule name including the `@` prefix.
    pub name: Cow<'i, str>,
    /// Prelude (text between the name and the body).
    pub prelude: Cow<'i, str>,
    /// Block body contents (raw text) if present.
    pub block: Option<Cow<'i, str>>,
    /// Source location.
    pub loc: Location,
}

/// Custom at-rule slot (lightningcss extension).
#[derive(Clone, Debug, PartialEq)]
pub struct CustomAtRule<'i> {
    /// At-rule name.
    pub name: Cow<'i, str>,
    /// Prelude text.
    pub prelude: Cow<'i, str>,
    /// Raw block body if present.
    pub block: Option<Cow<'i, str>>,
    /// Source location.
    pub loc: Location,
}
