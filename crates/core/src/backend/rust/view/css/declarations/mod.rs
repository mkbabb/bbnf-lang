//! CSS declaration AST — `bbnf::css::declarations` surface.
//!
//! AX.W1.B: typed property + declaration-block representation for
//! the `bbnf::css::StyleSheet` Value API.
//!
//! **Divergence vs lightningcss**: `lightningcss::properties::Property`
//! has hundreds of per-property variants (one per CSS property name
//! + parse-structured value type). bbnf's grammar parses property
//! NAMES structurally (via `colorProps`, `sizeProps`, etc. u8
//! discriminators) but captures property VALUES as a token list
//! (`(value ?w) *`). Semantic parity at the `Property<'i>` level
//! would require implementing every CSS property's value grammar —
//! out of scope for W1.B and not supplied by bbnf's L4 CSS grammar.
//!
//! The Value-API approach: a `Declaration` carries a typed
//! [`PropertyId`] + a parsed [`ValueList`]. Bridging to
//! `lightningcss::properties::Property` is via
//! [`Declaration::to_lightningcss_property_or_unparsed`], which
//! returns an `Unparsed` property for any property bbnf doesn't
//! know structurally (matching lightningcss's own fallback for
//! unparseable property values). This is the documented semantic
//! equivalence per invariant 5's "binary pass/fail" rule:
//! bbnf's `Declaration`s round-trip through lightningcss `Unparsed`.
//!
//! Details in `docs/tranches/AX/parity/css_divergence.md` §declarations.

use std::borrow::Cow;

use super::values::Value;

/// A declaration block — the `{ property: value; ... }` body of a
/// style rule.
///
/// Separates normal declarations from `!important` declarations per
/// lightningcss's layout.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct DeclarationBlock<'i> {
    /// Normal-priority declarations in source order.
    pub declarations: Vec<Declaration<'i>>,
    /// `!important` declarations in source order.
    pub important_declarations: Vec<Declaration<'i>>,
}

/// A single declaration: `property: value;`.
#[derive(Clone, Debug, PartialEq)]
pub struct Declaration<'i> {
    /// The property identifier.
    pub property_id: PropertyId<'i>,
    /// The value list (possibly multi-value shorthand).
    pub value: ValueList<'i>,
    /// `!important` flag.
    pub important: bool,
    /// Source span of the declaration (byte offsets into the input).
    pub span: (u32, u32),
}

/// A declaration's value list — one or more typed values separated
/// by whitespace / commas / slashes.
///
/// Keeps commas and slashes as inline [`Value::Comma`] / [`Value::Slash`]
/// tokens so multi-value shorthand round-trips.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ValueList<'i> {
    /// The value tokens in source order.
    pub values: Vec<Value<'i>>,
}

/// Property identifier.
///
/// Typed branches cover the groups bbnf's `properties.bbnf` dispatch
/// tables enumerate (`colorProps`, `sizeProps`, `spacingProps`,
/// `fontProps`, `bgProps`, `transformProps`, `transitionProps`,
/// `listTableProps`, plus the single-property rules). Any property
/// name outside these groups lands in [`Self::Custom`] or
/// [`Self::Unknown`].
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PropertyId<'i> {
    /// A colour property: `color`, `background-color`, `border-*-color`, etc.
    ///
    /// Discriminant matches the grammar's `colorProps` u8 tags.
    Color(ColorProp),
    /// A size property: `width`, `height`, `min-*`, `max-*`, etc.
    ///
    /// Discriminant matches the grammar's `sizeProps` u8 tags.
    Size(SizeProp),
    /// A spacing property: `margin-*`, `padding-*`, `top`/`right`/...,
    /// `gap`, etc.
    Spacing(SpacingProp),
    /// A font property: `font-family`, `text-transform`, etc.
    Font(FontProp),
    /// A background property: `background-image`, `background-size`, etc.
    Background(BackgroundProp),
    /// A transform property: `transform`, `transform-origin`, etc.
    Transform(TransformProp),
    /// A transition / animation property.
    Transition(TransitionProp),
    /// A list / table property.
    ListTable(ListTableProp),
    /// `display`.
    Display,
    /// `position`.
    Position,
    /// `overflow-x` / `overflow-y` / `overflow`.
    Overflow(OverflowAxis),
    /// `visibility`.
    Visibility,
    /// `flex-direction`.
    FlexDirection,
    /// `flex-wrap`.
    FlexWrap,
    /// `justify-*` / `align-*` family.
    Align(AlignProp),
    /// `flex-grow` / `flex-shrink` / `order` / `z-index`.
    FlexNum(FlexNumProp),
    /// `font-size`.
    FontSize,
    /// `font-weight`.
    FontWeight,
    /// `line-height`.
    LineHeight,
    /// `border-*-width`.
    BorderWidth(BorderEdge),
    /// `border-*-style`.
    BorderStyle(BorderEdge),
    /// `border-*-radius`.
    BorderRadius(BorderRadiusCorner),
    /// `opacity`.
    Opacity,
    /// `text-align`.
    TextAlign,
    /// `box-sizing`.
    BoxSizing,
    /// `cursor`.
    Cursor,
    /// A custom property declaration starting with `--`.
    Custom(Cow<'i, str>),
    /// A property whose name wasn't recognised by the grammar's
    /// typed dispatch tables — captured verbatim.
    Unknown(Cow<'i, str>),
}

// --- Color props ---

/// `colorProps` sub-discriminator.
///
/// Matches grammar `colorProps` u8 tags (see `properties.bbnf`).
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ColorProp {
    /// `color`.
    Color = 0,
    /// `background-color`.
    BackgroundColor = 1,
    /// `border-color` (shorthand).
    BorderColor = 2,
    /// `border-top-color`.
    BorderTopColor = 3,
    /// `border-right-color`.
    BorderRightColor = 4,
    /// `border-bottom-color`.
    BorderBottomColor = 5,
    /// `border-left-color`.
    BorderLeftColor = 6,
    /// `outline-color`.
    OutlineColor = 7,
    /// `text-decoration-color`.
    TextDecorationColor = 8,
    /// `caret-color`.
    CaretColor = 9,
    /// `accent-color`.
    AccentColor = 10,
}

// --- Size props ---

/// `sizeProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SizeProp {
    /// `width`.
    Width = 0,
    /// `height`.
    Height = 1,
    /// `min-width`.
    MinWidth = 2,
    /// `max-width`.
    MaxWidth = 3,
    /// `min-height`.
    MinHeight = 4,
    /// `max-height`.
    MaxHeight = 5,
    /// `flex-basis`.
    FlexBasis = 6,
    /// `block-size`.
    BlockSize = 7,
    /// `inline-size`.
    InlineSize = 8,
    /// `min-block-size`.
    MinBlockSize = 9,
    /// `max-block-size`.
    MaxBlockSize = 10,
    /// `min-inline-size`.
    MinInlineSize = 11,
    /// `max-inline-size`.
    MaxInlineSize = 12,
}

// --- Spacing props ---

/// `spacingProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SpacingProp {
    /// `margin`.
    Margin = 0,
    /// `margin-top`.
    MarginTop = 1,
    /// `margin-right`.
    MarginRight = 2,
    /// `margin-bottom`.
    MarginBottom = 3,
    /// `margin-left`.
    MarginLeft = 4,
    /// `padding`.
    Padding = 5,
    /// `padding-top`.
    PaddingTop = 6,
    /// `padding-right`.
    PaddingRight = 7,
    /// `padding-bottom`.
    PaddingBottom = 8,
    /// `padding-left`.
    PaddingLeft = 9,
    /// `top`.
    Top = 10,
    /// `right`.
    Right = 11,
    /// `bottom`.
    Bottom = 12,
    /// `left`.
    Left = 13,
    /// `inset`.
    Inset = 14,
    /// `gap`.
    Gap = 15,
    /// `row-gap`.
    RowGap = 16,
    /// `column-gap`.
    ColumnGap = 17,
}

// --- Font props ---

/// `fontProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum FontProp {
    /// `font-family`.
    FontFamily = 0,
    /// `font-style`.
    FontStyle = 1,
    /// `font-variant`.
    FontVariant = 2,
    /// `font` (shorthand).
    Font = 3,
    /// `text-transform`.
    TextTransform = 4,
    /// `text-decoration` (shorthand).
    TextDecoration = 5,
    /// `text-decoration-line`.
    TextDecorationLine = 6,
    /// `text-decoration-style`.
    TextDecorationStyle = 7,
    /// `text-decoration-thickness`.
    TextDecorationThickness = 8,
    /// `letter-spacing`.
    LetterSpacing = 9,
    /// `word-spacing`.
    WordSpacing = 10,
    /// `white-space`.
    WhiteSpace = 11,
    /// `text-indent`.
    TextIndent = 12,
    /// `text-overflow`.
    TextOverflow = 13,
    /// `word-break`.
    WordBreak = 14,
    /// `overflow-wrap`.
    OverflowWrap = 15,
    /// `hyphens`.
    Hyphens = 16,
}

// --- Background props ---

/// `bgProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BackgroundProp {
    /// `background`.
    Background = 0,
    /// `background-image`.
    BackgroundImage = 1,
    /// `background-position`.
    BackgroundPosition = 2,
    /// `background-size`.
    BackgroundSize = 3,
    /// `background-repeat`.
    BackgroundRepeat = 4,
    /// `background-attachment`.
    BackgroundAttachment = 5,
    /// `background-clip`.
    BackgroundClip = 6,
    /// `background-origin`.
    BackgroundOrigin = 7,
    /// `background-blend-mode`.
    BackgroundBlendMode = 8,
}

// --- Transform props ---

/// `transformProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TransformProp {
    /// `transform`.
    Transform = 0,
    /// `transform-origin`.
    TransformOrigin = 1,
    /// `transform-style`.
    TransformStyle = 2,
    /// `perspective`.
    Perspective = 3,
    /// `perspective-origin`.
    PerspectiveOrigin = 4,
    /// `backface-visibility`.
    BackfaceVisibility = 5,
}

// --- Transition props ---

/// `transitionProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TransitionProp {
    /// `transition` (shorthand).
    Transition = 0,
    /// `transition-property`.
    TransitionProperty = 1,
    /// `transition-duration`.
    TransitionDuration = 2,
    /// `transition-timing-function`.
    TransitionTimingFunction = 3,
    /// `transition-delay`.
    TransitionDelay = 4,
    /// `animation` (shorthand).
    Animation = 5,
    /// `animation-name`.
    AnimationName = 6,
    /// `animation-duration`.
    AnimationDuration = 7,
    /// `animation-timing-function`.
    AnimationTimingFunction = 8,
    /// `animation-delay`.
    AnimationDelay = 9,
    /// `animation-iteration-count`.
    AnimationIterationCount = 10,
    /// `animation-direction`.
    AnimationDirection = 11,
    /// `animation-fill-mode`.
    AnimationFillMode = 12,
    /// `animation-play-state`.
    AnimationPlayState = 13,
}

// --- List / table props ---

/// `listTableProps` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ListTableProp {
    /// `list-style` (shorthand).
    ListStyle = 0,
    /// `list-style-type`.
    ListStyleType = 1,
    /// `list-style-position`.
    ListStylePosition = 2,
    /// `list-style-image`.
    ListStyleImage = 3,
    /// `table-layout`.
    TableLayout = 4,
    /// `border-collapse`.
    BorderCollapse = 5,
    /// `border-spacing`.
    BorderSpacing = 6,
    /// `caption-side`.
    CaptionSide = 7,
    /// `empty-cells`.
    EmptyCells = 8,
    /// `vertical-align`.
    VerticalAlign = 9,
}

// --- Overflow axis ---

/// Overflow axis sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum OverflowAxis {
    /// `overflow` (shorthand).
    Shorthand = 0,
    /// `overflow-x`.
    X = 1,
    /// `overflow-y`.
    Y = 2,
}

// --- Align props ---

/// Align-family sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AlignProp {
    /// `justify-content`.
    JustifyContent = 0,
    /// `align-items`.
    AlignItems = 1,
    /// `align-content`.
    AlignContent = 2,
    /// `align-self`.
    AlignSelf = 3,
    /// `justify-items`.
    JustifyItems = 4,
    /// `justify-self`.
    JustifySelf = 5,
}

// --- Flex-num props ---

/// `flex-grow`/`flex-shrink`/`order`/`z-index` sub-discriminator.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum FlexNumProp {
    /// `flex-grow`.
    FlexGrow = 0,
    /// `flex-shrink`.
    FlexShrink = 1,
    /// `order`.
    Order = 2,
    /// `z-index`.
    ZIndex = 3,
}

// --- Border edge ---

/// Edge discriminator for `border-*-width` / `border-*-style`.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BorderEdge {
    /// Shorthand (`border-width` / `border-style`).
    Shorthand = 0,
    /// Top.
    Top = 1,
    /// Right.
    Right = 2,
    /// Bottom.
    Bottom = 3,
    /// Left.
    Left = 4,
}

// --- Border radius corner ---

/// Corner discriminator for `border-*-radius`.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BorderRadiusCorner {
    /// Shorthand `border-radius`.
    Shorthand = 0,
    /// Top-left.
    TopLeft = 1,
    /// Top-right.
    TopRight = 2,
    /// Bottom-left.
    BottomLeft = 3,
    /// Bottom-right.
    BottomRight = 4,
}

/// Lookup a property name against bbnf's typed tables.
///
/// Returns the matching [`PropertyId`] or [`PropertyId::Unknown`] if
/// the name isn't recognised. `--`-prefixed names route to
/// [`PropertyId::Custom`].
pub fn property_id_from_name<'i>(name: &'i str) -> PropertyId<'i> {
    if name.starts_with("--") {
        return PropertyId::Custom(Cow::Borrowed(name));
    }
    // Try the typed tables in the same order as the grammar's
    // declaration dispatch (`customPropertyDecl | colorDecl | sizeDecl
    // | spacingDecl | ...`). Names outside any table fall through
    // to `Unknown`.
    match name {
        // Color
        "color" => PropertyId::Color(ColorProp::Color),
        "background-color" => PropertyId::Color(ColorProp::BackgroundColor),
        "border-color" => PropertyId::Color(ColorProp::BorderColor),
        "border-top-color" => PropertyId::Color(ColorProp::BorderTopColor),
        "border-right-color" => PropertyId::Color(ColorProp::BorderRightColor),
        "border-bottom-color" => PropertyId::Color(ColorProp::BorderBottomColor),
        "border-left-color" => PropertyId::Color(ColorProp::BorderLeftColor),
        "outline-color" => PropertyId::Color(ColorProp::OutlineColor),
        "text-decoration-color" => PropertyId::Color(ColorProp::TextDecorationColor),
        "caret-color" => PropertyId::Color(ColorProp::CaretColor),
        "accent-color" => PropertyId::Color(ColorProp::AccentColor),
        // Size
        "width" => PropertyId::Size(SizeProp::Width),
        "height" => PropertyId::Size(SizeProp::Height),
        "min-width" => PropertyId::Size(SizeProp::MinWidth),
        "max-width" => PropertyId::Size(SizeProp::MaxWidth),
        "min-height" => PropertyId::Size(SizeProp::MinHeight),
        "max-height" => PropertyId::Size(SizeProp::MaxHeight),
        "flex-basis" => PropertyId::Size(SizeProp::FlexBasis),
        "block-size" => PropertyId::Size(SizeProp::BlockSize),
        "inline-size" => PropertyId::Size(SizeProp::InlineSize),
        "min-block-size" => PropertyId::Size(SizeProp::MinBlockSize),
        "max-block-size" => PropertyId::Size(SizeProp::MaxBlockSize),
        "min-inline-size" => PropertyId::Size(SizeProp::MinInlineSize),
        "max-inline-size" => PropertyId::Size(SizeProp::MaxInlineSize),
        // Spacing
        "margin" => PropertyId::Spacing(SpacingProp::Margin),
        "margin-top" => PropertyId::Spacing(SpacingProp::MarginTop),
        "margin-right" => PropertyId::Spacing(SpacingProp::MarginRight),
        "margin-bottom" => PropertyId::Spacing(SpacingProp::MarginBottom),
        "margin-left" => PropertyId::Spacing(SpacingProp::MarginLeft),
        "padding" => PropertyId::Spacing(SpacingProp::Padding),
        "padding-top" => PropertyId::Spacing(SpacingProp::PaddingTop),
        "padding-right" => PropertyId::Spacing(SpacingProp::PaddingRight),
        "padding-bottom" => PropertyId::Spacing(SpacingProp::PaddingBottom),
        "padding-left" => PropertyId::Spacing(SpacingProp::PaddingLeft),
        "top" => PropertyId::Spacing(SpacingProp::Top),
        "right" => PropertyId::Spacing(SpacingProp::Right),
        "bottom" => PropertyId::Spacing(SpacingProp::Bottom),
        "left" => PropertyId::Spacing(SpacingProp::Left),
        "inset" => PropertyId::Spacing(SpacingProp::Inset),
        "gap" => PropertyId::Spacing(SpacingProp::Gap),
        "row-gap" => PropertyId::Spacing(SpacingProp::RowGap),
        "column-gap" => PropertyId::Spacing(SpacingProp::ColumnGap),
        // Font / text
        "font-family" => PropertyId::Font(FontProp::FontFamily),
        "font-style" => PropertyId::Font(FontProp::FontStyle),
        "font-variant" => PropertyId::Font(FontProp::FontVariant),
        "font" => PropertyId::Font(FontProp::Font),
        "text-transform" => PropertyId::Font(FontProp::TextTransform),
        "text-decoration" => PropertyId::Font(FontProp::TextDecoration),
        "text-decoration-line" => PropertyId::Font(FontProp::TextDecorationLine),
        "text-decoration-style" => PropertyId::Font(FontProp::TextDecorationStyle),
        "text-decoration-thickness" => PropertyId::Font(FontProp::TextDecorationThickness),
        "letter-spacing" => PropertyId::Font(FontProp::LetterSpacing),
        "word-spacing" => PropertyId::Font(FontProp::WordSpacing),
        "white-space" => PropertyId::Font(FontProp::WhiteSpace),
        "text-indent" => PropertyId::Font(FontProp::TextIndent),
        "text-overflow" => PropertyId::Font(FontProp::TextOverflow),
        "word-break" => PropertyId::Font(FontProp::WordBreak),
        "overflow-wrap" => PropertyId::Font(FontProp::OverflowWrap),
        "hyphens" => PropertyId::Font(FontProp::Hyphens),
        // Background
        "background" => PropertyId::Background(BackgroundProp::Background),
        "background-image" => PropertyId::Background(BackgroundProp::BackgroundImage),
        "background-position" => PropertyId::Background(BackgroundProp::BackgroundPosition),
        "background-size" => PropertyId::Background(BackgroundProp::BackgroundSize),
        "background-repeat" => PropertyId::Background(BackgroundProp::BackgroundRepeat),
        "background-attachment" => PropertyId::Background(BackgroundProp::BackgroundAttachment),
        "background-clip" => PropertyId::Background(BackgroundProp::BackgroundClip),
        "background-origin" => PropertyId::Background(BackgroundProp::BackgroundOrigin),
        "background-blend-mode" => PropertyId::Background(BackgroundProp::BackgroundBlendMode),
        // Transform
        "transform" => PropertyId::Transform(TransformProp::Transform),
        "transform-origin" => PropertyId::Transform(TransformProp::TransformOrigin),
        "transform-style" => PropertyId::Transform(TransformProp::TransformStyle),
        "perspective" => PropertyId::Transform(TransformProp::Perspective),
        "perspective-origin" => PropertyId::Transform(TransformProp::PerspectiveOrigin),
        "backface-visibility" => PropertyId::Transform(TransformProp::BackfaceVisibility),
        // Transition / animation
        "transition" => PropertyId::Transition(TransitionProp::Transition),
        "transition-property" => PropertyId::Transition(TransitionProp::TransitionProperty),
        "transition-duration" => PropertyId::Transition(TransitionProp::TransitionDuration),
        "transition-timing-function" => {
            PropertyId::Transition(TransitionProp::TransitionTimingFunction)
        }
        "transition-delay" => PropertyId::Transition(TransitionProp::TransitionDelay),
        "animation" => PropertyId::Transition(TransitionProp::Animation),
        "animation-name" => PropertyId::Transition(TransitionProp::AnimationName),
        "animation-duration" => PropertyId::Transition(TransitionProp::AnimationDuration),
        "animation-timing-function" => {
            PropertyId::Transition(TransitionProp::AnimationTimingFunction)
        }
        "animation-delay" => PropertyId::Transition(TransitionProp::AnimationDelay),
        "animation-iteration-count" => {
            PropertyId::Transition(TransitionProp::AnimationIterationCount)
        }
        "animation-direction" => PropertyId::Transition(TransitionProp::AnimationDirection),
        "animation-fill-mode" => PropertyId::Transition(TransitionProp::AnimationFillMode),
        "animation-play-state" => PropertyId::Transition(TransitionProp::AnimationPlayState),
        // List / table
        "list-style" => PropertyId::ListTable(ListTableProp::ListStyle),
        "list-style-type" => PropertyId::ListTable(ListTableProp::ListStyleType),
        "list-style-position" => PropertyId::ListTable(ListTableProp::ListStylePosition),
        "list-style-image" => PropertyId::ListTable(ListTableProp::ListStyleImage),
        "table-layout" => PropertyId::ListTable(ListTableProp::TableLayout),
        "border-collapse" => PropertyId::ListTable(ListTableProp::BorderCollapse),
        "border-spacing" => PropertyId::ListTable(ListTableProp::BorderSpacing),
        "caption-side" => PropertyId::ListTable(ListTableProp::CaptionSide),
        "empty-cells" => PropertyId::ListTable(ListTableProp::EmptyCells),
        "vertical-align" => PropertyId::ListTable(ListTableProp::VerticalAlign),
        // Single-property rules
        "display" => PropertyId::Display,
        "position" => PropertyId::Position,
        "overflow" => PropertyId::Overflow(OverflowAxis::Shorthand),
        "overflow-x" => PropertyId::Overflow(OverflowAxis::X),
        "overflow-y" => PropertyId::Overflow(OverflowAxis::Y),
        "visibility" => PropertyId::Visibility,
        "flex-direction" => PropertyId::FlexDirection,
        "flex-wrap" => PropertyId::FlexWrap,
        "justify-content" => PropertyId::Align(AlignProp::JustifyContent),
        "align-items" => PropertyId::Align(AlignProp::AlignItems),
        "align-content" => PropertyId::Align(AlignProp::AlignContent),
        "align-self" => PropertyId::Align(AlignProp::AlignSelf),
        "justify-items" => PropertyId::Align(AlignProp::JustifyItems),
        "justify-self" => PropertyId::Align(AlignProp::JustifySelf),
        "flex-grow" => PropertyId::FlexNum(FlexNumProp::FlexGrow),
        "flex-shrink" => PropertyId::FlexNum(FlexNumProp::FlexShrink),
        "order" => PropertyId::FlexNum(FlexNumProp::Order),
        "z-index" => PropertyId::FlexNum(FlexNumProp::ZIndex),
        "font-size" => PropertyId::FontSize,
        "font-weight" => PropertyId::FontWeight,
        "line-height" => PropertyId::LineHeight,
        "border-width" => PropertyId::BorderWidth(BorderEdge::Shorthand),
        "border-top-width" => PropertyId::BorderWidth(BorderEdge::Top),
        "border-right-width" => PropertyId::BorderWidth(BorderEdge::Right),
        "border-bottom-width" => PropertyId::BorderWidth(BorderEdge::Bottom),
        "border-left-width" => PropertyId::BorderWidth(BorderEdge::Left),
        "border-style" => PropertyId::BorderStyle(BorderEdge::Shorthand),
        "border-top-style" => PropertyId::BorderStyle(BorderEdge::Top),
        "border-right-style" => PropertyId::BorderStyle(BorderEdge::Right),
        "border-bottom-style" => PropertyId::BorderStyle(BorderEdge::Bottom),
        "border-left-style" => PropertyId::BorderStyle(BorderEdge::Left),
        "border-radius" => PropertyId::BorderRadius(BorderRadiusCorner::Shorthand),
        "border-top-left-radius" => PropertyId::BorderRadius(BorderRadiusCorner::TopLeft),
        "border-top-right-radius" => PropertyId::BorderRadius(BorderRadiusCorner::TopRight),
        "border-bottom-left-radius" => PropertyId::BorderRadius(BorderRadiusCorner::BottomLeft),
        "border-bottom-right-radius" => PropertyId::BorderRadius(BorderRadiusCorner::BottomRight),
        "opacity" => PropertyId::Opacity,
        "text-align" => PropertyId::TextAlign,
        "box-sizing" => PropertyId::BoxSizing,
        "cursor" => PropertyId::Cursor,
        _ => PropertyId::Unknown(Cow::Borrowed(name)),
    }
}

impl<'i> PropertyId<'i> {
    /// The property's name as it appears in source (lowercase).
    pub fn name(&self) -> &str {
        match self {
            Self::Color(c) => match c {
                ColorProp::Color => "color",
                ColorProp::BackgroundColor => "background-color",
                ColorProp::BorderColor => "border-color",
                ColorProp::BorderTopColor => "border-top-color",
                ColorProp::BorderRightColor => "border-right-color",
                ColorProp::BorderBottomColor => "border-bottom-color",
                ColorProp::BorderLeftColor => "border-left-color",
                ColorProp::OutlineColor => "outline-color",
                ColorProp::TextDecorationColor => "text-decoration-color",
                ColorProp::CaretColor => "caret-color",
                ColorProp::AccentColor => "accent-color",
            },
            Self::Size(s) => match s {
                SizeProp::Width => "width",
                SizeProp::Height => "height",
                SizeProp::MinWidth => "min-width",
                SizeProp::MaxWidth => "max-width",
                SizeProp::MinHeight => "min-height",
                SizeProp::MaxHeight => "max-height",
                SizeProp::FlexBasis => "flex-basis",
                SizeProp::BlockSize => "block-size",
                SizeProp::InlineSize => "inline-size",
                SizeProp::MinBlockSize => "min-block-size",
                SizeProp::MaxBlockSize => "max-block-size",
                SizeProp::MinInlineSize => "min-inline-size",
                SizeProp::MaxInlineSize => "max-inline-size",
            },
            Self::Spacing(s) => match s {
                SpacingProp::Margin => "margin",
                SpacingProp::MarginTop => "margin-top",
                SpacingProp::MarginRight => "margin-right",
                SpacingProp::MarginBottom => "margin-bottom",
                SpacingProp::MarginLeft => "margin-left",
                SpacingProp::Padding => "padding",
                SpacingProp::PaddingTop => "padding-top",
                SpacingProp::PaddingRight => "padding-right",
                SpacingProp::PaddingBottom => "padding-bottom",
                SpacingProp::PaddingLeft => "padding-left",
                SpacingProp::Top => "top",
                SpacingProp::Right => "right",
                SpacingProp::Bottom => "bottom",
                SpacingProp::Left => "left",
                SpacingProp::Inset => "inset",
                SpacingProp::Gap => "gap",
                SpacingProp::RowGap => "row-gap",
                SpacingProp::ColumnGap => "column-gap",
            },
            Self::Font(f) => match f {
                FontProp::FontFamily => "font-family",
                FontProp::FontStyle => "font-style",
                FontProp::FontVariant => "font-variant",
                FontProp::Font => "font",
                FontProp::TextTransform => "text-transform",
                FontProp::TextDecoration => "text-decoration",
                FontProp::TextDecorationLine => "text-decoration-line",
                FontProp::TextDecorationStyle => "text-decoration-style",
                FontProp::TextDecorationThickness => "text-decoration-thickness",
                FontProp::LetterSpacing => "letter-spacing",
                FontProp::WordSpacing => "word-spacing",
                FontProp::WhiteSpace => "white-space",
                FontProp::TextIndent => "text-indent",
                FontProp::TextOverflow => "text-overflow",
                FontProp::WordBreak => "word-break",
                FontProp::OverflowWrap => "overflow-wrap",
                FontProp::Hyphens => "hyphens",
            },
            Self::Background(b) => match b {
                BackgroundProp::Background => "background",
                BackgroundProp::BackgroundImage => "background-image",
                BackgroundProp::BackgroundPosition => "background-position",
                BackgroundProp::BackgroundSize => "background-size",
                BackgroundProp::BackgroundRepeat => "background-repeat",
                BackgroundProp::BackgroundAttachment => "background-attachment",
                BackgroundProp::BackgroundClip => "background-clip",
                BackgroundProp::BackgroundOrigin => "background-origin",
                BackgroundProp::BackgroundBlendMode => "background-blend-mode",
            },
            Self::Transform(t) => match t {
                TransformProp::Transform => "transform",
                TransformProp::TransformOrigin => "transform-origin",
                TransformProp::TransformStyle => "transform-style",
                TransformProp::Perspective => "perspective",
                TransformProp::PerspectiveOrigin => "perspective-origin",
                TransformProp::BackfaceVisibility => "backface-visibility",
            },
            Self::Transition(t) => match t {
                TransitionProp::Transition => "transition",
                TransitionProp::TransitionProperty => "transition-property",
                TransitionProp::TransitionDuration => "transition-duration",
                TransitionProp::TransitionTimingFunction => "transition-timing-function",
                TransitionProp::TransitionDelay => "transition-delay",
                TransitionProp::Animation => "animation",
                TransitionProp::AnimationName => "animation-name",
                TransitionProp::AnimationDuration => "animation-duration",
                TransitionProp::AnimationTimingFunction => "animation-timing-function",
                TransitionProp::AnimationDelay => "animation-delay",
                TransitionProp::AnimationIterationCount => "animation-iteration-count",
                TransitionProp::AnimationDirection => "animation-direction",
                TransitionProp::AnimationFillMode => "animation-fill-mode",
                TransitionProp::AnimationPlayState => "animation-play-state",
            },
            Self::ListTable(l) => match l {
                ListTableProp::ListStyle => "list-style",
                ListTableProp::ListStyleType => "list-style-type",
                ListTableProp::ListStylePosition => "list-style-position",
                ListTableProp::ListStyleImage => "list-style-image",
                ListTableProp::TableLayout => "table-layout",
                ListTableProp::BorderCollapse => "border-collapse",
                ListTableProp::BorderSpacing => "border-spacing",
                ListTableProp::CaptionSide => "caption-side",
                ListTableProp::EmptyCells => "empty-cells",
                ListTableProp::VerticalAlign => "vertical-align",
            },
            Self::Display => "display",
            Self::Position => "position",
            Self::Overflow(a) => match a {
                OverflowAxis::Shorthand => "overflow",
                OverflowAxis::X => "overflow-x",
                OverflowAxis::Y => "overflow-y",
            },
            Self::Visibility => "visibility",
            Self::FlexDirection => "flex-direction",
            Self::FlexWrap => "flex-wrap",
            Self::Align(a) => match a {
                AlignProp::JustifyContent => "justify-content",
                AlignProp::AlignItems => "align-items",
                AlignProp::AlignContent => "align-content",
                AlignProp::AlignSelf => "align-self",
                AlignProp::JustifyItems => "justify-items",
                AlignProp::JustifySelf => "justify-self",
            },
            Self::FlexNum(n) => match n {
                FlexNumProp::FlexGrow => "flex-grow",
                FlexNumProp::FlexShrink => "flex-shrink",
                FlexNumProp::Order => "order",
                FlexNumProp::ZIndex => "z-index",
            },
            Self::FontSize => "font-size",
            Self::FontWeight => "font-weight",
            Self::LineHeight => "line-height",
            Self::BorderWidth(e) => match e {
                BorderEdge::Shorthand => "border-width",
                BorderEdge::Top => "border-top-width",
                BorderEdge::Right => "border-right-width",
                BorderEdge::Bottom => "border-bottom-width",
                BorderEdge::Left => "border-left-width",
            },
            Self::BorderStyle(e) => match e {
                BorderEdge::Shorthand => "border-style",
                BorderEdge::Top => "border-top-style",
                BorderEdge::Right => "border-right-style",
                BorderEdge::Bottom => "border-bottom-style",
                BorderEdge::Left => "border-left-style",
            },
            Self::BorderRadius(c) => match c {
                BorderRadiusCorner::Shorthand => "border-radius",
                BorderRadiusCorner::TopLeft => "border-top-left-radius",
                BorderRadiusCorner::TopRight => "border-top-right-radius",
                BorderRadiusCorner::BottomLeft => "border-bottom-left-radius",
                BorderRadiusCorner::BottomRight => "border-bottom-right-radius",
            },
            Self::Opacity => "opacity",
            Self::TextAlign => "text-align",
            Self::BoxSizing => "box-sizing",
            Self::Cursor => "cursor",
            Self::Custom(s) | Self::Unknown(s) => s,
        }
    }
}
