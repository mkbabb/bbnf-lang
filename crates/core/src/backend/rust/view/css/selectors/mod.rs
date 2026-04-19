//! CSS selector AST — `bbnf::css::selectors` surface.
//!
//! AX.W1.B: isomorphic-to-lightningcss selector representation for
//! the `bbnf::css::StyleSheet` Value API. Every variant required by
//! `lightningcss::selector::Component` is expressible here; bbnf's
//! grammar populates the subset its `selectors.bbnf` rules actually
//! parse (type, class, id, attribute, pseudo-class, pseudo-element,
//! combinators). Divergences against `parcel_selectors::parser::Component`
//! are listed in `docs/tranches/AX/parity/css_divergence.md` §selectors.
//!
//! The representation is a **flat compound list** per CSS Selectors L4
//! syntax:
//!   Selector = Vec<Component>
//!   SelectorList = Vec<Selector>
//! Complex selectors interleave compound components with
//! [`Combinator`] entries; `parse_compound_and_combinator` walks the
//! flat list into compound → combinator → compound form when a
//! consumer wants tree shape.
//!
//! Every struct and enum variant is field-complete per invariant 18;
//! no `_` catch-alls, no `todo!()`, no placeholder surfaces.

use std::borrow::Cow;

/// A list of selectors separated by `,` in source.
///
/// Isomorphic to `lightningcss::selector::SelectorList` (which is a
/// typedef for `parcel_selectors::SelectorList<'i, Selectors>`).
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct SelectorList<'i> {
    /// The individual selectors in the list.
    pub selectors: Vec<Selector<'i>>,
}

/// A single CSS selector — a flat sequence of components including
/// simple selectors and combinators.
///
/// Isomorphic to `lightningcss::selector::Selector`. Component order
/// is source order; combinators appear between the compound blocks
/// they join.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct Selector<'i> {
    /// The flat component sequence.
    pub components: Vec<Component<'i>>,
}

/// A combinator between two compound selectors.
///
/// Discriminant order matches `css_l4/selectors.bbnf` combinator rule:
/// Descendant=0, Child=1, NextSibling=2, LaterSibling=3 — which also
/// matches `parcel_selectors::parser::Combinator` (modulo PseudoElement /
/// SlotAssignment which bbnf doesn't populate; see divergence doc).
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Combinator {
    /// Descendant combinator (whitespace).
    Descendant = 0,
    /// Child combinator `>`.
    Child = 1,
    /// Next-sibling combinator `+`.
    NextSibling = 2,
    /// Later-sibling combinator `~` (general sibling).
    LaterSibling = 3,
    /// Pseudo-element implicit combinator (lightningcss-only divergence;
    /// populated only via `From<lightningcss::SelectorList>`).
    PseudoElement = 4,
    /// Slot-assignment implicit combinator (lightningcss-only divergence;
    /// populated only via `From<lightningcss::SelectorList>`).
    SlotAssignment = 5,
    /// Part pseudo-element implicit combinator (lightningcss-only
    /// divergence).
    Part = 6,
    /// Deep descendant combinator `>>>` (legacy, lightningcss-only).
    DeepDescendant = 7,
    /// Deep combinator `/deep/` (legacy, lightningcss-only).
    Deep = 8,
}

/// A single component within a [`Selector`].
///
/// Field-complete isomorphism against `parcel_selectors::parser::Component`:
/// every Component variant lightningcss exposes has a matching variant
/// here. bbnf's grammar populates the variants its `selectors.bbnf`
/// parses; lightningcss-only variants populate via `From<lightningcss>`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Component<'i> {
    /// Type selector (element name), e.g. `div`, `a`.
    LocalName {
        /// The local element name.
        name: Cow<'i, str>,
        /// The lowercase form for case-insensitive match.
        lower_name: Cow<'i, str>,
    },
    /// Explicit universal `*`.
    ExplicitUniversalType,
    /// Explicit no-namespace `|*` or `|elem`.
    ExplicitNoNamespace,
    /// Explicit any-namespace `*|*` or `*|elem`.
    ExplicitAnyNamespace,
    /// Namespace-qualified: `ns|local`.
    Namespace {
        /// The namespace prefix.
        prefix: Cow<'i, str>,
        /// The namespace URL (resolved via `@namespace`).
        url: Cow<'i, str>,
    },
    /// Default namespace (set via a preceding `@namespace` at-rule).
    DefaultNamespace(Cow<'i, str>),
    /// ID selector `#foo`.
    ID(Cow<'i, str>),
    /// Class selector `.foo`.
    Class(Cow<'i, str>),
    /// Attribute selector in *no* namespace, with matcher/value.
    ///
    /// `[attr]` → `operator = None`.
    /// `[attr=value]` / `[attr~=value]` / etc. → `operator = Some(...)`.
    AttributeInNoNamespace {
        /// Local name of the attribute.
        local_name: Cow<'i, str>,
        /// Lowercase local name.
        local_name_lower: Cow<'i, str>,
        /// The attribute match operator.
        operator: Option<AttributeOperator>,
        /// The match value (present iff operator is Some).
        value: Option<Cow<'i, str>>,
        /// Case-sensitivity modifier (`i`/`s`).
        case_sensitivity: AttributeCaseSensitivity,
        /// Never-matches-anything flag (grammar doesn't populate; set
        /// by lightningcss when `case_sensitivity` is incompatible
        /// with a known HTML attribute).
        never_matches: bool,
    },
    /// Attribute selector with an explicit namespace prefix.
    AttributeWithNamespace {
        /// The namespace prefix (resolved).
        namespace_url: Cow<'i, str>,
        /// Local attribute name.
        local_name: Cow<'i, str>,
        /// Lowercase local attribute name.
        local_name_lower: Cow<'i, str>,
        /// The attribute match operator.
        operator: Option<AttributeOperator>,
        /// The match value.
        value: Option<Cow<'i, str>>,
        /// Case-sensitivity modifier.
        case_sensitivity: AttributeCaseSensitivity,
        /// Never-matches-anything flag.
        never_matches: bool,
    },
    /// `:is(...)` functional pseudo-class.
    Is(SelectorList<'i>),
    /// `:where(...)` functional pseudo-class.
    Where(SelectorList<'i>),
    /// `:not(...)` functional pseudo-class.
    Negation(SelectorList<'i>),
    /// `:has(...)` functional pseudo-class.
    Has(SelectorList<'i>),
    /// `:nth-child(...)` / `:nth-last-child(...)` / `:nth-of-type(...)`
    /// / `:nth-last-of-type(...)`.
    Nth(NthSelector<'i>),
    /// `:lang(...)`.
    Lang(Vec<Cow<'i, str>>),
    /// `:dir(ltr|rtl)`.
    Dir(DirKeyword),
    /// `:host(...)` — shadow-DOM host compound.
    Host(Option<Box<Selector<'i>>>),
    /// `::slotted(...)` — shadow-DOM slotted compound.
    Slotted(Box<Selector<'i>>),
    /// `::part(...)` — shadow-DOM part list.
    Part(Vec<Cow<'i, str>>),
    /// `::highlight(...)` highlight pseudo.
    Highlight(Cow<'i, str>),
    /// Simple non-functional pseudo-class e.g. `:hover`, `:focus`,
    /// vendor-specific `:-moz-placeholder`.
    NonTSPseudoClass(Cow<'i, str>),
    /// Simple non-functional pseudo-element e.g. `::before`, `::after`.
    PseudoElement(Cow<'i, str>),
    /// Nesting parent `&` (CSS Nesting).
    Nesting,
    /// `:scope` pseudo-class.
    Scope,
    /// `:root` pseudo-class.
    Root,
    /// `:empty` pseudo-class (maps to `NonTSPseudoClass("empty")`;
    /// kept explicit for isomorphism against `parcel_selectors` which
    /// has it as a dedicated variant).
    Empty,
    /// `:any(...)` vendor prefixed; populated via `From<lightningcss>`.
    Any {
        /// The vendor prefix.
        prefix: VendorPrefix,
        /// The selector list.
        selectors: SelectorList<'i>,
    },
    /// Combinator between compounds.
    Combinator(Combinator),
}

/// Attribute match operator.
///
/// Discriminant order matches `css_l4/selectors.bbnf` attrMatcher rule:
/// Equal=0, Dash=1, Includes=2, Prefix=3, Suffix=4, Substring=5.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AttributeOperator {
    /// `=` exact match.
    Equal = 0,
    /// `|=` dash-separated prefix match.
    Dash = 1,
    /// `~=` whitespace-separated word match.
    Includes = 2,
    /// `^=` prefix match.
    Prefix = 3,
    /// `$=` suffix match.
    Suffix = 4,
    /// `*=` substring match.
    Substring = 5,
}

/// Attribute match case sensitivity modifier.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub enum AttributeCaseSensitivity {
    /// Default — per HTML-attribute-aware case-sensitivity rules.
    #[default]
    Default = 0,
    /// Explicit case-sensitive `s`.
    ExplicitCaseSensitive = 1,
    /// Explicit case-insensitive `i`.
    AsciiCaseInsensitive = 2,
    /// Implicit case-insensitive when `case_sensitivity_attribute_in_html`
    /// is applied (lightningcss; populated via From).
    CaseSensitiveIfInHtmlElementInHtmlDocument = 3,
}

/// `:nth-*()` selector payload.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NthSelector<'i> {
    /// Which nth flavour.
    pub kind: NthKind,
    /// The `a` coefficient of `an + b`.
    pub a: i32,
    /// The `b` offset of `an + b`.
    pub b: i32,
    /// `:nth-child(... of <selector-list>)` clause.
    pub of: Option<SelectorList<'i>>,
}

/// Flavour of the `:nth-*` selector.
///
/// Discriminant order matches `css_l4/selectors.bbnf` nthFunctionName:
/// NthChild=0, NthOfType=1, NthLastChild=2, NthLastOfType=3.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum NthKind {
    /// `:nth-child()`.
    NthChild = 0,
    /// `:nth-of-type()`.
    NthOfType = 1,
    /// `:nth-last-child()`.
    NthLastChild = 2,
    /// `:nth-last-of-type()`.
    NthLastOfType = 3,
}

/// `:dir()` argument.
///
/// Discriminant order matches `css_l4/selectors.bbnf` dirKeyword:
/// Ltr=0, Rtl=1.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DirKeyword {
    /// `ltr`.
    Ltr = 0,
    /// `rtl`.
    Rtl = 1,
}

/// Vendor prefix marker for prefixed pseudo-classes.
///
/// Bit flags so combinations (`moz` + `webkit`) can be represented in
/// a single value. Discriminants mirror `lightningcss::vendor_prefix`.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub enum VendorPrefix {
    /// No prefix.
    #[default]
    None = 0,
    /// `-webkit-`.
    WebKit = 1,
    /// `-moz-`.
    Moz = 2,
    /// `-ms-`.
    Ms = 4,
    /// `-o-`.
    O = 8,
}
