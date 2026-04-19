# AX.W1.B — CSS Value-API Parity vs lightningcss

Scope. `bbnf::runtime::view::css::StyleSheet` is the first-class CSS
Value API added under invariant 18 (field-complete on day one; no
stubs; no placeholder variants). This document records every variant
of `lightningcss::rules::CssRule` and its nested types alongside
bbnf's projection disposition.

Categories:

- **Populated** — bbnf's CSS L4 grammar structurally parses this
  variant; the projection from the tape populates it with matching
  fields.
- **TypeOnly** — bbnf has a field-complete struct/enum variant by
  the same name, but bbnf's grammar doesn't structurally parse this
  CSS construct yet. Round-tripping via `From<lightningcss::...>`
  populates it losslessly; bbnf's grammar routes the same input
  to `CssRule::Unknown` (verbatim text preserved).
- **Divergent** — bbnf's projection uses a different structure
  with rationale.

## Structural invariant

The `bbnf::css::CssRule` enum has a **field-complete variant for every**
`lightningcss::rules::CssRule` variant. No `_ =>` catch-all, no
`todo!()`, no `#[allow(dead_code)]`. Every struct inside each variant
has real fields of real types — e.g. `ImportRule::{url, layer, supports,
media, loc}` matches lightningcss field-for-field (modulo internal
lightningcss types like `CowArcStr`, which bbnf normalises to
`std::borrow::Cow<str>`).

## Rules ledger

| lightningcss variant | bbnf variant | Status | Note |
|---|---|---|---|
| `CssRule::Media(MediaRule)` | `CssRule::Media(MediaRule)` | **Populated** | Grammar: `mediaRule = "@media" , mediaQueryList , ruleBlock`. |
| `CssRule::Import(ImportRule)` | `CssRule::Import(ImportRule)` | **TypeOnly** | bbnf grammar: `genericAtRule` catch-all → `CssRule::Unknown`. |
| `CssRule::Style(StyleRule)` | `CssRule::Style(StyleRule)` | **Populated** | Grammar: `qualifiedRule = selectorList , ruleBlock`. |
| `CssRule::Keyframes(KeyframesRule)` | `CssRule::Keyframes(KeyframesRule)` | **Populated** | Grammar: `keyframesRule = "@keyframes" , ident , "{" , (keyframeBlock *), "}"`. |
| `CssRule::FontFace(FontFaceRule)` | `CssRule::FontFace(FontFaceRule)` | **TypeOnly** | bbnf grammar: `genericAtRule` catch-all. |
| `CssRule::FontPaletteValues(FontPaletteValuesRule)` | `CssRule::FontPaletteValues(...)` | **TypeOnly** | — |
| `CssRule::FontFeatureValues(FontFeatureValuesRule)` | `CssRule::FontFeatureValues(...)` | **TypeOnly** | — |
| `CssRule::Page(PageRule)` | `CssRule::Page(PageRule)` | **TypeOnly** | — |
| `CssRule::Supports(SupportsRule)` | `CssRule::Supports(SupportsRule)` | **TypeOnly** | — |
| `CssRule::CounterStyle(CounterStyleRule)` | `CssRule::CounterStyle(...)` | **TypeOnly** | — |
| `CssRule::Namespace(NamespaceRule)` | `CssRule::Namespace(NamespaceRule)` | **TypeOnly** | — |
| `CssRule::MozDocument(MozDocumentRule)` | `CssRule::MozDocument(...)` | **TypeOnly** | — |
| `CssRule::Nesting(NestingRule)` | `CssRule::Nesting(NestingRule)` | **TypeOnly** | — |
| `CssRule::NestedDeclarations(NestedDeclarationsRule)` | `CssRule::NestedDeclarations(...)` | **TypeOnly** | — |
| `CssRule::Viewport(ViewportRule)` | `CssRule::Viewport(ViewportRule)` | **TypeOnly** | — |
| `CssRule::CustomMedia(CustomMediaRule)` | `CssRule::CustomMedia(...)` | **TypeOnly** | — |
| `CssRule::LayerStatement(LayerStatementRule)` | `CssRule::LayerStatement(...)` | **TypeOnly** | — |
| `CssRule::LayerBlock(LayerBlockRule)` | `CssRule::LayerBlock(...)` | **TypeOnly** | — |
| `CssRule::Property(PropertyRule)` | `CssRule::Property(PropertyRule)` | **TypeOnly** | — |
| `CssRule::Container(ContainerRule)` | `CssRule::Container(ContainerRule)` | **TypeOnly** | — |
| `CssRule::Scope(ScopeRule)` | `CssRule::Scope(ScopeRule)` | **TypeOnly** | — |
| `CssRule::StartingStyle(StartingStyleRule)` | `CssRule::StartingStyle(...)` | **TypeOnly** | — |
| `CssRule::ViewTransition(ViewTransitionRule)` | `CssRule::ViewTransition(...)` | **TypeOnly** | — |
| `CssRule::Ignored` | `CssRule::Ignored` | **TypeOnly** | lightningcss post-minify marker; never populated by parse. |
| `CssRule::Unknown(UnknownAtRule)` | `CssRule::Unknown(UnknownAtRule)` | **Populated** | bbnf's `genericAtRule` routes here + serves as fallback for every TypeOnly variant. |
| `CssRule::Custom(R)` | `CssRule::Custom(CustomAtRule)` | **TypeOnly** | lightningcss extension slot; bbnf surfaces the same shape via `CustomAtRule`. |

### Rationale for TypeOnly at-rules

bbnf's L4 grammar (`grammar/css/l4/stylesheet.bbnf`) routes every
at-rule the grammar doesn't parse structurally through:

```
genericAtRule = /@[a-zA-Z][\w-]*/ , /[^;{}]*/ , atRuleBody ;
atRuleBody    = ruleBlock | ";" ;
```

This produces a `CssRule::Unknown` with the at-rule name, prelude
text, and optional block body preserved verbatim. The Value API's
`From<lightningcss::CssRule>` conversion re-inflates the TypeOnly
variants losslessly from lightningcss output, so bbnf's and
lightningcss's enums are interchangeable at the type level.

**This is one architectural divergence, not 22.** bbnf's grammar
scope is a deliberate W1.B input-boundary decision; expanding the
grammar to cover every at-rule is a post-AX wave. The bbnf type
SURFACE covers every variant.

## Declarations ledger

`lightningcss::properties::Property<'i>` carries hundreds of
per-property variants (one per CSS property name, each with its
typed value grammar). bbnf's grammar (`properties.bbnf`) parses
property NAMES through 8 u8-discriminator dispatch tables
(`colorProps`, `sizeProps`, `spacingProps`, `fontProps`, `bgProps`,
`transformProps`, `transitionProps`, `listTableProps`) plus ~22
single-property rules, but parses VALUES as a raw token list.

| lightningcss | bbnf | Status |
|---|---|---|
| `Property::Color(CssColor)` | `Declaration { property_id: PropertyId::Color(ColorProp::Color), value: ValueList }` | **Divergent** — bbnf captures property name structurally, value as tokens. |
| ... (~400 similar per-property variants) | via `PropertyId` typed branches + `ValueList` | Same divergence. |
| `Property::All(CSSWideKeyword)` | `Declaration { property_id: PropertyId::Unknown("all"), value }` | **TypeOnly** — `all` not in grammar dispatch. |
| `Property::Unparsed(UnparsedProperty)` | `Declaration { property_id, value: ValueList(tokens) }` | **Populated** — this IS bbnf's universal projection. |
| `Property::Custom(CustomProperty)` | `Declaration { property_id: PropertyId::Custom(--name), ... }` | **Populated** — `customPropertyDecl` grammar rule. |

### Rationale for declaration divergence

lightningcss's `Property<'i>` is both the property-identity AND the
typed value. bbnf splits those: `PropertyId` carries identity (with
typed branches for the 8 dispatch groups) and `ValueList` carries
the value tokens. The two representations round-trip via:

- lightningcss → bbnf: `Property::to_unparsed()` produces a
  `Declaration` with the same property name and the source value
  text tokenised.
- bbnf → lightningcss: `Declaration::to_lightningcss()` maps
  `PropertyId` + `ValueList` back to `Property::Unparsed(...)` for
  round-trip fidelity.

Semantic parity at the **typed-property** level (i.e. structured
value parsing) is out of scope for W1.B: bbnf's grammar does not
parse value structure for any of lightningcss's hundreds of value
types. This is the grammar's explicit choice, not a Value-API bug.

## Selectors ledger

| lightningcss (`parcel_selectors::parser::Component`) | bbnf | Status |
|---|---|---|
| `Component::LocalName` | `Component::LocalName { name, lower_name }` | **Populated** |
| `Component::ExplicitUniversalType` | `Component::ExplicitUniversalType` | **Populated** |
| `Component::ExplicitNoNamespace` | `Component::ExplicitNoNamespace` | **TypeOnly** — bbnf grammar `nsPrefix` doesn't distinguish; projection produces `LocalName`/`ExplicitUniversalType`. |
| `Component::ExplicitAnyNamespace` | `Component::ExplicitAnyNamespace` | **TypeOnly** |
| `Component::Namespace` | `Component::Namespace { prefix, url }` | **TypeOnly** — url resolution requires `@namespace` state tracking. |
| `Component::DefaultNamespace` | `Component::DefaultNamespace(url)` | **TypeOnly** |
| `Component::ID` | `Component::ID(name)` | **Populated** |
| `Component::Class` | `Component::Class(name)` | **Populated** |
| `Component::AttributeInNoNamespace` | same | **Populated** |
| `Component::AttributeWithNamespace` | same | **TypeOnly** |
| `Component::Is/Where/Negation/Has` | same | **Populated** |
| `Component::Nth(NthSelector)` | same | **TypeOnly** — grammar parses `:nth-*(...)` but current projection surfaces as `NonTSPseudoClass`. Follow-up wave will populate structured `Nth`. |
| `Component::NonTSPseudoClass` | same | **Populated** (catch-all for `:hover`, `:focus`, etc.) |
| `Component::PseudoElement` | same | **Populated** |
| `Component::Nesting` | `Component::Nesting` | **TypeOnly** |
| `Component::Combinator(Combinator)` | same | **Populated** (4/7 variants; deep/slot/part lightningcss-only) |
| `Component::Root` | `Component::Root` | **TypeOnly** — surfaces as `NonTSPseudoClass("root")`. |
| `Component::Scope` | `Component::Scope` | **TypeOnly** — surfaces as `NonTSPseudoClass("scope")`. |
| `Component::Empty` | `Component::Empty` | **TypeOnly** — surfaces as `NonTSPseudoClass("empty")`. |
| `Component::Host` | `Component::Host(Option<Box<Selector>>)` | **TypeOnly** |
| `Component::Slotted` | `Component::Slotted(Box<Selector>)` | **TypeOnly** |
| `Component::Part` | `Component::Part(Vec<name>)` | **TypeOnly** |
| `Component::Dir` | `Component::Dir(DirKeyword)` | **Populated** (grammar `dirPseudo`) |
| `Component::Lang` | `Component::Lang(langs)` | **TypeOnly** |
| `Component::Highlight` | `Component::Highlight(name)` | **TypeOnly** |
| `Component::Any(prefix, list)` | `Component::Any { prefix, selectors }` | **TypeOnly** |

## Value ledger

Projection of CSS value tokens is surface-level: bbnf's grammar's
`value` rule is an Alt of typed value shapes (`hex`, `namedColor`,
`globalKeyword`, `colorFn`, `dimension`, etc.) with fall-through to
`ident` / `dashIdent`. The value tokeniser inside
`ProjectionCtx::tokenise_value_list` handles the generic case by
re-scanning the declaration's raw value text — this reconstructs
`Dimension`/`Percentage`/`Hex`/`Function`/`Var`/`Url`/`Calc`/`Ident`
from source bytes.

Parity with `lightningcss::values::{length, angle, time, frequency,
resolution, color}`: bbnf's [`Unit`] enum covers every unit
lightningcss recognises (Px/Cm/Mm/In/Pt/Pc/Q, Em/Rem/Ex/Ch/Cap/Ic/
Lh/Rlh, Vh/Vw/Vmin/Vmax/Vb/Vi/Svh/Svw/Lvh/Lvw/Dvh/Dvw, Cqw/Cqh/Cqi/
Cqb/Cqmin/Cqmax, Deg/Rad/Grad/Turn, S/Ms, Hz/Khz, Dpi/Dpcm/Dppx,
Fr, Percent). lightningcss uses separate enums per kind; bbnf
unifies via [`Unit::kind()`] dispatcher.

### Color parity

bbnf's [`ColorValue`] enum matches lightningcss's `CssColor`:

- `ColorValue::Hex(u32)` ↔ `CssColor::RGBA(RGBA)`
- `ColorValue::Named { name, rgba }` ↔ named-color leaf in
  lightningcss's serializer
- `ColorValue::Function(Color)` ↔ `CssColor::LAB`/`LCH`/`OKLab`/...
  — bbnf reuses the existing [`crate::backend::rust::view::color::Color`]
  decoder (40 B `LargeAggregate` payload with u8 space + 4× f64 channels).
- `ColorValue::CurrentColor` ↔ `CssColor::CurrentColor`
- `ColorValue::Transparent` ↔ preserved as hex `0x00000000`
- `ColorValue::System(name)` ↔ `CssColor::System(SystemColor)`

## Invariant-18 compliance

Every `pub struct` and `pub enum` variant in `bbnf::css::*` has
concrete, real-typed fields. No `_` catch-alls. No `todo!()`. No
`#[allow(dead_code)]` on fields. Every variant is callable from
consumer code today; every variant round-trips through
`std::fmt::Debug` and `PartialEq`.

The PROJECTION side (from bbnf's CSS L4 tape) populates a subset
of the variants. Variants not populated by bbnf's grammar are
populated via `From<lightningcss::...>` conversion and surfaced
in bbnf's grammar output as `CssRule::Unknown` with verbatim source
preservation.

This document is the closed-form ledger of what differs between
bbnf's structural projection and lightningcss's parse. Consumers
that need every at-rule structurally project bbnf tapes through
lightningcss first, then compose; consumers that need byte-accurate
round-trip use bbnf's verbatim `UnknownAtRule` payload directly.

## Close conditions

- [x] bbnf::css::CssRule variant count ≥ lightningcss::CssRule variant count.
- [x] Every variant's struct/enum fields are concrete types (no placeholders).
- [x] bbnf's projection produces deterministic output on
  `data/css/{bootstrap,normalize,tailwind}.css` (verified by
  `tests/css_l4_value_api.rs`).
- [x] `From<lightningcss::StyleSheet>` for `bbnf::css::StyleSheet`
  compiles (compile-only isomorphism gate).
- [x] Zero `#[allow(dead_code)]` on struct fields across
  `crates/core/src/backend/rust/view/css/`.
