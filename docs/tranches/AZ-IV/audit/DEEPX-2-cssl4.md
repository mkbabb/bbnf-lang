# DEEPX-2 — Full CSS L4 Parity + Profile Audit

Read-only post-AZ-IV audit. Worktree
`/Users/mkbabb/Programming/bbnf-wt-deepX-2`, branch `deepX-cssl4` at master
`40e1835d`. `CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-deepX-2/target/deepX-2`.

The driving questions are three but one body. CSS L4 bootstrap is **606.4 ms
@ 0.46 MB/s** — the slowest production parse in the fleet (a **983×**
regression from AU's 616 µs / 454 MB/s flat-tape baseline). normalize is
**402.2 µs @ 15 MB/s** (48× regression). tailwind is **WATCHDOG_HALT** —
77.567 s observed against a 5 s wall (15.5× the cap; **10,583×** regression
from AU's 7.33 ms / 496 MB/s). All three resolve to two compounding
mechanisms named in DEEP-A and DEEP-B and reproduced statically by inspecting
`crates/core/src/grammar/generated/css_l4.rs` (107,138 LOC; 30× JSON's
generated size).

The audit does NOT rerun samply against the bootstrap fixture. The W6.1
close evidence in `docs/benchmarks/post-AZ-IV.json` is canonical;
DEEP-B's full-trace samply on the JSON sibling
(`Vec<OpenFrame>::clone` at 86.07 % inclusive) names the same architectural
mechanism that drives CSS L4's regression curve. Verification is by
generated-code structural analysis + registry inspection +
fixture density.

## I — Profile Per CSS L4 Entry

### I.1 — bootstrap (606.4 ms / 0.46 MB/s; 49 samples)

bootstrap is 280,311 bytes of well-formed Bootstrap-5 CSS: 2,671 rule
blocks, 5,545 declarations, 1,061 selector commas, 3,616 class selectors,
603 pseudo-class hits, 109 `@media` blocks, 5 `@keyframes`, **zero
backslash escapes**. The fixture is *clean* — the regression is not
data-pathological; it is structural to the parser path.

The parse path:

1. `CssL4Parser::parse(input)` enters at the eager dispatcher
   (`generated/css_l4.rs` ~line 200; mirror of `JsonParser::parse`).
2. Each of the 2,671 rule blocks routes through
   `parse_flat_CssL4Parser_qualifiedRule` (line 61122) which calls
   `builder.checkpoint()` once *and* opens a `StyleRule` `OpenFrame`
   carrying two `Vec`s (`selectors`, `declarations`).
3. The 5,545 declarations route through `parse_wrap_CssL4Parser_declaration`
   (line 59084) which is the alt-dispatch tower over the **27 typed
   `*Decl` rules + `customPropertyDecl` + `genericDecl`**. Each branch
   tries with a `builder.checkpoint()`; failed branches roll back via
   `builder.rollback(__chk)`.
4. The 1,061 selector commas drive `parse_unordered_CssL4Parser_compoundSelector`
   (line 48261) which is the unordered-shape `+`-quantified compound over
   five Alts (`classSelector | idSelector | attrSelector | colonSelector |
   typeSelector`). Each compound-selector iteration takes a checkpoint.
5. Every regex match — there are 28 distinct regex DFAs in the
   generated code — flows through `__regex_scan_CssL4Parser` (line 12823),
   a hand-rolled `if ::core::ptr::eq(pattern.as_ptr(), __DTA_REGEX_<N>) ||
   pattern == __DTA_REGEX_<N>` chain dispatching to per-pattern hand-coded
   DFAs. The dispatcher has 28 arms, one per pattern; the matching arm
   then runs a 64 KB-aware first-byte lookahead (from
   `__REGEX_LAST_BYTE_SET_CssL4Parser`) before driving the DFA.

**Allocation accounting (static, per-bootstrap-parse):**

- **1,018 `String::from` allocations per call into the layout literal**
  (every `StructLayout::rule_name: String::from("...")` site in
  `generated/css_l4.rs`; static count via `grep -c "String::from"`).
  Compared to JSON's 4 sites per parse-call invocation × ~15K compounds,
  CSS L4's *static codegen footprint* is ~250× larger but the *runtime
  call density* tracks compound count (≈2,671 rule begins + 5,545 decl
  begins + ~10K nested = ≈18K layout literals on bootstrap, each
  allocating a fresh `String`).
- **200 `rule_type: ::bbnf_ir::TypeDesc::Span` literals** — every layout
  emit site in CSS L4 hard-codes `Span` (vs JSON's 4, Sheets' 38). The
  registry-projected `rule_type` from `project_types` is **populated for
  187 layouts** — kind=Struct(123), UntaggedEnum(46), TaggedEnum(9),
  NewtypeWrapper(9) — but every emit site overwrites the projected
  type with `Span`. The registry is a vestigial substrate.
- **1,407 `builder.checkpoint()` call sites** in `generated/css_l4.rs`
  (vs JSON's smaller count). Each clones `self.stack: Vec<OpenFrame<'p>>`
  + `self.root` + `pending_value` per `runtime/css_l4/builder.rs:352-365`.
  Each `OpenFrame` arm holds 1–3 `Vec<…>`s (StyleSheet:rules,
  StyleRule:{selectors,declarations}, MediaRule:{rules},
  KeyframesRule:{blocks}, KeyframeBlock:{declarations},
  Declaration:{values}, SelectorList:{selectors},
  ColorFunction:{components}, Function:{args}). Every checkpoint
  recursively clones every Vec on the stack.
- **415 `begin_compound`/`end_compound` calls** (each pair allocating a
  fresh frame Vec via `Vec::new()` then cloning the inner Vec on
  `arena.push_*` slab grow). Bootstrap parse runs ~18K such pairs.

The 606 ms dominance attributes precisely:

- Per-decl alt-dispatch through 27 typed `*Decl` branches + `customPropertyDecl`
  + `genericDecl` is the inner loop. Each failed branch costs one
  checkpoint clone (Vec<OpenFrame> + 9 OpenFrame Vec children) + one
  `String::from(rule_name)` + one `Vec::new()`.
- Bootstrap's 5,545 declarations × ~7 average failed branches per
  successful match (the `colorDecl | sizeDecl | spacingDecl …` chain is
  deeply ordered; a `width:` decl tries `colorDecl` and fails before
  `sizeDecl` succeeds) ≈ **38,815 checkpoint clones per parse** —
  the runtime equivalent of cloning a 9-arm Vec stack 38K times.
- Each clone cost: ~9 OpenFrame variants × ~64 bytes/frame + 1–3
  Vec-children clones (each 24 bytes header + N elements). On bootstrap
  the OpenFrame depth at decl-time is typically 3 (StyleSheet → StyleRule →
  Declaration). The clone cost per checkpoint is ≈ 200 bytes including
  Vec-element copies. **38K × 200 B = 7.6 MB cloned per parse**, dominated
  by mi-malloc small-object churn.

**Architectural reason for 606 ms — single attribution:** the
`StructBuilder::checkpoint` discipline applied to the deeply-nested CSS L4
declaration alt-dispatch. The 9-frame `OpenFrame` enum (line 55-152 of
`runtime/css_l4/builder.rs`) is more variant-rich than JSON's 4-frame
equivalent; the alt-dispatch tower for `declaration` alone has 28 arms
(properties.bbnf:215-223). Every alt branch opens with a checkpoint; the
clone discipline is multiplicatively expensive in CSS L4.

### I.2 — normalize (402.2 µs / 15 MB/s)

normalize is 6,138 bytes / 34 rule blocks / 57 declarations / **zero
pseudo-classes / zero @media / zero @keyframes**. The fixture is
declaration-light; the alt-dispatch tower fires fewer times. The
absolute parse time is 402 µs but the per-rule cost is *similar*:
normalize:402µs/34rules = **11.8 µs/rule**; bootstrap:606ms/2671 rules =
**226 µs/rule**. The 19× per-rule slowdown on bootstrap reflects bootstrap
having 2× declaration density (avg 2.07 decls/rule vs normalize's 1.68)
plus deeply-nested complex selectors that require the
`compoundSelector`'s unordered-shape + selector alt-dispatch tree to
fire. The 48× absolute-time regression vs AU is the same root cause as
bootstrap's 983×, scaled by the structural element count.

### I.3 — tailwind (WATCHDOG_HALT — 77.567 s observed; 5 s cap)

tailwind is **3,642,321 bytes**, 39,150 rule blocks, 58,266
declarations, 51,383 selector commas, **65,444 pseudo-classes**, **58,344
backslash escapes** (the Tailwind `class\:hover` escape syntax), 35
`@media`, 4 `@keyframes`. Density vs bootstrap:
- 13× rule count, 14× byte size — *but 109× pseudo-class density* and
  ∞× escape density.
- Mean compound-selector depth is much higher (Tailwind classes like
  `lg\:dark\:hover\:bg-red-500` carry 4–6 escaped pseudo-class /
  modifier segments).

The static AZ-IV.json `note` field names two hotspots:

> Hotspot named: __declaration (38% self-time at AU baseline) +
> __compoundSelector (31% at AU baseline) — AU.2.7 structural bitmap is
> the substrate, AV-scale PHF + SIMD selector classifier is the lever.

But the regression *cause* is the speculative-clone discipline applied
to the worst-case fixture. compoundSelector is `+`-quantified over five
Alts (classSelector, idSelector, attrSelector, colonSelector,
typeSelector); each Tailwind class like `lg\:dark\:hover\:bg-red-500`
opens its compound selector as **a single compound** but the inner Alt
fires once per `\:`-segment, each triggering one `selectorIdent` regex
DFA (the regex `(?:-?[a-zA-Z_\x80-\xff]|\\[^\n])(?:[\w\x80-\xff-]|\\[^\n])*`
runs over the escape-rich text) **and** one checkpoint per Alt-branch
attempt. Tailwind has ~58K backslash escapes, each forcing the
selectorIdent regex to consume two bytes per escape (the `\` + the
escaped char), and each compound's Alt iterating five branches.

**Why is tailwind WATCHDOG?** Per AZ-IV's named cause: regex_scan
classifier + structural-bitmap absent. But the substrate-attribution
mechanism: **compoundSelector's checkpoint-per-iteration discipline,
amplified by Tailwind's selector-density, multiplied across the
declaration alt-dispatch tower.** Per the back-of-envelope: 39K rules ×
mean 3 compoundSelector iterations × 5 Alt branches × 2 checkpoints (one
inside compoundSelector, one inside each `*Decl`) ≈ **1.17 M checkpoint
clones**. Each clone walks ~3-frame OpenFrame stack with ~5-element Vec
children. That's an inner-loop allocation count that explains the
77.567 s wall.

The hot path is named in `runtime/css_l4/builder.rs:352-365` — the
`checkpoint` impl literally clones `self.stack` (Vec) + `pending_value`
(Option) + `root` (Option<StyleSheet>). It does *not* (and cannot) clone
the arena — `self.arena.truncate(…)` on rollback restores by counts
(rules/decls/selectors/values/keyframes/colors). But every Vec child of
every OpenFrame on the stack is recursively cloned. Tailwind's depth
imposes a stack of 4-6 frames at the inner compoundSelector iteration
time, each with 5-30 element children — the clone cost is quadratic in
nesting × selector density.

## II — Semantic Parity vs lightningcss

### II.1 — Type-by-type comparison

| lightningcss API | bbnf CSS L4 equivalent | Parity |
|---|---|---|
| `pub struct StyleSheet<'i, 'o, T> { rules: CssRuleList, sources: Vec<String>, source_map_urls, license_comments, content_hashes, options }` (`stylesheet.rs:73`) | `pub struct StyleSheet { rules: CssRuleListId }` (`runtime/css_l4/value.rs:802-805`) | **Partial** — bbnf lacks sources / source-map-urls / license-comments / content-hashes / options. Source-map fidelity per `feedback_preserve-rich-ast` requires these. |
| `pub enum CssRule<'i, R>` — 24 variants: Media, Import, Style, Keyframes, FontFace, FontPaletteValues, FontFeatureValues, Page, Supports, CounterStyle, Namespace, MozDocument, Nesting, NestedDeclarations, Viewport, CustomMedia, LayerStatement, LayerBlock, Property, Container, Scope, StartingStyle, ViewTransition, Ignored, Unknown, Custom (`rules/mod.rs:139-193`) | `pub enum CssRule<'p>` — **4 variants**: Style, Media, Keyframes, GenericAt (`runtime/css_l4/value.rs:788-797`) | **20 of 24 variants missing**. `@import`, `@font-face`, `@supports`, `@page`, `@counter-style`, `@layer`, `@property`, `@container`, `@scope`, `@starting-style`, `@view-transition`, `@namespace`, `@-moz-document`, etc. all collapse to the catch-all `GenericAt` carrying raw spans. |
| `pub struct DeclarationBlock<'i> { important_declarations: Vec<Property>, declarations: Vec<Property> }` (`declaration.rs:61`) | `Declaration<'p> { property: &'p str, value: CssTypedValue<'p>, important: bool }` (`value.rs:721-728`) | **No DeclarationBlock**. bbnf flattens the !important / non-!important split into a per-Declaration bool flag; lightningcss separates the two for shorthand-merge optimisation. |
| `pub enum Property<'i>` (~150 variants, e.g. `Color(CssColor)`, `Background(SmallVec<[Background; 1]>)`, `Width(Size)`, etc.) (`properties/mod.rs:684`) | None — bbnf has `CssTypedValue<'p>` (12 variants) but **no per-property typed payload**. Property names live as `&'p str` in the `property` field. | **Massive gap**. lightningcss exposes typed `Property::Color(_)`, `Property::Width(_)`, `Property::Margin(_)` etc. — bbnf gives `Declaration { property: "color", value: CssTypedValue::Color(..) }` where the type-of-value is not pre-bound to the property name. |
| `pub enum Length { Value(LengthValue), Calc(Box<Calc<Length>>) }` (`values/length.rs:541`) | `pub struct CssLength { value: f64, unit: CssLengthUnit }` (`value.rs:158-164`); `CssDimension::Length(CssLength)` is the public face (`value.rs:328-345`) | **Length::Calc gap**. bbnf's `CssLength` is always `(f64, unit)`; calc-typed lengths must round-trip through `CssTypedValue::Function(CssFunction::Calc{args})` losing the typed-Length-from-calc compose. |
| `pub enum CssColor { CurrentColor, RGBA(RGBA), LAB(Box<LABColor>), Predefined(Box<PredefinedColor>), Float(Box<FloatColor>), LightDark(Box<CssColor>, Box<CssColor>), System(SystemColor) }` (`values/color.rs:46-69`) | `pub enum CssColor<'p> { Hex(u32), Named { name: &'p str, packed: u32 }, Function(CssColorFunction), Predefined(CssColorPredefined), Mix(CssColorMix<'p>) }` (`value.rs:507-521`) | **Partial**. bbnf has Hex / Named / Function / Predefined / Mix — **lacks** CurrentColor (the `currentcolor` keyword is treated as a NamedColor with packed `0x000000FF` per `color.bbnf:56` not as a distinct variant), LAB-family-as-Box, Float, **LightDark, System**. The grammar projects rgb/rgba/hsl/lab/oklab/oklch all into `CssColor::Function` — *the colorspace-specific shape* (lightningcss has 4 distinct Box variants for typed math) is collapsed. |
| `selector::{Component, Combinator, …}` — full SelectorList AST with 30+ Component variants (Class, ID, Type, AttributeInNoNamespace, Empty, AnyLink, Link, Host, …) | `pub enum Selector<'p> { Universal, Type, Class, Id, Attribute, PseudoClass, PseudoElement, Combinator, Span }` — **9 variants, all wrapping `&'p str`** (`value.rs:690-714`) | **Major gap**. bbnf's Selector::Attribute carries a raw span (not parsed); PseudoClass carries a raw `:hover` span (not the typed `Component::Hover` lightningcss exposes). The 9-arm enum is structural — the typed AST is collapsed to spans. |
| `pub struct MediaQueryList<'i>` with typed `MediaCondition`/`MediaFeature` AST | `pub struct MediaRule<'p> { query: &'p str, rules: CssRuleListId }` — **query as raw &str** (`value.rs:743-750`) | **Gone**. The mediaQuery / mediaCondition / mediaFeature grammar rules exist (16 layouts in registry) but the runtime collapses them to a span; the typed AST never reaches MediaRule. |

### II.2 — The named_color carry — was it actually closed?

Per `runtime/css_l4/builder.rs:889-903` (`push_leaf_with_u64`):

```rust
fn push_leaf_with_u64(&mut self, value: u64) {
    if value <= u32::MAX as u64 {
        self.deposit_value(CssTypedValue::Color(CssColor::Hex(value as u32)));
    } else { self.deposit_value(CssTypedValue::Number(value as f64)); }
}
```

The named-color path **lands on `CssColor::Hex`, NOT `CssColor::Named`**.
The `CssColor::Named { name: &'p str, packed: u32 }` variant exists (line
514) but no codegen call site populates it — the
`namedColor = "aliceblue" -> 0xF0F8FFFFu32 | …` grammar (`color.bbnf:36-160`)
projects the packed u32 through the branch-tag mechanism but the builder
discards the name. The Named arm is unreachable; production tests
`crates/core/tests/css_l4_named_color_parity.rs` need to verify whether
the parity check actually compares `Named.name` or only `.packed`. The
carry is **partially closed** — the packed u32 reaches the typed graph
but the round-trip fidelity (`name` span) is lost.

### II.3 — Selector parsing

bbnf parses selectors but **collapses every variant to a span**. The
`runtime/css_l4/builder.rs:920-945` (`push_leaf_with_str`) catch-all on
`OpenFrame::SelectorList` is `selectors.push(Selector::Span(value))` —
i.e., every selector that the grammar dispatches to (typeSelector,
classSelector, idSelector, attrSelector, pseudoClass, pseudoElement,
combinator) lands as `Selector::Span(&'p str)` rather than the typed
`Selector::Type / Class / Id / Attribute / PseudoClass / PseudoElement /
Combinator` arms.

The `:dir(ltr)/:dir(rtl)` discriminant is wired (line 810-830 — the
DirPseudo frame produces `Selector::PseudoClass(":dir(ltr)")`) but
that's the *only* typed pseudo-class produced. Every other
`:hover`/`::before`/`.btn`/`#main`/`a`/`*` reaches the SelectorList as a
generic `Selector::Span(s)` and the consumer must re-parse the string.

lightningcss's `Selector` is the cssparser-canonical typed AST (Component
enum with full variant coverage). bbnf's Selector is a 9-arm typed sum
where 7 arms are actually populated as `Span(&'p str)` because the
codegen lacks per-shape Selector::* deposit calls.

### II.4 — Variant-select on CSS values

The DEEP-SYNTHESIS §V example:

```rust
let color: Option<&CssColor> = doc.get(path!(CssL4,
    "rules", 0, "declarations", 0, "value", "color"));
```

**Does NOT compile today.** Verification:

1. `path!(CssL4, "rules", 0, "declarations", 0, "value", "color")` —
   the path-macro proc lookup goes against
   `crates/core/src/grammar/generated/css_l4.registry.json`. The
   registry's `stylesheet` root (rule_id=149) has one field `ruleList`,
   not `rules`. Per `crates/core/tests/path_macro_compile.rs:57`, the
   only valid CssL4 path landed today is `path!(CssL4, "ruleList")`.
2. `CssPathQuery` has only **two `impl` blocks** (`document.rs:496`
   for `&str`, `document.rs:519` for `f64`). There is NO impl for
   `&CssColor`, `&CssLength`, `&CssDimension`, `&Declaration`,
   `&StyleRule`, `Iter<&Declaration>`. The trait surface is
   trait-callable in the abstract but no concrete leaf-type impl
   exists for CSS-value queries.
3. Even if both 1 and 2 worked, the path-walker (`document.rs:455-494`)
   bails on the second `Field` step beyond `value` — the cursor walks
   `Sheet → Rule → Decl → Value` but `Value(CssTypedValue) → "color"`
   is not implemented; the walker returns `None`.

Conclusion: **the DEEP-SYNTHESIS path-query example is aspirational, not
working code**.

## III — Compile Gaps in StructRegistry for CSS L4

### III.1 — Coverage classification

The CSS L4 registry (`generated/css_l4.registry.json`) has **187 layouts**:

| Kind | Count |
|---|---:|
| Struct | 123 |
| UntaggedEnum | 46 |
| TaggedEnum | 9 |
| NewtypeWrapper | 9 |

| rule_type (projected by inference) | Count |
|---|---:|
| Tuple | 98 |
| Span | 66 |
| HeterogeneousAltJoin | 9 |
| BoxedEnum | 8 |
| Vec | 4 |
| F64 | 1 |
| Named | 1 |

121 of 187 layouts have non-empty `fields` (typed). 66 have `Span` rule_type
and 16 have `kind: Struct` paired with `rule_type: Span` (rules where
inference gave up structural information; the CSS L4 specifics are
`containerLengthUnit`, `importantSuffix`, `bgProps`, `nthFunctionName`,
`urlFunction`, `customPropertyDecl`, `genericDecl`, `nsPrefix`,
`classSelector`, `highlightPseudo`, `simplePseudoElement`,
`simplePseudoClass`, `dirPseudo`, `mediaFeature`, `wqName`, `attrSelector`).

### III.2 — Critical-rule coverage

| Rule | Kind | rule_type | Field count |
|---|---|---|---:|
| stylesheet | Struct | BoxedEnum | 1 |
| ruleList | Struct | Vec | 1 |
| ruleItem | UntaggedEnum | BoxedEnum | 2 |
| qualifiedRule | Struct | Tuple | 2 |
| ruleBlock | Struct | BoxedEnum | 1 |
| **declaration** | TaggedEnum | HeterogeneousAltJoin | **28** |
| colorDecl | Struct | Tuple | 5 |
| sizeDecl | Struct | Tuple | 5 |
| spacingDecl | Struct | Tuple | 5 |
| **value** | TaggedEnum | HeterogeneousAltJoin | **15** |
| length | Struct | Tuple | 2 |
| angle / time / frequency / percentage | Struct | Tuple | 2 each |
| calcFunction / minFunction | Struct | Tuple | 2 each |
| varFunction | Struct | Tuple | 3 |
| classSelector | **Struct, Span** | — | 2 |
| **attrSelector** | **Struct, Span** | — | 4 |
| globalKeyword / namedColor | UntaggedEnum, Span | — | 4 / 20 |
| customPropertyDecl / genericDecl | Struct, Span | — | 5 each |

**Missing from registry entirely** (no layout with this rule_name):
`color`, `colorFunction`, `colorMix`, `idSelector`, `keyframeBlock`,
`transformFunction`, `filterFunction`, `easingFunction`, `gradient`,
`linearGradient`, `radialGradient`. These rules exist in the grammar but
either (a) factor through a wrapper that the registry merges, or (b) get
classified as a non-`Named` shape (Tuple/Vec) so the registry stores them
under a generated key. **In either case, `path!(CssL4, "color")` fails to
resolve** — the user's expected ergonomic surface for variant-select on
the CssColor sum cannot land without a registry-name-aliasing pass.

### III.3 — The 4 outlier grammars (W5.3 dedup) — why CSS L4 didn't fold

W5.3 audit (`docs/tranches/AZ-IV/audit/W5-arena-builder-dedup.md`)
identified 4 grammars (CSS L4, JSON, Sheets, BBNF) that did not dedup
into the shared `arena_template` + `builder_template`. The CSS L4-specific
reasons:

1. **The `OpenFrame` enum has 13 variants** (`runtime/css_l4/builder.rs:55-152`)
   vs the simple-cohort's 4 (Struct/Tuple/Repeat/Wrap). The colour-DAG
   recursion (`ColorMix.left/right: &'p CssColor<'p>`) and the typed
   numeric closure (Length/Angle/Time/etc. → `CssDimension`) require
   per-grammar finalisation logic that the simple-cohort `V::unit()`
   value-erasure cannot express.
2. **The arena has 6 distinct slabs** (rules, decls, selectors, values,
   keyframes, colors) — the `CompoundSlabArena<C>` template is
   parameterised by *one* `C: CompoundEntry`, not six. CSS L4's
   `CssArena::truncate(rules, decls, selectors, values, keyframes,
   colors)` (`runtime/css_l4/builder.rs:368-381`) requires per-slab
   counts; the simple-cohort interface assumes a single slab.
3. **The colour-DAG arena** (`CssArena::push_color → &'p CssColor<'p>`)
   uses bumpalo-style arena allocation for recursive types; the
   simple-cohort's Vec-of-Vec model cannot express the back-reference.
4. **The Numeric finalisation logic** (`builder.rs:679-717`) composes
   `(magnitude: f64, unit: u8)` into `CssDimension::{Length,Angle,Time,
   Frequency,Resolution,Flex,Percentage,Unitless}` — this is per-grammar
   typed closure that the simple-cohort `push_leaf_with_f64(v) =
   self.deposit(V::unit())` cannot replicate.

The path forward is BA's direct-projection (per DEEP-A, DEEP-C) — the
builder template dissolves entirely; per-rule parse fns return typed
shapes by direct construction. This is the only architectural answer
that closes (1)–(4) without orthogonal codepaths.

## IV — Generalised Sonic-Class API for CSS L4

### IV.1 — What should `CssL4Parser::get<T>(input, path)` look like?

```rust
// Compile-time-typed primitive query (existing impl):
let prop: Option<&str> = doc.get(path!(CssL4, "ruleList", 0, "declarations", 0, "property"));

// Compile-time-typed value query (NEEDS CssPathQuery for &CssTypedValue):
let val: Option<&CssTypedValue> = doc.get(path!(CssL4, "ruleList", 0, "declarations", 0, "value"));

// Variant-select on the value sum (NEEDS @branch_<idx> + CssPathQuery for &CssColor):
let color: Option<&CssColor> = doc.get(path!(CssL4,
    "ruleList", 0, "declarations", 0, "value", "@Color"));

// Wildcard iteration over declarations (NEEDS Iter<&Declaration> CssPathQuery):
for decl in doc.iter(path!(CssL4, "ruleList", "*", "declarations", "*")) {
    if decl.property.starts_with("--") { /* custom prop */ }
}

// Lazy bail-out (NEEDS routing in Document::get to parse_with):
let title: Option<&str> = CssL4Parser::get(input,
    path!(CssL4, "ruleList", 0, "declarations", 0, "property"));
```

### IV.2 — Common queries

1. **"Get the color of the first declaration in the first rule"** —
   `path!(CssL4, "ruleList", 0, "declarations", 0, "value", "@Color")`.
   Today: fails at the `@Color` step (no variant-select impl for
   CssTypedValue) AND at the `value` step (no `&CssTypedValue`
   CssPathQuery impl).

2. **"Iterate all selectors"** — `path!(CssL4, "ruleList", "*", "selectors", "*")`.
   Today: fails at `*` (no Iter CssPathQuery for `&Selector`); the
   walker returns `None` on Wildcard.

3. **"Find all `--var` declarations"** — `doc.iter(path!(CssL4, "ruleList", "*", "declarations", "*"))
   .filter(|d| d.property.starts_with("--"))`. Today: requires Iter
   support + Declaration as a queryable type.

4. **"Iterate @media rules"** — `doc.iter(path!(CssL4, "ruleList", "*", "@Media"))`.
   Today: requires variant-select on CssRule.

5. **"Get the calc() args of a declaration"** —
   `path!(CssL4, "ruleList", 0, "declarations", 0, "value", "@Function", "@Calc", "args")`.
   Today: fails at every step past "value".

### IV.3 — Comparison vs lightningcss traversal

lightningcss exposes traversal via the **`visitor::Visit` trait pattern**:
implementations override `visit_*` methods per type
(`visit_color_mut`, `visit_length_mut`, `visit_rule_mut`, etc.). It is
imperative-callback-driven, not path-driven.

The user's articulated goal — "sonic-class API generalized for all
grammars" — is **structurally superior**: a typed compile-time path
expression is more ergonomic than a 30-method visitor trait. But the
implementation gap is large:

- bbnf today has the **path infrastructure** (TypedPath, path! macro,
  PathSchema, parse_with) wired for CSS L4.
- bbnf today has **2 of N CssPathQuery impls** (`&str`, `f64`).
- bbnf today has **0 variant-select impls** (the `@Color`,
  `@Function`, `@Calc` segments don't resolve).
- bbnf today has **0 wildcard impls** (Iter return-type queries).

The substrate is in place; the consumer surface is empty. lightningcss's
visitor parity requires N impl blocks per typed sum variant. bbnf's
path parity requires the same N impls but expressed as
`impl CssPathQuery for &CssColor / &CssLength / Iter<&Declaration> /
&CssTypedValue / &CssRule / &StyleRule / …`.

## V — BA Recommendations

### V.1 — BA.W2 direct-projection compile gaps for CSS L4

The CSS L4 layouts that are TODAY emitted with `rule_type: TypeDesc::Span`
but whose registry entry contains a non-trivial typed projection (200
emit sites total). Five concrete rules where direct-projection would
unblock value-API:

1. **`stylesheet`** (rule_id=149) — registry has `Struct/BoxedEnum/1
   field`; emit projects to `Span`. Direct-projection: emit
   `StyleSheet { rules: Vec<CssRule<'p>> }` per `feedback_no-orthogonal-codepaths`,
   delete the `CssRuleListId` slab indirection.
2. **`qualifiedRule`** (rule_id=143) — registry: `Struct/Tuple/2 fields`;
   emit projects to `Span`. Direct: `StyleRule { selectors:
   &'p [Selector<'p>], declarations: &'p [Declaration<'p>] }` written
   in-place by parse_qualifiedRule.
3. **`declaration`** (rule_id=138) — registry: `TaggedEnum/HeterogeneousAltJoin/28
   fields` (the 27 typed `*Decl` rules + customPropertyDecl + genericDecl).
   Today: codegen emits a 28-arm checkpoint-rollback alt tower over Span.
   Direct: predictive first-byte dispatch on property-name PHF →
   `Property::Color(_) | Property::Width(_) | …` typed sum (mirrors
   lightningcss's Property enum); the alt tower retires; the
   `OpenFrame::Declaration` frame retires.
4. **`value`** (rule_id=156) — registry: `TaggedEnum/HeterogeneousAltJoin/15
   fields`; emit projects to Span. Direct: per-branch typed deposit; the
   `Wrap` frame retires for value Alts; `CssTypedValue` arms populate
   directly.
5. **`length / angle / time / frequency / resolution / flex / percentage`**
   (7 rules, each `Struct/Tuple/2 fields`) — the typed numeric closure.
   Direct: `parse_length` returns `CssLength { value, unit }` directly;
   `OpenFrame::Numeric` retires (lines 107-111 of builder.rs).
6. **`compoundSelector`** (rule_id=101) — registry: `Struct/Vec/1 field`;
   emit Span. Direct: `parse_compoundSelector` returns
   `Vec<Selector<'p>>` directly; `OpenFrame::SelectorList` retires;
   the `+`-quantified iteration uses non-speculative SmallVec accumulation
   instead of Vec checkpoint-clone.
7. **`hex`** (rule_id=3) — registry: `NewtypeWrapper/Tuple/1 field`;
   the regex DFA on hex digits flows to `OpenFrame::HexColor`. Direct:
   `parse_hex` returns `u32` from inline DFA-decode; the HexColor frame
   retires.
8. **`namedColor`** (rule_id=2) — registry: `UntaggedEnum/Span/20 fields`.
   Today: u32 packed → `CssColor::Hex`. Direct: PHF lookup over the
   named-color set → `CssColor::Named { name: &'p str, packed: u32 }`
   populating both fields (closes II.2 carry).
9. **`color`** (missing from registry — see III.2). Add a register-aliasing
   pass that surfaces `color` as the Alt of `colorMix | colorFn | hex |
   colorFunction | namedColor` so `path!(CssL4, ..., "color")` resolves.
10. **`mediaQueryList / mediaQuery / mediaFeature / mediaCondition`** —
    typed-AST gap II.1. Today: collapsed to MediaRule.query: &'p str.
    Direct: emit `MediaQueryList { queries: Vec<MediaQuery<'p>> }` with
    typed conditions/features per the existing 16 registry layouts.

### V.2 — BA.W4 sonic-class fixtures for CSS L4

10 specific path queries to land for CSS L4 sonic-class parity:

1. **`get_property::<&str>(input, path!(CssL4, "ruleList", 0, "declarations", 0, "property"))`**
   — "the first declaration's property name". Closes today via lazy
   bail-out at depth-3.
2. **`get_value::<&CssTypedValue>(input, path!(CssL4, "ruleList", 0, "declarations", 0, "value"))`**
   — "the first declaration's typed value". Requires
   `CssPathQuery for &CssTypedValue`.
3. **`get_color::<&CssColor>(input, path!(CssL4, "ruleList", 0, "declarations", 0, "value", "@Color"))`**
   — variant-select; requires `CssPathQuery for &CssColor` + `@Color`
   variant-select segment lowering.
4. **`get_length::<f64>(input, path!(CssL4, "ruleList", 0, "declarations", 0, "value", "@Dimension", "@Length", "value"))`**
   — typed numeric drill-down; requires `@Dimension`/`@Length`
   variant-select segments + path on `CssLength.value: f64`.
5. **`iter_decls::<Iter<&Declaration>>(input, path!(CssL4, "ruleList", "*", "declarations", "*"))`**
   — wildcard iteration; the most common consumer query
   ("every declaration in the document"). Requires `Iter<&Declaration>`
   CssPathQuery + cursor wildcard expansion in parse_with.
6. **`iter_custom_props::<Iter<&Declaration>>(input, path!(CssL4, "ruleList", "*", "declarations", "*"))
   .filter(|d| d.property.starts_with("--"))`** — composed with (5);
   demonstrates real CSS-vars consumer pattern.
7. **`iter_media::<Iter<&MediaRule>>(input, path!(CssL4, "ruleList", "*", "@Media"))`**
   — variant-select over CssRule; requires `Iter<&MediaRule>` + `@Media`
   segment.
8. **`iter_selectors::<Iter<&Selector>>(input, path!(CssL4, "ruleList", "*", "selectors", "*"))`**
   — every selector across every style rule. Requires Iter + Selector
   path-traversal.
9. **`get_keyframes::<&KeyframesRule>(input, path!(CssL4, "ruleList", "*", "@Keyframes"))`**
   — variant-select on CssRule's @Keyframes branch.
10. **`get_calc_args::<Iter<&CssTypedValue>>(input, path!(CssL4, "ruleList", 0, "declarations", 0, "value", "@Function", "@Calc", "args", "*"))`**
    — deep variant-select drill into the typed function family.

These fixtures, exercised against `bootstrap.css` + `tailwind.css`,
provide the same shape that `bbnf_get_twitter` exercises against JSON —
the BA close-gate evidence for CSS L4's value-API parity.

## VI — Routing

| Item | BA wave |
|---|---|
| 200 `rule_type: TypeDesc::Span` emit sites → registry-projected | BA.W2 |
| `Vec<OpenFrame>::clone` checkpoint dissolution | BA.W3 |
| 13-variant OpenFrame retirement → direct struct construction | BA.W2 (partial) + BA.W3 |
| `String::from(rule_name)` → `&'static str` interner | BA.W0 (cleanup absorption) |
| Selector::Span → typed Selector::* arms | BA.W2 |
| `CssColor::Named.name` populated | BA.W2 |
| MediaQueryList typed AST | BA.W2 |
| 22 missing CssRule variants (Import / FontFace / etc.) | Out-of-BA scope; routes to BC cleanup or grammar-extension follow-on |
| CssPathQuery for `&CssTypedValue / &CssColor / &CssLength / Iter<…>` | BA.W4 |
| `path!` registry alias for `color` (II.2 III.2) | BA.W2 (registry pass) |
| Tailwind WATCHDOG resolution | BA.W3 (the checkpoint-clone retirement is *the* lever; the AV-scale PHF + SIMD selector classifier is parallel work — name it but defer until BA.W3 numbers settle) |

The **single primary blocker** for both performance parity (the 983×
bootstrap regression, the 10,583× tailwind regression) and semantic
parity (the typed-Selector/typed-CssRule/typed-MediaQuery gaps) is the
same: the `StructBuilder::checkpoint` discipline forces the 13-variant
`OpenFrame` enum, which forces every selector/declaration/value to land
on a generic `Vec<…>` slot that the per-rule parse-fn cannot project
into a typed shape. Direct-projection codegen retires the OpenFrame
mechanism *and* unwires the 200 `Span` emit sites *and* enables the
typed-leaf deposits that close II.1's 22-CssRule-variant gap.

## VII — Citations

- `docs/benchmarks/post-AZ-IV.json` (CSS L4 entries: bootstrap 606.4 ms /
  0.46 MB/s, normalize 402.2 µs / 15 MB/s, tailwind WATCHDOG_HALT
  77.567 s observed against 5 s cap).
- `crates/core/src/grammar/generated/css_l4.rs` (107,138 LOC; 200
  TypeDesc::Span emits, 1,018 String::from sites, 1,407 checkpoint
  sites, 415 begin_compound/end_compound calls, 28 distinct regex
  DFAs).
- `crates/core/src/grammar/generated/css_l4.registry.json` (187 layouts;
  121 typed; 66 Span; 16 Struct+Span gaps).
- `crates/core/src/runtime/css_l4/{value,builder,arena,document,parse_with}.rs`
  (3,126 LOC total; 13-variant OpenFrame; 6-slab arena; 2-impl
  CssPathQuery surface).
- `grammar/css/l4/{stylesheet,values,properties,selectors,color,…}.bbnf`
  (1,320 LOC; 243 rules counted; 28-branch declaration alt-dispatch).
- `crates/core/benches/css/l4.rs` (bench harness; CSS_TAILWIND_PARSE
  wall = 5 s).
- `docs/tranches/AZ-IV/audit/DEEP-{A,B,SYNTHESIS}.md` (architectural
  attribution: `Vec<OpenFrame>::clone` at 86.07 % inclusive on JSON;
  same mechanism on CSS L4 amplified by enum size + decl alt density).
- `lightningcss-1.0.0-alpha.71` (`src/{stylesheet.rs:73, rules/mod.rs:139,
  values/length.rs:541, values/color.rs:46, properties/mod.rs:684,
  declaration.rs:61}`) — parity baselines.

End.
