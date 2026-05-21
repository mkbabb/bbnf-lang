# SK-V13 Scoping: CSS L4 Semantic Parity Gap and Wave Shortlist

**Date:** 2026-05-21
**Status:** Research scoping for SK-V13 wave planning
**Authority:** SK-V12 CAMPAIGN-CLOSE-SK-V12-V12 (admitted `css_l4/declaration_values/direct_to_struct/main` at 2.54× lightningcss); USER-PIN-W1-CSS-L4-SOTA (CSS is authoritative, semantic parity with lightningcss)

---

## Section 1: Current CSS L4 Coverage (SK-V12 Admitted Row)

The SK-V12 close admits ONE CSS L4 row:
```
css_l4/declaration_values/direct_to_struct/main
```

**Output plane:** `css_l4_declaration_value_fact_stream` (shared by generated Track 1, cssparser oracle/Track 2, lightningcss)

**Scope of admitted row:** Declaration-level CSS *values only*, not full stylesheet admission. The parser root is `properties.bbnf::declaration` within a rule block. Coverage is strictly **declaration value tokens and fragments** present in the fixture at `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css`.

**Fixture analysis** (187 bytes, 7 declarations across 3 depth levels):
```
a { color: #ff00ff; width: 50%; opacity: .5; margin-left: -10px; }
b { background-color: rgb(255 128 0 / 0.5) !important; }
@media (min-width: 640px) { c { height: 100px; color: red; } }
```

### Per-Production Coverage Status

| Production | Token/Feature | Fixture Present | Codegen Status | Notes |
|---|---|---|---|---|
| `hash` | Hex color literal `#ff00ff` | Yes | **IMPLEMENTED** | 6-digit hex; tracked via lexeme span |
| `number` (unitless) | `.5`, `50%` numerator, `255`, `128`, `0`, `0.5` | Yes | **IMPLEMENTED** | Includes decimals, leading-dot, multi-digit |
| `dimension` | `-10px`, `100px` | Yes | **IMPLEMENTED** | Dimension (value + unit); negative values supported |
| `percentage` | `50%` | Yes | **IMPLEMENTED** | Percentage discriminant; paired with number |
| `ident` | `red`, keyword fallback | Yes | **IMPLEMENTED** | Bare identifiers in function args; color names |
| `function` | `rgb(...)` | Yes | **IMPLEMENTED** | Function name + open-paren detection; arg content deferred |
| `delim` | `/` (slash in `rgb(.../ ...)`) | Yes | **IMPLEMENTED** | Component separator within function args |
| `paren_close` | `)` closing `rgb(...)` | Yes | **IMPLEMENTED** | Closing bracket |
| `calc()` function | Recursive math expressions | **NOT IN FIXTURE** | **ABSENT** | Grammar exists (`values.bbnf`); no codegen or test |
| `var()` function | CSS custom property fallback | **NOT IN FIXTURE** | **ABSENT** | Grammar exists; no codegen or test |
| `url()` function | Background image URI | **NOT IN FIXTURE** | **ABSENT** | Grammar exists; no codegen or test |
| `min()`, `max()`, `clamp()` | CSS math limits | **NOT IN FIXTURE** | **ABSENT** | Grammar exists; no codegen or test |
| Color functions (`hsl()`, `hwb()`, `lch()`, etc.) | Typed color syntax | **NOT IN FIXTURE** | **ABSENT** | Grammar exists; no codegen or test |
| Gradient functions | `linear-gradient()`, `radial-gradient()`, etc. | **NOT IN FIXTURE** | **ABSENT** | Grammar exists (`gradients.bbnf`); no codegen or test |
| Transform functions | `translate()`, `rotate()`, `scale()`, etc. | **NOT IN FIXTURE** | **ABSENT** | Grammar exists (`transforms.bbnf`); no codegen or test |
| Filter functions | `blur()`, `brightness()`, `contrast()`, etc. | **NOT IN FIXTURE** | **ABSENT** | Grammar exists (`filters.bbnf`); no codegen or test |
| Easing functions | `cubic-bezier()`, `steps()`, etc. | **NOT IN FIXTURE** | **ABSENT** | Grammar exists (`easing.bbnf`); no codegen or test |
| Whitespace & comments | CSS comments `/* ... */` | NOT IN FIXTURE | **PARTIALLY IMPLEMENTED** | WSifier defines comment regex; fixture uses no comments |
| String literals | Quoted property values (e.g., `font-family: "Arial"`) | **NOT IN FIXTURE** | **ABSENT** | Grammar rule exists (`cssString`); no test coverage |
| Escaped identifiers | `\26 ` hex escape, `\-` dash escape | **NOT IN FIXTURE** | **ABSENT** | Grammar absent in codegen; cssparser oracle only |

**Summary:** SK-V12 codegen covers only the **8 token kinds** present in the fixture (hash, number, dimension, percentage, ident, function, delim, paren_close). The grammar defines 15+ additional productions (calc, var, url, color functions, gradients, transforms, filters, easing, strings, escapes) **with zero codegen or test surface**. These are grammatically specified but functionally orphaned in the codegen.

---

## Section 2: lightningcss Feature Surface vs bbnf-lang Generator

lightningcss `1.0.0-alpha.71` exposes a **semantic, canonicalized AST** covering the CSS Cascading and Inheritance Level 4 specification and vendor-prefixed extensions. It parses full stylesheets with error recovery disabled to a typed rule and declaration tree.

### lightningcss Public API Coverage

| Feature Family | lightningcss Coverage | bbnf-lang Generator Status | Fixture Test | Notes |
|---|---|---|---|---|
| **Selectors** | Type selectors, class/ID, pseudo-classes (`:hover`, `:is()`, `:where()`), pseudo-elements (`::before`), combinators, attribute selectors | Grammar exists (`selectors.bbnf`) but NO codegen | Not present in W1b fixture | Codegen missing; test only via grammar reference |
| **At-rules** | `@media`, `@keyframes`, `@supports`, `@layer`, `@container`, `@scope`, `@import`, `@font-face` | Partial codegen (mediaRule, keyframesRule, genericAtRule) | `@media` in fixture | Only media/keyframes/generic dispatched; others caught by fallback |
| **Declaration block** | Property names, !important flag, typed value lists | **IMPLEMENTED for value tokens** | Full coverage | Depth tracking, property name dispatch, important flag |
| **Vendor prefixes** | `-webkit-`, `-moz-`, `-ms-`, `-o-` properties and values | Grammar ignores; codegen ignores | Not in fixture | Parser treats as bare `dashIdent`; no semantic dispatch |
| **CSS Variables** | `--custom-prop` names, `var()` fallback lists | Grammar defined; **NO codegen** | Not in fixture | cssparser oracle only; no generated fact stream |
| **Logical properties** | `-inline-*`, `-block-*` (flexbox/grid logical axes) | Grammar partially defined; no codegen | Not in fixture | Treated as generic property names |
| **Nested rules** | `&` selector nesting, scope nesting | Grammar absent | Not in fixture | lightningcss exposes nested `rules` on `StyleRule`; bbnf-lang has no grammar |
| **CSS Grid** | `grid-template-*`, `grid-area`, `place-*` | No semantic support; falls through to generic declaration | Not in fixture | Property names recognized but no typed value dispatch |
| **CSS Flexbox** | `flex-direction`, `justify-content`, `align-items`, etc. | Typed dispatch exists; values parsed generically | Not in fixture | Dispatch templates exist; value AST incomplete |
| **CSS Custom Properties** | `--name: value` syntax and var() resolution | Grammar branch exists; **NO codegen** | Not in fixture | Grammar defined but orphaned from codegen |
| **Calc expressions** | `calc()`, `min()`, `max()`, `clamp()` with recursive nesting | Grammar defined (`values.bbnf`); **NO codegen** | Not in fixture | Full recursive grammar exists; zero test or emission |
| **Color syntax** | Named colors, hex `#rgb`, `#rrggbb`, `rgb()`, `hsl()`, `hwb()`, `lch()`, relative colors | Color grammar (`color.bbnf`) exists; **NO codegen** | `#ff00ff`, `rgb(...)` parsed as tokens only | Only hash/function detected; internal color typing absent |
| **Gradients** | Linear, radial, conic, repeating variants with stops | Gradient grammar (`gradients.bbnf`); **NO codegen** | Not in fixture | Defined but unused in any test or emission |
| **Transforms** | `translate()`, `rotate()`, `scale()`, `skew()`, `matrix()` 2D/3D | Transform grammar (`transforms.bbnf`); **NO codegen** | Not in fixture | Typed function names defined; no argument parsing |
| **Filters** | `blur()`, `brightness()`, `contrast()`, `drop-shadow()`, etc. | Filter grammar (`filters.bbnf`); **NO codegen** | Not in fixture | Defined; never tested or emitted |
| **Easing functions** | `ease-in`, `linear`, `cubic-bezier()`, `steps()` | Easing grammar (`easing.bbnf`); **NO codegen** | Not in fixture | Defined; no codegen or test |
| **Media queries** | Syntax, logical operators, feature queries | Media grammar (`media.bbnf`); minimal codegen | `@media (min-width: 640px)` in fixture | Generic @media rule; query expression not parsed or tracked |
| **Feature flags** | `.supports()` @-rule conditions | Generic @-rule fallback; no typed support | Not in fixture | Falls through to genericAtRule catch-all |
| **Box model** | `margin`, `padding`, `border`, `outline` shorthand and longhand | Typed dispatch exists; values parsed generically | Not in fixture | Dispatch templates; value completeness TBD |
| **Text & fonts** | `font-family`, `font-size`, `font-weight`, `text-decoration`, `letter-spacing` | Typed dispatch exists; values parsed generically | Not in fixture | Dispatch templates; value AST incomplete |
| **Background & borders** | `background-*`, `border-*` properties, shorthand | Typed dispatch exists; values parsed generically | Not in fixture | Dispatch; no gradient/image parsing |
| **Position & sizing** | `position`, `top`/`left`/`right`/`bottom`, `width`/`height`, `min-width`, etc. | Typed dispatch exists; values parsed generically | `width: 50%`, `height: 100px` in fixture | Dispatch works; dimension/percentage only |
| **Display & visibility** | `display`, `visibility`, `opacity`, `z-index` | Typed dispatch exists; values parsed generically | `opacity: .5` in fixture | Generic `displayKeyword` dispatch; value coverage narrow |
| **Comments** | `/* ... */` CSS block comments | Comment WSifier regex defined; **NO emission** | Not in fixture; fixture has no comments | Comments recognized by parser; facts never emitted |
| **Whitespace preservation** | Pre/post-token/rule whitespace, formatting | Comment/WS WSifier; **NO fact emission** | Not in fixture | Recognized for parsing; output plane has zero whitespace facts |

**Summary:** lightningcss covers 24+ feature families spanning selectors, at-rules, typed properties, functions, values, nesting, and modern CSS extensions. bbnf-lang has grammar definitions for ~18 of these but **zero codegen or test surface for 16+ families**. The W1b-1 admitted generator covers **only declaration-value token emission**. Selectors, at-rule structure (beyond generic catch-all), nested rules, vendor prefixes, CSS variables, calc/var/url, colors beyond hash/function, gradients, transforms, filters, easing, and comment/whitespace facts are **completely absent from the codegen**.

---

## Section 3: Parity Gap Matrix (Exhaustive Feature Coverage)

| Feature | lightningcss Support | bbnf-lang Generator Support | Parity Status | SK-V13 Wave Candidate? | Notes |
|---|---|---|---|---|---|
| **Declaration values (tokens)** | Full typed AST | Track 1 token stream (hash, number, %, dim, ident, fn, delim) | **PARITY** | N/A (admitted in SK-V12) | 8-token fixture only; 16+ productions unexercised |
| **Declarations** | Full block, ordering, !important | Simple `prop: value !important` dispatch | **PARTIAL** | Yes (v2 corpus w/ all property groups) | Depth-nested blocks missing in codegen |
| **Selectors** | Full L4 AST, combinators, pseudo-classes | Grammar defined; zero codegen | **MISSING** | Yes (high priority, v1–v3) | Largest surface gap; required for stylesheet |
| **Stylesheet root** | Complete rule list, nesting | Grammar defined; zero codegen | **MISSING** | Yes (v1, foundation) | Cannot emit structured stylesheet facts |
| **At-rules (@media, @keyframes)** | Typed rule AST, nested rules | Generic catch-all only; mediaRule dispatch not consumed | **PARTIAL** | Yes (v2 for @media, v3 for others) | `@media` syntax in fixture; no fact emission |
| **Nested rules** | `CssRule::Style.rules` exposes nesting | Grammar absent | **MISSING** | Yes (v3, CSS Nesting L1) | Modern CSS; lightningcss nests under each rule |
| **CSS Variables (--custom-prop)** | Full typed support, var() resolution | Grammar branch; zero codegen | **MISSING** | Yes (v2 or later, corpus-gated) | Widespread in real CSS; varFunction defined but orphaned |
| **Calc expressions** | Parsed and evaluated | Grammar defined recursively; zero codegen | **MISSING** | Yes (v2, core math feature) | Calc fact facts stream needed for math-heavy designs |
| **Var and URL functions** | Full support with fallbacks | Grammar defined; zero codegen | **MISSING** | Yes (v2, tied to CSS Variables) | URL needed for image/font loading facts |
| **Color functions** | hsl, hwb, lch, oklch, relative colors | Hash and rgb() token-level only; color.bbnf unexercised | **PARTIAL** | Yes (v1, critical for color properties) | lightningcss normalizes to canonical form; bbnf-lang sees raw tokens |
| **Gradients** | linear, radial, conic, repeating stops | Grammar defined; zero codegen | **MISSING** | Yes (v2 or v3, lower priority) | Gradients.bbnf is complete; zero test or emission |
| **Transforms** | 2D/3D transform functions and matrices | Grammar defined; zero codegen | **MISSING** | Yes (v3, lower priority) | Transforms.bbnf defined; unused |
| **Filters** | blur, brightness, contrast, drop-shadow, etc. | Grammar defined; zero codegen | **MISSING** | Yes (v3, lower priority) | Filters.bbnf defined; never tested |
| **Easing functions** | cubic-bezier, steps, ease-*, etc. | Grammar defined; zero codegen | **MISSING** | Yes (v3, lower priority) | Easing.bbnf defined; unused |
| **Media queries** | Full feature/logical operator syntax | Generic @media dispatch; query expression not tracked | **PARTIAL** | Yes (v2, prerequisite for responsive) | Media.bbnf exists; feature conditions not parsed |
| **Container queries** | @container syntax, feature queries | No grammar | **MISSING** | No (out of scope unless CSS Conditionals L4 scope expands) | lightningcss supports; bbnf-lang has no grammar |
| **Layer queries** | @layer blocks and layer() function | No grammar | **MISSING** | No (CSS Cascade L5; post-V13 expansion) | lightningcss supports; bbnf-lang absent |
| **Scope queries** | @scope selector & body | No grammar | **MISSING** | No (CSS Scoping; post-V13 expansion) | lightningcss supports; bbnf-lang absent |
| **Vendor prefixes** | -webkit-, -moz-, -ms-, -o- dispatch | Treated as dashIdent; no semantic categorization | **MISSING** | No (lower priority; fallback to generic ident) | Recognized but not typed; could emit facts |
| **Custom at-rules** | @supports, @import, @font-face, @keyframes, user/vendor | Generic fallback; limited structured support | **PARTIAL** | Yes (v3, full at-rule taxonomy) | Generic@Rule catch-all works; no fact projection |
| **Pseudo-classes** | :is(), :where(), :not(), :has(), :nth-child(), etc. | Grammar defined in selectors.bbnf; zero codegen | **MISSING** | Yes (v1, tied to selector codegen) | Selector emission required before pseudo dispatch |
| **Pseudo-elements** | ::before, ::after, ::first-line, ::first-letter | Grammar defined in selectors.bbnf; zero codegen | **MISSING** | Yes (v1, tied to selector codegen) | Selector emission required |
| **Attribute selectors** | [attr], [attr=value], [attr^=value], etc. | Grammar defined in selectors.bbnf; zero codegen | **MISSING** | Yes (v1, tied to selector codegen) | Selector emission required |
| **Comments** | Recognized and parsed | Comment regex defined; zero fact emission | **MISSING** | No (diagnostic only; out of scope) | Could emit comment ranges; lower priority |
| **Whitespace preservation** | Lossless in On Demand; normalized in DOM | WSifier defined; zero fact emission | **MISSING** | No (output plane does not preserve WS; out of scope) | Explicit design choice to normalize |
| **Source mapping** | Supported for debugging | No grammar or codegen | **MISSING** | No (post-V13 enhancement) | Would require source offset tracking |
| **Error recovery** | Optional error recovery | All-or-nothing; single error ends scan | **MISSING** | No (out of scope; scalar-only for V13) | lightningcss error_recovery=false used for admission |
| **Strict vs permissive** | Configurable strictness | Fixture-specific oracle; no per-property strictness | **PARTIAL** | No (emission plane uses track1 semantics only) | Strictness already ensured by oracle equality gate |

**Parity count:**
- **PARITY:** 1
- **PARTIAL:** 7
- **MISSING:** 16
- **OUT_OF_SCOPE:** 6
- **Total:** 30

---

## Section 4: Wave-Candidate Productions and LOC Envelopes

Of the **PARTIAL** (7) and **MISSING** (16) parity rows, the following are SK-V13 tractable with measured corpus, oracle, and expected LOC envelope.

### Candidate Wave 1: Stylesheet root + selector framework (Foundation)
**Production:** `stylesheet`, `ruleList`, `selectorList` (selectors.bbnf main entry)
**Rationale:** Required for any stylesheet-level CSS L4 row; selectors are the structural spine of CSS.
**Fixture:** Minimal selectors (`a`, `b`, `c`) already in W1b corpus.
**Oracle:** lightningcss rule AST traversal + independent selector parsing via cssparser.
**Corpus:** W1b fixture + real-world CSS: Bootstrap selectors, Tailwind output, normalize.css (selector-heavy).
**Expected LOC:**
  - Codegen selector.rs: 120–180 LOC
  - Runtime selector module: 80–120 LOC
  - Test + oracle integration: 150–200 LOC
  - Total: **350–500 LOC**
**Consumer:** All subsequent stylesheet-level waves (W2–W4).
**Risk:** High; selector grammar is large and deeply nested; FIRST/follow dispatch complexity medium.

### Candidate Wave 2: Declaration-value expansion + CSS Variables + calc()
**Production:** `declaration` + `varFunction`, `calcFunction`, color functions (color.bbnf)
**Rationale:** Variables and calc() are ubiquitous in modern CSS (Tailwind, component design systems). Current codegen covers only 8 token kinds; 16+ are undefined.
**Fixture:** Tailwind CSS output (heavy var() usage), real gradient fixtures, calc expressions from Bootstrap/custom designs.
**Oracle:** lightningcss property value AST via `DeclarationBlock::iter()` cross-checked with cssparser token scan.
**Corpus:** Tailwind build output, CSS-in-JS libraries, component systems with calc() math.
**Expected LOC:**
  - Codegen expansion (varFunction, calcFunction, color dispatch): 200–280 LOC
  - Runtime var/calc handler module: 120–160 LOC
  - Color normalization and fact projection: 80–120 LOC
  - Tests + oracle: 200–280 LOC
  - Total: **600–840 LOC**
**Consumer:** Design system tooling, math-heavy responsive designs.
**Risk:** Medium-high; calc() recursion depth needs bounded guards; var() fallback complexity.

### Candidate Wave 3: Gradient, transform, filter, easing function codegen
**Production:** `gradient`, `transformFunction`, `filterFunction`, `easingFunction` (gradients.bbnf, transforms.bbnf, filters.bbnf, easing.bbnf)
**Rationale:** Modern visual design heavily uses these; currently zero codegen.
**Fixture:** Real CSS with gradients (Bootstrap, Material Design), SVG filters, animation easing.
**Oracle:** lightningcss AST + cssparser token cross-check.
**Corpus:** Animate.css, transition libraries, gradient-heavy designs.
**Expected LOC:**
  - Codegen gradient/transform/filter/easing modules: 300–400 LOC (each function grammar ~80–100 LOC codegen)
  - Runtime dispatch and normalization: 150–200 LOC
  - Tests + oracle: 250–350 LOC
  - Total: **700–950 LOC**
**Consumer:** Animation, visual effects, modern design tooling.
**Risk:** Medium; function arg parsing is straightforward; oracle cross-check is tedious but mechanical.

### Candidate Wave 4: At-rule structured dispatch (@media, @keyframes full) + media queries
**Production:** `mediaRule`, `keyframesRule` (stylesheet.bbnf) + `mediaQueryList` (media.bbnf)
**Rationale:** @media is the largest CSS at-rule; full query feature parsing enables responsive design facts.
**Fixture:** Bootstrap responsive breakpoints, mobile-first media queries.
**Oracle:** lightningcss rule AST + media feature condition lexing.
**Corpus:** Bootstrap, Material Design, real responsive frameworks.
**Expected LOC:**
  - Codegen media query feature parser: 150–200 LOC
  - @keyframes frame block handler: 100–150 LOC
  - Runtime media/keyframes dispatcher: 100–150 LOC
  - Tests + oracle: 200–300 LOC
  - Total: **550–800 LOC**
**Consumer:** Responsive design queries, animation sequencing.
**Risk:** Medium; media feature syntax is standardized but verbose.

### Candidate Wave 5: Nested rules (CSS Nesting Module) + scope/container queries
**Production:** Nested `ruleItem` under `StyleRule` (new grammar extension), @scope, @container (new)
**Rationale:** CSS Nesting L1 is now widely adopted (Sass-style, standardized); enables structural CSS facts.
**Fixture:** Modern component CSS using nesting and media-nesting combos.
**Oracle:** lightningcss `StyleRule::rules` traversal (nesting is native in L4).
**Corpus:** PostCSS output, modern bundler CSS, Tailwind with nesting.
**Expected LOC:**
  - Grammar extension for nesting: 50–80 LOC (new rule)
  - Codegen nesting dispatcher: 100–150 LOC
  - Runtime nested block handler: 80–120 LOC
  - Tests + oracle: 150–250 LOC
  - Total: **380–600 LOC**
**Consumer:** Modern component frameworks, structural CSS tooling.
**Risk:** High; nesting depth unbounded; orbit complexity comparable to stylesheet recursion.

### Candidate Wave 6: Vendor prefixes + custom at-rules full taxonomy
**Production:** `dashIdent` property dispatch (properties.bbnf), full `atRule` enum
**Rationale:** Vendor prefixes are still common in real CSS (especially animations, backdrop-filter); custom at-rules are extensible.
**Fixture:** Real-world CSS with -webkit-*, -moz-*, custom at-rules.
**Oracle:** lightningcss property name normalization + cssparser at-rule catch-all.
**Corpus:** Browser compatibility CSS, polyfill-heavy frameworks.
**Expected LOC:**
  - Codegen vendor prefix dispatch: 80–120 LOC
  - Custom at-rule taxonomy: 100–150 LOC
  - Runtime dispatch: 50–80 LOC
  - Tests + oracle: 120–200 LOC
  - Total: **350–550 LOC**
**Consumer:** Polyfill tooling, vendor-specific frameworks.
**Risk:** Low-medium; dispatch is mechanical; oracle coverage is straightforward.

### Candidate Wave 7: Source mapping + comment/whitespace facts (diagnostic)
**Production:** Source offset tracking, comment ranges, WS fact emission
**Rationale:** Enables source-map reconstruction, debug tooling; lower priority.
**Fixture:** CSS with comments and structured whitespace.
**Oracle:** lightningcss source positions + cssparser token offsets.
**Corpus:** Human-authored CSS, pretty-printed stylesheets.
**Expected LOC:**
  - Codegen source tracking: 100–150 LOC
  - Comment fact emitter: 60–100 LOC
  - WS fact projection: 80–120 LOC
  - Tests + oracle: 150–200 LOC
  - Total: **390–570 LOC**
**Consumer:** Dev tools, source mapping, linting/formatting.
**Risk:** Low; straightforward offset tracking; oracle is cssparser token positions.

---

## Section 5: Corpus Inventory

### Existing Test Corpora in bbnf-lang

| Corpus Name | Location | Size | Features | CSS Coverage | Notes |
|---|---|---|---|---|---|
| **W1b fixture (SK-V12)** | `restart/skinny/tranches/sk-v12/research/w1b/css_l4_declaration_values.css` | 187 B | 7 declarations, @media, depth nesting | Hash, number, %, dimension, rgb(), !important | Minimal; bare baseline |
| **CSS selector tests** | `skinny/crates/core/tests/css_*.rs` | ~50 CSS snippets | Selectors, property groups, keywords | Basic type/class/ID selectors | Test fixtures embedded in Rust |
| **Grammar reference CSS** | `grammar/css/l4/` fixtures (if any) | ~10–50 fragments | Grammar examples | Varies | Reference material; not a benchmark corpus |
| **Normalize.css mirror** | (if present) | ~2 KB | Box model, typography, reset | Broad but minimal | Potential corpus if mirrored in repo |

### Real-World CSS Corpora (Recommended for SK-V13)

| Corpus | Source | Size | Feature Coverage | Availability | Rationale |
|---|---|---|---|---|---|
| **Bootstrap CSS** (minified) | https://github.com/twbs/bootstrap/blob/main/dist/bootstrap.css | ~180 KB | Selectors, @media, calc, variables, shorthands, vendor prefixes | Public; easily vendored | Widely-used framework; real-world scale |
| **Tailwind CSS output** | Generated from typical Tailwind config | ~400 KB (unminified) | Heavy var() usage, @media, @supports, custom properties | Can generate from Tailwind CLI | Heavy CSS Variables, modern features |
| **Normalize.css** | https://github.com/necolas/normalize.css | ~10 KB | Box model, typography, form resets | Public | Simple, focused, complete coverage |
| **GitHub CSS dump** | github.com live CSS | ~500 KB+ | Vendor prefixes, nested rules (if Sass), animations, custom props | Web scrape (ToS check) | Real-world production CSS |
| **Material Design CSS** | https://github.com/material-components/material-components-web | ~300 KB | CSS Variables, custom properties, gradients, animations | Public; complex | Modern design patterns |
| **Animate.css** | https://github.com/animate-css/animate.css | ~80 KB | Keyframes, animations, transforms, easing | Public | Animation-heavy corpus |

### Recommended Corpus for SK-V13 Admission Waves

**Wave 1–2 (stylesheet + selectors + vars/calc):** Bootstrap (180 KB) + Tailwind output (400 KB)
- Both have diverse selectors, at-rules, modern CSS Variables, calc() usage.
- Representative of real-world CSS; forces comprehensive selector and function codegen.

**Wave 3 (gradients/transforms/filters):** Material Design (300 KB) + Animate.css (80 KB)
- Material Design: gradients, transforms, complex selectors.
- Animate.css: keyframe animation, transform-heavy, @keyframes.

**Wave 4 (media/keyframes):** Bootstrap + Material Design
- Both have extensive @media usage and animation keyframes.

**Wave 5+ (nested rules, custom at-rules):** Modern PostCSS/Sass output
- Requires collecting CSS with nesting and non-standard at-rules.

---

## Section 6: SK-V13 CSS Expansion Wave Shortlist

Ranked by parity impact / risk and corpus-gated readiness.

### Wave 1: Stylesheet Root + Selector Framework (Foundation)

**Target:** Stylesheet parsing, selector list emission, rule structural facts
**Productions:** `stylesheet`, `ruleList`, `selectorList`, `qualifiedRule` (selectors.bbnf main)
**Corpus:** W1b baseline + Bootstrap 180 KB (selector-heavy)
**Oracle:** lightningcss rule traversal + cssparser selector parsing
**Admission Row:** `css_l4/stylesheet_and_selectors/direct_to_struct/main`
**Expected Delta:** +350–500 LOC (codegen + runtime + test)
**vs lightningcss:** Selector AST codegen, rule structural facts; lightningcss emits typed selectors; bbnf-lang will emit selector token stream.
**Parity impact:** **HIGHEST** — Foundation for all stylesheet-level rows; unblocks Waves 2–5.
**Risk:** HIGH (selector grammar complexity); Prerequisite for W2+.

### Wave 2: Declaration-Value Expansion + CSS Variables + Calc Expressions

**Target:** var(), calc(), color functions; declaration value comprehensive coverage
**Productions:** `varFunction`, `calcFunction`, `color` functions (color.bbnf), expanded `value` dispatch
**Corpus:** Tailwind output (400 KB; heavy var() + calc)
**Oracle:** lightningcss property values + cssparser token cross-check
**Admission Row:** `css_l4/declaration_values_extended/direct_to_struct/main`
**Expected Delta:** +600–840 LOC
**vs lightningcss:** Codegen now covers all 16+ token kinds in `values.bbnf`, plus var/calc recursion and color normalization.
**Parity impact:** **VERY HIGH** — Modern CSS is dense with variables and math expressions; expands admission coverage from 8 to 24+ token kinds.
**Risk:** MEDIUM-HIGH (calc recursion unbounded; var fallback lists); but corpus and oracle are well-defined.

### Wave 3: Gradient, Transform, Filter, Easing Function Codegen

**Target:** Visual function families (gradients, transforms, filters, easing)
**Productions:** `gradient`, `transformFunction`, `filterFunction`, `easingFunction`
**Corpus:** Material Design (300 KB) + Animate.css (80 KB)
**Oracle:** lightningcss AST + cssparser token verification
**Admission Row:** `css_l4/visual_functions/direct_to_struct/main`
**Expected Delta:** +700–950 LOC
**vs lightningcss:** Codegen emits fact stream for gradient stops, transform matrix args, filter param types, easing curves; lightningcss emits semantic types.
**Parity impact:** **HIGH** — Visual functions are widespread in modern CSS (animations, responsive design). Zero codegen today.
**Risk:** MEDIUM (function arg parsing is mechanical; oracle cross-check tedious but straightforward).

### Wave 4: At-Rule Structured Dispatch + Media Query Features

**Target:** @media query feature parsing, @keyframes, @supports, @import
**Productions:** `mediaQueryList`, `mediaRule`, `keyframesRule`, full `atRule` dispatch
**Corpus:** Bootstrap + Material Design (both have extensive @media and keyframes)
**Oracle:** lightningcss media feature AST + cssparser at-rule catch-all
**Admission Row:** `css_l4/at_rules_and_media/direct_to_struct/main`
**Expected Delta:** +550–800 LOC
**vs lightningcss:** Codegen parses media feature conditions (min-width, hover, grid, etc.), keyframe selectors (% | from | to), and at-rule taxonomy.
**Parity impact:** **HIGH** — @media is the most common at-rule; media query features are essential for responsive design facts.
**Risk:** MEDIUM (media feature syntax standardized but verbose; keyframe selector dispatch straightforward).

### Wave 5: Nested Rules (CSS Nesting L1) + Scope/Container Queries

**Target:** Nested ruleItem under StyleRule, @scope, @container syntax
**Productions:** Extend stylesheet grammar with nesting, new `scopeRule`, `containerRule`
**Corpus:** Modern PostCSS/Sass output with nesting, future CSS Nesting L1 corpus
**Oracle:** lightningcss nested rule traversal + cssparser nesting parse
**Admission Row:** `css_l4/nested_rules_and_queries/direct_to_struct/main`
**Expected Delta:** +380–600 LOC
**vs lightningcss:** Codegen emits nested rule structural facts; lightningcss has native nesting in L4.
**Parity impact:** **MEDIUM-HIGH** — CSS Nesting is now standardized; widely adopted in tooling. Container/scope queries are newer, lower priority.
**Risk:** HIGH (nesting depth unbounded; mutual recursion with stylesheet). Recommend bounded depth guard.

### Wave 6: Vendor Prefixes + Custom At-Rule Full Taxonomy

**Target:** -webkit-*, -moz-*, -ms-* prefix dispatch; extensible at-rule handling
**Productions:** `dashIdent` dispatch (properties.bbnf), full `atRule` discriminant enum
**Corpus:** Browser compatibility CSS (can scrape production CSS with vendor prefixes)
**Oracle:** lightningcss property canonicalization + cssparser at-rule recognition
**Admission Row:** `css_l4/vendor_and_custom_atrules/direct_to_struct/main`
**Expected Delta:** +350–550 LOC
**vs lightningcss:** Codegen categorizes vendor prefixes and custom at-rules; lightningcss normalizes away vendor variants.
**Parity impact:** **MEDIUM** — Vendor prefixes are declining but still present; custom at-rules are extensible (future standards).
**Risk:** LOW-MEDIUM (dispatch is mechanical; oracle coverage straightforward).

### Wave 7: Source Mapping + Comment/Whitespace Facts (Diagnostic)

**Target:** Source position tracking, comment ranges, whitespace fact emission
**Productions:** Offset tracking in existing rules; new `commentFact`, `whitespaceFact` (if added to fact plane)
**Corpus:** Human-authored CSS, pretty-printed stylesheets
**Oracle:** lightningcss source positions + cssparser token offsets
**Admission Row:** `css_l4/source_and_diagnostics/direct_to_struct/main` (or advisory status)
**Expected Delta:** +390–570 LOC
**vs lightningcss:** Codegen tracks source offsets for debug/tooling; lightningcss stores source positions in AST.
**Parity impact:** **LOW** — Diagnostic; not needed for close admission unless source maps required.
**Risk:** LOW (offset tracking is straightforward; oracle is mechanical).

---

## Section 7: Non-CSS Fallback Status (USER PIN D1 Redress)

USER PIN D1 (CAMPAIGN-CLOSE section "Routed Remainder") states:

> Sheets and BBNF-self remain fallback history. CSS L4 satisfied the authoritative close target before fallbacks were needed.

**Current status:**

| Fallback | SK-V12 Role | SK-V13 Role |
|---|---|---|
| **Sheets (Google Sheets DSL)** | No admission row; preflight-failure fallback (demoted by D1) | **HISTORICAL ONLY** — CSS L4 is the authoritative target for SK-V13. Sheets may be re-opened ONLY if CSS L4 redress attempts are all blocked/rejected AND the candidate identifies a measured Sheets path that beats lightningcss on a non-CSS domain. (Out of scope for CSS expansion waves.) |
| **BBNF-self (BBNF grammar as a corpus)** | No admission row; fallback history | **HISTORICAL ONLY** — Same as Sheets. Not a CSS-domain candidate. |

**Implication for SK-V13:** CSS L4 is the first and primary target for all waves. Sheets and BBNF-self are NOT in scope unless a CSS L4 wave is measured impossible (BLOCKED or REJECTED by oracle/gate evidence) AND a candidate identifies a non-CSS alternative with higher priority.

**Recommended:** Assume CSS L4 expansion (Waves 1–6 above) is the full SK-V13 scope unless Wave 1 (stylesheet root) fails to close.

---

## Section 8: Summary and Recommended Next Steps

**CSS L4 Parity Status:**
- **Admitted (SK-V12):** 1 row (8 token kinds in declaration values)
- **Partial (grammar exists, codegen missing):** 7 feature families
- **Missing (grammar missing or incomplete):** 16 feature families
- **Out of scope for V13:** 6 features (container/scope/layer, error recovery, source mapping)

**SK-V13 Recommended Action:**

1. **Immediate (scoping):** Confirm Wave 1 (stylesheet + selectors) is tractable with current lock/grammar constraints. If Lock 14/16 issues arise, escalate before redress.

2. **Wave sequence:** Dispatch Waves in order 1 → 2 → 3 → 4 → 5 (6–7 optional, post-admission).

3. **Corpus strategy:** Vendor Bootstrap (180 KB) + Tailwind (400 KB) + Material Design (300 KB) + Animate.css (80 KB) into `skinny/corpora/css-l4-sk-v13/` as test fixtures.

4. **Oracle discipline:** Maintain lightningcss 1.0.0-alpha.71 as the SOTA anchor. Parallel cssparser oracle for token verification. Cross-check per-wave.

5. **Lock compliance:** Wave 1 (selectors) and Wave 2 (vars/calc) will stress Lock 14 (generated metadata). Plan GrammarConfig expansion or equivalent per-grammar fact plane before redress.

6. **Risk mitigation:** Wave 1 is HIGH risk due to selector complexity. Recommend pilot selector dispatch on W1b corpus before committing to full stylesheet grammar.

---

**End of scoping document.**
