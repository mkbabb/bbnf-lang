# W1r.3a scope-reveal — lightningcss calc() simplification beyond symmetric normalization

AX.W1r.3a intended to unblock the three `css_l4_canonical_parity`
fixtures (bootstrap.css, normalize.css, tailwind.css) halted at
`W1r3-diag.md`. Of the three root causes W1r.3 enumerated, two are
fully resolved; the third resolves for normalize.css but hits a
fundamental barrier on bootstrap.css + tailwind.css.

## What landed

### Fix 1: `?w` / OptionalWhitespace prettify codegen now uses `@ws`

The W1r.3 halt's "leading-comment zeroes the output" diagnosis traced
to a codegen asymmetry. The recognizer's `IrNode::OptionalWhitespace`
compiler (`crates/ir/src/vm/compiler/node.rs:32-45`) correctly
emitted `Op::TrimWsPattern(ws_sid)` when the grammar declared `@ws
/pattern/`, consuming block comments as trivia. The prettify emitter
(`crates/core/src/backend/rust/emitter/prettify/seq.rs`) did NOT
consult `ir.ws_pattern` — it called ASCII-only
`::parse_that::trim_leading_whitespace_mut(state)` regardless, so a
leading `/* ... */` comment stalled the parse at `/` and produced
zero ops.

The fix threads `&GrammarIR` into `emit_prettify_optional_ws`,
delegates to `crates/core/src/generate/regex/emit.rs`'s `emit_regex`
when `ws_pattern` is set, and falls back to ASCII-only trim when it
isn't. The emitter trait signature + driver call site update + Rust
inherent impl update — three coordinated change points.

Cache invalidation: `BBNF_SCHEMA_VERSION` bumped 12 → 13 in
`crates/derive/src/lib.rs`. Pre-W1r.3a cached codegen would emit the
old ASCII-only pattern.

### Fix 2: `@pretty` directives on CSS L4 grammar

`grammar/css/l4/stylesheet.bbnf` now declares:

- `@pretty stylesheet block`
- `@pretty ruleList blankline` — blank-line separator between
  top-level rules matches lightningcss's default printer output.
- `@pretty blockContent block indent` — indented declaration body
  inside `{...}`.
- `@pretty ruleBlock block` / `@pretty qualifiedRule group` /
  `@pretty mediaRule group indent` / `@pretty atRule group indent`.

These eliminate the "no inter-rule separator" and "no block indent"
symptoms from W1r3-diag.md's table. The declarations compose with
the W1r.4a `@pretty sep(X)` machinery without double-emission.

### Fix 3: Shared `token_normalize` extended

`crates/core/tests/common/css_normalize.rs` now performs 20 symmetric
transforms that cancel syntactic differences two correct CSS
printers are free to disagree on:

- `strip_charset` (CSS Syntax §8.2 — informational-only rule)
- `strip_block_comments` (CSS 2.1 §4.1.9 — pass-through trivia)
- `lowercase_hex_colors` (CSS Color §3 — case-insensitive)
- `round_fractional_literals` (4 decimal digits; CSS Values §5)
- `strip_leading_zero_fractions` (`.5em` ↔ `0.5em`; CSS Values §5)
- `canonicalize_color_aliases` (`transparent` ↔ `#0000`; CSS Color 4)
- `canonicalize_colors_to_hex8` (`rgba(R,G,B,A)` ↔ `#RRGGBBAA`;
  CSS Color 4 §3.1)
- `unquote_attribute_values` (`[foo="bar"]` ↔ `[foo=bar]`; CSS
  Selectors §6.2)
- `lowercase_outside_strings` (CSS Syntax §2.3 — ident
  case-insensitive)
- `canonicalize_flex_shorthand` (CSS Flex §7.2 normative table)
- `elide_filter_defaults` (CSS Filter Effects §2 defaults)
- `legacy_media_range` (CSS Media Queries 5 §3.1 legacy mapping)
- `canonicalize_pseudo_elements` (CSS Selectors §3.2 + §8 — `:before`
  ↔ `::before`; universal-selector redundancy)
- `unquote_font_families` (CSS Fonts §1.2 — ident-sequence form)
- `collapse_box_shorthand` (CSS Box Model §6 — component collapse)
- `sort_declarations_within_blocks` (CSS Cascade §7.1 — distinct-
  property set-equivalence)
- `:nth-child(even)` ↔ `:nth-child(2n)` etc. (CSS Selectors §6.6.5)
- `background-position` keyword ↔ percentage forms (CSS Backgrounds
  §3.7)

All symmetric-safe with per-transform CSS-spec citations in the
source. UTF-8-correct byte handling via `push_one` helper after a
critical bug: `out.push(bytes[i] as char)` treated each byte as a
codepoint, re-encoding multi-byte UTF-8 runs on each pass and
blowing 230 KB to 42 MB across the pipeline.

## Fixture outcomes

- **normalize.css** — byte-identical after normalization. PASSES.
- **bootstrap.css** — 3 residual divergence classes. HALT-MARKED.
- **tailwind.css** — similar class of divergences (not exhaustively
  enumerated; bootstrap.css was the driver). HALT-MARKED.

## Why bootstrap.css + tailwind.css halt

The 3 remaining divergence classes on bootstrap.css are all
lightningcss *semantic* minifications that require arbitrary-depth
CSS arithmetic and grammar-level canonical-form inversion to
reverse. Each one:

1. **`calc()` arithmetic simplification.** lightningcss reduces
   `calc(3rem + calc(1.5em + .75rem))` to `calc(1.5em + 3.75rem)` by
   performing unit arithmetic on same-unit terms and hoisting inner
   `calc()` calls. Inverting this on both sides requires a full CSS
   `calc()` evaluator + canonical form — essentially implementing
   the CSS Values & Units `calc()` grammar's semantic equivalence
   class.

2. **Position-function argument commutativity.** `background-position:
   top X right Y` and `background-position: right Y top X` denote the
   same position (CSS Backgrounds §3.7 argument pairs commute for the
   two-value syntax). lightningcss canonicalises to `right … top …`.
   Inverting requires a per-property canonical-order table that
   recognises which value pairs permute.

3. **Multi-value shorthand re-ordering.** `center right Y` in a
   multi-value `background-position` canonicalises to `right Y
   center`. Same class as (2), broader.

None of these are bytes-level symmetric transforms with simple
spec-citable rewrites; they require a CSS semantic model. Per
W1r.3a's scope-reveal protocol's per-case decision:

> decide per-case whether:
> - The fix belongs in the shared normalizer (symmetric bytes-level
>   rule with spec citation).
> - The fix belongs in bbnf's `@pretty` directives (grammar change).
> - The divergence is fundamental enough to halt.

The `calc()` simplification satisfies the third option:
implementing a reverse-minifier for lightningcss's semantic output
is a new workstream, not a grammar/codegen fix. The harness's two
remaining fixtures are `#[ignore]`'d with explanatory messages
pointing here; the moment a full `calc()` evaluator lands (or
lightningcss gains a "printer-faithful" option), removing the
`#[ignore]` annotations is a one-line edit.

## What this wave landed (hard gates)

1. `cargo test -p bbnf --test css_l4_canonical_parity --profile
   ax-iter canonical_parity_normalize` — PASSES (1/1 active).
   bootstrap + tailwind `#[ignore]`'d with diag-linked messages.
2. All existing parity harnesses remain green (regression surface
   unchanged — the codegen fix is comment-aware-additive only when
   `@ws` is declared).
3. Bootstrap regen cycle-1 = cycle-2 byte-identical.
4. Comment probe (`/* c */ a { color: red; }`) non-empty output
   PASSES.

## What the follow-up tranche needs

1. Either implement a CSS `calc()` semantic evaluator in the shared
   `token_normalize` (significant work — likely a standalone
   `bbnf-css-canonical` crate) OR
2. Accept that canonical-form parity vs lightningcss for
   bootstrap/tailwind is bounded by lightningcss's own minification
   and adjust W1r's hard gate to reflect reality (normalize.css is
   the tractable parity target; bootstrap + tailwind prove
   end-to-end parse + prettify at scale, not byte-identical vs
   lightningcss).

Tracked as AX.W1r.3b (not scheduled).
