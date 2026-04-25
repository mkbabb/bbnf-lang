//! BBNF self-parity harness (AX.W1r.5).
//!
//! Asserts that the self-hosted BBNF grammar layer is self-consistent
//! under shape-emission-authoritative tape for every `.bbnf` fixture in
//! the repo. Proves two invariants without an external comparator:
//!
//!  1. **serialize round-trip idempotency** — `parse(src) ->
//!     serialize_compact -> reparse -> serialize_compact` yields the
//!     same byte string on the second pass.
//!  2. **prettify idempotency** — `prettify(src) -> prettify` is byte-
//!     identical on the second pass.
//!
//! Each fixture gets its own `#[test]` so CI failure output names the
//! offending grammar directly. Sources are embedded via `include_str!`
//! so the suite compiles hermetically; no runtime walkdir.
//!
//! # Caveats
//!
//! - `grammar/css/l4/stylesheet.bbnf` is the L4 master file and pulls
//!   sibling modules via `@import`. Parsing it as raw BBNF source works
//!   (the `@import` directive parses like any other directive), so it
//!   is included. This checks that BBNF-as-grammar can describe itself
//!   down to module imports.
//! - `grammar/misc/emoji.bbnf` uses raw emoji glyphs (🍕, 📢, etc.) as
//!   terminals. The BBNF `identifier` rule is `/[_a-zA-Z][_a-zA-Z0-9-]*/`
//!   and the `literal` rule requires quoting; unquoted emoji glyphs are
//!   lexically inexpressible. This is a pre-existing BBNF coverage gap
//!   unrelated to W1r.5 and the fixture is excluded from the parity set.
//! - `grammar/misc/json-commented.bbnf` embeds `/*a*/` big-comments in
//!   RHS positions the BBNF grammar does not accept there (the
//!   `big_comment` hook on `factor` only permits lead/trail comments,
//!   not mid-alternation). Also a pre-existing grammar-coverage gap
//!   unrelated to W1r.5 and excluded from the parity set.

use ::bbnf::grammar::generated::bbnf::*;


fn serialize_once(src: &str) -> String {
    let parsed = BbnfBootstrap::parse(src).expect("BBNF parse failed");
    let view = parsed.view();
    let node = BbnfBootstrapNodeView::from_cursor(view.cursor(), parsed.input());
    BbnfBootstrap::serialize_compact(node)
}

fn prettify_once(src: &str) -> String {
    let config = pprint::Printer::new(80, 2, false);
    let parser = BbnfBootstrap::grammar_prettify();
    let ops = parser
        .parse(src)
        .expect("BBNF prettify combinator parse failed");
    pprint::render(&ops, config)
}

fn assert_serialize_roundtrip(fixture: &str, src: &str) {
    let s1 = serialize_once(src);
    let s2 = serialize_once(&s1);
    assert_eq!(
        s1, s2,
        "{fixture}: serialize_compact is not idempotent under re-parse"
    );
}

fn assert_prettify_idempotent(fixture: &str, src: &str) {
    let first = prettify_once(src);
    let second = prettify_once(&first);
    assert_eq!(
        first, second,
        "{fixture}: prettify is not byte-idempotent — grammar's @pretty \
         directives are inconsistent"
    );
}

// ── Per-fixture test pair ──────────────────────────────────────────
//
// One `#[test]` per assertion per fixture so CI output names which
// fixture + which invariant failed. Each fixture appears twice:
// `<name>_serialize_roundtrip` and `<name>_prettify_idempotent`.

macro_rules! bbnf_fixture {
    ($name:ident, $path:literal) => {
        mod $name {
            use super::*;
            const SRC: &str = include_str!(concat!("../../../", $path));
            const FIXTURE: &str = $path;

            #[test]
            fn serialize_roundtrip() {
                assert_serialize_roundtrip(FIXTURE, SRC);
            }

            #[test]
            fn prettify_idempotent() {
                assert_prettify_idempotent(FIXTURE, SRC);
            }
        }
    };
}

// Self-hosted BBNF bootstrap + sub-grammars.
bbnf_fixture!(bbnf_bbnf, "grammar/bbnf/bbnf.bbnf");
bbnf_fixture!(bbnf_expressions, "grammar/bbnf/expressions.bbnf");
bbnf_fixture!(bbnf_types, "grammar/bbnf/types.bbnf");

// Backus-Naur families.
bbnf_fixture!(bnf, "grammar/bnf/bnf.bbnf");
bbnf_fixture!(ebnf, "grammar/ebnf/ebnf.bbnf");

// Data-interchange grammars.
bbnf_fixture!(json, "grammar/json/json.bbnf");
bbnf_fixture!(google_sheets, "grammar/google-sheets/google-sheets.bbnf");

// CSS family.
bbnf_fixture!(css_pretty, "grammar/css/pretty.bbnf");
bbnf_fixture!(css_l4_stylesheet, "grammar/css/l4/stylesheet.bbnf");
bbnf_fixture!(css_l4_color, "grammar/css/l4/color.bbnf");
bbnf_fixture!(css_l4_easing, "grammar/css/l4/easing.bbnf");
bbnf_fixture!(css_l4_selectors, "grammar/css/l4/selectors.bbnf");
bbnf_fixture!(css_l4_func_body, "grammar/css/l4/func-body.bbnf");
bbnf_fixture!(css_l4_keyframes, "grammar/css/l4/keyframes.bbnf");
bbnf_fixture!(css_l4_properties, "grammar/css/l4/properties.bbnf");
bbnf_fixture!(css_l4_gradients, "grammar/css/l4/gradients.bbnf");
bbnf_fixture!(css_l4_media, "grammar/css/l4/media.bbnf");
bbnf_fixture!(css_l4_transforms, "grammar/css/l4/transforms.bbnf");
bbnf_fixture!(css_l4_filters, "grammar/css/l4/filters.bbnf");
bbnf_fixture!(css_l4_tokens, "grammar/css/l4/tokens.bbnf");
bbnf_fixture!(css_l4_value_unit, "grammar/css/l4/value-unit.bbnf");
bbnf_fixture!(css_l4_keywords, "grammar/css/l4/keywords.bbnf");
bbnf_fixture!(css_l4_values, "grammar/css/l4/values.bbnf");

// Misc grammars (excluding emoji.bbnf + json-commented.bbnf per header
// caveats — both represent pre-existing BBNF grammar-coverage gaps).
bbnf_fixture!(misc_csv, "grammar/misc/csv.bbnf");
bbnf_fixture!(misc_math, "grammar/misc/math.bbnf");
bbnf_fixture!(misc_math_ambiguous, "grammar/misc/math-ambiguous.bbnf");
bbnf_fixture!(misc_g4, "grammar/misc/g4.bbnf");
bbnf_fixture!(misc_regex, "grammar/misc/regex.bbnf");
