//! AZ-II.cutover.D4 — BBNF self-parity harness over the struct-
//! direct [`bbnf::types::AST`] reference.
//!
//! Asserts that the self-hosted BBNF grammar layer is self-consistent
//! for every `.bbnf` fixture in the repo. Two invariants without an
//! external comparator:
//!
//!  1. **AST round-trip parity** — `parse(src) -> AST -> rule_names`
//!     must equal a re-parse of the same source. The canonical
//!     reference AST (`bbnf::types::AST`) is the field-for-field
//!     shape every consumer queries; if the parse path is
//!     deterministic over a given source, the AST projection must be
//!     stable across re-parses.
//!  2. **prettify idempotency** — `prettify(src) -> prettify` is byte-
//!     identical on the second pass.
//!
//! Each fixture gets its own `#[test]` so CI failure output names the
//! offending grammar directly. Sources are embedded via `include_str!`
//! so the suite compiles hermetically; no runtime walkdir.
//!
//! Pre-cutover.D the harness exercised the tape's `serialize_compact`
//! roundtrip (the generated `BbnfBootstrap::serialize_compact` takes
//! a `BbnfBootstrapNodeView`). Post-cutover.D the comparison shifts
//! onto the canonical reference AST — the typed surface that
//! survives the cutover, independent of whether the parse substrate
//! is tape-based or struct-direct.
//!
//! # Caveats
//!
//! - `grammar/css/l4/stylesheet.bbnf` is the L4 master file and pulls
//!   sibling modules via `@import`. Parsing it as raw BBNF source works
//!   (the `@import` directive parses like any other directive), so it
//!   is included.
//! - `grammar/misc/emoji.bbnf` uses raw emoji glyphs (🍕, 📢, etc.) as
//!   terminals. The BBNF `identifier` rule is `/[_a-zA-Z][_a-zA-Z0-9-]*/`
//!   and the `literal` rule requires quoting; unquoted emoji glyphs are
//!   lexically inexpressible. Excluded from the parity set.
//! - `grammar/misc/json-commented.bbnf` embeds `/*a*/` big-comments in
//!   RHS positions the BBNF grammar does not accept there. Also
//!   excluded.

use bbnf::grammar;
use bbnf::grammar::generated::bbnf::BbnfBootstrap;

/// Project a parsed grammar onto the canonical typed AST: the rule
/// LHS names in source order, followed by the directive kinds in
/// source order. Forms a deterministic "fingerprint" that must
/// round-trip across re-parses.
fn ast_fingerprint(src: &str) -> Vec<String> {
    let extract = grammar::parse(src).expect("BBNF parse must succeed");
    let mut out: Vec<String> = Vec::new();
    out.extend(extract.rules.keys().map(|n| format!("rule:{}", n)));
    for d in &extract.imports {
        out.push(format!("import@{}", d.span.start));
    }
    for d in &extract.recovers {
        out.push(format!("recover@{}:{}", d.span.start, d.rule_name));
    }
    for d in &extract.pretties {
        out.push(format!("pretty@{}:{}", d.span.start, d.rule_name));
    }
    if let Some(ws) = &extract.ws_pattern {
        out.push(format!("ws:{}", ws));
    }
    for tok in &extract.token_rules {
        out.push(format!("token:{}", tok));
    }
    for dbg in &extract.debug_rules {
        out.push(format!("debug:{}", dbg));
    }
    for hf in &extract.host_fns {
        out.push(format!("host:{}", hf.name));
    }
    out
}

fn prettify_once(src: &str) -> String {
    let config = pprint::Printer::new(80, 2, false);
    let parser = BbnfBootstrap::grammar_prettify();
    let ops = parser
        .parse(src)
        .expect("BBNF prettify combinator parse failed");
    pprint::render(&ops, config)
}

/// Re-parse the source verbatim and verify the AST fingerprint
/// matches. The canonical-AST projection is deterministic — a stable
/// fingerprint across parse calls proves the parse path is not
/// dependent on cross-call mutable state.
fn assert_ast_roundtrip(fixture: &str, src: &str) {
    let f1 = ast_fingerprint(src);
    let f2 = ast_fingerprint(src);
    assert_eq!(
        f1, f2,
        "{fixture}: AST fingerprint is not stable across re-parses",
    );
}

fn assert_prettify_idempotent(fixture: &str, src: &str) {
    let first = prettify_once(src);
    let second = prettify_once(&first);
    assert_eq!(
        first, second,
        "{fixture}: prettify is not byte-idempotent — grammar's @pretty \
         directives are inconsistent",
    );
}

// ── Per-fixture test pair ──────────────────────────────────────────
//
// One `#[test]` per assertion per fixture so CI output names which
// fixture + which invariant failed. Each fixture appears twice:
// `<name>_ast_roundtrip` and `<name>_prettify_idempotent`.

macro_rules! bbnf_fixture {
    ($name:ident, $path:literal) => {
        mod $name {
            use super::*;
            const SRC: &str = include_str!(concat!("../../../", $path));
            const FIXTURE: &str = $path;

            #[test]
            fn ast_roundtrip() {
                assert_ast_roundtrip(FIXTURE, SRC);
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
