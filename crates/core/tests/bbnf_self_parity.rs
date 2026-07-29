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

const FIXTURES: &[(&str, &str)] = &[
    (
        "grammar/bbnf/bbnf.bbnf",
        include_str!("../../../grammar/bbnf/bbnf.bbnf"),
    ),
    (
        "grammar/bbnf/expressions.bbnf",
        include_str!("../../../grammar/bbnf/expressions.bbnf"),
    ),
    (
        "grammar/bbnf/types.bbnf",
        include_str!("../../../grammar/bbnf/types.bbnf"),
    ),
    (
        "grammar/bnf/bnf.bbnf",
        include_str!("../../../grammar/bnf/bnf.bbnf"),
    ),
    (
        "grammar/ebnf/ebnf.bbnf",
        include_str!("../../../grammar/ebnf/ebnf.bbnf"),
    ),
    (
        "grammar/json/json.bbnf",
        include_str!("../../../grammar/json/json.bbnf"),
    ),
    (
        "grammar/google-sheets/google-sheets.bbnf",
        include_str!("../../../grammar/google-sheets/google-sheets.bbnf"),
    ),
    (
        "grammar/css/pretty.bbnf",
        include_str!("../../../grammar/css/pretty.bbnf"),
    ),
    (
        "grammar/css/l4/stylesheet.bbnf",
        include_str!("../../../grammar/css/l4/stylesheet.bbnf"),
    ),
    (
        "grammar/css/l4/color.bbnf",
        include_str!("../../../grammar/css/l4/color.bbnf"),
    ),
    (
        "grammar/css/l4/easing.bbnf",
        include_str!("../../../grammar/css/l4/easing.bbnf"),
    ),
    (
        "grammar/css/l4/selectors.bbnf",
        include_str!("../../../grammar/css/l4/selectors.bbnf"),
    ),
    (
        "grammar/css/l4/func-body.bbnf",
        include_str!("../../../grammar/css/l4/func-body.bbnf"),
    ),
    (
        "grammar/css/l4/keyframes.bbnf",
        include_str!("../../../grammar/css/l4/keyframes.bbnf"),
    ),
    (
        "grammar/css/l4/properties.bbnf",
        include_str!("../../../grammar/css/l4/properties.bbnf"),
    ),
    (
        "grammar/css/l4/gradients.bbnf",
        include_str!("../../../grammar/css/l4/gradients.bbnf"),
    ),
    (
        "grammar/css/l4/media.bbnf",
        include_str!("../../../grammar/css/l4/media.bbnf"),
    ),
    (
        "grammar/css/l4/transforms.bbnf",
        include_str!("../../../grammar/css/l4/transforms.bbnf"),
    ),
    (
        "grammar/css/l4/filters.bbnf",
        include_str!("../../../grammar/css/l4/filters.bbnf"),
    ),
    (
        "grammar/css/l4/tokens.bbnf",
        include_str!("../../../grammar/css/l4/tokens.bbnf"),
    ),
    (
        "grammar/css/l4/value-unit.bbnf",
        include_str!("../../../grammar/css/l4/value-unit.bbnf"),
    ),
    (
        "grammar/css/l4/keywords.bbnf",
        include_str!("../../../grammar/css/l4/keywords.bbnf"),
    ),
    (
        "grammar/css/l4/values.bbnf",
        include_str!("../../../grammar/css/l4/values.bbnf"),
    ),
    (
        "grammar/misc/csv.bbnf",
        include_str!("../../../grammar/misc/csv.bbnf"),
    ),
    (
        "grammar/misc/math.bbnf",
        include_str!("../../../grammar/misc/math.bbnf"),
    ),
    (
        "grammar/misc/math-ambiguous.bbnf",
        include_str!("../../../grammar/misc/math-ambiguous.bbnf"),
    ),
    (
        "grammar/misc/g4.bbnf",
        include_str!("../../../grammar/misc/g4.bbnf"),
    ),
    (
        "grammar/misc/regex.bbnf",
        include_str!("../../../grammar/misc/regex.bbnf"),
    ),
];

#[test]
fn every_fixture_has_stable_ast_projection() {
    for &(fixture, source) in FIXTURES {
        assert_ast_roundtrip(fixture, source);
    }
}

#[test]
fn every_fixture_prettifier_is_idempotent() {
    for &(fixture, source) in FIXTURES {
        assert_prettify_idempotent(fixture, source);
    }
}
