//! AX.W1r.3a — leading-comment non-empty output probe.
//!
//! W1r.3's halt diagnostic (`docs/tranches/AX/audit/W1r3-diag.md`)
//! identified that `CssL4Parser::stylesheet_prettify()` returned an
//! empty byte-stream on any input whose first byte-run after `@ws`
//! was a `/* ... */` block comment. The root cause: the prettify
//! emitter's `OptionalWhitespace` node (`?w`) called ASCII-only
//! `trim_leading_whitespace_mut` regardless of the grammar's `@ws`
//! declaration, stalling the parse at the opening `/` and producing
//! zero ops.
//!
//! AX.W1r.3a fixed this by threading `ir.ws_pattern` through
//! `emit_prettify_optional_ws_impl` and emitting the compiled
//! `@ws` regex when declared. This probe asserts the fix stays
//! landed — any regression to ASCII-only trim would re-break
//! bootstrap.css / normalize.css / tailwind.css.

// Host function `parse_hex_color` referenced by
// `grammar/css/l4/color.bbnf` lives in `bbnf::css_types` (canonical
// shim per REAUDIT-2026-04-30 lane 3 §5.1).
#[allow(unused_imports)]
use bbnf::css_types as _css_types_link;

use ::bbnf::grammar::generated::css_l4::*;

fn prettify(src: &str) -> String {
    let cfg = pprint::Printer::new(80, 2, false);
    CssL4Parser::stylesheet_prettify()
        .parse(src)
        .map(|ops| pprint::render(&ops, cfg))
        .unwrap_or_default()
}

#[test]
fn leading_block_comment_non_empty() {
    let out = prettify("/* c */ a { color: red; }");
    assert!(!out.is_empty(), "leading comment produced empty output");
    assert!(
        out.contains("a") && out.contains("color"),
        "leading-comment output missing rule content: {out:?}"
    );
}

#[test]
fn leading_banner_comment_non_empty() {
    let out = prettify("/*! banner */\na { color: red; }\nb { color: blue; }");
    assert!(!out.is_empty(), "banner comment produced empty output");
    assert!(
        out.contains("a") && out.contains("b"),
        "banner-comment output missing rule content: {out:?}"
    );
}

#[test]
fn inline_comment_between_rules_non_empty() {
    let out = prettify("a { color: red; /* inline */ }");
    assert!(!out.is_empty(), "inline comment produced empty output");
    assert!(
        out.contains("color"),
        "inline-comment output missing declaration: {out:?}"
    );
}
