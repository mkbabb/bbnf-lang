//! AX.W0a.2.s — assert `CssL4Parser::parse` consumes the three real-
//! world CSS corpora end-to-end (bootstrap.css, normalize.css,
//! tailwind.css). These are the exact inputs the `css_l4` bench
//! binary loads; if they don't parse, the bench binary panics at
//! startup.
//!
//! Pre-AX.W0a.2.s these panicked at the support module's `skip_space`
//! helper: it only advanced past ASCII whitespace, never through the
//! `/* ... */` block comments the `@ws` directive admits. The fix
//! emits a comment-aware `skip_space` when the grammar's `@ws`
//! pattern classifies as `RegexClass::WhitespaceWithBlockComment`.

#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(_: &str) -> u32 {
        0
    }
}

use ::bbnf::grammar::generated::css_l4::*;


fn assert_parses_full(label: &str, path: &str) {
    let input = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("read {path}: {e}"));
    let size = input.len();
    match CssL4Parser::parse(&input) {
        Ok(doc) => {
            // AZ-I.W2-act.close B3 — the struct-direct CSS L4 path
            // returns a `CssDocument` directly. The post-substrate
            // structural metric is the top-level rule-list length plus
            // the declaration count discovered via `walk_declarations`,
            // matching the pre-substrate tape-record count's diagnostic
            // role: a non-zero number indicates the parse produced a
            // non-empty typed graph.
            let rule_count = doc.rules(doc.root().rules).len();
            let decl_count = doc.walk_declarations().count();
            eprintln!(
                "{label}: {size} bytes -> rules={rule_count} decls={decl_count}"
            );
        }
        Err(e) => panic!(
            "{label} ({size} bytes) failed: {e:?} — \
             the shape-dispatcher support module's skip_space did not \
             consume a leading or inter-rule `/* ... */` block comment."
        ),
    }
}

#[test]
fn bootstrap_full_parse() {
    assert_parses_full("bootstrap.css", "../../data/css/bootstrap.css");
}

#[test]
fn normalize_full_parse() {
    assert_parses_full("normalize.css", "../../data/css/normalize.css");
}

#[test]
fn tailwind_full_parse() {
    assert_parses_full("tailwind.css", "../../data/css/tailwind.css");
}
