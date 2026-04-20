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

use bbnf_derive::Parser;

// Host function referenced by `grammar/css/l4/color.bbnf`.
#[allow(dead_code)]
mod css_types {
    pub fn parse_hex_color(s: &str) -> u32 {
        let hex = s.as_bytes();
        match hex.len() {
            3 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | 0xFF
            }
            4 => {
                let r = hex_digit(hex[0]);
                let g = hex_digit(hex[1]);
                let b = hex_digit(hex[2]);
                let a = hex_digit(hex[3]);
                ((r << 4 | r) << 24) | ((g << 4 | g) << 16) | ((b << 4 | b) << 8) | (a << 4 | a)
            }
            6 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                (r << 24) | (g << 16) | (b << 8) | 0xFF
            }
            8 => {
                let r = hex_byte(hex[0], hex[1]);
                let g = hex_byte(hex[2], hex[3]);
                let b = hex_byte(hex[4], hex[5]);
                let a = hex_byte(hex[6], hex[7]);
                (r << 24) | (g << 16) | (b << 8) | a
            }
            _ => 0,
        }
    }
    #[inline(always)]
    fn hex_digit(b: u8) -> u32 {
        match b {
            b'0'..=b'9' => (b - b'0') as u32,
            b'a'..=b'f' => (b - b'a' + 10) as u32,
            b'A'..=b'F' => (b - b'A' + 10) as u32,
            _ => 0,
        }
    }
    #[inline(always)]
    fn hex_byte(hi: u8, lo: u8) -> u32 {
        (hex_digit(hi) << 4) | hex_digit(lo)
    }
}

#[derive(Parser)]
#[parser(path = "../../grammar/css/l4/stylesheet.bbnf", prettify, skip_recover)]
struct CssL4Parser;

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
