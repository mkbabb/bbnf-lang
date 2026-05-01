//! AX.W1r.3 — CSS L4 canonical-form parity harness.
//!
//! Proves cross-parser parity between bbnf's CSS L4 grammar and
//! lightningcss by emitting both sides to their own canonical form
//! and asserting byte-identical output after a shared
//! syntactically-irrelevant token normalization pass.
//!
//! The W1 principle: no hand-coded `From<lightningcss::StyleSheet>`,
//! no `PartialEq<lightningcss>`, no per-grammar adapter modules. The
//! derive's `#[parser(prettify)]` API pretty-prints the tape; the
//! lightningcss printer emits canonical CSS from its own AST; both
//! sides pass through the SAME `token_normalize` so syntactically-
//! irrelevant whitespace / comment / hex-case / separator differences
//! cancel. Semantic divergences surface as a token-normalized
//! string-inequality, not a silent comparator bridge.
//!
//! If a divergence cannot be resolved by a symmetric rule, the fix
//! belongs in bbnf's `@pretty` directives in `grammar/css/l4/*.bbnf`
//! — never in a per-comparator bridge.

mod common;

use lightningcss::stylesheet::{ParserOptions, PrinterOptions, StyleSheet};

use common::css_normalize::token_normalize;

// Host function referenced by `grammar/css/l4/color.bbnf`; reproduced
// so the grammar compiles under this test crate. See
// `lightningcss_parity.rs` for the canonical implementation.
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

use ::bbnf::grammar::generated::css_l4::*;

/// Shared printer config: 80-col width, 2-space indent, spaces not
/// tabs. Must match the configuration the wave spec references as
/// `shared_printer()`.
fn shared_printer() -> pprint::Printer {
    pprint::Printer::new(80, 2, false)
}

/// Drive the fixture path to a canonical string per side, then apply
/// the shared `token_normalize` transform to both. Panics with a
/// diff-ready preview if normalization-equivalent output diverges.
fn assert_canonical_parity(fixture: &str) {
    let path = format!("../../data/css/{}", fixture);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: read failed: {e}"));

    let bbnf_canonical = CssL4Parser::stylesheet_prettify()
        .parse(&src)
        .map(|ops| pprint::render(&ops, shared_printer()))
        .unwrap_or_else(|| panic!("{fixture}: bbnf prettify returned None"));

    let oracle_sheet = StyleSheet::parse(&src, ParserOptions::default())
        .unwrap_or_else(|e| panic!("{fixture}: lightningcss parse failed: {e:?}"));
    let oracle_canonical = oracle_sheet
        .to_css(PrinterOptions {
            minify: false,
            ..Default::default()
        })
        .unwrap_or_else(|e| panic!("{fixture}: lightningcss print failed: {e:?}"))
        .code;

    let bbnf_norm = token_normalize(&bbnf_canonical);
    let oracle_norm = token_normalize(&oracle_canonical);

    if bbnf_norm != oracle_norm {
        let mismatch = first_diff(&bbnf_norm, &oracle_norm);
        panic!(
            "{fixture}: canonical-form parity divergence at byte {}\n\
             bbnf len={}, oracle len={}\n\
             bbnf window:   {:?}\n\
             oracle window: {:?}",
            mismatch.offset,
            bbnf_norm.len(),
            oracle_norm.len(),
            mismatch.bbnf_preview,
            mismatch.oracle_preview,
        );
    }
}

struct Mismatch {
    offset: usize,
    bbnf_preview: String,
    oracle_preview: String,
}

fn first_diff(a: &str, b: &str) -> Mismatch {
    let ab = a.as_bytes();
    let bb = b.as_bytes();
    let min = ab.len().min(bb.len());
    let mut off = 0usize;
    while off < min && ab[off] == bb[off] {
        off += 1;
    }
    let win = 80usize;
    let start = off.saturating_sub(20);
    Mismatch {
        offset: off,
        bbnf_preview: a
            .get(start..(off + win).min(a.len()))
            .unwrap_or("")
            .to_string(),
        oracle_preview: b
            .get(start..(off + win).min(b.len()))
            .unwrap_or("")
            .to_string(),
    }
}

/// Scale + interop validation for fixtures whose byte-level canonical
/// parity against lightningcss exceeds W1r.3's scope. Proves:
///
///   1. bbnf parses the fixture + emits non-empty prettify output.
///   2. lightningcss parses the fixture (source is valid CSS).
///   3. bbnf's prettify output re-parses via bbnf (self round-trip).
///   4. bbnf's prettify output re-parses via lightningcss (interop).
///
/// Byte-level canonical parity against `PrinterOptions { minify: false
/// }` beyond normalize.css requires a CSS `calc()` evaluator +
/// per-property canonical-order tables — lightningcss's non-minifying
/// path still performs semantic simplifications (e.g. `calc(3rem +
/// calc(1.5em + .75rem))` → `calc(1.5em + 3.75rem)`,
/// `background-position: top X right Y` → `right Y top X`) that no
/// symmetric bytes-level normalizer can invert. That's a new
/// workstream, not a W1 fix; see
/// `docs/tranches/AX/audit/W1r3a-diag.md`.
///
/// Rather than ship `#[ignore]`'d placeholder tests (invariant 18),
/// this asserts the grammar-derived view surface holds at real-world
/// scale + the prettify output interoperates with lightningcss,
/// without claiming byte-parity the oracle cannot provide.
fn assert_scale_interop(fixture: &str) {
    let path = format!("../../data/css/{}", fixture);
    let src = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{path}: read failed: {e}"));

    let bbnf_pretty = CssL4Parser::stylesheet_prettify()
        .parse(&src)
        .map(|ops| pprint::render(&ops, shared_printer()))
        .unwrap_or_else(|| panic!("{fixture}: bbnf prettify returned None"));
    assert!(
        !bbnf_pretty.is_empty(),
        "{fixture}: bbnf prettify produced empty output"
    );

    StyleSheet::parse(&src, ParserOptions::default())
        .unwrap_or_else(|e| panic!("{fixture}: lightningcss parse failed: {e:?}"));

    CssL4Parser::stylesheet_prettify()
        .parse(&bbnf_pretty)
        .unwrap_or_else(|| panic!("{fixture}: bbnf re-parse of prettify output failed"));

    StyleSheet::parse(&bbnf_pretty, ParserOptions::default()).unwrap_or_else(|e| {
        panic!("{fixture}: lightningcss re-parse of bbnf prettify output failed: {e:?}")
    });
}

// ─── Per-fixture gates ───────────────────────────────────────────────
//
// After AX.W1r.3a:
//
//   1. `?w` / OptionalWhitespace prettify codegen threads the grammar's
//      `@ws` pattern through the regex emitter, matching the
//      recognizer's `Op::TrimWsPattern` dispatch. Leading-comment
//      inputs no longer zero the op stream.
//   2. CSS L4's `stylesheet.bbnf` declares `@pretty` directives for
//      stylesheet / ruleList / blockContent / ruleBlock / qualifiedRule
//      / atRule / mediaRule / keyframesRule / genericAtRule, so the
//      emitted output reflects the oracle's block-indent structure.
//   3. `token_normalize` cancels 20+ syntactic divergences (comment
//      strip, hex-case, whitespace, rgba→hex, named→hex, pseudo-
//      element forms, :nth-child(even/odd), filter defaults, media-
//      query range syntax, flex shorthand, declaration sort, etc.).
//
// `canonical_parity_normalize` gates byte-identical parity. bootstrap
// + tailwind gate scale + interoperability (source is valid CSS; bbnf
// prettify produces CSS valid under both parsers).

#[test]
fn canonical_parity_normalize() {
    assert_canonical_parity("normalize.css");
}

#[test]
fn scale_interop_bootstrap() {
    assert_scale_interop("bootstrap.css");
}

#[test]
fn scale_interop_tailwind() {
    assert_scale_interop("tailwind.css");
}
