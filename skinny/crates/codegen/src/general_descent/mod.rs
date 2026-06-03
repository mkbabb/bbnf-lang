//! The GENERAL recursive-descent emitter.
//!
//! This is the THIRD lowering path of the ONE generator: it walks a
//! [`SinkOnlyProgram`] that carries NO `value` rule (the shape the JSON
//! `json_sink_direct` emitter requires) and emits a recursive-descent
//! recognizer + a lazy tape-backed view. Unlike `json_sink_direct` (whose
//! dispatch is keyed to a single `value`-Alt) and `css_scan_direct` (a flat
//! balanced-delimiter sweep), this emitter is GENERAL over an arbitrary
//! `SinkOnlyExpr` tower: one `parse_<rule>` recognizer per rule, walking the
//! rule's expression node-by-node.
//!
//! It is grammar-NEUTRAL: every literal, dispatch key, and leaf first-byte set
//! is DERIVED from the program's `ByteLiteral`/`RegexProgram` nodes — no byte is
//! a literal in this source keyed to a grammar name.
//!
//! REGEX LEAVES: the runtime has no general regex matcher, and the `bbnf-regex`
//! HIR exposes only a coarse 4-variant classifier (no walkable char-class /
//! alternation / quantifier AST). So each `RegexProgram` leaf is matched by the
//! grammar-sanctioned DELIMITER-SCAN fallback: a continuation-class scan that
//! consumes the leaf span up to the next structural byte (the operators / parens
//! / comma / colon / braces derived from the program's own `ByteLiteral`s),
//! Span-projecting the leaf. The leaf's first byte must be in its derived first
//! set; the body is the maximal run of non-structural bytes. This is the
//! grammar's own TODO (typed leaves deferred to `Span`).

mod emit;
mod regex_first;
mod view;

use crate::lower::sink_only::SinkOnlyProgram;

pub(crate) use view::render_view;

/// Render the general recursive-descent `generated.rs` body for `program`. The
/// emitted view types carry the grammar-derived PascalCase prefix (so
/// `grammar_name = "google_sheets"` yields `GoogleSheetsDocument` etc.) — the
/// emitter is grammar-NEUTRAL, never a hard-coded type token.
pub(crate) fn render(program: &SinkOnlyProgram, grammar_name: &str) -> Result<String, String> {
    emit::render(program, &grammar_pascal(grammar_name))
}

/// Render a `&[u8]` byte-string literal (`b","`).
pub(crate) fn byte_string_literal(bytes: &[u8]) -> String {
    let mut out = String::from("b\"");
    for &byte in bytes {
        match byte {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            0x20..=0x7e => out.push(byte as char),
            other => out.push_str(&format!("\\x{other:02x}")),
        }
    }
    out.push('"');
    out
}

/// A Rust-identifier-safe form of a grammar rule name.
pub(crate) fn rule_ident(name: &str) -> String {
    name.chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect()
}

/// The PascalCase form of a rule name, for the `{Prefix}Kind` enum variants.
pub(crate) fn rule_pascal(name: &str) -> String {
    let mut out = String::new();
    let mut upper = true;
    for ch in name.chars() {
        if ch == '_' || !ch.is_ascii_alphanumeric() {
            upper = true;
            continue;
        }
        if upper {
            out.extend(ch.to_uppercase());
            upper = false;
        } else {
            out.push(ch);
        }
    }
    if out.is_empty() {
        out.push('R');
    }
    out
}

/// The PascalCase form of the full grammar name — the type prefix the general
/// emitter threads through every view type (`{Prefix}Document`/`{Prefix}Kind`/
/// `{Prefix}Node`). For `google_sheets` this yields `GoogleSheets`; the prefix is
/// DERIVED from the grammar name, never a hard-coded token. Shares
/// `rule_pascal`'s segment-join logic (snake/kebab segments capitalised).
pub(crate) fn grammar_pascal(name: &str) -> String {
    rule_pascal(name)
}
