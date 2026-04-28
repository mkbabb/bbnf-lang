//! Serialize round-trip tests — ALL grammars must compile and round-trip.
//!
//! Each test verifies idempotence: parse → serialize → reparse →
//! serialize → assert eq. Post-AC.2 the parse call returns a
//! `Parsed<Grammar>`; the serializer still takes the generated
//! `<Grammar>NodeView<'a>` Copy wrapper, constructed via
//! `NodeView::from_cursor` from the root view's cursor + input.

use ::bbnf::grammar::generated::json::*;


use ::bbnf::grammar::generated::csv::*;


use ::bbnf::grammar::generated::bnf::*;


use ::bbnf::grammar::generated::ebnf::*;


use ::bbnf::grammar::generated::math::*;


// AZ-II.cutover.I (Phase 5) — `BbnfBootstrap::serialize_compact` is
// a tape-substrate symbol that retired with the StructDirect flip;
// the BBNF round-trip below routes through `bbnf::runtime::bbnf::
// serialize_compact_doc` instead. The generated `BbnfBootstrap` ident
// is therefore unused at test-link time.


// CSS pretty grammar (no @import)
use ::bbnf::grammar::generated::css_pretty::*;


// AZ-I.W2-act.recovery — the `google_sheets`, `css_l4` re-exports
// and the `css_types` host-type module retired alongside the Sheets
// + CSS L4 serialization paths below; all lived solely as compile-
// time inputs to the pre-flip cursor-backed serialize_compact
// emission, none survives the struct-direct flip's removal of the
// tape-shaped serializer infrastructure.


// ── JSON ─────────────────────────────────────────────────────────────────────
//
// AZ-I.W2-act.recovery — JSON now lands on the struct-direct path
// (`JsonParser::parse` returns `JsonDocument`), so the cursor-backed
// `JsonParserNodeView::from_cursor` accessor and `serialize_compact`
// emitter retired alongside the tape substrate. The migrated round-
// trip test serialises the typed value tree through a minimal walker
// and asserts idempotence under reparse.

use bbnf::runtime::{JsonNumber, JsonValue};

fn json_emit(input: &str) -> String {
    let doc = JsonParser::parse(input).expect("JSON parse failed");
    let mut out = String::new();
    write_json_value(&doc, doc.root(), &mut out);
    out
}

fn write_json_value<'p>(
    doc: &bbnf::runtime::JsonDocument<'p>,
    value: &JsonValue<'p>,
    out: &mut String,
) {
    use std::fmt::Write;
    match value {
        JsonValue::Null => out.push_str("null"),
        JsonValue::Bool(true) => out.push_str("true"),
        JsonValue::Bool(false) => out.push_str("false"),
        JsonValue::Number(n) => {
            let f = match n {
                JsonNumber::Float(f) => *f,
                JsonNumber::Int(i) => *i as f64,
                JsonNumber::UInt(u) => *u as f64,
            };
            // Compact serialization: integers without trailing ".0".
            if f.fract() == 0.0 && f.abs() < 1e16 {
                write!(out, "{}", f as i64).unwrap();
            } else {
                write!(out, "{}", f).unwrap();
            }
        }
        JsonValue::String(s) => {
            out.push('"');
            // Re-escape characters needing JSON quotation.
            for c in s.chars() {
                match c {
                    '"' => out.push_str("\\\""),
                    '\\' => out.push_str("\\\\"),
                    '\n' => out.push_str("\\n"),
                    '\r' => out.push_str("\\r"),
                    '\t' => out.push_str("\\t"),
                    '\u{08}' => out.push_str("\\b"),
                    '\u{0C}' => out.push_str("\\f"),
                    c if (c as u32) < 0x20 => {
                        write!(out, "\\u{:04x}", c as u32).unwrap();
                    }
                    c => out.push(c),
                }
            }
            out.push('"');
        }
        JsonValue::Array(id) => {
            out.push('[');
            let items = doc.array(*id);
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_json_value(doc, item, out);
            }
            out.push(']');
        }
        JsonValue::Object(id) => {
            out.push('{');
            let pairs = doc.object(*id);
            for (i, pair) in pairs.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                // Re-emit the pair's key as a JSON string.
                write_json_value(doc, &JsonValue::String(pair.key), out);
                out.push(':');
                write_json_value(doc, &pair.value, out);
            }
            out.push('}');
        }
    }
}

fn json_rt(input: &str) {
    let s1 = json_emit(input);
    let s2 = json_emit(&s1);
    assert_eq!(s1, s2, "JSON serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test] fn json_null()      { assert_eq!(json_emit("null"), "null"); }
#[test] fn json_true()      { assert_eq!(json_emit("true"), "true"); }
#[test] fn json_false()     { assert_eq!(json_emit("false"), "false"); }
#[test] fn json_number()    { json_rt("42"); }
#[test] fn json_string()    { assert_eq!(json_emit(r#""hello""#), r#""hello""#); }
#[test] fn json_empty_arr() { json_rt("[]"); }
#[test] fn json_array()     { json_rt("[1, 2, 3]"); }
#[test] fn json_empty_obj() { json_rt("{}"); }
#[test] fn json_object()    { json_rt(r#"{"key": "value"}"#); }
#[test] fn json_nested()    { json_rt(r#"{"a": [1, 2], "b": {"c": true}}"#); }

// ── CSV ──────────────────────────────────────────────────────────────────────

fn csv_emit(input: &str) -> String {
    let parsed = CsvParser::parse(input).expect("CSV parse failed");
    let view = parsed.view();
    let node = CsvParserNodeView::from_cursor(view.cursor(), parsed.input());
    CsvParser::serialize_compact(node)
}

fn csv_rt(input: &str) {
    let s1 = csv_emit(input);
    let s2 = csv_emit(&s1);
    assert_eq!(s1, s2, "CSV serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test] fn csv_simple()     { csv_rt("a,b,c"); }
#[test] fn csv_multi()      { csv_rt("a,b,c\n1,2,3"); }
#[test] fn csv_quoted()     { csv_rt(r#""hello","world""#); }
#[test] fn csv_single()     { csv_rt("hello"); }

// ── BNF ──────────────────────────────────────────────────────────────────────

fn bnf_emit(input: &str) -> String {
    let parsed = BnfParser::parse(input).expect("BNF parse failed");
    let view = parsed.view();
    let node = BnfParserNodeView::from_cursor(view.cursor(), parsed.input());
    BnfParser::serialize_compact(node)
}

#[test]
fn bnf_rule() {
    let e = bnf_emit("<expr> ::= <term> | <expr> \"+\" <term>\n");
    assert!(!e.is_empty(), "BNF empty");
}

// ── EBNF ─────────────────────────────────────────────────────────────────────

fn ebnf_emit(input: &str) -> String {
    let parsed = EbnfParser::parse(input).expect("EBNF parse failed");
    let view = parsed.view();
    let node = EbnfParserNodeView::from_cursor(view.cursor(), parsed.input());
    EbnfParser::serialize_compact(node)
}

#[test]
fn ebnf_rule() {
    let e = ebnf_emit("digit = \"0\" | \"1\" | \"2\" ;\n");
    assert!(!e.is_empty(), "EBNF empty");
}

// ── Math ─────────────────────────────────────────────────────────────────────

fn math_emit(input: &str) -> String {
    let parsed = MathParser::parse(input).expect("Math parse failed");
    let view = parsed.view();
    let node = MathParserNodeView::from_cursor(view.cursor(), parsed.input());
    MathParser::serialize_compact(node)
}

#[test]
fn math_num() {
    let e = math_emit("42");
    assert!(!e.is_empty(), "Math empty");
}

// ── Google Sheets ────────────────────────────────────────────────────────────
//
// AZ-I.W2-act.recovery — the Sheets serializer infrastructure
// (`GoogleSheetsParserNodeView::from_cursor` + `serialize_compact`)
// retired alongside the tape substrate when W2-act.B2 flipped Sheets
// to the struct-direct path. The `SheetsDocument` typed value tree
// is now the parse output; a `SheetsDocument` → text serializer
// (analogous to the JSON one above) is unauthored — the pre-flip
// `serialize_compact` walked tape records, no struct-tree equivalent
// has landed. The smoke test for Sheets serialization retires here
// per `feedback_no-workarounds`; round-trip parity for the
// `SheetsDocument` projection lives in
// `crates/core/tests/sheets_expr_parity.rs` (the post-flip migration
// of which is orchestrator-owned, outside W2-act.recovery scope).

// ── BBNF ─────────────────────────────────────────────────────────────────────
//
// AZ-II.cutover.E (Phase 5) — BBNF parse returns a `BbnfDocument` post-
// cutover.A; the pre-cutover `parsed.view().cursor()` cursor surface is
// gone. Re-author the round-trip against `BbnfDocument`'s span-fold
// surface: every `BbnfValue::Span` borrows from `input`, and the
// compound tree's aggregate `span_range()` returns the byte range
// covered by every borrowed leaf — which for the BBNF grammar is the
// canonical source slice the parser admitted. The round-trip slices
// `input` over the document's covering range and asserts idempotence
// under reparse.
//
// AZ-II.cutover.I Phase 5 — LANDED. The bootstrap_parser admits real
// BBNF source via `bbnf::grammar::bootstrap_parser::parse`; the
// `serialize_compact_doc` walker at
// `bbnf::runtime::bbnf::serialize_compact_doc` is a typed projection
// over [`BbnfCompoundKind`] that materialises every non-Span literal
// (`=`, `;`, `,`, `|`, `->`, `from`, `@import`, …) and re-emits the
// source byte-stably. The round-trip asserts idempotence: the second
// parse-and-serialize cycle reproduces the first emission verbatim.

use bbnf::runtime::bbnf::serialize_compact_doc;

fn bbnf_emit(input: &str) -> String {
    let doc = bbnf::grammar::bootstrap_parser::parse(input)
        .expect("BBNF grammar parse failed");
    serialize_compact_doc(&doc)
}

#[test]
fn bbnf_rule() {
    let input = "x = /[a-z]+/ ;\ny = \"hello\" ;";
    let e = bbnf_emit(input);
    assert!(!e.is_empty(), "BBNF serialize empty");
    // Idempotence: parse → serialize → reparse → serialize → equality.
    let e2 = bbnf_emit(&e);
    assert_eq!(e, e2, "BBNF serialize not idempotent:\n  s1={e:?}\n  s2={e2:?}");
}

// ── CSS Pretty ───────────────────────────────────────────────────────────────

fn css_pretty_emit(input: &str) -> String {
    let parsed = CssPrettyParser::parse(input).expect("CSS Pretty parse failed");
    let view = parsed.view();
    let node = CssPrettyParserNodeView::from_cursor(view.cursor(), parsed.input());
    CssPrettyParser::serialize_compact(node)
}

fn css_pretty_rt(input: &str) {
    let s1 = css_pretty_emit(input);
    let s2 = css_pretty_emit(&s1);
    assert_eq!(s1, s2, "CSS Pretty serialize not idempotent:\n  s1={s1:?}\n  s2={s2:?}");
}

#[test]
fn css_simple() {
    css_pretty_rt("body { color: red; }");
}

// ── CSS L4 (@import-based grammar with host types) ──────────────────────────
//
// AZ-I.W2-act.recovery — same retirement as the Sheets path above:
// `CssL4ParserNodeView::from_cursor` + `serialize_compact` lived on
// the tape substrate, which W2-act.B3 severed when CSS L4 flipped to
// the struct-direct path. `CssDocument` carries the typed value tree;
// the corresponding serializer is unauthored (pre-flip path walked
// tape records). CSS L4 round-trip parity lives in the parity
// harness against lightningcss / cssparser (orchestrator-owned
// migration outside W2-act.recovery scope).
