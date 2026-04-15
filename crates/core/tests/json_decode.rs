//! AU.3.1: JSON string decode round-trip tests.
//!
//! Asserts that a JSON string with escape sequences decodes through
//! the tape's arena and reads back as the decoded UTF-8 via
//! `Tape::payload_string`. Uses the JSON grammar's `string` rule,
//! which is annotated with `-> decode_json_string_to_arena(input) :
//! String`.

use bbnf::runtime::tape::{Tape, TapeCursor, TapeKind, TapeOffset};
use bbnf_derive::Parser;

#[derive(Parser)]
#[parser(path = "../../grammar/json/json.bbnf")]
struct JsonParser;

/// Find the first tape record of `TapeKind::Span` with a non-empty
/// string payload, starting at `root` and walking in pre-order.
///
/// Uses `payload_string_with_source` so borrow-safe strings (no
/// escapes) resolve via direct source slicing — matching the bench
/// walker's accessor — and escape-bearing strings still read from
/// the arena frame.
fn find_first_decoded_string<'t, 's: 't>(
    tape: &'t Tape,
    root: TapeOffset,
    source: &'s str,
) -> Option<&'t str> {
    let cursor = TapeCursor::new(tape, root);
    walk_for_decoded(cursor, source.as_bytes())
}

fn walk_for_decoded<'t, 's: 't>(cursor: TapeCursor<'t>, source: &'s [u8]) -> Option<&'t str> {
    let rec = cursor.record();
    if rec.kind() == TapeKind::Span {
        if let Some(s) = cursor.tape().payload_string_with_source(rec, source) {
            return Some(s);
        }
    }
    for child in cursor.children() {
        if let Some(s) = walk_for_decoded(child, source) {
            return Some(s);
        }
    }
    None
}

#[test]
fn decode_plain_string_round_trip() {
    // No escapes — borrow-safe leaf, source-aware reader returns the
    // quoted body directly.
    let input = r#""hello""#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root = parsed.view().cursor().offset();
    let decoded = find_first_decoded_string(parsed.tape(), root, input)
        .expect("string payload present");
    assert_eq!(decoded, "hello");
}

#[test]
fn decode_simple_escapes_round_trip() {
    // \n, \t, \r, \", \\, \/
    let input = r#""line1\nline2\t\"quoted\"\\\/""#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root = parsed.view().cursor().offset();
    let decoded = find_first_decoded_string(parsed.tape(), root, input)
        .expect("string payload present");
    assert_eq!(decoded, "line1\nline2\t\"quoted\"\\/");
}

#[test]
fn decode_u_escape_round_trip() {
    // \u0041 = 'A'; \u00e9 = 'é' (2-byte UTF-8).
    let input = r#""A\u0041\u00e9""#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root = parsed.view().cursor().offset();
    let decoded = find_first_decoded_string(parsed.tape(), root, input)
        .expect("string payload present");
    assert_eq!(decoded, "AAé");
}

#[test]
fn decode_surrogate_pair_round_trip() {
    // U+1F600 (grinning face emoji) encoded as \uD83D\uDE00.
    let input = r#""\uD83D\uDE00""#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root = parsed.view().cursor().offset();
    let decoded = find_first_decoded_string(parsed.tape(), root, input)
        .expect("string payload present");
    assert_eq!(decoded, "\u{1F600}");
}

#[test]
fn decode_json_object_string_keys_and_values() {
    // Nested structure — every string reachable via the tape walk
    // resolves to its decoded UTF-8.
    let input = r#"{"key\n1": "value\u00e9", "key2": "plain"}"#;
    let parsed = JsonParser::parse(input).expect("parse");
    let root = parsed.view().cursor().offset();
    let mut collected = Vec::new();
    collect_strings(TapeCursor::new(parsed.tape(), root), input.as_bytes(), &mut collected);
    // The keys and values interleave; order in the tape follows
    // source order, so for `{"key1":"value1","key2":"value2"}` we
    // expect `key1, value1, key2, value2`.
    assert!(collected.contains(&"key\n1".to_string()));
    assert!(collected.contains(&"valueé".to_string()));
    assert!(collected.contains(&"key2".to_string()));
    assert!(collected.contains(&"plain".to_string()));
}

fn collect_strings<'t, 's: 't>(
    cursor: TapeCursor<'t>,
    source: &'s [u8],
    out: &mut Vec<String>,
) {
    let rec = cursor.record();
    if rec.kind() == TapeKind::Span {
        if let Some(s) = cursor.tape().payload_string_with_source(rec, source) {
            out.push(s.to_string());
        }
    }
    for child in cursor.children() {
        collect_strings(child, source, out);
    }
}
