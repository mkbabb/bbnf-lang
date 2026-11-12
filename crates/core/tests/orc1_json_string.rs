//! sk-v27 LW3.ORC1-STR (ruling ORC-1, 2026-11-12a): a JSON string holds no
//! unescaped U+0000 to U+001F.
//!
//! RFC 8259 §7: "All Unicode characters may be placed within the quotation
//! marks, except for the characters that MUST be escaped: quotation mark,
//! reverse solidus, and the control characters (U+0000 through U+001F)."
//! JSONTestSuite (in custody) names the defect: root and the TS package both
//! accept `n_string_unescaped_ctrl_char`, `n_string_unescaped_newline` and
//! `n_string_unescaped_tab`, because the grammar of record's `string` regex
//! admitted them (`[^"\\]`), and root's String-shape scanner read only the
//! quote and the backslash whatever the regex's class said.
//!
//! The falsifier reads every ASCII byte and a set of non-ASCII scalars in
//! four places of a string (alone, between letters, after an escape, before
//! an escape; the last two take the scanner's decode path), as an array
//! element and as an object key, against serde_json (an independent RFC 8259
//! reader). The grammar's text is checked too: the scanner is generated from
//! it, and a scanner that honours a class the grammar does not write is no
//! cure.

use bbnf::grammar::generated::json::JsonParser;

fn serde_accepts(s: &str) -> bool {
    serde_json::from_str::<serde_json::Value>(s).is_ok()
}

#[test]
fn string_bodies_equal_rfc_8259_for_every_ascii_byte() {
    let mut chars: Vec<char> = (0u8..=0x7F).map(char::from).collect();
    chars.extend(['\u{80}', '\u{A0}', '\u{2028}', '\u{FEFF}', '\u{10FFFF}', 'é', '😀']);
    let mut diverging = Vec::new();
    let mut read = 0;
    for c in chars {
        for body in [
            format!("{c}"),
            format!("a{c}b"),
            format!("\\n{c}"),
            format!("{c}\\t"),
        ] {
            for text in [format!("[\"{body}\"]"), format!("{{\"{body}\":1}}")] {
                read += 1;
                let root = JsonParser::parse(&text).is_ok();
                let rfc = serde_accepts(&text);
                if root != rfc {
                    diverging.push(format!(
                        "{text:?} (U+{:04X}): root {} / serde_json {}",
                        c as u32,
                        if root { "accepts" } else { "rejects" },
                        if rfc { "accepts" } else { "rejects" }
                    ));
                }
            }
        }
    }
    eprintln!("read {read} texts, {} diverging", diverging.len());
    assert!(
        diverging.is_empty(),
        "{} of {read} texts diverge from RFC 8259; first 20:\n{}",
        diverging.len(),
        diverging.iter().take(20).cloned().collect::<Vec<_>>().join("\n")
    );
}

#[test]
fn jsontestsuite_unescaped_control_cases_are_rejected() {
    // The three n_ files' texts (JSONTestSuite test_parsing/ at 1ef36fa).
    for text in ["[\"a\u{0}a\"]", "[\"new\nline\"]", "[\"\t\"]"] {
        assert!(JsonParser::parse(text).is_err(), "accepted {text:?}");
    }
}

#[test]
fn the_grammar_of_record_excludes_the_control_characters() {
    let grammar = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../grammar/json/json.bbnf"
    ))
    .expect("read grammar/json/json.bbnf");
    let string = grammar
        .lines()
        .find(|l| l.trim_start().starts_with("string"))
        .expect("a string rule");
    assert!(
        string.contains(r#"[^"\\\x00-\x1F]"#),
        "grammar/json/json.bbnf's string regex admits U+0000-U+001F: {string}"
    );
}
