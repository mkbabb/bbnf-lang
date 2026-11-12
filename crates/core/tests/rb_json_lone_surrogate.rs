//! sk-v27 LW3.ORC1-RB, SK/grammar/DIVERGENCE.tsv J-01..J-10 (R-b, rulings
//! ORC-1 and ORC-1a): root refuses a JSON string whose `\u` escapes spell a
//! lone or unpaired UTF-16 surrogate.
//!
//! Each case is a JSONTestSuite `i_` file (in custody): RFC 8259 leaves the
//! verdict to the implementation, so JSONTestSuite has no authority over it,
//! and the TS 0.3.0 package (an independent oracle) accepts all ten. The
//! grammar of record decides, by docs/bbnf/grammar-syntax.md: the string
//! regex matches every one of them (`\\u[0-9a-fA-F]{4}`), and "a mapping never
//! changes what the factor matches", yet root's `decode_json_string_to_arena`
//! projection refuses the factor when the decoded text is not well-formed
//! UTF-16. The cure is the spine's string projection (LW4.COMP-1, the plan-driven Rust emitter), which must
//! read what the factor matches whatever it decodes to; root is the quarry
//! and is retired after the cutover (PLAN CB-14).

use bbnf::grammar::generated::json::JsonParser;

/// The ten files' texts (JSONTestSuite test_parsing/ at 1ef36fa).
const CASES: [(&str, &str); 10] = [
    ("i_object_key_lone_2nd_surrogate", r#"{"\uDFAA":0}"#),
    ("i_string_1st_surrogate_but_2nd_missing", r#"["\uDADA"]"#),
    ("i_string_1st_valid_surrogate_2nd_invalid", r#"["\uD888ሴ"]"#),
    ("i_string_incomplete_surrogate_and_escape_valid", r#"["\uD800\n"]"#),
    ("i_string_incomplete_surrogate_pair", r#"["\uDd1ea"]"#),
    ("i_string_incomplete_surrogates_escape_valid", r#"["\uD800\uD800\n"]"#),
    ("i_string_invalid_lonely_surrogate", r#"["\ud800"]"#),
    ("i_string_invalid_surrogate", r#"["\ud800abc"]"#),
    ("i_string_inverted_surrogates_U+1D11E", r#"["\uDd1e\uD834"]"#),
    ("i_string_lone_second_surrogate", r#"["\uDFAA"]"#),
];

#[test]
#[ignore = "born-RED R-b J-01 J-02 J-03 J-04 J-05 J-06 J-07 J-08 J-09 J-10 (SK/grammar/DIVERGENCE.tsv): root's string projection refuses a factor the regex matches; cure LW4.COMP-1 at LW4"]
fn a_lone_surrogate_escape_is_read_as_the_regex_reads_it() {
    let refused: Vec<&str> = CASES
        .iter()
        .filter(|(_, text)| JsonParser::parse(text).is_err())
        .map(|(name, _)| *name)
        .collect();
    assert!(refused.is_empty(), "root refuses {} of 10: {refused:?}", refused.len());
}

#[test]
fn a_paired_surrogate_escape_is_read() {
    for text in [r#"["𝄞"]"#, r#"{"😀":0}"#] {
        assert!(JsonParser::parse(text).is_ok(), "refused {text}");
    }
}
