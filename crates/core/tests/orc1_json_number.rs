//! sk-v27 LW3.ORC1-NUM (ruling ORC-1, 2026-11-12a): the generated number
//! scanner reads the number the grammar's regex reads.
//!
//! `grammar/json/json.bbnf` writes `number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/`.
//! JSONTestSuite (in custody) names the defect: root accepts
//! `n_number_-01`, `n_number_neg_int_starting_with_zero` and
//! `n_number_with_leading_zero`, because the Number-shape scanner read any
//! digit run where the regex's `0|[1-9]\d*` reads a lone `0`.
//!
//! The falsifier is bounded-exhaustive: every string of length 1 to 6 over
//! `0 1 - + . e E` is parsed as a whole JSON text by root's generated parser
//! and by serde_json (an independent RFC 8259 reader), and the verdicts must
//! be equal. The digits stop at `1` so no exponent leaves f64's range (an
//! out-of-range number is JSONTestSuite's `i_`, implementation-defined).
//! Each string is also read as an array element (`[s]`) and as an object
//! value (`{"k":s}`), where the number ends at a structural byte.

use bbnf::grammar::generated::json::JsonParser;

const ALPHABET: &[u8] = b"01-+.eE";

fn strings(max: usize) -> Vec<String> {
    let mut out = Vec::new();
    let mut frontier = vec![String::new()];
    for _ in 0..max {
        let mut next = Vec::with_capacity(frontier.len() * ALPHABET.len());
        for s in &frontier {
            for &b in ALPHABET {
                let mut t = s.clone();
                t.push(b as char);
                next.push(t);
            }
        }
        out.extend(next.iter().cloned());
        frontier = next;
    }
    out
}

/// serde_json's verdict, or `None` when it refuses only because the number
/// leaves f64's range (JSONTestSuite's `i_number_huge_exp` class: the RFC
/// leaves the range to the implementation, so neither verdict is wrong).
fn serde_accepts(s: &str) -> Option<bool> {
    match serde_json::from_str::<serde_json::Value>(s) {
        Ok(_) => Some(true),
        Err(e) if e.to_string().contains("number out of range") => None,
        Err(_) => Some(false),
    }
}

#[test]
fn number_scanner_equals_rfc_8259_on_every_short_string() {
    let mut diverging = Vec::new();
    let mut read = 0usize;
    let mut out_of_range = 0usize;
    for s in strings(6) {
        for text in [s.clone(), format!("[{s}]"), format!("{{\"k\":{s}}}")] {
            let Some(rfc) = serde_accepts(&text) else {
                out_of_range += 1;
                continue;
            };
            read += 1;
            let root = JsonParser::parse(&text).is_ok();
            if root != rfc {
                diverging.push(format!(
                    "{text:?}: root {} / serde_json {}",
                    if root { "accepts" } else { "rejects" },
                    if rfc { "accepts" } else { "rejects" }
                ));
            }
        }
    }
    eprintln!(
        "read {read} texts ({out_of_range} out of f64's range not judged), {} diverging",
        diverging.len()
    );
    assert!(
        diverging.is_empty(),
        "{} of {read} texts diverge from RFC 8259; first 20:\n{}",
        diverging.len(),
        diverging.iter().take(20).cloned().collect::<Vec<_>>().join("\n")
    );
}

#[test]
fn jsontestsuite_leading_zero_cases_are_rejected() {
    // The three n_ files' texts (JSONTestSuite test_parsing/ at 1ef36fa).
    for text in ["[-01]", "[-012]", "[012]"] {
        assert!(JsonParser::parse(text).is_err(), "accepted {text}");
    }
    for text in ["[0]", "[-0]", "[0.5]", "[-0e1]", "[10]", "[0E+1]"] {
        assert!(JsonParser::parse(text).is_ok(), "rejected {text}");
    }
}
