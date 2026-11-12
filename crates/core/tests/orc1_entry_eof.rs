//! sk-v27 LW3.ORC1-CSV (ruling ORC-1, 2026-11-12a): root reads the whole
//! input as the entry rule's language, with no whitespace after it that the
//! grammar does not read.
//!
//! The TS 0.3.0 package (an independent oracle, in custody) names the defect
//! on `grammar/misc/csv.bbnf`: `csv = record, ( /\r?\n/ >> record ) * ;`
//! reads no line break after the last record, yet root accepted both CSV
//! inputs that end in one, because root's generated `parse` skipped
//! whitespace after the entry rule before its end-of-input test. The same
//! skip let `grammar/misc/math.bbnf`'s entry `number` accept `1\n`.
//! Grammars that read their own trailing whitespace (JSON's `json = ws >>
//! value << ws`) are unchanged.

use bbnf::grammar::generated::{csv::CsvParser, json::JsonParser, math::MathParser};

#[test]
fn csv_reads_no_line_break_after_the_last_record() {
    for text in ["a,b\n", "a\r\n", "a,b\nc,d\n", "a \n"] {
        assert!(CsvParser::parse(text).is_err(), "accepted {text:?}");
    }
    for text in ["a,b", "a,b\nc,d", "a\r\nb", "\"q\",x"] {
        assert!(CsvParser::parse(text).is_ok(), "refused {text:?}");
    }
}

#[test]
fn math_number_reads_no_trailing_whitespace() {
    for text in ["1\n", "1 ", "1.5\t"] {
        assert!(MathParser::parse(text).is_err(), "accepted {text:?}");
    }
    for text in ["1", "1.5", "1e3"] {
        assert!(MathParser::parse(text).is_ok(), "refused {text:?}");
    }
}

#[test]
fn a_grammar_that_reads_its_trailing_whitespace_still_does() {
    for text in ["{}\n", " [1] \r\n", "\t0\t"] {
        assert!(JsonParser::parse(text).is_ok(), "refused {text:?}");
    }
}
