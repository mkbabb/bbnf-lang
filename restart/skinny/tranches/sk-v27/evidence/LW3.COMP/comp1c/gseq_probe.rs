//! probe (eleventh seat): what google_sheets reads with and without a leading `=`.
use bbnf::grammar::generated::google_sheets::GoogleSheetsParser;

#[test]
fn probe_leading_eq() {
    for src in ["=1+2", "1+2", "=A1", "A1", "=SUM(A1:B2)", "SUM(A1:B2)", "42", "=42", " =1", "\"x\"", "=\"x\""] {
        eprintln!("PROBE {src:?} -> {:?}", GoogleSheetsParser::parse(src).is_ok());
    }
}
