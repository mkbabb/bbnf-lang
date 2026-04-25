use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/google_sheets.rs`;
// marker `GoogleSheetsParser`.
pub use ::bbnf::grammar::generated::google_sheets::GoogleSheetsParser;

/// Parse a Google Sheets formula. Returns true if the input is valid.
pub fn parse_formula(input: &str) -> Option<()> {
    GoogleSheetsParser::parse(input).ok().map(|_| ())
}

/// Parse and pretty-print a Google Sheets formula.
pub fn prettify_formula(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = GoogleSheetsParser::formula_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
