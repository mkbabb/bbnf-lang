use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Debug, Parser)]
#[parser(
    path = "grammar/google-sheets/google-sheets.bbnf",
    prettify
)]
pub struct GoogleSheetsParser;

/// Parse a Google Sheets formula. Returns true if the input is valid.
pub fn parse_formula(input: &str) -> Option<()> {
    GoogleSheetsParser::parse(input).ok().map(|_| ())
}

/// Parse and pretty-print a Google Sheets formula.
pub fn prettify_formula(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = GoogleSheetsParser::formula_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
