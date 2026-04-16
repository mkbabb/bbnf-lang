use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(
    path = "grammar/bbnf/bbnf.bbnf",
    prettify
)]
pub struct BbnfParser;

/// Pretty-print a BBNF grammar string.
pub fn prettify_bbnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = BbnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
