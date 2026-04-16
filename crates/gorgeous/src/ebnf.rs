use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(
    path = "grammar/ebnf/ebnf.bbnf",
    prettify
)]
pub struct EbnfParser;

/// Pretty-print an EBNF grammar string.
pub fn prettify_ebnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = EbnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
