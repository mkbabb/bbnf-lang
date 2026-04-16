use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(
    path = "grammar/bnf/bnf.bbnf",
    prettify
)]
pub struct BnfParser;

/// Pretty-print a BNF grammar string.
pub fn prettify_bnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = BnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
