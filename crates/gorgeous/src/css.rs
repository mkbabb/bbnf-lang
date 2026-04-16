use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(path = "grammar/css/pretty.bbnf", prettify, skip_recover)]
pub struct CssParser;

/// Pretty-print a CSS stylesheet via fused parse+format pipeline.
pub fn prettify_css(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = CssParser::stylesheet_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
