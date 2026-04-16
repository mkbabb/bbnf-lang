use bbnf_derive::Parser;

use crate::PrinterConfig;

#[derive(Parser)]
#[parser(
    path = "grammar/json/json.bbnf",
    prettify
)]
pub struct JsonParser;

/// Pretty-print a JSON string via fused parse+format pipeline.
pub fn prettify_json(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = JsonParser::value_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
