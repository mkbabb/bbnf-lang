use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/json.rs`;
// marker `JsonParser`.
pub use ::bbnf::grammar::generated::json::JsonParser;

/// Pretty-print a JSON string via fused parse+format pipeline.
pub fn prettify_json(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = JsonParser::value_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
