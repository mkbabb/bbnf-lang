use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/bnf.rs`;
// marker `BnfParser`.
pub use ::bbnf::grammar::generated::bnf::BnfParser;

/// Pretty-print a BNF grammar string.
pub fn prettify_bnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = BnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
