use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/ebnf.rs`;
// marker `EbnfParser`.
pub use ::bbnf::grammar::generated::ebnf::EbnfParser;

/// Pretty-print an EBNF grammar string.
pub fn prettify_ebnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = EbnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
