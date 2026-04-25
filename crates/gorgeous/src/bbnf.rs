use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/bbnf.rs`;
// marker `BbnfBootstrap`. The pre-B2 gorgeous-side `BbnfParser` name is
// preserved via alias so existing callers compile unchanged.
pub use ::bbnf::grammar::generated::bbnf::BbnfBootstrap as BbnfParser;

/// Pretty-print a BBNF grammar string.
pub fn prettify_bbnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = BbnfParser::grammar_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
