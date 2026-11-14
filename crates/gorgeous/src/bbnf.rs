use crate::PrinterConfig;

// B2.W1: xtask-emitted at `crates/core/src/grammar/generated/bbnf.rs`;
// marker `BbnfBootstrap`. The pre-B2 gorgeous-side `BbnfParser` name is
// preserved via alias so existing callers compile unchanged.
pub use ::bbnf::grammar::generated::bbnf::BbnfBootstrap as BbnfParser;

/// Pretty-print a BBNF grammar string. Input the grammar does not consume
/// is refused, not formatted as its parsed prefix (sk-v27 LW3.S3c-PP).
pub fn prettify_bbnf(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = BbnfParser::grammar_prettify().eof().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
