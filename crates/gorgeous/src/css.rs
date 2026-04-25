use crate::PrinterConfig;

// B2.W1: replaces `the proc-macro derive (retired B2) #[parser(path = "grammar/css/pretty.bbnf",
// prettify, skip_recover)] pub struct CssParser;`. The xtask emits the
// per-grammar source at `crates/core/src/grammar/generated/css_pretty.rs`
// with marker `CssPrettyParser`; we alias it to `CssParser` so existing
// prettify_css callers resolve through the same surface.
pub use ::bbnf::grammar::generated::css_pretty::CssPrettyParser as CssParser;

/// Pretty-print a CSS stylesheet via fused parse+format pipeline.
pub fn prettify_css(input: &str, config: &PrinterConfig) -> Option<String> {
    let ops = CssParser::stylesheet_prettify().parse(input)?;
    Some(pprint::render(&ops, config.to_printer()))
}
