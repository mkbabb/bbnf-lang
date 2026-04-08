//! BBNF grammar parser.
//!
//! Single-call parse: generated bootstrap parser + host extraction.
//!
//! Sub-modules:
//! - `generated` — auto-generated parser from bbnf.bbnf (bootstrap, checked-in)
//! - `host` — extraction: BbnfBootstrapEnum → ParsedGrammar

#[allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::all)]
pub mod generated;
pub mod host;
pub mod schema;

use crate::types::ParsedGrammar;

/// Parse a BBNF grammar source into a `ParsedGrammar`.
///
/// Single-call parse: the bootstrap parser's `grammar` rule handles comments,
/// directives, and rules natively via `grammar_item`.
pub fn parse(source: &str) -> Option<ParsedGrammar<'_>> {
    let ctx = Box::leak(Box::new(
        generated::__BbnfBootstrapEnumCtx::with_capacity(source.len() / 4),
    ));
    let leaked: &str = Box::leak(source.to_string().into_boxed_str());
    let (result, _state) = generated::BbnfBootstrap::grammar()
        .parse_return_state_with_context(leaked, ctx);

    result.map(|ast| {
        let ast = Box::leak(Box::new(ast));
        host::extract_grammar(ast)
    })
}

/// Parse a BBNF grammar file, returning both the result and the parser state.
pub fn parse_with_state(source: &str) -> (Option<ParsedGrammar<'_>>, parse_that::ParserState<'_>) {
    let ctx = Box::leak(Box::new(
        generated::__BbnfBootstrapEnumCtx::with_capacity(source.len() / 4),
    ));
    let leaked: &str = Box::leak(source.to_string().into_boxed_str());
    let (result, state) = generated::BbnfBootstrap::grammar()
        .parse_return_state_with_context(leaked, ctx);

    let parsed = result.map(|ast| {
        let ast = Box::leak(Box::new(ast));
        host::extract_grammar(ast)
    });

    (parsed, state)
}
