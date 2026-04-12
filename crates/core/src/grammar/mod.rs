//! BBNF grammar parser.
//!
//! Single-call parse: generated tape-first bootstrap parser + host extraction.
//!
//! Sub-modules:
//! - `generated` — auto-generated parser from bbnf.bbnf (bootstrap, checked-in)
//! - `host` — extraction: BbnfBootstrapNodeView → ParsedGrammar
//!
//! Tranche AC.2: `parse` / `parse_with_state` now drive the tape-first
//! generated parser. `generated::BbnfBootstrap::parse(source)` returns a
//! [`crate::runtime::Parsed`] that owns both the finished tape and the
//! source string; `host::extract_grammar` walks the root view to lift the
//! bootstrap tree into the typed `ParsedGrammar`. The returned
//! `ParsedGrammar<'_>` borrows from a leaked `Parsed` so downstream
//! pipeline code can keep its existing `'static`-flavoured lifetime
//! assumptions.

#[allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::all)]
pub mod generated;
pub mod host;
pub mod schema;

use crate::runtime::Parsed;
use crate::types::ParsedGrammar;

/// Parse a BBNF grammar source into a `ParsedGrammar`.
///
/// The tape-first bootstrap parser owns its own source buffer and tape
/// via [`crate::runtime::Parsed`]. We leak that structure so the
/// resulting `ParsedGrammar<'_>` — which borrows cursors and text
/// slices from both — lives for the rest of the compile, matching the
/// pre-tranche arena-style ownership model expected by callers.
pub fn parse(source: &str) -> Option<ParsedGrammar<'_>> {
    // Leak the input string so the borrowed Parsed<'static, _> and
    // the resulting ParsedGrammar<'_> live for the rest of the
    // compile. Library-internal scratch: the bootstrap flow runs
    // once per compile; the pipeline assumes 'static lifetimes.
    let input: &'static str = Box::leak(source.to_owned().into_boxed_str());
    let parsed = generated::BbnfBootstrap::parse(input).ok()?;
    let parsed: &'static Parsed<'static, generated::BbnfBootstrap> =
        Box::leak(Box::new(parsed));
    Some(host::extract_grammar(parsed))
}

/// Parse a BBNF grammar file.
///
/// Tranche AC.2: the tape-first `BbnfBootstrap::parse` entry point
/// internally manages its own parser state, so we no longer surface a
/// `ParserState<'_>` to callers. The function is retained for API
/// compatibility with pre-AC call sites as a thin alias over [`parse`];
/// it will be audited and likely removed during AC.3 once the
/// analysis crate migrates off `parser_state.furthest_offset`.
pub fn parse_with_state(source: &str) -> Option<ParsedGrammar<'_>> {
    parse(source)
}
