//! BBNF grammar parser.
//!
//! Single-call parse: generated tape-first bootstrap parser + extraction.
//!
//! Sub-modules:
//! - `generated` — auto-generated parser from bbnf.bbnf (bootstrap, checked-in)
//! - `host` — tape walkers: bootstrap view → observational `GrammarExtract`
//!   (LSP / gorgeous / debug) or pipeline-direct `(AST, DirectiveMaps)`
//!   (compile).
//!
//! Tranche AC.2: drives the tape-first generated parser.
//! `generated::BbnfBootstrap::parse(source)` returns a
//! [`crate::runtime::Parsed`] that owns both the finished tape and the
//! source string.
//!
//! Tranche AU.4.1: deleted the historical `ParsedGrammar` intermediate.
//! The public `parse` entry point leaks the `Parsed` so callers can
//! keep their `'static`-flavoured lifetime assumptions, then returns a
//! [`crate::types::GrammarExtract`] for observational callers. The
//! compile pipeline bypasses `parse` in favour of
//! [`crate::pipeline::directives::parse_to_pipeline_inputs`], which
//! walks the tape straight into `DirectiveMaps` + `AST`.

#[allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::all)]
pub mod generated;
pub mod host;
pub mod schema;

use crate::runtime::Parsed;
use crate::types::GrammarExtract;

/// Parse a BBNF grammar source into a [`GrammarExtract`].
///
/// The tape-first bootstrap parser owns its own source buffer and tape
/// via [`crate::runtime::Parsed`]. This entry point leaks the input so
/// the resulting `GrammarExtract<'_>` — which borrows cursors and text
/// slices from the tape — lives for the rest of the compile, matching
/// the pre-AU.4.1 arena-style ownership model observational callers
/// (LSP analysis, gorgeous JIT, `debug_parse`) already rely on.
///
/// Compile-side callers should instead route through
/// [`crate::pipeline`]; the pipeline avoids this allocation by landing
/// results straight in its internal containers.
pub fn parse(source: &str) -> Option<GrammarExtract<'_>> {
    // Leak the input string so the borrowed BbnfDocument<'static> and
    // the resulting GrammarExtract<'_> live for the rest of the
    // compile. Library-internal scratch: the bootstrap flow runs
    // once per compile; observational callers assume 'static lifetimes.
    //
    // AZ-II.cutover.D — BbnfBootstrap::parse now returns
    // crate::runtime::bbnf::BbnfDocument<'_> per the StructDirect
    // resolver-arm flip. The leak preserves the same observational
    // ownership model (LSP analysis, gorgeous JIT, debug_parse).
    let input: &'static str = Box::leak(source.to_owned().into_boxed_str());
    let document = generated::BbnfBootstrap::parse(input).ok()?;
    let document: &'static crate::runtime::bbnf::BbnfDocument<'static> =
        Box::leak(Box::new(document));
    Some(host::extract_observational(document))
}

/// Parse a BBNF grammar file.
///
/// Tranche AC.2: the tape-first `BbnfBootstrap::parse` entry point
/// internally manages its own parser state, so we no longer surface a
/// `ParserState<'_>` to callers. The function is retained for API
/// compatibility with pre-AC call sites as a thin alias over [`parse`];
/// it will be audited and likely removed during AC.3 once the
/// analysis crate migrates off `parser_state.furthest_offset`.
pub fn parse_with_state(source: &str) -> Option<GrammarExtract<'_>> {
    parse(source)
}
