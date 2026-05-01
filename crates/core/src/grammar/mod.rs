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
//! Tranche AU.4.1: deleted the historical `ParsedGrammar` intermediate.
//! The public `parse` entry point leaks the concrete BBNF document so
//! callers can keep their `'static`-flavoured lifetime assumptions,
//! then returns a [`crate::types::GrammarExtract`] for observational
//! callers. The compile pipeline bypasses `parse` in favour of
//! [`crate::pipeline::directives::parse_to_pipeline_inputs`], which
//! walks the tape straight into `DirectiveMaps` + `AST`.
//!
//! AZ-III.W2.4: BBNF self-host is canonical. `parse` and the
//! pipeline entry point both call the codegen-emitted
//! `generated::BbnfBootstrap::parse`. The hand-written bootstrap
//! parser was deleted; structural dispatch in
//! `crate::lower::expression::lower_term` consumes the alt_dispatch
//! Term compound's `branch_tag` so `lower_term` no longer relies on
//! synthetic punctuator spans the hand-written parser used to push.

#[allow(
    unused,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    clippy::all
)]
pub mod generated;
pub mod host;
pub mod schema;

use crate::types::GrammarExtract;

/// Parse a BBNF grammar source into a [`GrammarExtract`].
///
/// The codegen-emitted `BbnfBootstrap::parse` returns a concrete BBNF
/// document. This entry point leaks the input and document so the
/// resulting `GrammarExtract<'_>` — which borrows cursors and text
/// slices from the document — lives for the rest of the compile,
/// matching the pre-AU.4.1 arena-style ownership model observational
/// callers (LSP analysis, gorgeous JIT, `debug_parse`) already rely on.
///
/// Compile-side callers should instead route through
/// [`crate::pipeline`]; the pipeline avoids this allocation by landing
/// results straight in its internal containers.
pub fn parse(source: &str) -> Option<GrammarExtract<'_>> {
    // Leak the input string so the borrowed BbnfDocument<'static> and
    // the resulting GrammarExtract<'_> live for the rest of the
    // compile. Library-internal scratch: the bootstrap flow runs
    // once per compile; observational callers assume 'static lifetimes.
    let input: &'static str = Box::leak(source.to_owned().into_boxed_str());
    // AZ-III.W2.4: canonical self-host — generated::BbnfBootstrap::parse
    // is the bootstrap entry point. The codegen path emits the alt
    // branch_tag for the `term` compound; lower_term dispatches on
    // that tag instead of relying on the hand-written parser's
    // synthetic punctuator spans.
    let document = generated::BbnfBootstrap::parse(input).ok()?;
    let document: &'static crate::runtime::bbnf::BbnfDocument<'static> =
        Box::leak(Box::new(document));
    Some(host::extract_observational(document))
}
