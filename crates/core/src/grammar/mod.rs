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

pub mod bootstrap_parser;
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
/// The bootstrap parser returns a concrete BBNF document. This entry
/// point leaks the input and document so the resulting
/// `GrammarExtract<'_>` — which borrows cursors and text slices from
/// the document — lives for the rest of the compile, matching the
/// pre-AU.4.1 arena-style ownership model observational callers (LSP
/// analysis, gorgeous JIT, `debug_parse`) already rely on.
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
    // AZ-II.cutover.H Phase 1 — route through the hand-written
    // bootstrap parser. The post-cutover.H regen output compiles
    // and the BBNF self-parity tests pass against the bootstrap
    // parser, but the regen-derived `BbnfBootstrap::parse` itself
    // does not yet self-parse (cutover.G's chicken-and-egg parser
    // covers compound shape coercion the codegen-emitted parser
    // doesn't yet reproduce). cutover.H Phase 1 retains the
    // bootstrap parser as the canonical entry point; the codegen
    // parser self-host is a deferred follow-up.
    let document = bootstrap_parser::parse(input).ok()?;
    let document: &'static crate::runtime::bbnf::BbnfDocument<'static> =
        Box::leak(Box::new(document));
    Some(host::extract_observational(document))
}
