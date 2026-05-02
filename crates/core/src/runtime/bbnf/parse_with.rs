//! AZ-IV.W3.7 — BBNF `parse_with` entry point (cursor-threaded).
//!
//! Lazy bail-out parse surface for the BBNF self-host grammar.
//! Constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Bbnf, T>`](crate::path::TypedPath)), wires the cursor's
//! decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and threads the cursor
//! through the cursor-aware generated dispatcher
//! [`parse_BbnfBootstrap_grammar`](crate::grammar::generated::bbnf::parse_BbnfBootstrap_grammar).
//! Subtrees the path does not visit are byte-skipped and never push
//! records into the [`BbnfStructBuilder`]. After the dispatcher
//! returns, the builder is finalised against `input` and the leaf is
//! projected through [`BbnfDocument::get`].
//!
//! BBNF compounds are positional, so the cursor sees `Index` steps
//! exclusively in well-formed paths; `Field` steps against a BBNF
//! compound resolve to `None` per `BbnfPathQuery::query`.

use super::document::{BbnfDocument, BbnfPathQuery};
use crate::grammar::generated::bbnf::{
    __path_plan, __shape_support_BbnfBootstrap, parse_BbnfBootstrap_grammar,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Bbnf;
use crate::runtime::bbnf::BbnfStructBuilder;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};

/// Lower a typed-path segment into the legacy borrowed alphabet the
/// document's `get` consumes. `Wildcard` returns `None`.
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}

/// Run a path-driven BBNF parse and project the leaf at `path`.
///
/// `T` is the leaf type; the bound is the document-owned
/// [`BbnfPathQuery`] trait. Returns `None` when the parse fails
/// inside the path's reach, the path falls outside the document, or
/// any segment fails to resolve. Parse errors past the path's reach
/// are silently elided — the lazy contract.
pub fn parse_with<T>(input: &str, path: &TypedPath<Bbnf, T>) -> Option<T>
where
    T: BbnfPathQuery,
{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        },
        |src, cursor| {
            let mut state = __shape_support_BbnfBootstrap::ScanState::new();
            let mut builder = BbnfStructBuilder::new();
            let mut pos: usize = 0;
            parse_BbnfBootstrap_grammar(src.as_bytes(), &mut pos, &mut state, &mut builder, cursor)
                .ok()?;
            let doc: BbnfDocument<'_> = builder.finalise(src);
            let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                legacy.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(LegacyPath::new(&legacy))
        },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::OwnedPathSegment;
    use crate::runtime::bbnf::value::BbnfValue;

    #[test]
    fn parse_with_resolves_root_value() {
        // Empty path returns the root BbnfValue. Use BbnfValue as the
        // identity leaf; minimal grammar that parses cleanly.
        let path: TypedPath<Bbnf, BbnfValue<'_>> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<BbnfValue<'_>>("a = b ;\n", &path);
        assert!(out.is_some(), "BBNF root should resolve as identity");
    }

    #[test]
    fn parse_with_returns_none_on_invalid_input() {
        let path: TypedPath<Bbnf, BbnfValue<'_>> = TypedPath::from_owned(Vec::new());
        let out = parse_with::<BbnfValue<'_>>("@@@ not bbnf @@@", &path);
        assert!(out.is_none());
    }
}
