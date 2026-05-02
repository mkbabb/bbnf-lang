//! AZ-IV.W3.2 — BBNF `parse_with` entry point.
//!
//! Lazy bail-out parse surface for the BBNF self-host grammar.
//! Constructs a [`PathCursor`] over a [`PathSchema`] (today only
//! [`TypedPath<Bbnf, T>`](crate::path::TypedPath)), wires the cursor's
//! decision-lookup closure to the codegen-emitted
//! `__path_plan::lookup` static-search, and hands the cursor to
//! [`PathExecutor::execute`]. The executor's parse-fn closure runs the
//! existing eager [`BbnfBootstrap::parse`] and projects the path's
//! leaf through [`BbnfDocument::get`] using the `BbnfPathQuery` trait
//! family the document already owns.
//!
//! BBNF compounds are positional, so the cursor sees `Index` steps
//! exclusively in well-formed paths; `Field` steps against a BBNF
//! compound resolve to `None` per `BbnfPathQuery::query`.

use super::document::{BbnfDocument, BbnfPathQuery};
use crate::grammar::generated::bbnf::{__path_plan, BbnfBootstrap};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Bbnf;
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
/// [`BbnfPathQuery`] trait.
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
        |src, _cursor| {
            let doc: BbnfDocument<'_> = BbnfBootstrap::parse(src).ok()?;
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
