//! Path schema — the abstract shape an executor consumes.
//!
//! [`PathSchema`] is the narrow interface the W3 lazy bail-out parser
//! sees when threading a [`PathCursor`](super::cursor::PathCursor)
//! through generated parse functions. The cursor never speaks to a
//! `TypedPath<G, T>` directly; it speaks to whatever implements
//! [`PathSchema`]. The today-impl is the W2-landed
//! [`TypedPath`](super::ir::TypedPath); the design leaves a seam for
//! future tranches (BB rule-discovery dynamic paths, per-grammar
//! synthesised paths) to plug in without touching the executor.
//!
//! Three obligations the trait imposes:
//!
//! - [`PathSchema::Output`] — the type the executor returns when a
//!   path successfully reaches its terminal. For `TypedPath<G, T>`
//!   this is `T`; for a future dynamic path it might be a borrowed
//!   `&dyn Any` or a per-grammar `Value` enum. The cursor stays
//!   parametric over the choice.
//! - [`PathSchema::segments`] — read-only view onto the path's
//!   segment sequence as borrowed [`PathSegment`]s. The cursor walks
//!   these in order, advancing one step per recursive descent into a
//!   child rule.
//! - [`PathSchema::grammar_marker`] — a `&'static str` naming the
//!   grammar (`"Json"`, `"CssL4"`, `"Sheets"`, `"Bbnf"`). The
//!   per-grammar `parse_with` dispatch routes on this marker; today
//!   it lives as a string for simplicity, and W3.2 carves the route
//!   so the dispatch table is keyed off the marker without a literal
//!   match arm in the runtime hot path.

use super::ir::{OwnedPathSegment, PathSegment, TypedPath};

/// Abstract shape an executor walks.
///
/// Implementors expose a finite segment sequence, a grammar marker
/// for entry-point dispatch, and an output type the executor returns
/// when the path resolves to a terminal value.
///
/// `'p` is the lifetime of borrows the schema hands back — the
/// segments are projected from the schema's owned storage and live
/// only as long as the schema reference itself.
pub trait PathSchema<'p> {
    /// Type the executor produces when a traversal reaches the path's
    /// terminal. For [`TypedPath<G, T>`] this is `T`.
    type Output;

    /// Borrowed view onto the path's segments in left-to-right order.
    ///
    /// The returned vector is a re-projection of the schema's owned
    /// storage; callers that need zero-allocation iteration use
    /// [`PathSchema::segment_count`] + [`PathSchema::segment_at`]
    /// instead.
    fn segments(&'p self) -> Vec<PathSegment<'p>>;

    /// Number of segments without re-projecting.
    fn segment_count(&self) -> usize;

    /// Return the `n`-th segment (zero-indexed) or `None` if out of
    /// range. Re-projects one segment, no allocation.
    fn segment_at(&'p self, n: usize) -> Option<PathSegment<'p>>;

    /// True iff the schema has zero segments (root path).
    fn is_empty(&self) -> bool {
        self.segment_count() == 0
    }

    /// Static name of the grammar this schema resolves against.
    /// Drives the per-grammar `parse_with` route.
    fn grammar_marker() -> &'static str
    where
        Self: Sized;
}

impl<'p, G, T> PathSchema<'p> for TypedPath<G, T>
where
    G: GrammarMarker,
{
    type Output = T;

    fn segments(&'p self) -> Vec<PathSegment<'p>> {
        TypedPath::<G, T>::segments(self)
    }

    fn segment_count(&self) -> usize {
        TypedPath::<G, T>::len(self)
    }

    fn segment_at(&'p self, n: usize) -> Option<PathSegment<'p>> {
        let owned: &OwnedPathSegment = self.owned_segments().get(n)?;
        Some(owned.as_borrowed())
    }

    fn is_empty(&self) -> bool {
        TypedPath::<G, T>::is_empty(self)
    }

    fn grammar_marker() -> &'static str {
        G::MARKER
    }
}

/// Compile-time-known grammar marker name.
///
/// Each W2 marker ZST in [`super::markers`] implements this so
/// [`PathSchema::grammar_marker`] can return a `&'static str` without
/// runtime branching. The trait is internal to the path surface; the
/// W3.2 entry-point dispatch reads the marker through this trait when
/// it routes [`super::executor::PathExecutor::execute`] to the right
/// per-grammar `parse_with`.
pub trait GrammarMarker {
    /// Static lowercase name. Used as the dispatch key.
    const MARKER: &'static str;
}

impl GrammarMarker for super::markers::Json {
    const MARKER: &'static str = "Json";
}

impl GrammarMarker for super::markers::CssL4 {
    const MARKER: &'static str = "CssL4";
}

impl GrammarMarker for super::markers::Sheets {
    const MARKER: &'static str = "Sheets";
}

impl GrammarMarker for super::markers::Bbnf {
    const MARKER: &'static str = "Bbnf";
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::OwnedPathSegment;
    use crate::path::markers::Json;

    #[test]
    fn typed_path_schema_round_trip() {
        let segments = vec![
            OwnedPathSegment::Field("statuses".to_owned()),
            OwnedPathSegment::Index(0),
            OwnedPathSegment::Field("text".to_owned()),
        ];
        let path: TypedPath<Json, &str> = TypedPath::from_owned(segments);

        assert_eq!(path.segment_count(), 3);
        assert_eq!(
            <TypedPath<Json, &str> as PathSchema>::grammar_marker(),
            "Json"
        );
        assert!(!path.is_empty());

        let projected = path.segments();
        assert!(matches!(projected[0], PathSegment::Field("statuses")));
        assert!(matches!(projected[1], PathSegment::Index(0)));
        assert!(matches!(projected[2], PathSegment::Field("text")));

        let one = path.segment_at(1).expect("index 1 exists");
        assert!(matches!(one, PathSegment::Index(0)));
        assert!(path.segment_at(99).is_none());
    }

    #[test]
    fn empty_path_marker() {
        let path: TypedPath<Json, ()> = TypedPath::from_owned(Vec::new());
        assert_eq!(path.segment_count(), 0);
        assert!(path.is_empty());
    }
}
