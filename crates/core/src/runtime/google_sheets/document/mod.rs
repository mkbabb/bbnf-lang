//! AZ-I.W2-act.B2 — `SheetsDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct Sheets parse path returns a [`SheetsDocument`]
//! whose root [`SheetsValue`] borrows from the input lifetime `'p`
//! and whose [`SheetsArena`] owns every compound child slice. This
//! module wraps the document with the same API the JSON runtime
//! exposes (per W2-act.A's accessor contract): `view`, `to_value`,
//! `get::<T>(path)`.
//!
//! The accessor surface mirrors `JsonDocument`; consumers writing
//! against either path observe a uniform shape across the three
//! data grammars.
//!
//! AZ-IV.AUDIT-B — split from the prior monolithic `document.rs`
//! (732 LOC) into a directory module with one sub-file per concern:
//!
//! - [`mod@canonical`] — canonical-form serializer (`serialize_compact`
//!   and its `write_value`/`write_compound`/`write_func_call`
//!   walkers; `error_lexeme`/`tag_lexeme` operator-tag projections).
//! - [`mod@view`] — borrowed view newtype ([`SheetsView`],
//!   [`SheetsKind`]).
//! - [`mod@path_query`] — typed path-query trait ([`SheetsPathQuery`])
//!   and its impls for `f64`/`bool`/`u8`/`&str`/`SheetsValue`, plus
//!   the shared `walk_path` helper.
//!
//! All previously-public API (`SheetsDocument`, `SheetsKind`,
//! `SheetsPathQuery`, `SheetsView`) is preserved verbatim by
//! `pub use`-re-exports below.

pub mod canonical;
pub mod path_query;
pub mod view;

use crate::runtime::google_sheets::arena::{SheetsArena, SheetsCompoundId, SheetsCompoundView};
use crate::runtime::google_sheets::value::SheetsValue;
use crate::runtime::path::Path;

pub use self::path_query::SheetsPathQuery;
pub use self::view::{SheetsKind, SheetsView};

/// The root document returned by
/// `bbnf::grammar::generated::google_sheets::GoogleSheetsParser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root value. Borrows the input bytes via the `'p` lifetime.
#[derive(Debug)]
pub struct SheetsDocument<'p> {
    /// The compound child arena — owns every `[SheetsValue]` slice
    /// the document references via handles.
    pub arena: SheetsArena<'p>,
    /// The root value of the document.
    pub root: SheetsValue<'p>,
    /// AZ-I.W2-act.close A.fix — the input slice the parse consumed.
    /// Threaded through `finalise(input)` so [`SheetsView`] can satisfy
    /// the `RuntimeView::input()` surface without re-acquiring the
    /// source from the call site.
    pub input: &'p str,
}

impl<'p> SheetsDocument<'p> {
    /// Construct a document from a populated arena, root value, and
    /// the input slice the parse consumed. The typical caller is the
    /// generated parse function; consumers outside the emitter rarely
    /// build a `SheetsDocument` directly.
    #[inline]
    pub fn new(arena: SheetsArena<'p>, root: SheetsValue<'p>, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Borrow the underlying [`SheetsArena`].
    #[inline]
    pub fn arena(&self) -> &SheetsArena<'p> {
        &self.arena
    }

    /// AZ-I.W2-act.close A.fix — borrow the input slice the parse
    /// consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a [`SheetsCompoundId`] handle to the compound entry
    /// (kind + child slice).
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'_, 'p> {
        self.arena.compound(id)
    }

    /// Borrowed root view, mirroring the
    /// `JsonDocument::view()` surface.
    #[inline]
    pub fn view<'a>(&'a self) -> SheetsView<'a, 'p> {
        SheetsView::focused(self, self.root)
    }

    /// Borrowed root value, mirroring `JsonDocument::to_value()`
    /// semantics. The struct-direct path's [`SheetsDocument`]
    /// already carries the typed value tree, so `to_value()` simply
    /// lends its root by reference.
    #[inline]
    pub fn to_value(&self) -> &SheetsValue<'p> {
        &self.root
    }

    /// Typed path query, mirroring `JsonDocument::get::<T>(path)`
    /// semantics.
    ///
    /// The walker descends from `doc.root()` following
    /// [`crate::runtime::path::PathSegment::Index`] steps against
    /// [`SheetsValue::Compound`] child slices. There is no
    /// field-keyed step in Sheets's grammar (compounds are
    /// positional, not keyed); a `PathSegment::Field` step against a
    /// Sheets compound returns `None`.
    #[inline]
    pub fn get<T: SheetsPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }

    /// AZ-I.W2-act.close B2 — canonical compact serialization of the
    /// struct-tree.
    ///
    /// Walks the [`SheetsValue`] tree depth-first and emits a string
    /// whose tokens reproduce the grammar's surface syntax. Borrowed
    /// leaves (`String`, `CellRef`, `Identifier`, `SheetPrefix`) emit
    /// their borrowed slice verbatim; numeric / bool / tag projections
    /// emit the canonical lexeme matching the grammar's declaration
    /// order (`true` -> `TRUE`, `Tag(0)` inside `AddExpr` -> `+`, etc.);
    /// compound rules emit their children with the structural
    /// separators the grammar requires (commas inside arg-lists,
    /// `:` between range endpoints, `(` `)` around paren-expr,
    /// `{` `}` around array-literal, `;` between array rows).
    ///
    /// Pre-W2-act this surface lived as
    /// `GoogleSheetsParser::serialize_compact(node)` against the
    /// cursor-backed [`::tape::TapeCursor`]; that
    /// emitter retired alongside the tape substrate when the
    /// struct-direct flip activated. The struct-tree walker is the
    /// substrate-with-consumer authentic equivalent.
    pub fn serialize_compact(&self) -> String {
        canonical::serialize_compact(self)
    }
}
