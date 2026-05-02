//! Borrowed view newtype for [`super::SheetsDocument`].
//!
//! AZ-I.W2-act.B2 — `SheetsView` is a thin newtype over
//! `&SheetsDocument` mirroring `JsonView`; the two-lifetime parameter
//! shape preserves compositional invariance through the arena's
//! `Vec<SheetsValue<'p>>` owner.
//!
//! [`SheetsKind`] is the discriminator the [`crate::runtime::RuntimeView`]
//! impl (in `runtime/google_sheets/view.rs`) consumes via its
//! `Kind` associated type.

use crate::runtime::google_sheets::arena::{SheetsArena, SheetsCompoundId, SheetsCompoundView};
use crate::runtime::google_sheets::value::SheetsValue;

use super::SheetsDocument;

/// AZ-I.W2-act.B2 — a thin newtype over `&SheetsDocument`.
///
/// Mirrors `JsonView`; the two-lifetime parameter shape preserves
/// compositional invariance through the arena's `Vec<SheetsValue<'p>>`
/// owner.
#[derive(Debug, Clone, Copy)]
pub struct SheetsView<'a, 'p: 'a> {
    pub(crate) doc: &'a SheetsDocument<'p>,
    /// AZ-I.W2-act.close A.fix — the focused [`SheetsValue`] this view
    /// observes. Defaults to `doc.root` for `SheetsDocument::view()`;
    /// `RuntimeView::children()` yields views with the same `doc` but
    /// a different focus.
    pub(crate) focus: SheetsValue<'p>,
}

impl<'a, 'p: 'a> SheetsView<'a, 'p> {
    /// Construct a view focused on a specific [`SheetsValue`] within
    /// the document.
    #[inline]
    pub fn focused(doc: &'a SheetsDocument<'p>, focus: SheetsValue<'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a SheetsDocument<'p> {
        self.doc
    }

    /// AZ-I.W2-act.close A.fix — the focused [`SheetsValue`] this view
    /// observes (root for top-level views; sub-tree for descendants
    /// produced by `children()`).
    #[inline]
    pub fn focus(&self) -> SheetsValue<'p> {
        self.focus
    }

    /// Borrow the root [`SheetsValue`].
    #[inline]
    pub fn root(&self) -> &'a SheetsValue<'p> {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a SheetsArena<'p> {
        &self.doc.arena
    }

    /// Resolve a compound handle through the document's arena.
    #[inline]
    pub fn compound(&self, id: SheetsCompoundId) -> SheetsCompoundView<'a, 'p> {
        self.doc.compound(id)
    }

    /// Discriminator over the focused value's typed shape.
    #[inline]
    pub fn kind(&self) -> SheetsKind {
        match &self.focus {
            SheetsValue::Number(_) => SheetsKind::Number,
            SheetsValue::String(_) => SheetsKind::String,
            SheetsValue::Bool(_) => SheetsKind::Bool,
            SheetsValue::Error(_) => SheetsKind::Error,
            SheetsValue::CellRef(_) => SheetsKind::CellRef,
            SheetsValue::Identifier(_) => SheetsKind::Identifier,
            SheetsValue::SheetPrefix { .. } => SheetsKind::SheetPrefix,
            SheetsValue::Tag(_) => SheetsKind::Tag,
            SheetsValue::Compound(_) => SheetsKind::Compound,
        }
    }

    /// `true` iff the focused value is a compound (any non-leaf rule).
    #[inline]
    pub fn is_compound(&self) -> bool {
        matches!(self.focus, SheetsValue::Compound(_))
    }

    /// `true` iff the focused value is a number.
    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self.focus, SheetsValue::Number(_))
    }

    /// `true` iff the focused value is a string-shaped leaf (string /
    /// cell_ref / identifier / sheet_prefix text).
    #[inline]
    pub fn is_string(&self) -> bool {
        matches!(
            self.focus,
            SheetsValue::String(_)
                | SheetsValue::CellRef(_)
                | SheetsValue::Identifier(_)
                | SheetsValue::SheetPrefix { .. }
        )
    }
}

/// Discriminator over the typed shapes a [`SheetsValue`] takes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SheetsKind {
    /// `number = /…/ -> f64`.
    Number,
    /// `string = /"…"/`.
    String,
    /// `boolean = /TRUE/i | /FALSE/i`.
    Bool,
    /// `error_literal = "#N/A" -> 0u8 | …`.
    Error,
    /// `cell_ref = /…/`.
    CellRef,
    /// `identifier = /…/`.
    Identifier,
    /// `sheet_prefix` projection.
    SheetPrefix,
    /// Operator-tag projection (`compare_op`, `add_op`, etc.).
    Tag,
    /// Compound shape — any non-leaf rule.
    Compound,
}
