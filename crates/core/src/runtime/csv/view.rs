//! AZ-II.cutover.E (Phase 2) — [`crate::runtime::RuntimeView`] impl
//! for [`super::CsvView`].
//!
//! Mirrors the JSON / Sheets / BBNF view trait impls. The CSV
//! struct-direct runtime focuses on a [`CsvValue`] within a
//! [`CsvDocument`]. The trait's `Kind` associated type is the existing
//! [`CsvKind`] discriminator; `kind()` reports the focused value's
//! typed shape; `span()` returns the borrowed source slice for
//! `Span` focuses; `input()` returns the full input slice the parse
//! consumed; `children()` walks the focused compound's structural
//! children in source order via the document's [`CsvArena`].

use crate::runtime::RuntimeView;
use crate::runtime::csv::document::{CsvDocument, CsvKind, CsvView};
use crate::runtime::csv::value::CsvValue;

impl<'a, 'p: 'a> RuntimeView<'p> for CsvView<'a, 'p> {
    type Kind = CsvKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            CsvValue::Span(_) => CsvKind::Span,
            CsvValue::Unit => CsvKind::Unit,
            CsvValue::Compound(_) => CsvKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        // Span focuses carry a borrowed source slice; compounds and
        // units do not project to a single contiguous source slice.
        match self.focus {
            CsvValue::Span(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        CsvChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

/// Child iterator for CSV views.
///
/// Walks the focused value's structural children in source order:
/// [`CsvValue::Compound`] yields one sub-view per element of the
/// compound's child slice. Leaf shapes (spans, units) yield nothing.
pub struct CsvChildrenIter<'a, 'p: 'a> {
    doc: &'a CsvDocument<'p>,
    focus: CsvValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for CsvChildrenIter<'a, 'p> {
    type Item = CsvView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            CsvValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(CsvView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
