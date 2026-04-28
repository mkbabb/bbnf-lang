//! AZ-II.cutover.A — [`crate::runtime::RuntimeView`] impl for
//! [`super::BbnfView`].
//!
//! The BBNF struct-direct runtime focuses on a [`BbnfValue`] within a
//! [`BbnfDocument`]; the trait's `Kind` associated type is the
//! existing [`BbnfKind`] discriminator. `kind()` reports the focused
//! value's typed shape; `span()` returns the borrowed source slice
//! for [`BbnfValue::Span`] focuses; `input()` returns the full input
//! the parse consumed; `children()` walks the focused compound's
//! structural children.

use crate::runtime::bbnf::document::{BbnfKind, BbnfView};
use crate::runtime::bbnf::value::BbnfValue;
use crate::runtime::RuntimeView;

impl<'a, 'p: 'a> RuntimeView<'p> for BbnfView<'a, 'p> {
    type Kind = BbnfKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            BbnfValue::Int(_) => BbnfKind::Int,
            BbnfValue::Float(_) => BbnfKind::Float,
            BbnfValue::Bool(_) => BbnfKind::Bool,
            BbnfValue::Span(_) => BbnfKind::Span,
            BbnfValue::Tag(_) => BbnfKind::Tag,
            BbnfValue::Unit => BbnfKind::Unit,
            BbnfValue::Compound(_) => BbnfKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        // Span leaves carry the borrowed input slice; other shapes
        // do not project to a single contiguous source slice.
        match self.focus {
            BbnfValue::Span(s) => Some(s),
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
        BbnfChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

/// Child iterator for BBNF views.
///
/// Compound focuses yield one sub-view per child entry, in source
/// order. Leaf shapes yield nothing.
pub struct BbnfChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::bbnf::BbnfDocument<'p>,
    focus: BbnfValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for BbnfChildrenIter<'a, 'p> {
    type Item = BbnfView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            BbnfValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(BbnfView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
