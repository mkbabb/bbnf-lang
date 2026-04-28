//! AZ-II.cutover.E (Phase 2) — [`crate::runtime::RuntimeView`] impl
//! for [`super::MathView`]. Mirror of `CsvView`'s impl.

use crate::runtime::RuntimeView;
use crate::runtime::math::document::{MathDocument, MathKind, MathView};
use crate::runtime::math::value::MathValue;

impl<'a, 'p: 'a> RuntimeView<'p> for MathView<'a, 'p> {
    type Kind = MathKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            MathValue::Span(_) => MathKind::Span,
            MathValue::Unit => MathKind::Unit,
            MathValue::Compound(_) => MathKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            MathValue::Span(s) => Some(s),
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
        MathChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

/// Child iterator for math views.
pub struct MathChildrenIter<'a, 'p: 'a> {
    doc: &'a MathDocument<'p>,
    focus: MathValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for MathChildrenIter<'a, 'p> {
    type Item = MathView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            MathValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(MathView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
