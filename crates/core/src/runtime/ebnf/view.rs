//! AZ-II.cutover.E (Phase 2) — [`crate::runtime::RuntimeView`] impl
//! for [`super::EbnfView`]. Mirror of `CsvView`'s impl.

use crate::runtime::RuntimeView;
use crate::runtime::ebnf::document::{EbnfDocument, EbnfKind, EbnfView};
use crate::runtime::ebnf::value::EbnfValue;

impl<'a, 'p: 'a> RuntimeView<'p> for EbnfView<'a, 'p> {
    type Kind = EbnfKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            EbnfValue::Span(_) => EbnfKind::Span,
            EbnfValue::Unit => EbnfKind::Unit,
            EbnfValue::Compound(_) => EbnfKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            EbnfValue::Span(s) => Some(s),
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
        EbnfChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

pub struct EbnfChildrenIter<'a, 'p: 'a> {
    doc: &'a EbnfDocument<'p>,
    focus: EbnfValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for EbnfChildrenIter<'a, 'p> {
    type Item = EbnfView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            EbnfValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(EbnfView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
