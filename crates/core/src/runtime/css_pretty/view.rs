//! AZ-II.cutover.E (Phase 2) — [`crate::runtime::RuntimeView`] impl
//! for [`super::CssPrettyView`]. Mirror of `CsvView`'s impl.

use crate::runtime::RuntimeView;
use crate::runtime::css_pretty::document::{CssPrettyDocument, CssPrettyKind, CssPrettyView};
use crate::runtime::css_pretty::value::CssPrettyValue;

impl<'a, 'p: 'a> RuntimeView<'p> for CssPrettyView<'a, 'p> {
    type Kind = CssPrettyKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
            CssPrettyValue::Span(_) => CssPrettyKind::Span,
            CssPrettyValue::Unit => CssPrettyKind::Unit,
            CssPrettyValue::Compound(_) => CssPrettyKind::Compound,
        }
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            CssPrettyValue::Span(s) => Some(s),
            _ => None,
        }
    }

    #[inline]
    fn input(&self) -> &'p str { self.doc.input }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        CssPrettyChildrenIter { doc, focus, index: 0 }
    }
}

pub struct CssPrettyChildrenIter<'a, 'p: 'a> {
    doc: &'a CssPrettyDocument<'p>,
    focus: CssPrettyValue<'p>,
    index: usize,
}

impl<'a, 'p: 'a> Iterator for CssPrettyChildrenIter<'a, 'p> {
    type Item = CssPrettyView<'a, 'p>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            CssPrettyValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(CssPrettyView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
