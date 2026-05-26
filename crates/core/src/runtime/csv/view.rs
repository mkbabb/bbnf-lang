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
