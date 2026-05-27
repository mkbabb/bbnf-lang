use crate::runtime::RuntimeView;
use crate::runtime::google_sheets::document::{SheetsKind, SheetsView};
use crate::runtime::google_sheets::value::SheetsValue;
impl<'a, 'p: 'a> RuntimeView<'p> for SheetsView<'a, 'p> {
    type Kind = SheetsKind;
    #[inline]
    fn kind(&self) -> Self::Kind {
        match self.focus {
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
    #[inline]
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            SheetsValue::String(s)
            | SheetsValue::CellRef(s)
            | SheetsValue::Identifier(s)
            | SheetsValue::SheetPrefix { text: s, .. } => Some(s),
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
        SheetsChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}
pub struct SheetsChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::google_sheets::SheetsDocument<'p>,
    focus: SheetsValue<'p>,
    index: usize,
}
impl<'a, 'p: 'a> Iterator for SheetsChildrenIter<'a, 'p> {
    type Item = SheetsView<'a, 'p>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            SheetsValue::Compound(id) => {
                let entry = self.doc.compound(id);
                let item = entry.children.get(self.index)?;
                self.index += 1;
                Some(SheetsView::focused(self.doc, *item))
            }
            _ => None,
        }
    }
}
