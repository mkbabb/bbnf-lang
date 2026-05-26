use crate::runtime::RuntimeView;
use crate::runtime::css_l4::document::{CssDocumentKind, CssFocus, CssView};
use crate::runtime::css_l4::value::{CssRule, CssTypedValue, KeyframeBlock};
impl<'a, 'p: 'a> RuntimeView<'p> for CssView<'a, 'p> {
    type Kind = CssDocumentKind;
    fn kind(&self) -> Self::Kind {
        CssView::kind(self)
    }
    fn span(&self) -> Option<&'p str> {
        match self.focus {
            CssFocus::Stylesheet(_) => None,
            CssFocus::Rule(rule) => {
                match rule {
                    CssRule::Style(s) => Some(s.span),
                    CssRule::Media(_) | CssRule::Keyframes(_) | CssRule::GenericAt(_) => {
                        None
                    }
                }
            }
            CssFocus::Decl(_) => None,
            CssFocus::Value(value) => {
                match value {
                    CssTypedValue::String(s)
                    | CssTypedValue::Ident(s)
                    | CssTypedValue::Span(s) => Some(*s),
                    _ => None,
                }
            }
            CssFocus::KeyframeBlock(block) => Some(block.selector),
        }
    }
    fn input(&self) -> &'p str {
        self.doc.input
    }
    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        CssChildrenIter {
            doc: self.doc,
            focus: self.focus,
            index: 0,
        }
    }
}
pub struct CssChildrenIter<'a, 'p: 'a> {
    doc: &'a crate::runtime::css_l4::CssDocument<'p>,
    focus: CssFocus<'a, 'p>,
    index: usize,
}
impl<'a, 'p: 'a> Iterator for CssChildrenIter<'a, 'p> {
    type Item = CssView<'a, 'p>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.focus {
            CssFocus::Stylesheet(sheet) => {
                let rule = self.doc.rules(sheet.rules).get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Rule(rule)))
            }
            CssFocus::Rule(rule) => {
                match rule {
                    CssRule::Style(style) => {
                        let decl = self.doc.decls(style.declarations).get(self.index)?;
                        self.index += 1;
                        Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
                    }
                    CssRule::Media(media) => {
                        let nested = self.doc.rules(media.rules).get(self.index)?;
                        self.index += 1;
                        Some(CssView::focused(self.doc, CssFocus::Rule(nested)))
                    }
                    CssRule::Keyframes(kf) => {
                        let blocks: &'a [KeyframeBlock<'p>] = self
                            .doc
                            .keyframes(kf.blocks);
                        let block = blocks.get(self.index)?;
                        self.index += 1;
                        Some(CssView::focused(self.doc, CssFocus::KeyframeBlock(block)))
                    }
                    CssRule::GenericAt(_) => None,
                }
            }
            CssFocus::Decl(decl) => {
                if self.index == 0 {
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Value(&decl.value)))
                } else {
                    None
                }
            }
            CssFocus::Value(_) => None,
            CssFocus::KeyframeBlock(block) => {
                let decl = self.doc.decls(block.declarations).get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
            }
        }
    }
}
