//! AZ-I.W2-act.close A.fix — [`crate::runtime::RuntimeView`] impl
//! for [`super::CssView`].
//!
//! The CSS L4 struct-direct runtime focuses on a [`CssFocus`] node
//! within a [`CssDocument`]. The trait's `Kind` associated type is
//! the existing [`CssDocumentKind`] discriminator (extended in
//! W2-act.close A.fix to cover sub-tree focuses); `kind()` reports
//! the focused node's typed shape; `span()` returns the borrowed
//! source slice when the focused node has a contiguous span (style
//! rules carry one; declarations / values do not at the per-node
//! level); `input()` returns the full input slice the parse
//! consumed; `children()` walks the focused node's structural
//! children via the document's [`CssArena`].

use crate::runtime::RuntimeView;
use crate::runtime::css_l4::document::{CssDocumentKind, CssFocus, CssView};
use crate::runtime::css_l4::value::{CssRule, CssTypedValue, KeyframeBlock};

impl<'a, 'p: 'a> RuntimeView<'p> for CssView<'a, 'p> {
    type Kind = CssDocumentKind;

    #[inline]
    fn kind(&self) -> Self::Kind {
        CssView::kind(self)
    }

    #[inline]
    fn span(&self) -> Option<&'p str> {
        // Each CSS node-shape carries a single contiguous span when
        // the parse rule projects one. Style rules carry an
        // explicit `span` field; keyframe blocks carry a selector
        // (the closest contiguous span); generic @-rules carry name
        // / prelude / body spans (none of which is the whole rule).
        // Stylesheet roots, declarations, and typed values do not
        // project a single contiguous span at the focus level.
        match self.focus {
            CssFocus::Stylesheet(_) => None,
            CssFocus::Rule(rule) => match rule {
                CssRule::Style(s) => Some(s.span),
                CssRule::Media(_) => None,
                CssRule::Keyframes(_) => None,
                CssRule::GenericAt(_) => None,
            },
            CssFocus::Decl(_) => None,
            CssFocus::Value(value) => match value {
                CssTypedValue::String(s)
                | CssTypedValue::Ident(s)
                | CssTypedValue::Span(s) => Some(*s),
                _ => None,
            },
            CssFocus::KeyframeBlock(block) => Some(block.selector),
        }
    }

    #[inline]
    fn input(&self) -> &'p str {
        self.doc.input
    }

    fn children(&self) -> impl Iterator<Item = Self> + '_ {
        let doc = self.doc;
        let focus = self.focus;
        CssChildrenIter {
            doc,
            focus,
            index: 0,
        }
    }
}

/// AZ-I.W2-act.close A.fix — child iterator for CSS L4 views.
///
/// Walks the focused node's structural children:
/// - [`CssFocus::Stylesheet`] — yields one sub-view per top-level rule.
/// - [`CssFocus::Rule`] (Style)     — yields one sub-view per declaration.
/// - [`CssFocus::Rule`] (Media)     — yields one sub-view per inner rule.
/// - [`CssFocus::Rule`] (Keyframes) — yields one sub-view per keyframe block.
/// - [`CssFocus::Rule`] (Generic)   — yields nothing (children not modelled).
/// - [`CssFocus::Decl`]             — yields the typed value.
/// - [`CssFocus::Value`]            — yields nothing (leaf focus).
/// - [`CssFocus::KeyframeBlock`]    — yields one sub-view per declaration.
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
                let rules = self.doc.rules(sheet.rules);
                let rule = rules.get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Rule(rule)))
            }
            CssFocus::Rule(rule) => match rule {
                CssRule::Style(style) => {
                    let decls = self.doc.decls(style.declarations);
                    let decl = decls.get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
                }
                CssRule::Media(media) => {
                    let inner = self.doc.rules(media.rules);
                    let nested = inner.get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(self.doc, CssFocus::Rule(nested)))
                }
                CssRule::Keyframes(kf) => {
                    let blocks: &'a [KeyframeBlock<'p>] = self.doc.keyframes(kf.blocks);
                    let block = blocks.get(self.index)?;
                    self.index += 1;
                    Some(CssView::focused(
                        self.doc,
                        CssFocus::KeyframeBlock(block),
                    ))
                }
                CssRule::GenericAt(_) => None,
            },
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
                let decls = self.doc.decls(block.declarations);
                let decl = decls.get(self.index)?;
                self.index += 1;
                Some(CssView::focused(self.doc, CssFocus::Decl(decl)))
            }
        }
    }
}
