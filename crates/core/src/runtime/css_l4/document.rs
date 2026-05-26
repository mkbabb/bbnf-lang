use crate::runtime::css_l4::arena::{
    CssArena, CssDeclListId, CssKeyframeListId, CssRuleListId, CssSelectorListId,
    CssValueListId,
};
use crate::runtime::css_l4::value::{
    CssRule, CssTypedValue, Declaration, KeyframeBlock, Selector, StyleSheet,
};
use crate::runtime::path::{Path, PathSegment};
#[derive(Debug)]
pub struct CssDocument<'p> {
    pub arena: CssArena<'p>,
    pub root: StyleSheet,
    pub input: &'p str,
}
impl<'p> CssDocument<'p> {
    pub fn new(arena: CssArena<'p>, root: StyleSheet, input: &'p str) -> Self {
        Self { arena, root, input }
    }
    pub fn root(&self) -> &StyleSheet {
        &self.root
    }
    pub fn arena(&self) -> &CssArena<'p> {
        &self.arena
    }
    pub fn input(&self) -> &'p str {
        self.input
    }
    pub fn rules(&self, id: CssRuleListId) -> &[CssRule<'p>] {
        self.arena.rules(id)
    }
    pub fn decls(&self, id: CssDeclListId) -> &[Declaration<'p>] {
        self.arena.decls(id)
    }
    pub fn selectors(&self, id: CssSelectorListId) -> &[Selector<'p>] {
        self.arena.selectors(id)
    }
    pub fn values(&self, id: CssValueListId) -> &[CssTypedValue<'p>] {
        self.arena.values(id)
    }
    pub fn keyframes(&self, id: CssKeyframeListId) -> &[KeyframeBlock<'p>] {
        self.arena.keyframes(id)
    }
    pub fn view<'a>(&'a self) -> CssView<'a, 'p> {
        CssView {
            doc: self,
            focus: CssFocus::Stylesheet(&self.root),
        }
    }
    pub fn to_value(&self) -> &StyleSheet {
        &self.root
    }
    pub fn get<T: CssPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }
    pub fn walk_declarations(&self) -> CssDeclWalk<'_, 'p> {
        CssDeclWalk {
            doc: self,
            stack: vec![CssWalkItem::RuleList(self.root.rules, 0)],
        }
    }
    pub fn walk_values<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a {
        self.walk_declarations()
            .flat_map(|decl| {
                let property = decl.property;
                let primary = std::iter::once((property, &decl.value));
                let list_extra: Box<
                    dyn Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a,
                > = match &decl.value {
                    CssTypedValue::List(id) => {
                        Box::new(self.values(*id).iter().map(move |v| (property, v)))
                    }
                    _ => Box::new(std::iter::empty()),
                };
                primary.chain(list_extra)
            })
    }
}
#[derive(Debug)]
enum CssWalkItem {
    RuleList(CssRuleListId, usize),
    DeclList(CssDeclListId, usize),
    KeyframeList(CssKeyframeListId, usize),
}
pub struct CssDeclWalk<'a, 'p: 'a> {
    doc: &'a CssDocument<'p>,
    stack: Vec<CssWalkItem>,
}
impl<'a, 'p: 'a> Iterator for CssDeclWalk<'a, 'p> {
    type Item = &'a Declaration<'p>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let top = self.stack.last_mut()?;
            match top {
                CssWalkItem::RuleList(id, idx) => {
                    let rules = self.doc.rules(*id);
                    if let Some(rule) = rules.get(*idx) {
                        *idx += 1;
                        match rule {
                            CssRule::Style(style) => {
                                self.stack
                                    .push(CssWalkItem::DeclList(style.declarations, 0))
                            }
                            CssRule::Media(media) => {
                                self.stack.push(CssWalkItem::RuleList(media.rules, 0))
                            }
                            CssRule::Keyframes(kf) => {
                                self.stack.push(CssWalkItem::KeyframeList(kf.blocks, 0))
                            }
                            CssRule::GenericAt(_) => {}
                        }
                    } else {
                        self.stack.pop();
                    }
                }
                CssWalkItem::DeclList(id, idx) => {
                    let decls = self.doc.decls(*id);
                    if let Some(decl) = decls.get(*idx) {
                        *idx += 1;
                        return Some(decl);
                    }
                    self.stack.pop();
                }
                CssWalkItem::KeyframeList(id, idx) => {
                    let blocks = self.doc.keyframes(*id);
                    if let Some(block) = blocks.get(*idx) {
                        *idx += 1;
                        self.stack.push(CssWalkItem::DeclList(block.declarations, 0));
                    } else {
                        self.stack.pop();
                    }
                }
            }
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub struct CssView<'a, 'p: 'a> {
    pub(crate) doc: &'a CssDocument<'p>,
    pub(crate) focus: CssFocus<'a, 'p>,
}
#[derive(Debug, Clone, Copy)]
pub enum CssFocus<'a, 'p: 'a> {
    Stylesheet(&'a StyleSheet),
    Rule(&'a CssRule<'p>),
    Decl(&'a Declaration<'p>),
    Value(&'a CssTypedValue<'p>),
    KeyframeBlock(&'a KeyframeBlock<'p>),
}
impl<'a, 'p: 'a> CssView<'a, 'p> {
    pub fn focused(doc: &'a CssDocument<'p>, focus: CssFocus<'a, 'p>) -> Self {
        Self { doc, focus }
    }
    pub fn document(&self) -> &'a CssDocument<'p> {
        self.doc
    }
    pub fn focus(&self) -> CssFocus<'a, 'p> {
        self.focus
    }
    pub fn root(&self) -> &'a StyleSheet {
        &self.doc.root
    }
    pub fn arena(&self) -> &'a CssArena<'p> {
        &self.doc.arena
    }
    pub fn rules(&self, id: CssRuleListId) -> &'a [CssRule<'p>] {
        self.doc.rules(id)
    }
    pub fn decls(&self, id: CssDeclListId) -> &'a [Declaration<'p>] {
        self.doc.decls(id)
    }
    pub fn selectors(&self, id: CssSelectorListId) -> &'a [Selector<'p>] {
        self.doc.selectors(id)
    }
    pub fn values(&self, id: CssValueListId) -> &'a [CssTypedValue<'p>] {
        self.doc.values(id)
    }
    pub fn keyframes(&self, id: CssKeyframeListId) -> &'a [KeyframeBlock<'p>] {
        self.doc.keyframes(id)
    }
    pub fn kind(&self) -> CssDocumentKind {
        match self.focus {
            CssFocus::Stylesheet(sheet) => {
                if sheet.rules.is_empty() {
                    CssDocumentKind::Empty
                } else {
                    CssDocumentKind::StyleSheet
                }
            }
            CssFocus::Rule(_) => CssDocumentKind::Rule,
            CssFocus::Decl(_) => CssDocumentKind::Declaration,
            CssFocus::Value(_) => CssDocumentKind::Value,
            CssFocus::KeyframeBlock(_) => CssDocumentKind::KeyframeBlock,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CssDocumentKind {
    Empty,
    StyleSheet,
    Rule,
    Declaration,
    Value,
    KeyframeBlock,
}
pub trait CssPathQuery: Sized {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self>;
}
enum CssWalkCursor<'a, 'p> {
    Sheet(&'a StyleSheet, &'a CssArena<'p>),
    Rule(&'a CssRule<'p>, &'a CssArena<'p>),
    Decl(&'a Declaration<'p>, &'a CssArena<'p>),
    Value(&'a CssTypedValue<'p>, #[allow(dead_code)] &'a CssArena<'p>),
}
fn walk_path<'a, 'p>(
    doc: &'a CssDocument<'p>,
    path: Path<'_>,
) -> Option<CssWalkCursor<'a, 'p>> {
    let mut current = CssWalkCursor::Sheet(&doc.root, &doc.arena);
    for segment in path.iter() {
        current = match (current, segment) {
            (CssWalkCursor::Sheet(sheet, arena), PathSegment::Index(idx)) => {
                CssWalkCursor::Rule(arena.rules(sheet.rules).get(*idx)?, arena)
            }
            (CssWalkCursor::Rule(rule, arena), PathSegment::Index(idx)) => {
                match rule {
                    CssRule::Style(style) => {
                        CssWalkCursor::Decl(
                            arena.decls(style.declarations).get(*idx)?,
                            arena,
                        )
                    }
                    CssRule::Media(media) => {
                        CssWalkCursor::Rule(arena.rules(media.rules).get(*idx)?, arena)
                    }
                    _ => return None,
                }
            }
            (
                CssWalkCursor::Decl(decl, arena),
                PathSegment::Field(name),
            ) if *name == "value" => CssWalkCursor::Value(&decl.value, arena),
            _ => return None,
        };
    }
    Some(current)
}
impl CssPathQuery for &str {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            CssWalkCursor::Decl(decl, _) => {
                let extended: &'p str = decl.property;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            CssWalkCursor::Value(value, _) => {
                match value {
                    CssTypedValue::String(s)
                    | CssTypedValue::Ident(s)
                    | CssTypedValue::Span(s) => {
                        let extended: &'p str = *s;
                        Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
impl CssPathQuery for f64 {
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        match walk_path(doc, path)? {
            CssWalkCursor::Value(value, _) => {
                match value {
                    CssTypedValue::Number(n) => Some(*n),
                    CssTypedValue::Dimension(d) => {
                        Some(
                            match d {
                                crate::runtime::css_l4::value::CssDimension::Length(v) => {
                                    v.value
                                }
                                crate::runtime::css_l4::value::CssDimension::Angle(v) => {
                                    v.value
                                }
                                crate::runtime::css_l4::value::CssDimension::Time(v) => {
                                    v.value
                                }
                                crate::runtime::css_l4::value::CssDimension::Frequency(
                                    v,
                                ) => v.value,
                                crate::runtime::css_l4::value::CssDimension::Resolution(
                                    v,
                                ) => v.value,
                                crate::runtime::css_l4::value::CssDimension::Flex(v) => {
                                    v.value
                                }
                                crate::runtime::css_l4::value::CssDimension::Percentage(
                                    v,
                                ) => v.value,
                                crate::runtime::css_l4::value::CssDimension::Unitless(v) => {
                                    *v
                                }
                            },
                        )
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
