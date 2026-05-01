//! AZ-I.W2-act.B3 — `CssDocument` + view / value / path accessor
//! surface.
//!
//! The struct-direct CSS L4 parse path returns a [`CssDocument`] whose
//! root is a [`StyleSheet`] borrowing from the input lifetime `'p` and
//! whose [`CssArena`] owns every compound child slice. This module
//! mirrors the JSON document API surface (`view()` / `to_value()` /
//! `get::<T>(path)`) so consumers writing against either grammar see
//! the same call shape.
//!
//! # Lifetime discipline
//!
//! Identical to `runtime::json::document` — `CssDocument<'p>`'s
//! lifetime parameter binds the input slice lifetime; every borrowed
//! span on every [`StyleRule`] / [`Declaration`] / [`CssTypedValue`]
//! borrows from `'p`. [`CssView<'a, 'p>`] borrows the document with
//! lifetime `'a` (typically shorter than `'p`).

use crate::runtime::css_l4::arena::{
    CssArena, CssDeclListId, CssKeyframeListId, CssRuleListId, CssSelectorListId, CssValueListId,
};
use crate::runtime::css_l4::value::{
    CssRule, CssTypedValue, Declaration, KeyframeBlock, Selector, StyleSheet,
};
use crate::runtime::path::{Path, PathSegment};

/// The root document returned by `bbnf::grammar::generated::css_l4::CssL4Parser::parse`.
///
/// Holds the parse arena (which owns every compound child slice) and
/// the root [`StyleSheet`]. Borrows the input bytes via the `'p`
/// lifetime.
#[derive(Debug)]
pub struct CssDocument<'p> {
    /// The compound child arena — owns rule lists, declaration lists,
    /// selector lists, value lists, keyframe lists, and the colour
    /// DAG.
    pub arena: CssArena<'p>,
    /// The root stylesheet — the typed top-level entry the grammar's
    /// `stylesheet` rule projects.
    pub root: StyleSheet,
    /// AZ-I.W2-act.close A.fix — the input slice the parse consumed.
    /// Threaded through `finalise(input)` so [`CssView`] can satisfy
    /// the `RuntimeView::input()` surface without re-acquiring the
    /// source from the call site.
    pub input: &'p str,
}

impl<'p> CssDocument<'p> {
    /// Construct a document from a populated arena, root stylesheet,
    /// and the input slice the parse consumed. The typical caller is
    /// the generated parse function; consumers outside the emitter
    /// rarely build a `CssDocument` directly.
    #[inline]
    pub fn new(arena: CssArena<'p>, root: StyleSheet, input: &'p str) -> Self {
        Self { arena, root, input }
    }

    /// Borrow the root [`StyleSheet`].
    ///
    /// Mirrors `JsonDocument::root` — the lower-level accessor used
    /// internally by [`CssView`]. Consumers who want the
    /// high-level value accessor surface call [`Self::to_value`].
    #[inline]
    pub fn root(&self) -> &StyleSheet {
        &self.root
    }

    /// Borrow the underlying [`CssArena`].
    #[inline]
    pub fn arena(&self) -> &CssArena<'p> {
        &self.arena
    }

    /// AZ-I.W2-act.close A.fix — borrow the input slice the parse
    /// consumed.
    #[inline]
    pub fn input(&self) -> &'p str {
        self.input
    }

    /// Resolve a rule list handle.
    #[inline]
    pub fn rules(&self, id: CssRuleListId) -> &[CssRule<'p>] {
        self.arena.rules(id)
    }

    /// Resolve a declaration list handle.
    #[inline]
    pub fn decls(&self, id: CssDeclListId) -> &[Declaration<'p>] {
        self.arena.decls(id)
    }

    /// Resolve a selector list handle.
    #[inline]
    pub fn selectors(&self, id: CssSelectorListId) -> &[Selector<'p>] {
        self.arena.selectors(id)
    }

    /// Resolve a value list handle.
    #[inline]
    pub fn values(&self, id: CssValueListId) -> &[CssTypedValue<'p>] {
        self.arena.values(id)
    }

    /// Resolve a keyframe block list handle.
    #[inline]
    pub fn keyframes(&self, id: CssKeyframeListId) -> &[KeyframeBlock<'p>] {
        self.arena.keyframes(id)
    }

    /// AZ-I.W2-act.B3 — root view over the document graph.
    #[inline]
    pub fn view<'a>(&'a self) -> CssView<'a, 'p> {
        CssView {
            doc: self,
            focus: CssFocus::Stylesheet(&self.root),
        }
    }

    /// AZ-I.W2-act.B3 — borrowed root stylesheet.
    ///
    /// The struct-direct path's [`CssDocument`] already carries the
    /// typed graph, so `to_value()` lends its root by reference.
    #[inline]
    pub fn to_value(&self) -> &StyleSheet {
        &self.root
    }

    /// AZ-I.W2-act.B3 — typed path query.
    ///
    /// Forwards to the [`CssPathQuery`] trait; impls land per leaf type.
    #[inline]
    pub fn get<T: CssPathQuery>(&self, path: Path<'_>) -> Option<T> {
        T::query(self, path)
    }

    /// AZ-I.W2-act.close B3 — pre-order walk over every [`Declaration`]
    /// in the document.
    ///
    /// Yields declarations in source order, descending into media-rule
    /// inner rules and keyframes-rule blocks. The returned iterator
    /// borrows the document (and the arena it owns) for the iteration's
    /// lifetime so consumers can match on each declaration's
    /// [`Declaration::value`] without intermediate cloning.
    ///
    /// Used by the CSS L4 typed-payload parity tests to find every
    /// `(property, typed-value)` pair without knowing the structural
    /// path in advance.
    #[inline]
    pub fn walk_declarations(&self) -> CssDeclWalk<'_, 'p> {
        CssDeclWalk {
            doc: self,
            stack: vec![CssWalkItem::RuleList(self.root.rules, 0)],
        }
    }

    /// AZ-I.W2-act.close B3 — pre-order walk over every typed value in
    /// the document, yielding `(property, &CssTypedValue)` pairs.
    ///
    /// Each declaration contributes its [`Declaration::value`] under the
    /// declaration's `property` name; if the value resolves to a
    /// [`CssTypedValue::List`] handle, the walker descends into the
    /// arena-backed list and yields each element under the same property
    /// name.
    ///
    /// Used by the CSS L4 typed-payload parity tests to assert that
    /// typed leaves (`CssDimension::Percentage`, `CssColor::Hex(...)`,
    /// etc.) reach the document graph — the post-tape equivalent of the
    /// pre-W2-act tape-walk parity surface.
    pub fn walk_values<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a {
        self.walk_declarations().flat_map(|decl| {
            let property = decl.property;
            let primary = std::iter::once((property, &decl.value));
            let list_extra: Box<dyn Iterator<Item = (&'p str, &'a CssTypedValue<'p>)> + 'a> =
                match &decl.value {
                    CssTypedValue::List(id) => {
                        Box::new(self.values(*id).iter().map(move |v| (property, v)))
                    }
                    _ => Box::new(std::iter::empty()),
                };
            primary.chain(list_extra)
        })
    }
}

/// AZ-I.W2-act.close B3 — frame on the [`CssDocument::walk_declarations`]
/// stack.
///
/// The walker visits rule lists / style rules / media rules / keyframe
/// rules / keyframe blocks in turn, descending in pre-order through the
/// document's typed tree.
#[derive(Debug)]
enum CssWalkItem {
    /// A rule list (top-level stylesheet rules or @media inner rules);
    /// `usize` is the next-index cursor.
    RuleList(CssRuleListId, usize),
    /// A declaration list (style-rule decls or keyframe-block decls);
    /// `usize` is the next-index cursor.
    DeclList(CssDeclListId, usize),
    /// A keyframe-block list inside an @keyframes rule; `usize` is the
    /// next-index cursor.
    KeyframeList(CssKeyframeListId, usize),
}

/// AZ-I.W2-act.close B3 — pre-order [`Declaration`] walker over a
/// [`CssDocument`].
///
/// Maintains a stack of in-flight rule / declaration / keyframe lists so
/// the iteration is allocation-free past the initial stack push and
/// progresses one declaration per `next()` call. The arena-backed
/// declaration slices outlive the iterator (they live for `'p`); the
/// iterator's lifetime `'a` bounds the borrow on the [`CssDocument`].
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
                    let id = *id;
                    let rules = self.doc.rules(id);
                    if let Some(rule) = rules.get(*idx) {
                        *idx += 1;
                        match rule {
                            CssRule::Style(style) => {
                                self.stack
                                    .push(CssWalkItem::DeclList(style.declarations, 0));
                            }
                            CssRule::Media(media) => {
                                self.stack.push(CssWalkItem::RuleList(media.rules, 0));
                            }
                            CssRule::Keyframes(kf) => {
                                self.stack.push(CssWalkItem::KeyframeList(kf.blocks, 0));
                            }
                            CssRule::GenericAt(_) => {
                                // Generic at-rules carry no declarations.
                            }
                        }
                    } else {
                        self.stack.pop();
                    }
                }
                CssWalkItem::DeclList(id, idx) => {
                    let id = *id;
                    let decls = self.doc.decls(id);
                    if let Some(decl) = decls.get(*idx) {
                        *idx += 1;
                        return Some(decl);
                    }
                    self.stack.pop();
                }
                CssWalkItem::KeyframeList(id, idx) => {
                    let id = *id;
                    let blocks = self.doc.keyframes(id);
                    if let Some(block) = blocks.get(*idx) {
                        *idx += 1;
                        self.stack
                            .push(CssWalkItem::DeclList(block.declarations, 0));
                    } else {
                        self.stack.pop();
                    }
                }
            }
        }
    }
}

/// AZ-I.W2-act.B3 — a thin newtype over `&CssDocument`.
///
/// `CssView<'a, 'p>` is the struct-tree equivalent of the cursor-
/// backed `View<'p>` used by the pre-W2-act cursor surface.
/// Exposes the root, the arena, and ergonomic resolution of compound
/// handles. Mirrors [`crate::runtime::json::JsonView`] in surface and
/// lifetime discipline.
#[derive(Debug, Clone, Copy)]
pub struct CssView<'a, 'p: 'a> {
    pub(crate) doc: &'a CssDocument<'p>,
    /// AZ-I.W2-act.close A.fix — the focused node this view observes.
    /// Defaults to [`CssFocus::Stylesheet`] for `CssDocument::view()`;
    /// `RuntimeView::children()` projects sub-views onto rules /
    /// declarations / values discovered structurally.
    pub(crate) focus: CssFocus<'a, 'p>,
}

/// AZ-I.W2-act.close A.fix — focusable node within a [`CssDocument`].
///
/// CSS L4's typed graph has multiple compound shapes (stylesheet,
/// rules, declarations, values), so the view's focus is a sum of
/// pointers rather than a single typed value. The variant determines
/// the structural-children iteration strategy in
/// [`RuntimeView::children`].
#[derive(Debug, Clone, Copy)]
pub enum CssFocus<'a, 'p: 'a> {
    /// Top-level stylesheet — children are the rules in `root.rules`.
    Stylesheet(&'a StyleSheet),
    /// A rule (style / media / keyframes / generic). Children are
    /// declarations (style) / inner rules (media) / keyframe blocks
    /// (keyframes).
    Rule(&'a CssRule<'p>),
    /// A declaration — children are the value list bound to
    /// `decl.value`.
    Decl(&'a Declaration<'p>),
    /// A typed value — leaf (no further structural descent).
    Value(&'a CssTypedValue<'p>),
    /// A keyframe block — children are the declarations in the block.
    KeyframeBlock(&'a KeyframeBlock<'p>),
}

impl<'a, 'p: 'a> CssView<'a, 'p> {
    /// Construct a view focused on a specific node within the
    /// document.
    #[inline]
    pub fn focused(doc: &'a CssDocument<'p>, focus: CssFocus<'a, 'p>) -> Self {
        Self { doc, focus }
    }

    /// Borrow the underlying document.
    #[inline]
    pub fn document(&self) -> &'a CssDocument<'p> {
        self.doc
    }

    /// AZ-I.W2-act.close A.fix — the focused node this view observes.
    #[inline]
    pub fn focus(&self) -> CssFocus<'a, 'p> {
        self.focus
    }

    /// Borrow the root [`StyleSheet`].
    #[inline]
    pub fn root(&self) -> &'a StyleSheet {
        &self.doc.root
    }

    /// Borrow the underlying arena.
    #[inline]
    pub fn arena(&self) -> &'a CssArena<'p> {
        &self.doc.arena
    }

    /// Resolve a rule list handle through the document's arena.
    #[inline]
    pub fn rules(&self, id: CssRuleListId) -> &'a [CssRule<'p>] {
        self.doc.rules(id)
    }

    /// Resolve a declaration list handle through the document's arena.
    #[inline]
    pub fn decls(&self, id: CssDeclListId) -> &'a [Declaration<'p>] {
        self.doc.decls(id)
    }

    /// Resolve a selector list handle through the document's arena.
    #[inline]
    pub fn selectors(&self, id: CssSelectorListId) -> &'a [Selector<'p>] {
        self.doc.selectors(id)
    }

    /// Resolve a value list handle through the document's arena.
    #[inline]
    pub fn values(&self, id: CssValueListId) -> &'a [CssTypedValue<'p>] {
        self.doc.values(id)
    }

    /// Resolve a keyframe block list handle through the document's arena.
    #[inline]
    pub fn keyframes(&self, id: CssKeyframeListId) -> &'a [KeyframeBlock<'p>] {
        self.doc.keyframes(id)
    }

    /// Discriminator over the focused node's shape.
    ///
    /// AZ-I.W2-act.close A.fix — when the focus is the stylesheet
    /// (the default for `doc.view()`), reports `Empty` /
    /// `StyleSheet` per rule-list emptiness. Sub-tree focuses
    /// produced by `RuntimeView::children` report `Rule` /
    /// `Declaration` / `Value` / `KeyframeBlock`.
    #[inline]
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

/// Discriminator over the typed shapes a [`CssView`]'s focus takes.
///
/// The `stylesheet` rule always projects to a [`StyleSheet`]; the
/// discriminator distinguishes empty-document from non-empty for view
/// callers branching on `view.kind()`. The non-stylesheet variants
/// (`Rule`, `Declaration`, `Value`, `KeyframeBlock`) appear when a
/// [`CssView`] is focused on a sub-tree produced by
/// [`crate::runtime::RuntimeView::children`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CssDocumentKind {
    /// `stylesheet = ruleList ?w` with an empty rule list.
    Empty,
    /// `stylesheet = ruleList ?w` with at least one rule.
    StyleSheet,
    /// A rule (style / media / keyframes / generic).
    Rule,
    /// A declaration `property: value` pair.
    Declaration,
    /// A typed value leaf.
    Value,
    /// A keyframe block (selector list + declaration list).
    KeyframeBlock,
}

/// AZ-I.W2-act.B3 — typed path-query trait, mirroring
/// `JsonPathQuery`.
///
/// The walker descends from `doc.root()` following
/// [`PathSegment::Index`] steps against rule lists / declaration
/// lists / selector lists, and [`PathSegment::Field`] steps against
/// declaration `property` lookups. The terminal value at the end of
/// the path coerces to `T` per the per-`T` impl.
pub trait CssPathQuery: Sized {
    /// Resolve `path` against `doc`, yielding the extracted leaf or
    /// `None` if any segment fails to match.
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self>;
}

/// Walk-state for path queries on the CSS document.
enum CssWalkCursor<'a, 'p> {
    /// Stylesheet root — admits Index steps over the rule list.
    Sheet(&'a StyleSheet, &'a CssArena<'p>),
    /// Rule (style / media / keyframes / generic).
    Rule(&'a CssRule<'p>, &'a CssArena<'p>),
    /// Declaration — admits Field("property") / Field("value").
    Decl(&'a Declaration<'p>, &'a CssArena<'p>),
    /// Typed value — terminal path target.
    Value(&'a CssTypedValue<'p>, #[allow(dead_code)] &'a CssArena<'p>),
}

#[inline]
fn walk_path<'a, 'p>(doc: &'a CssDocument<'p>, path: Path<'_>) -> Option<CssWalkCursor<'a, 'p>> {
    let mut current = CssWalkCursor::Sheet(&doc.root, &doc.arena);
    for segment in path.iter() {
        current = match (current, segment) {
            (CssWalkCursor::Sheet(sheet, arena), PathSegment::Index(idx)) => {
                let rules = arena.rules(sheet.rules);
                let rule = rules.get(*idx)?;
                CssWalkCursor::Rule(rule, arena)
            }
            (CssWalkCursor::Rule(rule, arena), PathSegment::Index(idx)) => {
                // Indexing into a Rule descends into its declaration list
                // (style rules) or rule list (media / keyframes / generic
                // at-rule). The struct-direct projection routes the index
                // through whichever compound the rule carries.
                match rule {
                    CssRule::Style(style) => {
                        let decls = arena.decls(style.declarations);
                        let decl = decls.get(*idx)?;
                        CssWalkCursor::Decl(decl, arena)
                    }
                    CssRule::Media(media) => {
                        let inner = arena.rules(media.rules);
                        let nested = inner.get(*idx)?;
                        CssWalkCursor::Rule(nested, arena)
                    }
                    _ => return None,
                }
            }
            (CssWalkCursor::Decl(decl, arena), PathSegment::Field(name)) => {
                if *name == "value" {
                    CssWalkCursor::Value(&decl.value, arena)
                } else {
                    return None;
                }
            }
            _ => return None,
        };
    }
    Some(current)
}

impl CssPathQuery for &str {
    #[inline]
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        let cursor = walk_path(doc, path)?;
        // Lifetime extension: every borrowed span lives for `'p`,
        // outlives the caller's borrow on the document.
        match cursor {
            CssWalkCursor::Decl(decl, _) => {
                let extended: &'p str = decl.property;
                Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
            }
            CssWalkCursor::Value(value, _) => match value {
                CssTypedValue::String(s) | CssTypedValue::Ident(s) | CssTypedValue::Span(s) => {
                    let extended: &'p str = *s;
                    Some(unsafe { core::mem::transmute::<&'p str, &str>(extended) })
                }
                _ => None,
            },
            _ => None,
        }
    }
}

impl CssPathQuery for f64 {
    #[inline]
    fn query<'p>(doc: &CssDocument<'p>, path: Path<'_>) -> Option<Self> {
        let cursor = walk_path(doc, path)?;
        match cursor {
            CssWalkCursor::Value(value, _) => match value {
                CssTypedValue::Number(n) => Some(*n),
                CssTypedValue::Dimension(d) => Some(match d {
                    crate::runtime::css_l4::value::CssDimension::Length(l) => l.value,
                    crate::runtime::css_l4::value::CssDimension::Angle(a) => a.value,
                    crate::runtime::css_l4::value::CssDimension::Time(t) => t.value,
                    crate::runtime::css_l4::value::CssDimension::Frequency(f) => f.value,
                    crate::runtime::css_l4::value::CssDimension::Resolution(r) => r.value,
                    crate::runtime::css_l4::value::CssDimension::Flex(f) => f.value,
                    crate::runtime::css_l4::value::CssDimension::Percentage(p) => p.value,
                    crate::runtime::css_l4::value::CssDimension::Unitless(v) => *v,
                }),
                _ => None,
            },
            _ => None,
        }
    }
}
