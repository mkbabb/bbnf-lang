//! AZ-I.W2-act.B3 — CSS L4 parse arena.
//!
//! The arena owns every compound child slice — rule lists, selector
//! lists, declaration lists, value lists, keyframe lists — and the
//! arena-allocated colour DAG referenced by [`CssColorMix`]. Each
//! compound carries an opaque handle ([`CssRuleListId`],
//! [`CssDeclListId`], etc.) that resolves through the document's
//! arena accessors.
//!
//! # Allocation strategy
//!
//! Slab-of-Vec model identical to the JSON arena pattern (per
//! `crates/core/src/runtime/json/arena.rs`). One Vec per non-empty
//! compound; empty compounds resolve through the `EMPTY` constant
//! without allocating a slab entry. `feedback_no-orthogonal-codepaths`
//! is in force: every CSS L4 compound child slice flows through this
//! one substrate, no orthogonal collection paths.
//!
//! # Capacity hints
//!
//! `with_capacity` constructors take rule / declaration / selector /
//! value / keyframe slab pre-sizes. The emitter folds capacity
//! estimates from
//! [`bbnf_ir::registry::StructRegistry`] into the parse function so
//! every parse starts with arenas pre-sized for the grammar's typical
//! aggregate width.

use crate::runtime::css_l4::value::{CssColor, CssRule, Declaration, KeyframeBlock, Selector};

// ---------------------------------------------------------------------
// Compound handles
// ---------------------------------------------------------------------

/// Opaque handle for a stylesheet's rule list.
///
/// `0` is reserved for [`Self::EMPTY`]; non-empty handles take values
/// `1..=u32::MAX`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssRuleListId(u32);

impl CssRuleListId {
    /// Empty rule list — resolves to `&[]` regardless of arena state.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Opaque handle for a declaration list (per style rule body).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssDeclListId(u32);

impl CssDeclListId {
    /// Empty declaration list.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Opaque handle for a selector list (per style rule head).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssSelectorListId(u32);

impl CssSelectorListId {
    /// Empty selector list.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Opaque handle for a value list (per declaration body or function
/// argument list).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssValueListId(u32);

impl CssValueListId {
    /// Empty value list.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

/// Opaque handle for a keyframe block list (per `@keyframes` body).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssKeyframeListId(u32);

impl CssKeyframeListId {
    /// Empty keyframe list.
    pub const EMPTY: Self = Self(0);

    /// True iff this is the empty handle.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 {
            None
        } else {
            Some((self.0 - 1) as usize)
        }
    }
}

// ---------------------------------------------------------------------
// CssArena — owning slab
// ---------------------------------------------------------------------

/// Owning slab for CSS L4 compound child slices.
///
/// One slab per typed compound family — rules / declarations /
/// selectors / values / keyframes. Empty compounds resolve to `&[]`
/// without consulting the slab.
///
/// The colour-mix DAG (per `color.bbnf::colorMix`) is recursive: each
/// nested colour reaches the parent through a borrowed reference into
/// `colors` (a separate slab whose elements are typed [`CssColor`]
/// records). The aggregate's `&'p CssColor<'p>` field hands the parent
/// a stable address into this slab.
#[derive(Debug, Default)]
pub struct CssArena<'p> {
    /// Per-stylesheet rule slabs.
    rules: Vec<Vec<CssRule<'p>>>,
    /// Per-style-rule declaration slabs.
    decls: Vec<Vec<Declaration<'p>>>,
    /// Per-style-rule selector slabs.
    selectors: Vec<Vec<Selector<'p>>>,
    /// Per-declaration value slabs.
    values: Vec<Vec<crate::runtime::css_l4::value::CssTypedValue<'p>>>,
    /// Per-`@keyframes` block slabs.
    keyframes: Vec<Vec<KeyframeBlock<'p>>>,
    /// Recursive colour-mix DAG entries — referenced via `&'p CssColor<'p>`.
    /// The Vec's elements are `Box<CssColor<'p>>`-equivalent owning
    /// records; the slab itself owns each colour record so addresses
    /// stay stable for the document's lifetime.
    colors: Vec<Box<CssColor<'p>>>,
}

impl<'p> CssArena<'p> {
    /// Construct an empty arena.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Construct an arena pre-sized to the registry's hinted
    /// capacities.
    #[inline]
    pub fn with_capacity(
        rules: usize,
        decls: usize,
        selectors: usize,
        values: usize,
        keyframes: usize,
    ) -> Self {
        Self {
            rules: Vec::with_capacity(rules),
            decls: Vec::with_capacity(decls),
            selectors: Vec::with_capacity(selectors),
            values: Vec::with_capacity(values),
            keyframes: Vec::with_capacity(keyframes),
            colors: Vec::new(),
        }
    }

    /// Push a populated rule list and return the resolving handle.
    #[inline]
    pub fn push_rules(&mut self, items: Vec<CssRule<'p>>) -> CssRuleListId {
        if items.is_empty() {
            return CssRuleListId::EMPTY;
        }
        self.rules.push(items);
        CssRuleListId(self.rules.len() as u32)
    }

    /// Push a populated declaration list.
    #[inline]
    pub fn push_decls(&mut self, items: Vec<Declaration<'p>>) -> CssDeclListId {
        if items.is_empty() {
            return CssDeclListId::EMPTY;
        }
        self.decls.push(items);
        CssDeclListId(self.decls.len() as u32)
    }

    /// Push a populated selector list.
    #[inline]
    pub fn push_selectors(&mut self, items: Vec<Selector<'p>>) -> CssSelectorListId {
        if items.is_empty() {
            return CssSelectorListId::EMPTY;
        }
        self.selectors.push(items);
        CssSelectorListId(self.selectors.len() as u32)
    }

    /// Push a populated value list.
    #[inline]
    pub fn push_values(
        &mut self,
        items: Vec<crate::runtime::css_l4::value::CssTypedValue<'p>>,
    ) -> CssValueListId {
        if items.is_empty() {
            return CssValueListId::EMPTY;
        }
        self.values.push(items);
        CssValueListId(self.values.len() as u32)
    }

    /// Push a populated keyframe block list.
    #[inline]
    pub fn push_keyframes(&mut self, items: Vec<KeyframeBlock<'p>>) -> CssKeyframeListId {
        if items.is_empty() {
            return CssKeyframeListId::EMPTY;
        }
        self.keyframes.push(items);
        CssKeyframeListId(self.keyframes.len() as u32)
    }

    /// Push a colour record into the colour DAG, returning a stable
    /// `&'p CssColor<'p>` reference. The arena owns the record; the
    /// returned reference lives as long as the arena does.
    pub fn push_color(&mut self, color: CssColor<'p>) -> &'p CssColor<'p> {
        let boxed = Box::new(color);
        self.colors.push(boxed);
        // SAFETY: the arena owns the Box for the document's lifetime;
        // the returned reference therefore lives for `'p` (the
        // document arena's borrow scope, which spans the parse). The
        // transmute lifts the slab's `'_` borrow to `'p` because the
        // record itself never moves once boxed.
        let last = self.colors.last().expect("just pushed");
        let raw: &CssColor<'_> = last.as_ref();
        unsafe { core::mem::transmute::<&CssColor<'_>, &'p CssColor<'p>>(raw) }
    }

    /// Resolve a rule list handle.
    #[inline]
    pub fn rules(&self, id: CssRuleListId) -> &[CssRule<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.rules[i].as_slice(),
        }
    }

    /// Resolve a declaration list handle.
    #[inline]
    pub fn decls(&self, id: CssDeclListId) -> &[Declaration<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.decls[i].as_slice(),
        }
    }

    /// Resolve a selector list handle.
    #[inline]
    pub fn selectors(&self, id: CssSelectorListId) -> &[Selector<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.selectors[i].as_slice(),
        }
    }

    /// Resolve a value list handle.
    #[inline]
    pub fn values(
        &self,
        id: CssValueListId,
    ) -> &[crate::runtime::css_l4::value::CssTypedValue<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.values[i].as_slice(),
        }
    }

    /// Resolve a keyframe block list handle.
    #[inline]
    pub fn keyframes(&self, id: CssKeyframeListId) -> &[KeyframeBlock<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.keyframes[i].as_slice(),
        }
    }

    /// Number of rule slabs registered.
    #[inline]
    pub fn rule_slab_count(&self) -> usize {
        self.rules.len()
    }

    /// Number of declaration slabs registered.
    #[inline]
    pub fn decl_slab_count(&self) -> usize {
        self.decls.len()
    }

    /// Number of selector slabs registered.
    #[inline]
    pub fn selector_slab_count(&self) -> usize {
        self.selectors.len()
    }

    /// Number of value slabs registered.
    #[inline]
    pub fn value_slab_count(&self) -> usize {
        self.values.len()
    }

    /// Number of keyframe slabs registered.
    #[inline]
    pub fn keyframe_slab_count(&self) -> usize {
        self.keyframes.len()
    }

    /// Number of colour records boxed into the colour DAG.
    #[inline]
    pub fn color_count(&self) -> usize {
        self.colors.len()
    }

    /// Roll back every slab to a prior count snapshot.
    #[inline]
    pub fn truncate(
        &mut self,
        rules: usize,
        decls: usize,
        selectors: usize,
        values: usize,
        keyframes: usize,
        colors: usize,
    ) {
        self.rules.truncate(rules);
        self.decls.truncate(decls);
        self.selectors.truncate(selectors);
        self.values.truncate(values);
        self.keyframes.truncate(keyframes);
        self.colors.truncate(colors);
    }
}
