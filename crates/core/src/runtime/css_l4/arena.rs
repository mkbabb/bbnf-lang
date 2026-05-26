use crate::runtime::css_l4::value::{
    CssColor, CssRule, CssTypedValue, Declaration, KeyframeBlock, Selector,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssRuleListId(u32);
impl CssRuleListId {
    pub const EMPTY: Self = Self(0);
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssDeclListId(u32);
impl CssDeclListId {
    pub const EMPTY: Self = Self(0);
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssSelectorListId(u32);
impl CssSelectorListId {
    pub const EMPTY: Self = Self(0);
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssValueListId(u32);
impl CssValueListId {
    pub const EMPTY: Self = Self(0);
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssKeyframeListId(u32);
impl CssKeyframeListId {
    pub const EMPTY: Self = Self(0);
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn slab_index(self) -> Option<usize> {
        if self.0 == 0 { None } else { Some((self.0 - 1) as usize) }
    }
}
#[derive(Debug, Default)]
pub struct CssArena<'p> {
    rules: Vec<Vec<CssRule<'p>>>,
    decls: Vec<Vec<Declaration<'p>>>,
    selectors: Vec<Vec<Selector<'p>>>,
    values: Vec<Vec<CssTypedValue<'p>>>,
    keyframes: Vec<Vec<KeyframeBlock<'p>>>,
    colors: Vec<Box<CssColor<'p>>>,
}
impl<'p> CssArena<'p> {
    pub fn new() -> Self {
        Self::default()
    }
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
    pub fn push_rules(&mut self, items: Vec<CssRule<'p>>) -> CssRuleListId {
        if items.is_empty() {
            return CssRuleListId::EMPTY;
        }
        self.rules.push(items);
        CssRuleListId(self.rules.len() as u32)
    }
    pub fn push_decls(&mut self, items: Vec<Declaration<'p>>) -> CssDeclListId {
        if items.is_empty() {
            return CssDeclListId::EMPTY;
        }
        self.decls.push(items);
        CssDeclListId(self.decls.len() as u32)
    }
    pub fn push_selectors(&mut self, items: Vec<Selector<'p>>) -> CssSelectorListId {
        if items.is_empty() {
            return CssSelectorListId::EMPTY;
        }
        self.selectors.push(items);
        CssSelectorListId(self.selectors.len() as u32)
    }
    pub fn push_values(&mut self, items: Vec<CssTypedValue<'p>>) -> CssValueListId {
        if items.is_empty() {
            return CssValueListId::EMPTY;
        }
        self.values.push(items);
        CssValueListId(self.values.len() as u32)
    }
    pub fn push_keyframes(
        &mut self,
        items: Vec<KeyframeBlock<'p>>,
    ) -> CssKeyframeListId {
        if items.is_empty() {
            return CssKeyframeListId::EMPTY;
        }
        self.keyframes.push(items);
        CssKeyframeListId(self.keyframes.len() as u32)
    }
    pub fn push_color(&mut self, color: CssColor<'p>) -> &'p CssColor<'p> {
        let boxed = Box::new(color);
        self.colors.push(boxed);
        let last = self.colors.last().expect("just pushed");
        let raw: &CssColor<'_> = last.as_ref();
        unsafe { core::mem::transmute::<&CssColor<'_>, &'p CssColor<'p>>(raw) }
    }
    pub fn rules(&self, id: CssRuleListId) -> &[CssRule<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.rules[i].as_slice(),
        }
    }
    pub fn decls(&self, id: CssDeclListId) -> &[Declaration<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.decls[i].as_slice(),
        }
    }
    pub fn selectors(&self, id: CssSelectorListId) -> &[Selector<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.selectors[i].as_slice(),
        }
    }
    pub fn values(&self, id: CssValueListId) -> &[CssTypedValue<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.values[i].as_slice(),
        }
    }
    pub fn keyframes(&self, id: CssKeyframeListId) -> &[KeyframeBlock<'p>] {
        match id.slab_index() {
            None => &[],
            Some(i) => self.keyframes[i].as_slice(),
        }
    }
    pub fn rule_slab_count(&self) -> usize {
        self.rules.len()
    }
    pub fn decl_slab_count(&self) -> usize {
        self.decls.len()
    }
    pub fn selector_slab_count(&self) -> usize {
        self.selectors.len()
    }
    pub fn value_slab_count(&self) -> usize {
        self.values.len()
    }
    pub fn keyframe_slab_count(&self) -> usize {
        self.keyframes.len()
    }
    pub fn color_count(&self) -> usize {
        self.colors.len()
    }
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
