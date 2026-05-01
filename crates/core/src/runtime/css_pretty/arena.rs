//! AZ-II.cutover.E (Phase 2) — CssPretty parse arena.
//!
//! Mirror of `crates/core/src/runtime/csv/arena.rs`.

use crate::runtime::css_pretty::value::CssPrettyValue;

/// Discriminator — structural shape of a [`CssPrettyValue::Compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CssPrettyCompoundKind {
    Ws,
    SelectorSpan,
    ValueSpan,
    PropertyName,
    Important,
    OptSemicolon,
    Declaration,
    BlockContent,
    RuleBlock,
    QualifiedRule,
    MediaRule,
    SupportsRule,
    FontFaceRule,
    ImportRule,
    AtRuleBody,
    GenericAtRule,
    AtRule,
    RuleItem,
    RuleList,
    Stylesheet,
    Other,
}

impl CssPrettyCompoundKind {
    pub fn from_rule_name(name: &str) -> Self {
        match name {
            "ws" => Self::Ws,
            "selectorSpan" => Self::SelectorSpan,
            "valueSpan" => Self::ValueSpan,
            "propertyName" => Self::PropertyName,
            "important" => Self::Important,
            "optSemicolon" => Self::OptSemicolon,
            "declaration" => Self::Declaration,
            "blockContent" => Self::BlockContent,
            "ruleBlock" => Self::RuleBlock,
            "qualifiedRule" => Self::QualifiedRule,
            "mediaRule" => Self::MediaRule,
            "supportsRule" => Self::SupportsRule,
            "fontFaceRule" => Self::FontFaceRule,
            "importRule" => Self::ImportRule,
            "atRuleBody" => Self::AtRuleBody,
            "genericAtRule" => Self::GenericAtRule,
            "atRule" => Self::AtRule,
            "ruleItem" => Self::RuleItem,
            "ruleList" => Self::RuleList,
            "stylesheet" => Self::Stylesheet,
            _ => Self::Other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CssPrettyCompound<'p> {
    pub kind: CssPrettyCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<CssPrettyValue<'p>>,
}

impl<'p> Default for CssPrettyCompound<'p> {
    fn default() -> Self {
        Self {
            kind: CssPrettyCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CssPrettyCompoundId(u32);

impl CssPrettyCompoundId {
    pub const EMPTY: Self = Self(0);

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

#[derive(Debug, Default)]
pub struct CssPrettyArena<'p> {
    compounds: Vec<CssPrettyCompound<'p>>,
    empty: CssPrettyCompound<'p>,
}

impl<'p> CssPrettyArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: CssPrettyCompound::default(),
        }
    }

    #[inline]
    pub fn push_compound(&mut self, compound: CssPrettyCompound<'p>) -> CssPrettyCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        CssPrettyCompoundId(idx)
    }

    #[inline]
    pub fn compound(&self, id: CssPrettyCompoundId) -> &CssPrettyCompound<'p> {
        match id.slab_index() {
            None => &self.empty,
            Some(i) => &self.compounds[i],
        }
    }

    #[inline]
    pub fn compound_count(&self) -> usize {
        self.compounds.len()
    }

    /// Roll back the arena to a prior compound-count snapshot.
    #[inline]
    pub fn truncate(&mut self, compounds: usize) {
        self.compounds.truncate(compounds);
    }
}
