//! AZ-II.cutover.E (Phase 2) — CssPretty parse arena.
//!
//! Mirror of `crates/core/src/runtime/csv/arena.rs`.

use bbnf_ir::RuleId;

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
    /// Resolve a rule id to a kind. Integer literals match the rule-id
    /// allocation in `crates/core/src/grammar/generated/css_pretty.rs`.
    /// Ids not in the alphabet collapse to [`Self::Other`].
    pub fn from_rule_id(rule_id: RuleId) -> Self {
        match rule_id {
            0 => Self::Important,
            1 => Self::ImportRule,
            2 => Self::Declaration,
            3 => Self::GenericAtRule,
            4 => Self::QualifiedRule,
            5 => Self::MediaRule,
            6 => Self::SupportsRule,
            7 => Self::FontFaceRule,
            9 => Self::RuleBlock,
            10 => Self::BlockContent,
            13 => Self::RuleList,
            14 => Self::Stylesheet,
            // Ws / SelectorSpan / ValueSpan / PropertyName /
            // OptSemicolon / AtRuleBody / AtRule / RuleItem are
            // retained for AST exhaustiveness; no layout is emitted
            // for those rules in the current generated css_pretty.rs.
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
