//! AZ-II.cutover.E (Phase 2) — Bnf parse arena.
//!
//! Mirror of `crates/core/src/runtime/csv/arena.rs`.

use bbnf_ir::RuleId;

use crate::runtime::bnf::value::BnfValue;

/// Discriminator — structural shape of a [`BnfValue::Compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BnfCompoundKind {
    Identifier,
    Terminal,
    Nonterminal,
    Term,
    Expression,
    Alternation,
    Rhs,
    Lhs,
    Rule,
    Grammar,
    Other,
}

impl BnfCompoundKind {
    /// Resolve a rule id to a kind. Integer literals match the rule-id
    /// allocation in `crates/core/src/grammar/generated/bnf.rs`. Ids
    /// not in the alphabet collapse to [`Self::Other`].
    pub fn from_rule_id(rule_id: RuleId) -> Self {
        match rule_id {
            0 => Self::Terminal,
            1 => Self::Nonterminal,
            2 => Self::Alternation,
            3 => Self::Rule,
            4 => Self::Grammar,
            // Identifier / Term / Expression / Rhs / Lhs are retained
            // for AST exhaustiveness; no layout is emitted for those
            // rules in the current generated bnf.rs (they collapse via
            // structural Wrap shape).
            _ => Self::Other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BnfCompound<'p> {
    pub kind: BnfCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<BnfValue<'p>>,
}

impl<'p> Default for BnfCompound<'p> {
    fn default() -> Self {
        Self {
            kind: BnfCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BnfCompoundId(u32);

impl BnfCompoundId {
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
pub struct BnfArena<'p> {
    compounds: Vec<BnfCompound<'p>>,
    empty: BnfCompound<'p>,
}

impl<'p> BnfArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: BnfCompound::default(),
        }
    }

    #[inline]
    pub fn push_compound(&mut self, compound: BnfCompound<'p>) -> BnfCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        BnfCompoundId(idx)
    }

    #[inline]
    pub fn compound(&self, id: BnfCompoundId) -> &BnfCompound<'p> {
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
