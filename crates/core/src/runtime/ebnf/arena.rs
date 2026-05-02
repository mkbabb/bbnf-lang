//! AZ-II.cutover.E (Phase 2) — Ebnf parse arena.
//!
//! Mirror of `crates/core/src/runtime/csv/arena.rs`.

use bbnf_ir::RuleId;

use crate::runtime::ebnf::value::EbnfValue;

/// Discriminator — structural shape of a [`EbnfValue::Compound`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EbnfCompoundKind {
    Letter,
    Digit,
    Symbol,
    Character,
    Identifier,
    S,
    Terminal,
    Terminator,
    Term,
    Factor,
    Concatenation,
    Alternation,
    Rhs,
    Lhs,
    Rule,
    Grammar,
    Other,
}

impl EbnfCompoundKind {
    /// Resolve a rule id to a kind. Integer literals match the rule-id
    /// allocation in `crates/core/src/grammar/generated/ebnf.rs`. Ids
    /// not in the alphabet collapse to [`Self::Other`].
    pub fn from_rule_id(rule_id: RuleId) -> Self {
        match rule_id {
            0 => Self::Letter,
            2 => Self::Symbol,
            3 => Self::Identifier,
            4 => Self::Character,
            6 => Self::Concatenation,
            7 => Self::Alternation,
            9 => Self::Term,
            10 => Self::Factor,
            11 => Self::Rule,
            12 => Self::Grammar,
            // Digit / S / Terminal / Terminator / Rhs / Lhs are
            // retained for AST exhaustiveness; no layout is emitted
            // for those rules in the current generated ebnf.rs.
            _ => Self::Other,
        }
    }
}

#[derive(Debug, Clone)]
pub struct EbnfCompound<'p> {
    pub kind: EbnfCompoundKind,
    pub branch_tag: Option<u32>,
    pub children: Vec<EbnfValue<'p>>,
}

impl<'p> Default for EbnfCompound<'p> {
    fn default() -> Self {
        Self {
            kind: EbnfCompoundKind::Other,
            branch_tag: None,
            children: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EbnfCompoundId(u32);

impl EbnfCompoundId {
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
pub struct EbnfArena<'p> {
    compounds: Vec<EbnfCompound<'p>>,
    empty: EbnfCompound<'p>,
}

impl<'p> EbnfArena<'p> {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn with_capacity(compounds: usize) -> Self {
        Self {
            compounds: Vec::with_capacity(compounds),
            empty: EbnfCompound::default(),
        }
    }

    #[inline]
    pub fn push_compound(&mut self, compound: EbnfCompound<'p>) -> EbnfCompoundId {
        self.compounds.push(compound);
        let idx = self.compounds.len() as u32;
        EbnfCompoundId(idx)
    }

    #[inline]
    pub fn compound(&self, id: EbnfCompoundId) -> &EbnfCompound<'p> {
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
