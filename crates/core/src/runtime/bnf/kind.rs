//! AZ-IV.W5.3 — BNF compound kind discriminator + arena entry shape.
//!
//! Hosts the [`BnfCompoundKind`] enum and the [`BnfCompound`] entry
//! shape that the
//! [`crate::runtime::arena_template::CompoundSlabArena`] instantiation
//! stores.

use bbnf_ir::registry::{StructLayout, StructRegistry};

use crate::runtime::bnf::value::BnfValue;

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
    pub fn from_layout(layout: &StructLayout) -> Self {
        match StructRegistry::compound_kind_for_layout(layout) {
            "terminal" => Self::Terminal,
            "nonterminal" => Self::Nonterminal,
            "alternation" => Self::Alternation,
            "rule" => Self::Rule,
            "grammar" => Self::Grammar,
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
