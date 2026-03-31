//! Helper types and utility functions for type inference.

use std::collections::HashMap;

use crate::{GrammarIR, RuleId, TypeDesc};

/// Context for type inference — avoids threading many parameters.
pub struct InferCtx<'a> {
    pub ir: &'a GrammarIR,
    pub cache: &'a HashMap<RuleId, TypeDesc>,
    pub acyclic_rules: &'a std::collections::HashSet<RuleId>,
    /// Whether the current rule being inferred is cyclic (for B.4).
    pub cyclic_context: bool,
    /// Consumable flag for @pretty tuple preservation (B.2).
    /// Only applies to the first (top-level) Seq encountered.
    pub pretty_preserve: bool,
}

impl InferCtx<'_> {
    /// Return a copy with pretty_preserve consumed (set to false).
    pub fn consumed(&self) -> InferCtx<'_> {
        InferCtx {
            ir: self.ir,
            cache: self.cache,
            acyclic_rules: self.acyclic_rules,
            cyclic_context: self.cyclic_context,
            pretty_preserve: false,
        }
    }
}

/// Try to flatten a 2-element tuple where one is `T` and the other is `Vec<T>`.
/// Only flattens same-type pairs (A, Vec<A>) or (Vec<A>, A).
pub fn try_flatten_pair(a: &TypeDesc, b: &TypeDesc) -> Option<TypeDesc> {
    // (T, Vec<T>) → Vec<T>
    if let TypeDesc::Vec(inner) = b {
        if **inner == *a {
            return Some(b.clone());
        }
    }
    // (Vec<T>, T) → Vec<T>
    if let TypeDesc::Vec(inner) = a {
        if **inner == *b {
            return Some(a.clone());
        }
    }
    None
}
