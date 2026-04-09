//! `TypeDomain` — the CSP domain for type inference variables.
//!
//! Wraps `Option<TypeDesc>`. Starts as `None` (bottom/unsolved). Once
//! assigned to `Some(ty)`, the domain is a singleton. The lattice `join`
//! assigns the type if currently bottom; re-assignment to a different type
//! is allowed during convergence.

use csp_solver::domain::{Domain, LatticeDomain};

use crate::TypeDesc;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDomain {
    pub solved: Option<TypeDesc>,
}

impl TypeDomain {
    pub fn unsolved() -> Self {
        Self { solved: None }
    }

    pub fn ground(ty: TypeDesc) -> Self {
        Self { solved: Some(ty) }
    }
}

impl Domain for TypeDomain {
    type Value = Option<TypeDesc>;

    fn size(&self) -> usize {
        // Lattice domain: always a "singleton" from the solver's perspective.
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.solved.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.solved == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        // Lattice domains don't support removal -- they only grow.
        false
    }

    fn add(&mut self, _val: &Self::Value) {
        // No-op for lattice domains.
    }

    fn values(&self) -> Vec<Self::Value> {
        vec![self.solved.clone()]
    }
}

impl LatticeDomain for TypeDomain {
    fn bottom() -> Self {
        Self { solved: None }
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.solved, &other.solved) {
            (None, Some(ty)) => {
                self.solved = Some(ty.clone());
                true
            }
            (Some(existing), Some(ty)) if existing != ty => {
                // Re-assignment to a different type: allow it (the solver
                // may refine types during convergence).
                self.solved = Some(ty.clone());
                true
            }
            _ => false,
        }
    }
}
