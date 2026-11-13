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

#[cfg(test)]
mod lattice_laws {
    use super::TypeDomain;
    use crate::TypeDesc;
    use crate::passes::lattice_laws::{Rng, generated_violations};

    fn ty(rng: &mut Rng, depth: usize) -> TypeDesc {
        let leaf = |rng: &mut Rng| match rng.below(6) {
            0 => TypeDesc::Span,
            1 => TypeDesc::F64,
            2 => TypeDesc::Bool,
            3 => TypeDesc::U32,
            4 => TypeDesc::BoxedEnum,
            _ => TypeDesc::Enum,
        };
        if depth == 0 {
            return leaf(rng);
        }
        match rng.below(5) {
            0 => TypeDesc::Option(Box::new(ty(rng, depth - 1))),
            1 => TypeDesc::Vec(Box::new(ty(rng, depth - 1))),
            2 => TypeDesc::Tuple(vec![ty(rng, depth - 1), ty(rng, depth - 1)]),
            _ => leaf(rng),
        }
    }

    /// sk-v27 LW3.COMP-6: `TypeDomain::join` is a lattice join, read over
    /// 10⁵ generated cases. Born RED: a decided type joined with a
    /// different one is overwritten (`a ⊔ b = b`, `b ⊔ a = a`), so the
    /// join is not commutative.
    #[test]
    fn type_domain_join_is_a_lattice_join() {
        let violations = generated_violations(0x5eed_7e9e, |rng| {
            if rng.below(4) == 0 {
                TypeDomain::unsolved()
            } else {
                TypeDomain::ground(ty(rng, 2))
            }
        });
        assert!(violations.is_empty(), "{violations:#?}");
    }
}
