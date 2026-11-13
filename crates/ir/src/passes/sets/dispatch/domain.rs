//! `DispatchDomain` — the CSP lattice domain for dispatch eligibility.
//!
//! Each Alt node becomes a CSP variable whose domain monotonically converges
//! from the bottom (`Unknown`) to a singleton concrete decision
//! (`Dispatchable` or `NonDispatchable`). AC-3 propagation is sufficient —
//! no search/backtracking is needed.

use csp_solver::domain::{Domain, LatticeDomain};

/// Tri-state dispatch eligibility: Unknown → Dispatchable or NonDispatchable.
///
/// Models each Alt node as a CSP variable whose domain monotonically
/// converges from `Unknown` to a concrete decision.
#[derive(Clone, Debug, PartialEq)]
pub enum DispatchDecision {
    Unknown,
    Dispatchable,
    NonDispatchable,
}

/// Lattice domain wrapping `DispatchDecision`.
///
/// Bottom = Unknown. Once resolved to Dispatchable/NonDispatchable, the
/// domain is a singleton — no search needed, AC-3 propagation suffices.
#[derive(Clone, Debug, PartialEq)]
pub struct DispatchDomain {
    pub(super) decision: DispatchDecision,
}

impl DispatchDomain {
    pub fn unknown() -> Self {
        Self {
            decision: DispatchDecision::Unknown,
        }
    }
}

impl Domain for DispatchDomain {
    type Value = DispatchDecision;

    fn size(&self) -> usize {
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<Self::Value> {
        Some(self.decision.clone())
    }

    fn contains(&self, val: &Self::Value) -> bool {
        self.decision == *val
    }

    fn remove(&mut self, _val: &Self::Value) -> bool {
        false
    }

    fn add(&mut self, _val: &Self::Value) {}

    fn values(&self) -> Vec<Self::Value> {
        vec![self.decision.clone()]
    }
}

impl LatticeDomain for DispatchDomain {
    fn bottom() -> Self {
        Self::unknown()
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&self.decision, &other.decision) {
            (DispatchDecision::Unknown, d) if *d != DispatchDecision::Unknown => {
                self.decision = d.clone();
                true
            }
            _ => false,
        }
    }
}

#[cfg(test)]
mod lattice_laws {
    use super::{DispatchDecision, DispatchDomain};
    use crate::passes::lattice_laws::generated_violations;

    /// sk-v27 LW3.COMP-6: `DispatchDomain::join` is a lattice join, read
    /// over 10⁵ generated cases. Born RED: two different decisions join
    /// to whichever came first, so the join is not commutative.
    #[test]
    fn dispatch_domain_join_is_a_lattice_join() {
        let violations = generated_violations(0x5eed_0d15, |rng| DispatchDomain {
            decision: match rng.below(3) {
                0 => DispatchDecision::Unknown,
                1 => DispatchDecision::Dispatchable,
                _ => DispatchDecision::NonDispatchable,
            },
        });
        assert!(violations.is_empty(), "{violations:#?}");
    }
}
