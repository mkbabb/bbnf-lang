//! LatticeDomain — wrapper for monotonic CSP (types, FIRST/FOLLOW sets).

use super::bitset::BitsetDomain;
use super::traits::{Domain, LatticeDomain};

/// A lattice domain backed by a `BitsetDomain`.
///
/// The domain is always a singleton (size == 1) from the solver's perspective:
/// it holds a single "current value" that grows monotonically via `join` (union).
/// Because it's always assigned, no backtracking search is needed -- AC-3
/// propagation alone reaches the fixed point.
#[derive(Debug, Clone, PartialEq)]
pub struct BitsetLatticeDomain {
    /// The current lattice value (grows via union).
    value: BitsetDomain,
}

impl BitsetLatticeDomain {
    /// Create a lattice domain initialized with the given bitset value.
    pub fn new(value: BitsetDomain) -> Self {
        Self { value }
    }

    /// Access the underlying bitset.
    pub fn inner(&self) -> &BitsetDomain {
        &self.value
    }
}

impl Domain for BitsetLatticeDomain {
    type Value = BitsetDomain;

    fn size(&self) -> usize {
        // Lattice domains are always "singletons" -- one current value.
        1
    }

    fn is_singleton(&self) -> bool {
        true
    }

    fn singleton_value(&self) -> Option<BitsetDomain> {
        Some(self.value.clone())
    }

    fn contains(&self, val: &BitsetDomain) -> bool {
        self.value == *val
    }

    fn remove(&mut self, _val: &BitsetDomain) -> bool {
        // Lattice domains don't support removal -- they only grow.
        false
    }

    fn add(&mut self, _val: &BitsetDomain) {
        // No-op for lattice domains.
    }

    fn values(&self) -> Vec<BitsetDomain> {
        vec![self.value.clone()]
    }

    fn iter(&self) -> impl Iterator<Item = BitsetDomain> {
        std::iter::once(self.value.clone())
    }
}

impl LatticeDomain for BitsetLatticeDomain {
    fn bottom() -> Self {
        Self {
            value: BitsetDomain::empty(),
        }
    }

    fn join(&mut self, other: &Self) -> bool {
        let old = self.value.bits();
        self.value.union_with(&other.value);
        self.value.bits() != old
    }
}
