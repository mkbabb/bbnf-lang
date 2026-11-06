//! Core Domain trait and LatticeDomain extension.

use std::fmt::Debug;

/// A domain of possible values for a CSP variable.
///
/// The iteration primitive is `iter()`, not `values()`. Implementations should
/// provide zero-allocation iterators where possible (e.g. `BitsetDomain` iterates
/// bits from a `u128` without heap allocation).
pub trait Domain: Clone + PartialEq + Debug {
    /// The type of individual values in this domain.
    type Value: Clone + PartialEq + Debug;

    /// Number of values currently in the domain.
    fn size(&self) -> usize;

    /// Whether the domain is empty (wipe-out).
    fn is_empty(&self) -> bool {
        self.size() == 0
    }

    /// Whether the domain contains exactly one value.
    fn is_singleton(&self) -> bool {
        self.size() == 1
    }

    /// If the domain is a singleton, return its sole value.
    fn singleton_value(&self) -> Option<Self::Value> {
        if self.is_singleton() {
            self.iter().next()
        } else {
            None
        }
    }

    /// Test membership.
    fn contains(&self, val: &Self::Value) -> bool;

    /// Remove a value from the domain. Returns `true` if the value was present.
    fn remove(&mut self, val: &Self::Value) -> bool;

    /// Add a value back into the domain.
    fn add(&mut self, val: &Self::Value);

    /// Collect all current values into a Vec.
    fn values(&self) -> Vec<Self::Value>;

    /// Iterate over current values without allocating.
    ///
    /// Default delegates to `values().into_iter()`. Override for zero-alloc
    /// iteration (e.g. `BitsetDomain` yields bits from a `u128` copy).
    ///
    /// The returned iterator is owned — it does NOT borrow `&self`, so the
    /// domain can be mutated while iteration is in progress (the iterator
    /// holds a snapshot).
    ///
    /// `+ use<Self>` is load-bearing, not decorative: edition 2024's
    /// default RPIT-capture rule ties the opaque return type to the
    /// elided `&self` lifetime even though no implementation actually
    /// borrows anything, which blocks exactly the
    /// `for v in x.iter() { x.remove(&v) }` pattern this trait's own doc
    /// comment above promises works. The precise-capture bound tells the
    /// compiler what's already true of every implementation.
    fn iter(&self) -> impl Iterator<Item = Self::Value> + use<Self> {
        self.values().into_iter()
    }

    /// Restrict the domain to hold only `val`, removing every other value.
    /// Returns an iterator over the values that were removed, for the
    /// caller's undo log (`Variable::restrict_to`).
    ///
    /// Default: iterate + remove one at a time — zero heap allocation
    /// given `iter()`'s owned-snapshot contract, but O(domain size) bit
    /// ops. Override where the internal representation allows a single
    /// bulk mutation instead of a per-value one — e.g. `BitsetDomain`
    /// clears every bit but `val`'s in one bitwise AND (`bitset.rs`),
    /// turning an O(domain size) sequence of individual clears into O(1).
    fn restrict_to(&mut self, val: &Self::Value) -> impl Iterator<Item = Self::Value> + use<Self> {
        let mut removed = Vec::new();
        for v in self.iter() {
            if v != *val {
                self.remove(&v);
                removed.push(v);
            }
        }
        removed.into_iter()
    }
}

/// A domain whose values carry associated costs for optimization.
///
/// Extends `Domain` with cost information, enabling branch-and-bound search
/// to prune infeasible branches and order values by preference.
pub trait CostDomain: Domain {
    /// Cost of assigning this value. Lower is better.
    fn cost(&self, val: &Self::Value) -> f64;

    /// Lower bound on the minimum cost achievable from this domain.
    ///
    /// Default implementation scans all values; override for O(1) if your
    /// domain tracks the minimum incrementally.
    fn min_cost(&self) -> f64 {
        self.values()
            .into_iter()
            .map(|v| self.cost(&v))
            .fold(f64::INFINITY, f64::min)
    }
}

/// Extension trait for monotonic lattice domains (e.g. FIRST/FOLLOW sets).
///
/// A lattice domain contains a single "current value" that grows monotonically
/// via `join`. Because the value is always a singleton, no search is needed —
/// propagation alone reaches the fixed point.
pub trait LatticeDomain: Domain {
    /// The bottom element of the lattice.
    fn bottom() -> Self;

    /// Join (least upper bound) with `other`. Returns `true` if `self` changed.
    fn join(&mut self, other: &Self) -> bool;
}
