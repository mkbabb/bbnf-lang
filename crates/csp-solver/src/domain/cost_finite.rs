//! CostFiniteDomain — finite integer domain with parallel per-value cost vector.
//!
//! Wraps `FiniteDomain<i32>` with a sorted `(value, cost)` table so that
//! `CostDomain::cost` is O(log n) via binary search. The cost table is
//! immutable from construction; removals prune the underlying
//! `FiniteDomain<i32>` but leave the table untouched so that the surviving
//! entries keep their original costs across `remove`/`add` cycles.
//!
//! # Why `i32` and not `u32`?
//!
//! Bipartite-assignment COPs routinely need a "no match" sentinel value
//! distinct from every valid target index. Using `i32` lets the sentinel
//! be `-1` — a natural, immediately-recognizable marker — without
//! consuming a valid index or widening the type. Clients that only need
//! non-negative values are free to pass them; the domain is agnostic.
//!
//! # Typical assignment-COP usage
//!
//! ```
//! use csp_solver::domain::CostFiniteDomain;
//! use csp_solver::domain::traits::{CostDomain, Domain};
//!
//! // Row i of a 3×3 cost matrix, with a −1 sentinel priced at the
//! // unmatched penalty. Each source row gets its own domain built from
//! // the corresponding slice of the cost matrix.
//! let cost_row     = [4.2, 1.1, 7.8];
//! let unmatch_cost = 100.0;
//!
//! let values = vec![-1_i32, 0, 1, 2];
//! let costs  = vec![unmatch_cost, cost_row[0], cost_row[1], cost_row[2]];
//! let domain = CostFiniteDomain::new(values, costs);
//!
//! assert_eq!(domain.size(), 4);
//! assert_eq!(domain.cost(&1), 1.1);
//! assert_eq!(domain.cost(&-1), unmatch_cost);
//! assert_eq!(domain.min_cost(), 1.1);
//! ```

use std::cell::Cell;

use super::finite::FiniteDomain;
use super::traits::{CostDomain, Domain};

/// A finite integer domain paired with a per-value cost vector.
///
/// Construction takes `(values, costs)` in any order; the constructor
/// canonicalizes to ascending value order so that `cost()` can binary
/// search in O(log n). Removals shrink the active domain but leave the
/// cost table alone, so values removed during search retain their
/// original costs if they're added back later.
///
/// All `Domain` methods delegate to the wrapped `FiniteDomain<i32>`.
/// `CostDomain::cost` looks the value up in the sorted cost table;
/// `CostDomain::min_cost` uses a `Cell`-cached minimum that is
/// lazily recomputed after any `remove`/`add` mutation — amortized
/// O(1) for the repeated lower-bound queries the branch-and-bound
/// solver issues at every search node.
#[derive(Debug, Clone)]
pub struct CostFiniteDomain {
    /// Active values, pruned by the solver during search.
    inner: FiniteDomain<i32>,
    /// `(value, cost)` pairs sorted ascending by value. All values
    /// present in `inner` are also in this table; values removed from
    /// `inner` may remain here harmlessly because `cost()` is only
    /// ever queried for live values.
    costs: Vec<(i32, f64)>,
    /// Lazily cached minimum cost over the live domain. `None` means
    /// "dirty — recompute on next `min_cost()` call". Invalidated by
    /// `remove`/`add`; populated by `min_cost`.
    min_cost_cache: Cell<Option<f64>>,
}

impl PartialEq for CostFiniteDomain {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && self.costs == other.costs
    }
}

impl CostFiniteDomain {
    /// Construct a new cost-carrying domain from parallel `values` and
    /// `costs` slices.
    ///
    /// The two inputs are zipped element-wise, then sorted by value so
    /// that the internal cost table is canonical regardless of input
    /// order. Duplicate values are not deduplicated — callers are
    /// responsible for passing a unique value set.
    ///
    /// # Panics
    ///
    /// Panics if `values.len() != costs.len()`.
    pub fn new(values: Vec<i32>, costs: Vec<f64>) -> Self {
        assert_eq!(
            values.len(),
            costs.len(),
            "CostFiniteDomain::new: values and costs length mismatch ({} vs {})",
            values.len(),
            costs.len(),
        );
        let mut pairs: Vec<(i32, f64)> = values.into_iter().zip(costs).collect();
        pairs.sort_by_key(|(v, _)| *v);
        let sorted_values: Vec<i32> = pairs.iter().map(|(v, _)| *v).collect();
        Self {
            inner: FiniteDomain::new(sorted_values),
            costs: pairs,
            min_cost_cache: Cell::new(None),
        }
    }
}

impl Domain for CostFiniteDomain {
    type Value = i32;

    fn size(&self) -> usize {
        self.inner.size()
    }

    fn contains(&self, val: &i32) -> bool {
        self.inner.contains(val)
    }

    fn remove(&mut self, val: &i32) -> bool {
        let changed = self.inner.remove(val);
        if changed {
            self.min_cost_cache.set(None);
        }
        changed
    }

    fn add(&mut self, val: &i32) {
        self.inner.add(val);
        self.min_cost_cache.set(None);
    }

    fn values(&self) -> Vec<i32> {
        self.inner.values()
    }

    fn iter(&self) -> impl Iterator<Item = i32> + use<> {
        self.inner.iter()
    }
}

impl CostDomain for CostFiniteDomain {
    /// Lookup cost for `val` via binary search over the sorted table.
    ///
    /// Returns `f64::INFINITY` if the value was never registered at
    /// construction time. This is defensive — the solver only ever
    /// queries values it read out of the domain, so a miss indicates
    /// a client contract violation and the infinity acts as a soft
    /// fence (the branch-and-bound search will reject it).
    fn cost(&self, val: &i32) -> f64 {
        match self.costs.binary_search_by(|(v, _)| v.cmp(val)) {
            Ok(idx) => self.costs[idx].1,
            Err(_) => f64::INFINITY,
        }
    }

    /// Amortized O(1) minimum cost over the live domain.
    ///
    /// The result is lazily cached in a `Cell` and invalidated by
    /// `remove`/`add`. The branch-and-bound solver calls `min_cost()`
    /// once per unassigned variable per search node, so caching
    /// eliminates the dominant O(domain) scan at large domain sizes.
    fn min_cost(&self) -> f64 {
        if let Some(cached) = self.min_cost_cache.get() {
            return cached;
        }
        let min = self
            .inner
            .iter()
            .map(|v| self.cost(&v))
            .fold(f64::INFINITY, f64::min);
        self.min_cost_cache.set(Some(min));
        min
    }
}
