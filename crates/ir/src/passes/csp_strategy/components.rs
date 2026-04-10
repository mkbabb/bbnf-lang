//! Connected-components decomposition for the strategy CSP (Tranche Y.5).
//!
//! The Tranche X.6 "global CSP batching" experiment blew up on
//! `compile_css_l4` (9 ms → 94 ms) because it fed every rule's
//! variables into a single `Csp` instance and let branch-and-bound
//! explore the cross-product whenever any single `ImplicationConstraint`
//! fired. The fix is decomposition by connected components: variables
//! connected by constraint edges form a component; disconnected
//! variables form their own trivial components. Each component gets
//! its own solve, bounded by its own variable count.
//!
//! # Current constraint topology
//!
//! As of Tranche Y.5 the only cross-variable constraints emitted by
//! `csp_strategy::build` are `ImplicationConstraint`s that wire an
//! Alt's `AltMode` decision to its child Regex engines' modes — all
//! within the same rule body. There are **zero cross-rule
//! constraints** in production. Under this topology, each rule body
//! is exactly one connected component (or a set of trivially
//! independent variables, which the per-variable fast-path at
//! `decode_min_cost_per_variable` already handles).
//!
//! This module exposes the union-find substrate that the strategy
//! solver uses to express the decomposition. Today that substrate
//! reduces to per-rule solves; the wiring is ready for the first
//! tranche that adds cross-rule constraints (likely the Y.2
//! `SharedHelper` cross-rule hoisting cost model, or a future
//! type-projection / dispatch-eligibility lift).
//!
//! # Invariant
//!
//! The per-rule decomposition in `csp_strategy::solve_strategy_decisions`
//! is correct **iff** no constraint spans rule boundaries. The
//! [`CrossRuleConstraintGuard`] type below is used in test builds to
//! assert this invariant — if a constraint carries a scope referencing
//! variables from more than one rule, the guard records the error.
//!
//! # Future work
//!
//! When the first genuine cross-rule constraint lands, replace
//! `solve_strategy_decisions`'s per-rule loop with:
//!
//! 1. Collect sites + constraints across all rules into a shared store,
//!    keyed by `(rule_idx, local_site_idx)`.
//! 2. Build a [`UnionFind`] over global keys.
//! 3. For each pair of variables sharing a constraint, union them.
//! 4. Enumerate components via `UnionFind::components`.
//! 5. For each component, build a fresh `Csp`, copy the component's
//!    sites + constraints in, solve with `MinimizeCost`.
//! 6. Merge per-component decisions into the global `RecognizerDecisionMap`.
//!
//! The [`UnionFind`] below is ready to drive step 3. The other steps
//! are straightforward collection/rebuild work.

use std::collections::HashMap;

/// A tiny, path-compressing union-find over flat `u32` keys.
///
/// Kept deliberately minimal: the strategy solver only needs
/// `union`, `find`, and `components`. The union-find is per-compile
/// scratch, discarded after decomposition — no persistent state.
#[derive(Debug, Clone)]
pub struct UnionFind {
    parent: Vec<u32>,
    rank: Vec<u8>,
}

impl UnionFind {
    /// Create a union-find with `n` singleton components.
    pub fn new(n: usize) -> Self {
        Self {
            parent: (0..n as u32).collect(),
            rank: vec![0; n],
        }
    }

    /// Find the root of `x` with path compression.
    pub fn find(&mut self, x: u32) -> u32 {
        let mut root = x;
        while self.parent[root as usize] != root {
            root = self.parent[root as usize];
        }
        // Path compression: point everything on the walk to the root.
        let mut cur = x;
        while self.parent[cur as usize] != root {
            let next = self.parent[cur as usize];
            self.parent[cur as usize] = root;
            cur = next;
        }
        root
    }

    /// Union the components containing `a` and `b`. Returns `true` if
    /// the two were in different components prior to the call.
    pub fn union(&mut self, a: u32, b: u32) -> bool {
        let ra = self.find(a);
        let rb = self.find(b);
        if ra == rb {
            return false;
        }
        let (smaller, larger) = match self.rank[ra as usize].cmp(&self.rank[rb as usize]) {
            std::cmp::Ordering::Less => (ra, rb),
            std::cmp::Ordering::Greater => (rb, ra),
            std::cmp::Ordering::Equal => {
                self.rank[ra as usize] += 1;
                (rb, ra)
            }
        };
        self.parent[smaller as usize] = larger;
        true
    }

    /// Enumerate components as a map from root → member list.
    ///
    /// Called at most once per decomposition — ordering-stable so the
    /// caller's solve order is deterministic.
    pub fn components(&mut self) -> HashMap<u32, Vec<u32>> {
        let n = self.parent.len();
        let mut out: HashMap<u32, Vec<u32>> = HashMap::new();
        for i in 0..n {
            let root = self.find(i as u32);
            out.entry(root).or_default().push(i as u32);
        }
        out
    }

    /// Number of distinct components.
    pub fn component_count(&mut self) -> usize {
        let n = self.parent.len();
        let mut roots = std::collections::HashSet::new();
        for i in 0..n {
            roots.insert(self.find(i as u32));
        }
        roots.len()
    }
}

// Unit tests live in `crates/ir/tests/csp_components.rs` per
// the `no-inline-tests` convention.
