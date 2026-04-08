//! Cost-model-driven extraction.
//!
//! Given a saturated e-graph, extraction picks the cheapest term rooted at
//! a chosen e-class. The cheapest term is selected bottom-up: each e-class
//! is assigned the cost of its cheapest e-node, computed from child-class
//! costs.
//!
//! This is the Egg "greedy bottom-up" algorithm. It's not optimal for
//! all cost models (e.g., cost models that depend on sharing require a
//! more sophisticated extractor), but it works for the monotone cost
//! models used by grammar/regex optimization.

use std::collections::HashMap;
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use crate::analysis::Analysis;
use crate::egraph::EGraph;
use crate::id::Id;
use crate::language::Language;

type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// A cost model assigns a scalar cost to each e-node, given the costs
/// of its children's best representatives.
pub trait CostModel<N: Language> {
    /// The cost type. Must be totally ordered (`PartialOrd` is sufficient
    /// for extraction since we compare against existing bests).
    type Cost: Clone + PartialOrd;

    /// Cost of an e-node, given a closure that returns the pre-computed
    /// best cost of each child e-class.
    fn cost(&self, node: &N, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost;
}

/// The trivial `AstSize` cost model: each node costs 1 + sum of children.
/// Extraction minimizes total node count.
#[derive(Clone, Copy, Debug, Default)]
pub struct AstSize;

impl<N: Language> CostModel<N> for AstSize {
    type Cost = usize;

    fn cost(&self, node: &N, child_cost: impl Fn(Id) -> Self::Cost) -> Self::Cost {
        let mut sum: usize = 1;
        for &child in node.children() {
            sum = sum.saturating_add(child_cost(child));
        }
        sum
    }
}

/// Extractor: computes best costs per class and reconstructs expression
/// trees on demand.
pub struct Extractor<'a, N: Language, A: Analysis<N>, C: CostModel<N>> {
    egraph: &'a EGraph<N, A>,
    cost_model: &'a C,
    best: FxHashMap<Id, (C::Cost, N)>,
}

impl<'a, N: Language, A: Analysis<N>, C: CostModel<N>> Extractor<'a, N, A, C> {
    /// Build an extractor and compute best costs for every e-class in the
    /// e-graph via fixed-point iteration.
    pub fn new(egraph: &'a EGraph<N, A>, cost_model: &'a C) -> Self {
        let mut ext = Self {
            egraph,
            cost_model,
            best: FxHashMap::default(),
        };
        ext.compute_best();
        ext
    }

    fn compute_best(&mut self) {
        // Fixed-point iteration: repeatedly scan all e-classes and update
        // each class's best node/cost based on current child bests. Stop
        // when no class's best improves.
        loop {
            let mut changed = false;
            for class in self.egraph.classes() {
                for node in class.iter() {
                    // Skip nodes whose children aren't fully resolved yet.
                    if !node
                        .children()
                        .iter()
                        .all(|c| self.best.contains_key(&self.egraph.find_ref(*c)))
                    {
                        continue;
                    }
                    let cost = self.cost_model.cost(node, |c| {
                        self.best[&self.egraph.find_ref(c)].0.clone()
                    });
                    let canonical = self.egraph.find_ref(class.id);
                    let improved = match self.best.get(&canonical) {
                        Some((existing, _)) => cost < *existing,
                        None => true,
                    };
                    if improved {
                        self.best.insert(canonical, (cost, node.clone()));
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
        }
    }

    /// The best cost found for class `id`. Returns `None` if extraction
    /// didn't reach this class (e.g., it has no fully-extracted children).
    pub fn best_cost(&self, id: Id) -> Option<C::Cost> {
        self.best
            .get(&self.egraph.find_ref(id))
            .map(|(c, _)| c.clone())
    }

    /// Find the best e-node in class `id`, cloning it with canonical child IDs.
    pub fn best_node(&self, id: Id) -> Option<&N> {
        self.best
            .get(&self.egraph.find_ref(id))
            .map(|(_, n)| n)
    }

    /// Extract a full tree rooted at `id` by recursively descending into
    /// each child's best e-node. Returns a sequence of `(Id, N)` pairs
    /// suitable for rebuilding a standalone term.
    pub fn extract_tree(&self, id: Id) -> Option<Vec<(Id, N)>> {
        let mut result = Vec::new();
        let mut visited: FxHashMap<Id, ()> = FxHashMap::default();
        self.extract_recurse(id, &mut result, &mut visited)?;
        Some(result)
    }

    fn extract_recurse(
        &self,
        id: Id,
        result: &mut Vec<(Id, N)>,
        visited: &mut FxHashMap<Id, ()>,
    ) -> Option<()> {
        let canonical = self.egraph.find_ref(id);
        if visited.insert(canonical, ()).is_some() {
            return Some(());
        }
        let node = self.best_node(canonical)?;
        for &child in node.children() {
            self.extract_recurse(child, result, visited)?;
        }
        result.push((canonical, node.clone()));
        Some(())
    }
}
