//! CSP variable with domain tracking and pruned-value undo log.

use crate::domain::Domain;

/// A CSP variable holding its current domain and an undo log for backtracking.
#[derive(Debug, Clone)]
pub struct Variable<D: Domain> {
    /// Current (working) domain -- mutated during search.
    pub domain: D,
    /// Original domain -- used for full reset.
    original_domain: D,
    /// Undo log: `(depth, removed_value)` pairs. During backtracking, all entries
    /// at the given depth are restored into the domain.
    pruned: Vec<(usize, D::Value)>,
}

impl<D: Domain> Variable<D> {
    /// Create a new variable with the given initial domain.
    pub fn new(domain: D) -> Self {
        Self {
            original_domain: domain.clone(),
            domain,
            pruned: Vec::new(),
        }
    }

    /// Record that `val` was pruned at `depth`, and remove it from the domain.
    /// Returns `true` if the value was actually present and removed.
    pub fn prune(&mut self, val: &D::Value, depth: usize) -> bool {
        if self.domain.remove(val) {
            self.pruned.push((depth, val.clone()));
            true
        } else {
            false
        }
    }

    /// Restore all values pruned at the given `depth`.
    pub fn restore(&mut self, depth: usize) {
        while let Some(&(d, _)) = self.pruned.last() {
            if d == depth {
                let (_, val) = self.pruned.pop().unwrap();
                self.domain.add(&val);
            } else {
                break;
            }
        }
    }

    /// Reset to the original domain, clearing the undo log.
    pub fn reset(&mut self) {
        self.domain = self.original_domain.clone();
        self.pruned.clear();
    }

    /// Replace the current domain entirely (used during initial propagation).
    pub fn set_domain(&mut self, domain: D) {
        self.domain = domain;
    }

    /// Drop the undo log without touching the working domain.
    ///
    /// Used by the restart driver to bake a post-propagation "baseline"
    /// domain in place: the reductions already applied to `domain` are kept,
    /// but their depth-tagged log entries are discarded so subsequent
    /// `restore()` calls at any search depth cannot un-prune them. This is
    /// the seam fix that stops a depth-0 backtrack from silently undoing
    /// `solve_with_given`'s initial AC-3.
    pub fn clear_log(&mut self) {
        self.pruned.clear();
    }

    /// Reset the working domain to `domain` and clear the undo log.
    ///
    /// The restart driver calls this to return every variable to the shared
    /// post-initial-propagation baseline before re-descending.
    pub fn reset_to(&mut self, domain: D) {
        self.domain = domain;
        self.pruned.clear();
    }

    /// Restrict domain to a single value, recording all other removals at `depth`.
    /// This is the fast path for backtracking assignment — avoids collecting
    /// domain values into a Vec just to prune them.
    ///
    /// Delegates the actual domain mutation to `Domain::restrict_to`, which
    /// for `BitsetDomain` clears every bit but `val`'s in one bitwise AND
    /// (O(1)) instead of this method's previous per-value iterate-and-remove
    /// loop (O(domain size) bit ops, on top of the `Vec` collect it used to
    /// allocate). This method's own job shrinks to what only it can do:
    /// recording each removed value on the depth-keyed undo log.
    pub fn restrict_to(&mut self, val: &D::Value, depth: usize) {
        for v in self.domain.restrict_to(val) {
            self.pruned.push((depth, v));
        }
    }
}
