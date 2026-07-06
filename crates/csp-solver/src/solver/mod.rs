//! Solver dispatch: AC-3 propagation, forward checking, and the unified
//! monomorphized search kernel.

pub mod ac3;
pub mod gac;
pub mod heuristic;
pub mod monotonic;
pub mod nogoods;
pub mod optimize;
pub mod propagate;
pub mod restart;
pub mod search;

use crate::constraint::VarId;
use crate::domain::Domain;
use crate::variable::Variable;

/// Solution type: a vector of values indexed by variable ID.
pub type Solution<D> = Vec<<D as Domain>::Value>;

/// Global undo trail of *touched* variables, owned by the search.
///
/// Replaces the per-`Variable` `pruned` restore-sweep: on backtrack the old
/// search re-probed **every** variable (`for v in variables { v.restore(depth) }`,
/// O(num_vars) per node). Here every prune records the affected `VarId`, so undo
/// visits only the variables actually touched at that depth — O(removed).
///
/// The trail records *only* which variable changed; each `Variable` keeps its
/// own depth-keyed `pruned` log, so restore semantics are unchanged. Double
/// recording a variable is harmless: the first `restore(depth)` pops all of that
/// depth's entries, later repeats are O(1) no-ops.
#[derive(Default)]
pub(crate) struct Trail {
    touched: Vec<VarId>,
}

impl Trail {
    /// Snapshot the trail length. Undo restores back to this checkpoint.
    #[inline]
    pub(crate) fn checkpoint(&self) -> usize {
        self.touched.len()
    }

    /// Record that `var` was touched (a value pruned) at the current depth.
    #[inline]
    pub(crate) fn push(&mut self, var: VarId) {
        self.touched.push(var);
    }

    /// Restore every variable touched since `mark`, undoing prunes at `depth`.
    #[inline]
    pub(crate) fn undo_to<D: Domain>(
        &mut self,
        mark: usize,
        depth: usize,
        variables: &mut [Variable<D>],
    ) {
        while self.touched.len() > mark {
            let var = self.touched.pop().unwrap();
            variables[var as usize].restore(depth);
        }
    }
}
