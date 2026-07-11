//! AC-3 worklist propagation using the adjacency graph.
//!
//! Uses a bitset worklist for lower overhead than VecDeque + `Vec<bool>`.

use crate::solver::adjacency::Adjacency;
use crate::bitscan::pop_lowest_bit;
use crate::constraint::{ConstraintEnum, Revision, VarId};
use crate::domain::Domain;
use crate::solver::Trail;
use crate::variable::Variable;
use crate::{SolveStats, Unsatisfiable};

/// A simple bitset worklist — O(1) insert/membership, O(words) scan for next.
///
/// Sized once (to the constraint count) and reused as scratch across an entire
/// search — the kernel's `Kernel` (and the `propagate_with`/`solve_with_given`
/// root calls) own one instance and every candidate-value attempt calls
/// `clear()`/`fill_full()` (an O(words) sweep, no allocation) instead of
/// constructing a fresh `Vec<u64>` per call (Pass-1 propagation audit, P2-2).
pub(crate) struct BitsetWorklist {
    words: Vec<u64>,
    /// Constraint count this worklist was sized for — needed by `fill_full`
    /// to mask off the unused high bits of the last word.
    capacity: usize,
}

impl BitsetWorklist {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            words: vec![0; capacity.div_ceil(64)],
            capacity,
        }
    }

    /// Zero every word — O(words), no allocation. Used before reseeding from a
    /// single variable's incident constraints.
    fn clear(&mut self) {
        self.words.fill(0);
    }

    /// Set every constraint index in `0..capacity` — O(words), no allocation.
    /// Used before a full sweep (`ac3_full`).
    fn fill_full(&mut self) {
        self.words.fill(u64::MAX);
        let remainder = self.capacity % 64;
        if remainder != 0
            && let Some(last) = self.words.last_mut()
        {
            *last = (1u64 << remainder) - 1;
        }
    }

    fn insert(&mut self, idx: usize) {
        self.words[idx / 64] |= 1u64 << (idx % 64);
    }

    fn contains(&self, idx: usize) -> bool {
        self.words[idx / 64] & (1u64 << (idx % 64)) != 0
    }

    fn pop(&mut self) -> Option<usize> {
        for (wi, word) in self.words.iter_mut().enumerate() {
            if let Some(bit) = pop_lowest_bit(word) {
                return Some(wi * 64 + bit);
            }
        }
        None
    }
}

/// Run AC-3 propagation over all constraints.
///
/// Returns `Err(Unsatisfiable)` if a domain wipe-out is detected. `worklist` is
/// caller-owned reusable scratch (see `BitsetWorklist`) — reset here via
/// `fill_full()` rather than freshly allocated.
pub(crate) fn ac3_full<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    stats: &mut SolveStats,
    worklist: &mut BitsetWorklist,
    depth: usize,
) -> Result<(), Unsatisfiable>
where
    D::Value: PartialEq + 'static,
{
    worklist.fill_full();

    while let Some(idx) = worklist.pop() {
        match constraints[idx].revise(variables, depth) {
            Revision::Unchanged => {}
            Revision::Changed => {
                stats.propagations += 1;
                for &neighbor in adjacency.neighbors_of_constraint(idx) {
                    worklist.insert(neighbor as usize);
                }
            }
            Revision::Unsatisfiable => return Err(Unsatisfiable),
        }
    }

    Ok(())
}

/// Run AC-3 propagation seeded from a single variable assignment (MAC).
///
/// Every constraint that revises records its scope on `trail` — a superset of
/// the variables actually pruned (scopes are small), so backtrack undoes only
/// touched variables instead of sweeping all of them.
///
/// Returns `Some(ci)` if constraint `ci`'s revision wiped out a domain (the
/// "blame" signal for conflict-history weighting), `None` on success.
///
/// `worklist` is caller-owned reusable scratch — reset here via `clear()`
/// rather than freshly allocated on every candidate-value attempt (P2-2).
#[allow(clippy::too_many_arguments)]
pub(crate) fn ac3_from_variable<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &[Option<D::Value>],
    stats: &mut SolveStats,
    trail: &mut Trail,
    worklist: &mut BitsetWorklist,
    depth: usize,
) -> Option<usize>
where
    D::Value: PartialEq + 'static,
{
    worklist.clear();

    for &ci in adjacency.constraints_for(var) {
        let ci = ci as usize;
        let scope = constraints[ci].scope();
        if scope
            .iter()
            .any(|&v| v != var && assignment[v as usize].is_none())
        {
            worklist.insert(ci);
        }
    }

    while let Some(idx) = worklist.pop() {
        match constraints[idx].revise(variables, depth) {
            Revision::Unchanged => {}
            Revision::Changed => {
                stats.propagations += 1;
                // Record the whole scope before any early return: revise() may
                // have pruned several scope vars, and the trail must hold all of
                // them so backtrack restores every one.
                let scope = constraints[idx].scope();
                for &v in scope {
                    trail.push(v);
                }
                if scope
                    .iter()
                    .any(|&v| variables[v as usize].domain.is_empty())
                {
                    return Some(idx);
                }
                for &neighbor in adjacency.neighbors_of_constraint(idx) {
                    let n = neighbor as usize;
                    if !worklist.contains(n) {
                        let scope = constraints[n].scope();
                        if scope.iter().any(|&v| assignment[v as usize].is_none()) {
                            worklist.insert(n);
                        }
                    }
                }
            }
            Revision::Unsatisfiable => {
                // revise() may prune several scope vars before detecting the
                // wipe-out (e.g. AllDifferent's singleton-removal loop empties
                // a peer mid-iteration, after real prunes to earlier peers).
                // Those prunes live on each Variable's own depth-keyed undo log
                // but must also be on the external Trail, or Trail::undo_to —
                // which only restores variables it was told were touched —
                // leaks them permanently into sibling branches. Record the
                // whole scope before returning, exactly as the Changed arm does.
                let scope = constraints[idx].scope();
                for &v in scope {
                    trail.push(v);
                }
                return Some(idx);
            }
        }
    }

    None
}
