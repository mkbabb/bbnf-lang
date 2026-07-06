//! Variable ordering heuristics.

use crate::constraint::VarId;
use crate::domain::Domain;
use crate::variable::Variable;

/// Variable ordering strategy for search.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ordering {
    /// Chronological: pick variables in the order they appear on the stack.
    Chronological,
    /// Fail-first (MRV): pick the variable with the smallest domain.
    FailFirst,
    /// Mrv: pick the variable minimizing `domain-size / Σ constraint-weights`.
    /// The weights are frozen at 1.0 (no dom/wdeg bumping is wired to the
    /// kernel), so this is a static heuristic; `Chs` shares the same scan with
    /// dynamically evolving weights.
    Mrv,
    /// CHS (Conflict-History Search): same `dom / Σ weights` scan as `Mrv`,
    /// but the weights are updated dynamically by an exponential recency-weighted
    /// average on each conflict (see [`crate::solver::heuristic`]).
    Chs,
}

/// Select the next unassigned variable from the stack according to the ordering heuristic.
///
/// Returns the index into `stack` of the chosen variable, or `None` if empty.
pub fn select_variable<D: Domain>(
    stack: &[VarId],
    variables: &[Variable<D>],
    ordering: Ordering,
    constraint_weights: &[f64],
    var_constraint_ids: &[Vec<usize>],
) -> Option<usize> {
    if stack.is_empty() {
        return None;
    }

    match ordering {
        Ordering::Chronological => Some(stack.len() - 1),

        Ordering::FailFirst => {
            let mut best_idx = 0;
            let mut best_size = usize::MAX;
            for (i, &var) in stack.iter().enumerate() {
                let sz = variables[var as usize].domain.size();
                if sz < best_size {
                    best_size = sz;
                    best_idx = i;
                }
            }
            Some(best_idx)
        }

        // CHS and Mrv share the `dom / Σ weights` branching rule; they
        // differ only in *how* `constraint_weights` evolves (static 1.0 for
        // Mrv, ERWA-updated for CHS — see `solver::heuristic`).
        Ordering::Mrv | Ordering::Chs => {
            let mut best_idx = 0;
            let mut best_score = f64::MAX;
            for (i, &var) in stack.iter().enumerate() {
                let dom_size = variables[var as usize].domain.size() as f64;
                let wdeg: f64 = var_constraint_ids[var as usize]
                    .iter()
                    .map(|&cid| constraint_weights[cid])
                    .sum();
                let score = dom_size / wdeg.max(1e-9);
                if score < best_score {
                    best_score = score;
                    best_idx = i;
                }
            }
            Some(best_idx)
        }
    }
}
