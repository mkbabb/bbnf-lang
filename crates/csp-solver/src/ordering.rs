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
    /// dom/wdeg: pick the variable with the smallest domain/weighted-degree ratio.
    DomWdeg,
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

        Ordering::DomWdeg => {
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
