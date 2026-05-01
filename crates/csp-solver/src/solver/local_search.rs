//! Min-conflicts hill-climbing local search.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, VarId};
use crate::domain::Domain;
use crate::variable::Variable;

// ---------------------------------------------------------------------------
// Simple LCG PRNG (no external deps)
// ---------------------------------------------------------------------------

struct Rng(u64);

impl Rng {
    fn new() -> Self {
        let seed = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;
        Self(seed)
    }

    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.0
    }

    fn usize(&mut self, bound: usize) -> usize {
        (self.next() % bound as u64) as usize
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Count the number of constraints violated if `var` is assigned `val` given
/// the current `assignment`.
fn num_conflicts<D: Domain>(
    var: VarId,
    val: &D::Value,
    assignment: &mut [Option<D::Value>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
) -> usize
where
    D::Value: PartialEq + Clone,
{
    // Temporarily patch var's slot, count violations, then restore.
    let original = assignment[var as usize].take();
    assignment[var as usize] = Some(val.clone());

    let mut count = 0;
    for &ci in adjacency.constraints_for(var) {
        if !constraints[ci as usize].check(assignment) {
            count += 1;
        }
    }

    // Restore.
    assignment[var as usize] = original;
    count
}

/// Return the set of variable ids that participate in at least one violated
/// constraint under the current (complete) assignment.
fn conflicting_variables<D: Domain>(
    assignment: &[Option<D::Value>],
    constraints: &[ConstraintEnum<D>],
    _adjacency: &Adjacency,
) -> Vec<VarId>
where
    D::Value: PartialEq + Clone,
{
    let num_vars = assignment.len();
    let mut in_conflict = vec![false; num_vars];

    for constraint in constraints {
        if !constraint.check(assignment) {
            for &v in constraint.scope() {
                in_conflict[v as usize] = true;
            }
        }
    }

    (0..num_vars as VarId)
        .filter(|&v| in_conflict[v as usize])
        .collect()
}

// ---------------------------------------------------------------------------
// Min-conflicts solver
// ---------------------------------------------------------------------------

/// Run min-conflicts local search.
///
/// Starts from a random complete assignment drawn from each variable's current
/// domain, then iteratively picks a random conflicting variable and reassigns
/// it to the value that minimises the number of violated constraints.
///
/// Returns `Some(assignment)` if a solution is found within `max_iterations`,
/// or `None` if the budget is exhausted.
pub fn min_conflicts<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    max_iterations: usize,
) -> Option<Vec<D::Value>>
where
    D::Value: PartialEq + Clone,
{
    let n = variables.len();
    if n == 0 {
        return Some(Vec::new());
    }

    let mut rng = Rng::new();

    // 1. Random initial assignment.
    let mut assignment: Vec<Option<D::Value>> = Vec::with_capacity(n);
    for var in variables.iter() {
        let vals = var.domain.values();
        if vals.is_empty() {
            return None; // empty domain → unsatisfiable
        }
        let idx = rng.usize(vals.len());
        assignment.push(Some(vals[idx].clone()));
    }

    // 2. Iterative improvement.
    for _ in 0..max_iterations {
        let conflicts = conflicting_variables(&assignment, constraints, adjacency);
        if conflicts.is_empty() {
            // All constraints satisfied — extract solution.
            return Some(assignment.into_iter().map(|v| v.unwrap()).collect());
        }

        // Pick a random conflicting variable.
        let var = conflicts[rng.usize(conflicts.len())];
        let domain_vals = variables[var as usize].domain.values();
        if domain_vals.is_empty() {
            return None;
        }

        // Find the value that minimises conflicts.  On ties, keep the first
        // encountered (which is effectively random given domain iteration order
        // and the random initial state).
        let mut best_val = domain_vals[0].clone();
        let mut best_count = usize::MAX;

        for val in &domain_vals {
            let c = num_conflicts::<D>(var, val, &mut assignment, constraints, adjacency);
            if c < best_count {
                best_count = c;
                best_val = val.clone();
                if best_count == 0 {
                    break; // can't do better
                }
            }
        }

        assignment[var as usize] = Some(best_val);
    }

    // Budget exhausted.
    None
}
