//! AC-3 worklist propagation using the adjacency graph.
//!
//! Uses a bitset worklist for lower overhead than VecDeque + Vec<bool>.

use crate::adjacency::Adjacency;
use crate::constraint::{ConstraintEnum, Revision, VarId};
use crate::domain::Domain;
use crate::variable::Variable;
use crate::{SolveStats, Unsatisfiable};

/// A simple bitset worklist — O(1) insert/membership, O(words) scan for next.
struct BitsetWorklist {
    words: Vec<u64>,
}

impl BitsetWorklist {
    fn new(capacity: usize) -> Self {
        let num_words = capacity.div_ceil(64);
        Self {
            words: vec![0; num_words],
        }
    }

    fn new_full(capacity: usize) -> Self {
        let num_words = capacity.div_ceil(64);
        let mut words = vec![u64::MAX; num_words];
        let remainder = capacity % 64;
        if remainder != 0 && !words.is_empty() {
            *words.last_mut().unwrap() = (1u64 << remainder) - 1;
        }
        Self { words }
    }

    fn insert(&mut self, idx: usize) {
        self.words[idx / 64] |= 1u64 << (idx % 64);
    }

    fn contains(&self, idx: usize) -> bool {
        self.words[idx / 64] & (1u64 << (idx % 64)) != 0
    }

    fn pop(&mut self) -> Option<usize> {
        for (wi, word) in self.words.iter_mut().enumerate() {
            if *word != 0 {
                let bit = word.trailing_zeros() as usize;
                *word &= *word - 1;
                return Some(wi * 64 + bit);
            }
        }
        None
    }
}

/// Run AC-3 propagation over all constraints.
///
/// Returns `Err(Unsatisfiable)` if a domain wipe-out is detected.
pub fn ac3_full<D: Domain>(
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    stats: &mut SolveStats,
    depth: usize,
) -> Result<(), Unsatisfiable>
where
    D::Value: PartialEq,
{
    let mut worklist = BitsetWorklist::new_full(constraints.len());

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
pub fn ac3_from_variable<D: Domain>(
    var: VarId,
    variables: &mut [Variable<D>],
    constraints: &[ConstraintEnum<D>],
    adjacency: &Adjacency,
    assignment: &[Option<D::Value>],
    stats: &mut SolveStats,
    depth: usize,
) -> bool
where
    D::Value: PartialEq,
{
    let mut worklist = BitsetWorklist::new(constraints.len());

    for &ci in adjacency.constraints_for(var) {
        let ci = ci as usize;
        let scope = constraints[ci].scope();
        if scope.iter().any(|&v| v != var && assignment[v as usize].is_none()) {
            worklist.insert(ci);
        }
    }

    while let Some(idx) = worklist.pop() {
        match constraints[idx].revise(variables, depth) {
            Revision::Unchanged => {}
            Revision::Changed => {
                stats.propagations += 1;
                for &v in constraints[idx].scope() {
                    if variables[v as usize].domain.is_empty() {
                        return true;
                    }
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
            Revision::Unsatisfiable => return true,
        }
    }

    false
}
