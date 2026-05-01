//! Solver dispatch: AC-3 propagation, forward checking, backtracking search.

pub mod ac3;
pub mod backjump;
pub mod backtrack;
pub mod gac_alldiff;
pub mod gac_alldiff_except;
pub mod local_search;
pub mod monotonic;
pub mod nogoods;
pub mod optimize;
pub mod propagate;

use crate::adjacency::Adjacency;
use crate::constraint::ConstraintEnum;
use crate::domain::Domain;
use crate::variable::Variable;
use crate::SolveStats;

/// Shared search context passed to all recursive search strategies.
/// Carries the problem definition (variables, constraints, adjacency) and the
/// mutable statistics accumulator. Extracted to tame `too_many_arguments` on
/// `backtrack_recurse`, `backjump_recurse`, and `bb_recurse`.
pub(crate) struct SearchContext<'a, D: Domain> {
    pub variables: &'a mut [Variable<D>],
    pub constraints: &'a [ConstraintEnum<D>],
    pub adjacency: &'a Adjacency,
    pub stats: &'a mut SolveStats,
}
