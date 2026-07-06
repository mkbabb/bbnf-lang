//! A generalized CSP (Constraint Satisfaction Problem) solver.
//!
//! Isomorphic to the Python CSP solver. Supports:
//! - Backtracking search with configurable pruning and variable ordering
//! - AC-3 (Maintaining Arc Consistency) propagation
//! - Forward checking
//! - AC-FC hybrid
//! - Lattice domains for monotonic fixed-point propagation
//!
//! The crate-root surface is assembled from three internal modules:
//! [`config`] (solve vocabulary + the `Csp<D>` container), [`csp`] (the
//! builder methods), and [`csp::solve`] (propagation + search dispatch).

pub mod adjacency;
pub(crate) mod bitscan;
pub mod builder;
pub mod cancel;
pub(crate) mod config;
pub mod constraint;
pub(crate) mod csp;
pub mod domain;
pub mod error;
pub mod ordering;
pub mod puzzles;
#[cfg(feature = "py")]
pub mod py;
pub mod solver;
pub mod variable;

pub use builder::assignment::{
    AssignmentBuilder, AssignmentError, AssignmentSolution, SENTINEL, assignment,
};
pub use cancel::CancelToken;
pub use config::{Csp, OptimizationMode, PropagationStrategy, Pruning, SolveConfig, SolveStats};
pub use csp::solve::Unsatisfiable;
pub use error::CspError;
pub use puzzles::sudoku;
