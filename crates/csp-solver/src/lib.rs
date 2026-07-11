//! A generalized CSP (Constraint Satisfaction Problem) solver.
//!
//! This is the sole solver in the workspace. Supports:
//! - Backtracking search with configurable pruning and variable ordering
//! - AC-3 (Maintaining Arc Consistency) propagation
//! - Forward checking and the AC-FC hybrid
//! - GAC all-different (Régin 1994), default-ON
//! - Lattice domains for monotonic fixed-point propagation
//!
//! The optional PyO3 (`feature = "py"`) and wasm bindings expose this same
//! core to Python and JavaScript — they wrap it, they do not mirror a
//! separate implementation.
//!
//! The crate-root surface is assembled from three internal modules:
//! [`config`] (solve vocabulary + the `Csp<D>` container), [`csp`] (the
//! builder methods), and [`csp::solve`] (propagation + search dispatch).

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
