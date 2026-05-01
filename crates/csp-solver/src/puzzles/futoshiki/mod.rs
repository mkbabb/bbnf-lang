//! Futoshiki puzzle solver.
//!
//! N×N Latin square with inequality constraints between adjacent cells.

pub mod csp;

pub use csp::{create_futoshiki_csp, solve_futoshiki, FutoshikiPuzzle};
