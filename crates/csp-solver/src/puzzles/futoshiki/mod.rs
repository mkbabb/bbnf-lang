//! Futoshiki puzzle solver.
//!
//! N×N Latin square with inequality constraints between adjacent cells.

pub mod csp;
pub mod generate;

pub use csp::{FutoshikiPuzzle, create_futoshiki_csp, solve_futoshiki};
pub use generate::{
    generate_futoshiki, generate_futoshiki_seeded, generate_futoshiki_tuned_seeded,
    measure_difficulty,
};
