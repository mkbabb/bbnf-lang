//! Sudoku puzzle creation, solving, generation, and symmetry transforms.

pub mod csp;
pub mod generate;
pub(crate) mod rng;
pub mod transform;

pub use csp::{create_sudoku_csp, solve_sudoku};
pub use generate::{
    Difficulty, generate_board, generate_board_with_templates, generate_from_template,
    measure_difficulty,
};
pub use transform::{SudokuTransform, apply_random_transform};
