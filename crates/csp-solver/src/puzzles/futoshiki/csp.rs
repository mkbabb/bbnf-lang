//! Futoshiki CSP construction and solving.
//!
//! Puzzle format (flat arrays):
//!   n: grid size
//!   fixed_cells: [(cell_index, value), ...] — pre-filled cells
//!   inequalities: [(a, b), ...] — cell_a > cell_b

use crate::constraint::VarId;
use crate::domain::bitset::BitsetDomain;
use crate::ordering::Ordering;
use crate::{Csp, Pruning, SolveConfig};

/// A parsed Futoshiki puzzle.
pub struct FutoshikiPuzzle {
    pub n: u32,
    pub fixed_cells: Vec<(usize, u32)>,
    pub inequalities: Vec<(usize, usize)>,
}

impl FutoshikiPuzzle {
    /// Parse from the standard file format:
    /// ```text
    /// N
    /// L0 L1 ... (cell indices with fixed values)
    /// V0 V1 ... (corresponding values)
    /// A0 A1 ... (greater-than left sides)
    /// B0 B1 ... (greater-than right sides)
    /// ```
    pub fn parse(input: &str) -> Self {
        let mut lines = input.lines();
        let n: u32 = lines.next().unwrap().trim().parse().unwrap();

        let ls: Vec<usize> = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();
        let vs: Vec<u32> = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        let fixed_cells: Vec<(usize, u32)> = ls.into_iter().zip(vs).collect();

        let a_s: Vec<usize> = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();
        let b_s: Vec<usize> = lines
            .next()
            .unwrap()
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect();

        let inequalities: Vec<(usize, usize)> = a_s.into_iter().zip(b_s).collect();

        Self {
            n,
            fixed_cells,
            inequalities,
        }
    }
}

/// Build a Futoshiki CSP from a parsed puzzle.
pub fn create_futoshiki_csp(puzzle: &FutoshikiPuzzle) -> Csp<BitsetDomain> {
    let n = puzzle.n;
    let total = (n * n) as usize;

    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=n);

    for _ in 0..total {
        csp.add_variable(domain.clone());
    }

    // Fixed cell values.
    for &(cell, value) in &puzzle.fixed_cells {
        csp.add_equals(cell as VarId, value);
    }

    // Inequality constraints: cell_a > cell_b.
    for &(a, b) in &puzzle.inequalities {
        csp.add_greater_than(a as VarId, b as VarId);
    }

    // Row all-different.
    for i in 0..n {
        let row: Vec<VarId> = (0..n).map(|j| (i * n + j) as VarId).collect();
        csp.add_all_different(row);
    }

    // Column all-different.
    for j in 0..n {
        let col: Vec<VarId> = (0..n).map(|i| (i * n + j) as VarId).collect();
        csp.add_all_different(col);
    }

    csp.finalize();
    csp
}

/// Solve a Futoshiki puzzle. Returns all solutions as flat Vec<u32>.
pub fn solve_futoshiki(puzzle: &FutoshikiPuzzle) -> Vec<Vec<u32>> {
    let mut csp = create_futoshiki_csp(puzzle);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: usize::MAX,
        ..Default::default()
    };
    csp.solve(&config)
}
