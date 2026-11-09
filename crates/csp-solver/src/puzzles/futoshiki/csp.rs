//! Futoshiki CSP construction and solving.
//!
//! Puzzle format (flat arrays):
//!   n: grid size
//!   fixed_cells: [(cell_index, value), ...] — pre-filled cells
//!   inequalities: [(a, b), ...] — cell_a > cell_b

use crate::constraint::VarId;
use crate::domain::bitset::BitsetDomain;
use crate::error::CspError;
use crate::ordering::Ordering;
use crate::{Csp, Pruning, SolveConfig};

/// A parsed Futoshiki puzzle.
#[derive(Debug, Clone, PartialEq, Eq)]
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

    /// Construct a validated puzzle from wire parts — the network-facing entry
    /// point. [`parse`](Self::parse) reads the CSC411 CLI text format and must
    /// **not** reach the wire (F11); this constructor is what the PyO3/wasm/HTTP
    /// layers call, and it rejects structurally invalid input up front with the
    /// existing [`CspError::InvalidInput`] — **no new error variant** (F4):
    ///
    /// - every fixed-cell index `< n²`, its value in `1..=n`;
    /// - every inequality index `< n²`, and each pair orthogonally adjacent
    ///   (`|Δrow| + |Δcol| == 1`) — the only relation a boundary caret can
    ///   render, so a valid-per-solver but unrenderable-per-frontend pair
    ///   (opposite corners, say) is rejected here rather than solved silently.
    pub fn from_parts(
        n: u32,
        fixed_cells: Vec<(usize, u32)>,
        inequalities: Vec<(usize, usize)>,
    ) -> Result<Self, CspError> {
        let nn = n as usize;
        let total = nn * nn;

        for &(cell, value) in &fixed_cells {
            if cell >= total {
                return Err(CspError::invalid_input(format!(
                    "fixed-cell index {cell} out of range for a {n}×{n} board (must be < {total})"
                )));
            }
            if value == 0 || value as usize > nn {
                return Err(CspError::invalid_input(format!(
                    "fixed-cell value {value} out of range for a {n}×{n} board (must be 1..={n})"
                )));
            }
        }

        for &(a, b) in &inequalities {
            if a >= total || b >= total {
                return Err(CspError::invalid_input(format!(
                    "inequality pair ({a}, {b}) out of range for a {n}×{n} board \
                     (both indices must be < {total})"
                )));
            }
            let (ra, ca) = (a / nn, a % nn);
            let (rb, cb) = (b / nn, b % nn);
            let manhattan = ra.abs_diff(rb) + ca.abs_diff(cb);
            if manhattan != 1 {
                return Err(CspError::invalid_input(format!(
                    "inequality pair ({a}, {b}) is not orthogonally adjacent: cells \
                     ({ra},{ca}) and ({rb},{cb}) are Manhattan distance {manhattan} apart, \
                     but a caret can only sit on a shared cell edge (distance 1)"
                )));
            }
        }

        Ok(Self {
            n,
            fixed_cells,
            inequalities,
        })
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

/// Solve a Futoshiki puzzle. Returns all solutions as flat `Vec<u32>`.
///
/// **F1 production override.** The shipped default (`ForwardChecking` +
/// `FailFirst`) cannot find a first solution to an *empty* N≥6 board inside the
/// 1M-node budget (measured: 4.56M backtracks at N=6, budget-dead). Mirroring
/// the Sudoku production override (`py::solve_sudoku` uses `Ac3` + `Mrv`) fixes
/// it: N=4–9 empty boards solve in 0 backtracks, sub-0.1 ms each. Regression
/// guarded at N=6/7 in `tests/futoshiki.rs`.
pub fn solve_futoshiki(puzzle: &FutoshikiPuzzle) -> Vec<Vec<u32>> {
    let mut csp = create_futoshiki_csp(puzzle);
    let config = SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::Mrv,
        max_solutions: usize::MAX,
        ..Default::default()
    };
    csp.solve(&config)
}
