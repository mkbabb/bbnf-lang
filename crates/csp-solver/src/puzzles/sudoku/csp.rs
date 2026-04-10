//! Sudoku CSP construction and solving.

use crate::constraint::VarId;
use crate::domain::bitset::BitsetDomain;
use crate::{Csp, SolveConfig};

/// Create a Sudoku CSP from a flat board array where `0` = empty cell.
///
/// `n` is the sub-grid size (3 for standard 9x9, 4 for 16x16, 2 for 4x4).
/// Returns the CSP (finalized) and the given values for `solve_with_given`.
pub fn create_sudoku_csp(board: &[u32], n: u32) -> (Csp<BitsetDomain>, Vec<(VarId, u32)>) {
    let m = n * n;
    let total = (m * m) as usize;
    assert_eq!(board.len(), total, "board must have M*M = {} elements", total);

    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=m);

    for _ in 0..total {
        csp.add_variable(domain.clone());
    }

    for r in 0..m {
        let row_vars: Vec<VarId> = (0..m).map(|c| (r * m + c) as VarId).collect();
        csp.add_all_different(row_vars);
    }
    for c in 0..m {
        let col_vars: Vec<VarId> = (0..m).map(|r| (r * m + c) as VarId).collect();
        csp.add_all_different(col_vars);
    }
    for bi in 0..n {
        for bj in 0..n {
            let box_vars: Vec<VarId> = (0..n)
                .flat_map(|di| (0..n).map(move |dj| ((bi * n + di) * m + (bj * n + dj)) as VarId))
                .collect();
            csp.add_all_different(box_vars);
        }
    }

    csp.finalize();

    let given: Vec<(VarId, u32)> = board
        .iter()
        .enumerate()
        .filter(|&(_, &v)| v != 0)
        .map(|(i, &v)| (i as VarId, v))
        .collect();

    (csp, given)
}

/// Solve a Sudoku puzzle. Returns `None` if unsolvable.
pub fn solve_sudoku(board: &[u32], n: u32, config: &SolveConfig) -> Option<Vec<u32>> {
    let (mut csp, given) = create_sudoku_csp(board, n);
    let solutions = csp.solve_with_given(config, &given);
    solutions.into_iter().next()
}
