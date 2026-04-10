//! Futoshiki puzzle tests.

use csp_solver::puzzles::futoshiki::{FutoshikiPuzzle, solve_futoshiki};

#[test]
fn solve_4x4_futoshiki() {
    // 4×4 puzzle:
    // Fixed: cell 0 = 1, cell 5 = 3
    // Inequality: cell 1 > cell 2 (row 0: col 1 > col 2)
    let input = "4\n0 5\n1 3\n1\n2\n";
    let puzzle = FutoshikiPuzzle::parse(input);

    assert_eq!(puzzle.n, 4);
    assert_eq!(puzzle.fixed_cells, vec![(0, 1), (5, 3)]);
    assert_eq!(puzzle.inequalities, vec![(1, 2)]);

    let solutions = solve_futoshiki(&puzzle);
    assert!(!solutions.is_empty(), "puzzle should have at least one solution");

    // Verify each solution
    for sol in &solutions {
        assert_eq!(sol.len(), 16);
        // Check fixed cells
        assert_eq!(sol[0], 1);
        assert_eq!(sol[5], 3);
        // Check inequality: cell 1 > cell 2
        assert!(sol[1] > sol[2], "cell 1 ({}) must be > cell 2 ({})", sol[1], sol[2]);
        // Check rows are all-different
        for r in 0..4 {
            let row: Vec<u32> = (0..4).map(|c| sol[r * 4 + c]).collect();
            let mut sorted = row.clone();
            sorted.sort();
            sorted.dedup();
            assert_eq!(sorted.len(), 4, "row {r} not all-different: {row:?}");
        }
        // Check columns are all-different
        for c in 0..4 {
            let col: Vec<u32> = (0..4).map(|r| sol[r * 4 + c]).collect();
            let mut sorted = col.clone();
            sorted.sort();
            sorted.dedup();
            assert_eq!(sorted.len(), 4, "col {c} not all-different: {col:?}");
        }
    }
}

#[test]
fn solve_3x3_trivial() {
    // 3×3, no fixed cells, no inequalities — should have many solutions (Latin squares).
    let input = "3\n\n\n\n\n";
    let puzzle = FutoshikiPuzzle::parse(input);
    let solutions = solve_futoshiki(&puzzle);
    // 3×3 Latin squares = 12
    assert_eq!(solutions.len(), 12, "3×3 unconstrained has 12 Latin squares");
}
