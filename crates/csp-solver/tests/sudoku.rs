//! Sudoku puzzle tests (4x4, 9x9, generation, transforms).
//! Recreated using the public sudoku API.

use csp_solver::ordering::Ordering;
use csp_solver::sudoku::{
    create_sudoku_csp, generate_board, measure_difficulty, solve_sudoku,
    apply_random_transform, Difficulty, SudokuTransform,
};
use csp_solver::{Pruning, SolveConfig};

/// Validate a complete 9x9 Sudoku solution.
fn is_valid_solution_9x9(sol: &[u32]) -> bool {
    if sol.len() != 81 {
        return false;
    }
    // All values 1-9
    for &v in sol {
        if !(1..=9).contains(&v) {
            return false;
        }
    }
    // Rows
    for r in 0..9 {
        let mut row: Vec<u32> = (0..9).map(|c| sol[r * 9 + c]).collect();
        row.sort();
        if row != vec![1, 2, 3, 4, 5, 6, 7, 8, 9] {
            return false;
        }
    }
    // Columns
    for c in 0..9 {
        let mut col: Vec<u32> = (0..9).map(|r| sol[r * 9 + c]).collect();
        col.sort();
        if col != vec![1, 2, 3, 4, 5, 6, 7, 8, 9] {
            return false;
        }
    }
    // 3x3 boxes
    for bi in 0..3usize {
        for bj in 0..3usize {
            let mut bx: Vec<u32> = (0..3)
                .flat_map(|di| (0..3).map(move |dj| sol[(bi * 3 + di) * 9 + (bj * 3 + dj)]))
                .collect();
            bx.sort();
            if bx != vec![1, 2, 3, 4, 5, 6, 7, 8, 9] {
                return false;
            }
        }
    }
    true
}

/// Validate a complete 4x4 Sudoku solution.
fn is_valid_solution_4x4(sol: &[u32]) -> bool {
    if sol.len() != 16 {
        return false;
    }
    for &v in sol {
        if !(1..=4).contains(&v) {
            return false;
        }
    }
    // Rows
    for r in 0..4 {
        let mut row: Vec<u32> = (0..4).map(|c| sol[r * 4 + c]).collect();
        row.sort();
        if row != vec![1, 2, 3, 4] {
            return false;
        }
    }
    // Columns
    for c in 0..4 {
        let mut col: Vec<u32> = (0..4).map(|r| sol[r * 4 + c]).collect();
        col.sort();
        if col != vec![1, 2, 3, 4] {
            return false;
        }
    }
    // 2x2 boxes
    for bi in 0..2usize {
        for bj in 0..2usize {
            let mut bx: Vec<u32> = (0..2)
                .flat_map(|di| (0..2).map(move |dj| sol[(bi * 2 + di) * 4 + (bj * 2 + dj)]))
                .collect();
            bx.sort();
            if bx != vec![1, 2, 3, 4] {
                return false;
            }
        }
    }
    true
}

// -----------------------------------------------------------------------
// test_solve_4x4: solve a known 4x4 puzzle
// -----------------------------------------------------------------------
#[test]
fn test_solve_4x4() {
    // Known 4x4 puzzle (0 = empty):
    // 1 _ _ _
    // _ _ _ 2
    // _ _ _ _
    // _ 3 _ _
    let board: [u32; 16] = [
        1, 0, 0, 0,
        0, 0, 0, 2,
        0, 0, 0, 0,
        0, 3, 0, 0,
    ];

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let sol = solve_sudoku(&board, 2, &config);
    assert!(sol.is_some(), "Should find a solution for 4x4 puzzle");
    let sol = sol.unwrap();
    assert!(is_valid_solution_4x4(&sol), "Solution must be valid");
    // Verify given cells are preserved
    assert_eq!(sol[0], 1);
    assert_eq!(sol[7], 2);
    assert_eq!(sol[13], 3);
}

// -----------------------------------------------------------------------
// test_solve_9x9: solve a known 9x9 puzzle
// -----------------------------------------------------------------------
#[test]
fn test_solve_9x9() {
    #[allow(clippy::zero_prefixed_literal)]
    let board: [u32; 81] = [
        5,3,0,0,7,0,0,0,0,
        6,0,0,1,9,5,0,0,0,
        0,9,8,0,0,0,0,6,0,
        8,0,0,0,6,0,0,0,3,
        4,0,0,8,0,3,0,0,1,
        7,0,0,0,2,0,0,0,6,
        0,6,0,0,0,0,2,8,0,
        0,0,0,4,1,9,0,0,5,
        0,0,0,0,8,0,0,7,9,
    ];

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let sol = solve_sudoku(&board, 3, &config);
    assert!(sol.is_some(), "Should find a solution for 9x9 puzzle");
    let sol = sol.unwrap();
    assert!(is_valid_solution_9x9(&sol), "Solution must be valid");
    // Verify some given cells
    assert_eq!(sol[0], 5);
    assert_eq!(sol[1], 3);
    assert_eq!(sol[4], 7);
}

// -----------------------------------------------------------------------
// test_create_csp_returns_correct_given: verify given extraction
// -----------------------------------------------------------------------
#[test]
fn test_create_csp_returns_correct_given() {
    let board: [u32; 16] = [
        1, 0, 0, 0,
        0, 0, 0, 2,
        0, 0, 0, 0,
        0, 3, 0, 0,
    ];

    let (_csp, given) = create_sudoku_csp(&board, 2);

    // Should extract exactly 3 given values
    assert_eq!(given.len(), 3);

    // Verify the given values
    let given_map: std::collections::HashMap<u32, u32> =
        given.iter().map(|&(v, val)| (v, val)).collect();
    assert_eq!(given_map[&0], 1);   // cell (0,0) = 1
    assert_eq!(given_map[&7], 2);   // cell (1,3) = 2
    assert_eq!(given_map[&13], 3);  // cell (3,1) = 3
}

// -----------------------------------------------------------------------
// test_generate_4x4: generate an easy 4x4 and verify solvable
// -----------------------------------------------------------------------
#[test]
fn test_generate_4x4() {
    let board = generate_board(2, Difficulty::Easy);
    assert_eq!(board.len(), 16);

    // Verify it has some given cells and some empty cells
    let given_count = board.iter().filter(|&&v| v != 0).count();
    assert!(given_count > 0, "Generated board should have some given cells");
    assert!(given_count < 16, "Generated board should have some empty cells");

    // Verify it is solvable
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        backjumping: false,
        ..Default::default()
    };

    let sol = solve_sudoku(&board, 2, &config);
    assert!(sol.is_some(), "Generated 4x4 should be solvable");
    assert!(is_valid_solution_4x4(&sol.unwrap()), "Solution must be valid");
}

// -----------------------------------------------------------------------
// test_transform_preserves_validity: random transform of valid board stays valid
// -----------------------------------------------------------------------
#[test]
fn test_transform_preserves_validity() {
    // Start with a complete valid 4x4 solution
    let board: [u32; 16] = [
        1, 2, 3, 4,
        3, 4, 1, 2,
        2, 1, 4, 3,
        4, 3, 2, 1,
    ];
    assert!(is_valid_solution_4x4(&board), "Seed board must be valid");

    // Apply a random transform
    let transformed = apply_random_transform(&board, 2);
    assert_eq!(transformed.len(), 16);

    // The transformed board should still be a valid solution
    assert!(is_valid_solution_4x4(&transformed),
        "Transformed board should still be a valid solution: {:?}", transformed);
}

// -----------------------------------------------------------------------
// test_identity_transform: identity transform is no-op
// -----------------------------------------------------------------------
#[test]
fn test_identity_transform() {
    let board: [u32; 16] = [
        1, 2, 3, 4,
        3, 4, 1, 2,
        2, 1, 4, 3,
        4, 3, 2, 1,
    ];

    // Construct an identity transform
    let identity = SudokuTransform {
        digit_perm: vec![0, 1, 2, 3, 4], // identity permutation (index 0 unused)
        row_perms: vec![vec![0, 1], vec![0, 1]],
        col_perms: vec![vec![0, 1], vec![0, 1]],
        band_perm: vec![0, 1],
        stack_perm: vec![0, 1],
        do_transpose: false,
    };

    let result = identity.apply(&board, 2);
    assert_eq!(result, board.to_vec(), "Identity transform should be no-op");
}

// -----------------------------------------------------------------------
// test_measure_difficulty: easy puzzle has few backtracks
// -----------------------------------------------------------------------
#[test]
fn test_measure_difficulty() {
    // Use the known easy 4x4 puzzle
    let board: [u32; 16] = [
        1, 0, 0, 0,
        0, 0, 0, 2,
        0, 0, 0, 0,
        0, 3, 0, 0,
    ];

    let backtracks = measure_difficulty(&board, 2);
    // A 4x4 with 3 givens should be relatively easy (few backtracks)
    // Just verify it returns a reasonable number
    assert!(backtracks < 1000, "4x4 puzzle should not need many backtracks, got {}", backtracks);
}
