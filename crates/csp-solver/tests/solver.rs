//! Backtracking, queens, map coloring, stats, cross-config tests.
//! Extracted from lib.rs inline tests.

use csp_solver::constraint::{AllDifferent, LambdaConstraint, NotEqual, VarId};
use csp_solver::domain::Domain;
use csp_solver::domain::bitset::BitsetDomain;
use csp_solver::domain::finite::FiniteDomain;
use csp_solver::ordering::Ordering;
use csp_solver::{Csp, Pruning, SolveConfig};

// -----------------------------------------------------------------------
// 1. Map coloring: Australia 3-color problem
// -----------------------------------------------------------------------
#[test]
fn test_australia_map_coloring() {
    // 7 regions: WA=0, NT=1, SA=2, Q=3, NSW=4, V=5, T=6
    // 3 colors: 0, 1, 2
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let vars = csp.add_variables(&domain, 7);
    let [wa, nt, sa, q, nsw, v, _t] = [
        vars[0], vars[1], vars[2], vars[3], vars[4], vars[5], vars[6],
    ];

    // Adjacent regions must have different colors
    let edges = [
        (wa, nt),
        (wa, sa),
        (nt, sa),
        (nt, q),
        (sa, q),
        (sa, nsw),
        (sa, v),
        (q, nsw),
        (nsw, v),
    ];
    for (a, b) in edges {
        csp.add_constraint(NotEqual::new(a, b));
    }

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "Should find at least one solution");

    let sol = &solutions[0];
    // Verify all constraints
    for (a, b) in &edges {
        assert_ne!(
            sol[*a as usize], sol[*b as usize],
            "Adjacent regions {:?} and {:?} have same color",
            a, b
        );
    }
}

// -----------------------------------------------------------------------
// Test map coloring with all pruning strategies
// -----------------------------------------------------------------------
#[test]
fn test_australia_all_pruning() {
    for pruning in [
        Pruning::None,
        Pruning::ForwardChecking,
        Pruning::Ac3,
        Pruning::AcFc,
    ] {
        let mut csp = Csp::new();
        let domain = BitsetDomain::new(0..3);
        let _vars = csp.add_variables(&domain, 7);

        let edges: [(VarId, VarId); 9] = [
            (0, 1),
            (0, 2),
            (1, 2),
            (1, 3),
            (2, 3),
            (2, 4),
            (2, 5),
            (3, 4),
            (4, 5),
        ];
        for (a, b) in edges {
            csp.add_constraint(NotEqual::new(a, b));
        }
        csp.finalize();

        let config = SolveConfig {
            pruning,
            ordering: Ordering::Chronological,
            max_solutions: 1,
            ..Default::default()
        };

        let solutions = csp.solve(&config);
        assert!(
            !solutions.is_empty(),
            "Pruning {:?} should find a solution",
            pruning
        );
    }
}

// -----------------------------------------------------------------------
// 2. 4-Queens
// -----------------------------------------------------------------------
#[test]
fn test_4_queens() {
    let n = 4u32;
    let mut csp = Csp::new();

    // Variable i = row for queen in column i. Domain = {0, 1, 2, 3}.
    let domain = BitsetDomain::new(0..n);
    let vars = csp.add_variables(&domain, n as usize);

    // All queens in different rows
    csp.add_constraint(AllDifferent::new(vars.clone()));

    // No two queens on the same diagonal
    for i in 0..n {
        for j in (i + 1)..n {
            let vi = vars[i as usize];
            let vj = vars[j as usize];
            let diff = j - i;
            csp.add_constraint(LambdaConstraint::new(
                vec![vi, vj],
                move |assignment: &[Option<u32>]| match (
                    &assignment[vi as usize],
                    &assignment[vj as usize],
                ) {
                    (Some(ri), Some(rj)) => {
                        let row_diff = ri.abs_diff(*rj);
                        row_diff != diff
                    }
                    _ => true,
                },
                format!("diag({i},{j})"),
            ));
        }
    }

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // 4-Queens has exactly 2 solutions
    assert_eq!(
        solutions.len(),
        2,
        "4-Queens should have exactly 2 solutions"
    );

    // Verify solutions
    for sol in &solutions {
        for i in 0..n as usize {
            for j in (i + 1)..n as usize {
                assert_ne!(sol[i], sol[j], "Same row");
                let row_diff = sol[i].abs_diff(sol[j]);
                let col_diff = (j - i) as u32;
                assert_ne!(row_diff, col_diff, "Same diagonal");
            }
        }
    }
}

// -----------------------------------------------------------------------
// 3. Simple 4x4 Sudoku
// -----------------------------------------------------------------------
#[test]
fn test_4x4_sudoku() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=4);

    // 16 variables: cell (r, c) = variable r*4 + c
    let vars: Vec<VarId> = (0..16).map(|_| csp.add_variable(domain.clone())).collect();

    // Row constraints: all different in each row
    for r in 0..4 {
        let row_vars: Vec<VarId> = (0..4).map(|c| vars[r * 4 + c]).collect();
        csp.add_constraint(AllDifferent::new(row_vars));
    }

    // Column constraints: all different in each column
    for c in 0..4 {
        let col_vars: Vec<VarId> = (0..4).map(|r| vars[r * 4 + c]).collect();
        csp.add_constraint(AllDifferent::new(col_vars));
    }

    // Box constraints: 2x2 boxes
    for br in 0..2usize {
        for bc in 0..2usize {
            let mut box_vars = Vec::new();
            for dr in 0..2usize {
                for dc in 0..2usize {
                    box_vars.push(vars[(br * 2 + dr) * 4 + (bc * 2 + dc)]);
                }
            }
            csp.add_constraint(AllDifferent::new(box_vars));
        }
    }

    csp.finalize();

    // Given clues:
    // 1 _ _ _
    // _ _ _ 2
    // _ _ _ _
    // _ 3 _ _
    let given = vec![
        (vars[0], 1u32),  // (0,0) = 1
        (vars[7], 2u32),  // (1,3) = 2
        (vars[13], 3u32), // (3,1) = 3
    ];

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve_with_given(&config, &given);
    assert!(
        !solutions.is_empty(),
        "Should find a solution for 4x4 sudoku"
    );

    let sol = &solutions[0];

    // Verify given cells
    assert_eq!(sol[0], 1);
    assert_eq!(sol[7], 2);
    assert_eq!(sol[13], 3);

    // Verify all rows have distinct values 1-4
    for r in 0..4 {
        let mut row: Vec<u32> = (0..4).map(|c| sol[r * 4 + c]).collect();
        row.sort();
        assert_eq!(row, vec![1, 2, 3, 4], "Row {r} invalid");
    }

    // Verify all columns
    for c in 0..4 {
        let mut col: Vec<u32> = (0..4).map(|r| sol[r * 4 + c]).collect();
        col.sort();
        assert_eq!(col, vec![1, 2, 3, 4], "Column {c} invalid");
    }

    // Verify 2x2 boxes
    for br in 0..2 {
        for bc in 0..2 {
            let mut bx: Vec<u32> = (0..2)
                .flat_map(|dr| (0..2).map(move |dc| sol[(br * 2 + dr) * 4 + (bc * 2 + dc)]))
                .collect();
            bx.sort();
            assert_eq!(bx, vec![1, 2, 3, 4], "Box ({br},{bc}) invalid");
        }
    }
}

// -----------------------------------------------------------------------
// 5. FiniteDomain with string-like values
// -----------------------------------------------------------------------
#[test]
fn test_finite_domain_strings() {
    let mut csp: Csp<FiniteDomain<String>> = Csp::new();

    let colors = FiniteDomain::new(vec![
        "red".to_string(),
        "green".to_string(),
        "blue".to_string(),
    ]);

    let v0 = csp.add_variable(colors.clone());
    let v1 = csp.add_variable(colors.clone());
    let v2 = csp.add_variable(colors.clone());

    csp.add_constraint(NotEqual::new(v0, v1));
    csp.add_constraint(NotEqual::new(v1, v2));
    csp.add_constraint(NotEqual::new(v0, v2));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // 3 colors, 3 variables, all different => 3! = 6 solutions
    assert_eq!(solutions.len(), 6, "Should find exactly 6 colorings");

    for sol in &solutions {
        assert_ne!(sol[0], sol[1]);
        assert_ne!(sol[1], sol[2]);
        assert_ne!(sol[0], sol[2]);
    }
}

// -----------------------------------------------------------------------
// 6. Map coloring (forward checking + fail-first)
// -----------------------------------------------------------------------
#[test]
fn test_map_coloring_fc_failfirst() {
    // Australia-style map coloring solved with forward checking + fail-first.
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 7);

    let edges: [(VarId, VarId); 9] = [
        (0, 1),
        (0, 2),
        (1, 2),
        (1, 3),
        (2, 3),
        (2, 4),
        (2, 5),
        (3, 4),
        (4, 5),
    ];
    for (a, b) in edges {
        csp.add_constraint(NotEqual::new(a, b));
    }
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "map coloring should find a solution");

    // Verify solution
    let sol = &solutions[0];
    for (a, b) in &edges {
        assert_ne!(sol[*a as usize], sol[*b as usize]);
    }
}

// -----------------------------------------------------------------------
// 7. Mrv ordering
// -----------------------------------------------------------------------
#[test]
fn test_mrv_ordering() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 7);

    let edges: [(VarId, VarId); 9] = [
        (0, 1),
        (0, 2),
        (1, 2),
        (1, 3),
        (2, 3),
        (2, 4),
        (2, 5),
        (3, 4),
        (4, 5),
    ];
    for (a, b) in edges {
        csp.add_constraint(NotEqual::new(a, b));
    }
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Mrv,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "Mrv should find a solution");
}

// -----------------------------------------------------------------------
// 8. 8-Queens
// -----------------------------------------------------------------------
#[test]
fn test_8_queens() {
    let n = 8u32;
    let mut csp = Csp::new();

    let domain = BitsetDomain::new(0..n);
    let vars = csp.add_variables(&domain, n as usize);

    csp.add_constraint(AllDifferent::new(vars.clone()));

    for i in 0..n {
        for j in (i + 1)..n {
            let vi = vars[i as usize];
            let vj = vars[j as usize];
            let diff = j - i;
            csp.add_constraint(LambdaConstraint::new(
                vec![vi, vj],
                move |assignment: &[Option<u32>]| match (
                    &assignment[vi as usize],
                    &assignment[vj as usize],
                ) {
                    (Some(ri), Some(rj)) => {
                        let row_diff = ri.abs_diff(*rj);
                        row_diff != diff
                    }
                    _ => true,
                },
                format!("diag({i},{j})"),
            ));
        }
    }

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 92,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(
        solutions.len(),
        92,
        "8-Queens should have exactly 92 solutions"
    );
}

// -----------------------------------------------------------------------
// 9. Unsatisfiable problem
// -----------------------------------------------------------------------
#[test]
fn test_unsatisfiable() {
    let mut csp = Csp::new();

    // 3 variables with domain {0, 1} — all must be different (impossible: pigeonhole)
    let domain = BitsetDomain::new(0..2);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(
        solutions.is_empty(),
        "Should find no solutions (pigeonhole)"
    );
}

// -----------------------------------------------------------------------
// 10. BitsetDomain basic operations
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_operations() {
    let mut d = BitsetDomain::new(0..5);
    assert_eq!(d.size(), 5);
    assert!(d.contains(&0));
    assert!(d.contains(&4));
    assert!(!d.contains(&5));

    assert!(d.remove(&2));
    assert!(!d.contains(&2));
    assert_eq!(d.size(), 4);

    assert!(!d.remove(&2)); // already removed
    assert_eq!(d.size(), 4);

    d.add(&2);
    assert!(d.contains(&2));
    assert_eq!(d.size(), 5);

    let vals = d.values();
    assert_eq!(vals, vec![0, 1, 2, 3, 4]);

    // Singleton
    let mut s = BitsetDomain::new([42]);
    assert!(s.is_singleton());
    assert_eq!(s.singleton_value(), Some(42));

    s.add(&7);
    assert!(!s.is_singleton());
}

// -----------------------------------------------------------------------
// 11. Stats tracking
// -----------------------------------------------------------------------
#[test]
fn test_stats() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 4);

    csp.add_constraint(NotEqual::new(0, 1));
    csp.add_constraint(NotEqual::new(1, 2));
    csp.add_constraint(NotEqual::new(2, 3));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let _solutions = csp.solve(&config);
    let stats = csp.stats();
    assert!(stats.nodes_explored > 0, "Should have explored some nodes");
}

// -----------------------------------------------------------------------
// 12. Multiple solutions
// -----------------------------------------------------------------------
#[test]
fn test_multiple_solutions() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 2);

    csp.add_constraint(NotEqual::new(0, 1));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::None,
        ordering: Ordering::Chronological,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // 3 values, 2 variables, all different => 3 * 2 = 6 solutions
    assert_eq!(solutions.len(), 6);
}

// -----------------------------------------------------------------------
// 13. Simple 2-variable CSP
// -----------------------------------------------------------------------
#[test]
fn test_simple_2var_csp() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=3);
    let a = csp.add_variable(domain.clone());
    let b = csp.add_variable(domain);

    csp.add_constraint(NotEqual::new(a, b));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 1);
    assert_ne!(solutions[0][a as usize], solutions[0][b as usize]);
}

// -----------------------------------------------------------------------
// 14. BitsetDomain clone independence
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_clone_independence() {
    let d1 = BitsetDomain::new([1, 2, 3]);
    let mut d2 = d1.clone();
    d2.remove(&2);
    assert!(d1.contains(&2), "Original should still contain 2");
    assert!(!d2.contains(&2), "Clone should not contain 2");
}

// -----------------------------------------------------------------------
// 15. BitsetDomain union
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_union() {
    let mut d1 = BitsetDomain::new([1, 2]);
    let d2 = BitsetDomain::new([3, 4]);
    d1.union_with(&d2);
    assert_eq!(d1.values(), vec![1, 2, 3, 4]);
}

// -----------------------------------------------------------------------
// 16. Empty BitsetDomain
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_empty() {
    let d = BitsetDomain::empty();
    assert_eq!(d.size(), 0);
    assert!(d.is_empty());
    assert_eq!(d.values(), Vec::<u32>::new());
}

// -----------------------------------------------------------------------
// 19. Forward checking DWO detection
// -----------------------------------------------------------------------
#[test]
fn test_forward_check_dwo() {
    // 2 variables with domain {0}, must be different -> immediate DWO
    let mut csp = Csp::new();
    let domain = BitsetDomain::new([0]);
    let _vars = csp.add_variables(&domain, 2);

    csp.add_constraint(NotEqual::new(0, 1));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(solutions.is_empty(), "DWO: singleton domains with NotEqual");
}

// -----------------------------------------------------------------------
// 20. Unsolvable pigeonhole with backtrack count
// -----------------------------------------------------------------------
#[test]
fn test_unsolvable_pigeonhole_backtracks() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new([0]);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(solutions.is_empty());
    assert!(csp.stats().backtracks > 0, "Should have backtracked");
}

// -----------------------------------------------------------------------
// 21. Multiple solutions with 3! = 6 permutations
// -----------------------------------------------------------------------
#[test]
fn test_all_permutations_3var() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=3);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 6, "3! = 6 permutations");
}

// -----------------------------------------------------------------------
// 22. AC-3 full solve
// -----------------------------------------------------------------------
#[test]
fn test_ac3_full_solve() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=3);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "AC-3 should find a solution");
    // Verify all different
    let sol = &solutions[0];
    assert_ne!(sol[0], sol[1]);
    assert_ne!(sol[1], sol[2]);
    assert_ne!(sol[0], sol[2]);
}

// -----------------------------------------------------------------------
// 23. Initial AC-3 propagation reduces domains
// -----------------------------------------------------------------------
#[test]
fn test_initial_ac3_propagation() {
    // 4x4 sudoku with first row fully given: strong propagation, few backtracks.
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=4);
    let _vars: Vec<VarId> = (0..16).map(|_| csp.add_variable(domain.clone())).collect();

    // Row constraints
    for r in 0..4 {
        let row_vars: Vec<VarId> = (0..4).map(|c| (r * 4 + c) as VarId).collect();
        csp.add_constraint(AllDifferent::new(row_vars));
    }
    // Column constraints
    for c in 0..4 {
        let col_vars: Vec<VarId> = (0..4).map(|r| (r * 4 + c) as VarId).collect();
        csp.add_constraint(AllDifferent::new(col_vars));
    }
    // Box constraints
    for br in 0..2u32 {
        for bc in 0..2u32 {
            let box_vars: Vec<VarId> = (0..2)
                .flat_map(|dr| (0..2).map(move |dc| (br * 2 + dr) * 4 + (bc * 2 + dc)))
                .collect();
            csp.add_constraint(AllDifferent::new(box_vars));
        }
    }

    csp.finalize();

    // First row fully given: 1, 2, 3, 4
    let given = vec![(0u32, 1u32), (1, 2), (2, 3), (3, 4)];

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty());
    // With strong initial propagation, should need very few backtracks
    assert!(
        csp.stats().backtracks <= 20,
        "Expected few backtracks with heavy initial propagation, got {}",
        csp.stats().backtracks
    );
}

// -----------------------------------------------------------------------
// Helper: build N-Queens CSP
// -----------------------------------------------------------------------
fn build_nqueens(n: u32) -> Csp<BitsetDomain> {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..n);
    let vars = csp.add_variables(&domain, n as usize);
    csp.add_constraint(AllDifferent::new(vars.clone()));
    for i in 0..n {
        for j in (i + 1)..n {
            let vi = vars[i as usize];
            let vj = vars[j as usize];
            let diff = j - i;
            csp.add_constraint(LambdaConstraint::new(
                vec![vi, vj],
                move |assignment: &[Option<u32>]| match (
                    &assignment[vi as usize],
                    &assignment[vj as usize],
                ) {
                    (Some(ri), Some(rj)) => ri.abs_diff(*rj) != diff,
                    _ => true,
                },
                format!("diag({i},{j})"),
            ));
        }
    }
    csp.finalize();
    csp
}

// -----------------------------------------------------------------------
// Helper: build 9x9 Sudoku CSP from a flat 81-element grid
// -----------------------------------------------------------------------
fn build_sudoku_9x9(grid: &[u32; 81]) -> (Csp<BitsetDomain>, Vec<(VarId, u32)>) {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=9);
    let _vars: Vec<VarId> = (0..81).map(|_| csp.add_variable(domain.clone())).collect();

    // Row constraints
    for r in 0..9 {
        let row_vars: Vec<VarId> = (0..9).map(|c| (r * 9 + c) as VarId).collect();
        csp.add_constraint(AllDifferent::new(row_vars));
    }
    // Column constraints
    for c in 0..9 {
        let col_vars: Vec<VarId> = (0..9).map(|r| (r * 9 + c) as VarId).collect();
        csp.add_constraint(AllDifferent::new(col_vars));
    }
    // 3x3 box constraints
    for bi in 0..3u32 {
        for bj in 0..3u32 {
            let box_vars: Vec<VarId> = (0..3)
                .flat_map(|di| (0..3).map(move |dj| (bi * 3 + di) * 9 + (bj * 3 + dj)))
                .collect();
            csp.add_constraint(AllDifferent::new(box_vars));
        }
    }

    csp.finalize();

    // Extract given clues
    let given: Vec<(VarId, u32)> = grid
        .iter()
        .enumerate()
        .filter(|&(_, v)| *v != 0)
        .map(|(i, &v)| (i as VarId, v))
        .collect();

    (csp, given)
}

fn validate_sudoku_solution(sol: &[u32]) {
    assert_eq!(sol.len(), 81);
    // All values 1-9
    for &v in sol {
        assert!((1..=9).contains(&v), "Value {v} out of range");
    }
    // Rows
    for r in 0..9 {
        let mut row: Vec<u32> = (0..9).map(|c| sol[r * 9 + c]).collect();
        row.sort();
        assert_eq!(row, vec![1, 2, 3, 4, 5, 6, 7, 8, 9], "Row {r} invalid");
    }
    // Columns
    for c in 0..9 {
        let mut col: Vec<u32> = (0..9).map(|r| sol[r * 9 + c]).collect();
        col.sort();
        assert_eq!(col, vec![1, 2, 3, 4, 5, 6, 7, 8, 9], "Col {c} invalid");
    }
    // 3x3 boxes
    for bi in 0..3 {
        for bj in 0..3 {
            let mut bx: Vec<u32> = (0..3)
                .flat_map(|di| (0..3).map(move |dj| sol[(bi * 3 + di) * 9 + (bj * 3 + dj)]))
                .collect();
            bx.sort();
            assert_eq!(
                bx,
                vec![1, 2, 3, 4, 5, 6, 7, 8, 9],
                "Box ({bi},{bj}) invalid"
            );
        }
    }
}

// -----------------------------------------------------------------------
// 24. Simple 4-variable problem
// -----------------------------------------------------------------------
#[test]
fn test_simple_4var() {
    // 4 variables, domain {0, 1}, two all-different constraints on disjoint pairs
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..2);
    let _vars = csp.add_variables(&domain, 4);

    csp.add_constraint(AllDifferent::new(vec![0, 1]));
    csp.add_constraint(AllDifferent::new(vec![2, 3]));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // 2! * 2! = 4 solutions (each pair independently)
    assert_eq!(solutions.len(), 4);

    for sol in &solutions {
        assert_ne!(sol[0], sol[1]);
        assert_ne!(sol[2], sol[3]);
    }
}

// -----------------------------------------------------------------------
// 25. Cross-config correctness: all pruning x ordering combos on map coloring
// -----------------------------------------------------------------------
#[test]
fn test_cross_config_correctness_map_coloring() {
    let prunings = [
        Pruning::None,
        Pruning::ForwardChecking,
        Pruning::Ac3,
        Pruning::AcFc,
    ];
    let orderings = [Ordering::Chronological, Ordering::FailFirst, Ordering::Mrv];

    for pruning in &prunings {
        for ordering in &orderings {
            let mut csp = Csp::new();
            let domain = BitsetDomain::new(0..3);
            let _vars = csp.add_variables(&domain, 7);

            let edges: [(VarId, VarId); 9] = [
                (0, 1),
                (0, 2),
                (1, 2),
                (1, 3),
                (2, 3),
                (2, 4),
                (2, 5),
                (3, 4),
                (4, 5),
            ];
            for (a, b) in edges {
                csp.add_constraint(NotEqual::new(a, b));
            }
            csp.finalize();

            let config = SolveConfig {
                pruning: *pruning,
                ordering: *ordering,
                max_solutions: 1,
                ..Default::default()
            };

            let solutions = csp.solve(&config);
            assert!(
                !solutions.is_empty(),
                "Map coloring failed with pruning={:?}, ordering={:?}",
                pruning,
                ordering
            );

            let sol = &solutions[0];
            for (a, b) in &edges {
                assert_ne!(
                    sol[*a as usize], sol[*b as usize],
                    "Adjacent regions same color: pruning={:?}, ordering={:?}",
                    pruning, ordering
                );
            }
        }
    }
}

// -----------------------------------------------------------------------
// 26. Cross-config correctness: 8-Queens with all pruning strategies
// -----------------------------------------------------------------------
#[test]
fn test_cross_config_8queens() {
    // Note: Pruning::None with 8-Queens causes deep recursion (8^8 search space).
    // Only test strategies that prune the search space.
    for pruning in [Pruning::ForwardChecking, Pruning::Ac3, Pruning::AcFc] {
        let mut csp = build_nqueens(8);

        let config = SolveConfig {
            pruning,
            ordering: Ordering::FailFirst,
            max_solutions: 1,
            ..Default::default()
        };

        let solutions = csp.solve(&config);
        assert!(
            !solutions.is_empty(),
            "8-Queens failed with pruning={:?}",
            pruning
        );
    }
}

// -----------------------------------------------------------------------
// 27. Max solutions limit respected
// -----------------------------------------------------------------------
#[test]
fn test_max_solutions_limit() {
    let mut csp = build_nqueens(8);

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 5,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 5, "Should respect max_solutions=5");
}

// -----------------------------------------------------------------------
// 28. AC-FC hybrid finds solutions
// -----------------------------------------------------------------------
#[test]
fn test_ac_fc_hybrid_solve() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(1..=3);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::AcFc,
        ordering: Ordering::FailFirst,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(
        solutions.len(),
        6,
        "AC-FC should find all 3! = 6 permutations"
    );
}

// -----------------------------------------------------------------------
// 29. AC-FC hybrid on map coloring
// -----------------------------------------------------------------------
#[test]
fn test_ac_fc_map_coloring() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 7);

    let edges: [(VarId, VarId); 9] = [
        (0, 1),
        (0, 2),
        (1, 2),
        (1, 3),
        (2, 3),
        (2, 4),
        (2, 5),
        (3, 4),
        (4, 5),
    ];
    for (a, b) in edges {
        csp.add_constraint(NotEqual::new(a, b));
    }
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::AcFc,
        ordering: Ordering::Mrv,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(!solutions.is_empty(), "AC-FC should find a map coloring");
}

// -----------------------------------------------------------------------
// 30. Domain wipe-out with various pruning strategies
// -----------------------------------------------------------------------
#[test]
fn test_domain_wipeout_all_pruning() {
    for pruning in [
        Pruning::None,
        Pruning::ForwardChecking,
        Pruning::Ac3,
        Pruning::AcFc,
    ] {
        let mut csp = Csp::new();
        let domain = BitsetDomain::new(0..2);
        let vars = csp.add_variables(&domain, 3);
        csp.add_constraint(AllDifferent::new(vars));
        csp.finalize();

        let config = SolveConfig {
            pruning,
            ordering: Ordering::Chronological,
            max_solutions: 1,
            ..Default::default()
        };

        let solutions = csp.solve(&config);
        assert!(
            solutions.is_empty(),
            "Pigeonhole should be unsolvable with pruning={:?}",
            pruning
        );
    }
}

// -----------------------------------------------------------------------
// 31. Constraint validation edge case: single variable
// -----------------------------------------------------------------------
#[test]
fn test_single_variable() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..5);
    let var = csp.add_variable(domain);

    // Unary constraint: value must be 3
    csp.add_constraint(LambdaConstraint::new(
        vec![var],
        move |assignment: &[Option<u32>]| match &assignment[var as usize] {
            Some(v) => *v == 3,
            None => true,
        },
        "var == 3",
    ));

    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 10,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(solutions.len(), 1);
    assert_eq!(solutions[0][0], 3);
}

// -----------------------------------------------------------------------
// 32. Constraint validation edge case: no constraints
// -----------------------------------------------------------------------
#[test]
fn test_no_constraints() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 2);
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    // 3 * 3 = 9 unconstrained solutions
    assert_eq!(solutions.len(), 9);
}

// -----------------------------------------------------------------------
// 33. Unsatisfiable AllDifferent (3 vars over domain 0..2)
// -----------------------------------------------------------------------
#[test]
fn test_unsatisfiable_alldifferent() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..2);
    let vars = csp.add_variables(&domain, 3);
    csp.add_constraint(AllDifferent::new(vars));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert!(
        solutions.is_empty(),
        "unsatisfiable AllDifferent should return empty"
    );
}

// -----------------------------------------------------------------------
// 34. Forward-checking backtrack counts are deterministic (regression test)
// -----------------------------------------------------------------------
#[test]
fn test_forward_checking_backtracks_deterministic() {
    let build_map = || {
        let mut csp = Csp::new();
        let domain = BitsetDomain::new(0..3);
        let _vars = csp.add_variables(&domain, 7);
        let edges: [(VarId, VarId); 9] = [
            (0, 1),
            (0, 2),
            (1, 2),
            (1, 3),
            (2, 3),
            (2, 4),
            (2, 5),
            (3, 4),
            (4, 5),
        ];
        for (a, b) in edges {
            csp.add_constraint(NotEqual::new(a, b));
        }
        csp.finalize();
        csp
    };

    let mut csp_chrono = build_map();
    let config_chrono = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let _sol = csp_chrono.solve(&config_chrono);
    let bt_chrono = csp_chrono.stats().backtracks;

    let mut csp_b = build_map();
    let config_b = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let _sol = csp_b.solve(&config_b);
    let bt_b = csp_b.stats().backtracks;

    // Two runs of the same config must report identical backtrack counts.
    assert_eq!(
        bt_b, bt_chrono,
        "run B ({bt_b}) must match run A ({bt_chrono}) — search is deterministic"
    );
}

// -----------------------------------------------------------------------
// 35. FiniteDomain operations
// -----------------------------------------------------------------------
#[test]
fn test_finite_domain_operations() {
    let mut d = FiniteDomain::new(vec![10, 20, 30]);
    assert_eq!(d.size(), 3);
    assert!(d.contains(&20));
    assert!(!d.contains(&40));

    assert!(d.remove(&20));
    assert!(!d.contains(&20));
    assert_eq!(d.size(), 2);

    assert!(!d.remove(&20)); // already removed
    assert_eq!(d.size(), 2);

    d.add(&20);
    assert!(d.contains(&20));
    assert_eq!(d.size(), 3);

    d.add(&20); // duplicate add should not increase size
    assert_eq!(d.size(), 3);
}

// -----------------------------------------------------------------------
// 36. BitsetDomain intersection and difference
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_intersection_difference() {
    let mut d1 = BitsetDomain::new([1, 2, 3, 4]);
    let d2 = BitsetDomain::new([2, 4, 6]);

    // Intersection
    let mut d1_isect = d1.clone();
    d1_isect.intersect_with(&d2);
    assert_eq!(d1_isect.values(), vec![2, 4]);

    // Difference
    d1.difference_with(&d2);
    assert_eq!(d1.values(), vec![1, 3]);
}

// -----------------------------------------------------------------------
// 37. Stats: propagations tracked
// -----------------------------------------------------------------------
#[test]
fn test_stats_propagations() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..4);
    let _vars = csp.add_variables(&domain, 4);

    csp.add_constraint(NotEqual::new(0, 1));
    csp.add_constraint(NotEqual::new(1, 2));
    csp.add_constraint(NotEqual::new(2, 3));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };

    let _solutions = csp.solve(&config);
    let stats = csp.stats();
    assert!(stats.propagations > 0, "FC should track propagations");
    assert!(stats.nodes_explored > 0);
}

// -----------------------------------------------------------------------
// 38. Solve resets state between calls
// -----------------------------------------------------------------------
#[test]
fn test_solve_resets_state() {
    let mut csp = Csp::new();
    let domain = BitsetDomain::new(0..3);
    let _vars = csp.add_variables(&domain, 2);
    csp.add_constraint(NotEqual::new(0, 1));
    csp.finalize();

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 100,
        ..Default::default()
    };

    // Solve twice - should get same results
    let sol1 = csp.solve(&config);
    let sol2 = csp.solve(&config);
    assert_eq!(
        sol1.len(),
        sol2.len(),
        "Repeated solve should give same count"
    );
}

// -----------------------------------------------------------------------
// Hard Sudoku puzzle constants
// -----------------------------------------------------------------------

#[allow(clippy::zero_prefixed_literal)]
const AL_ESCARGOT: [u32; 81] = [
    1, 0, 0, 0, 0, 7, 0, 9, 0, 0, 3, 0, 0, 2, 0, 0, 0, 8, 0, 0, 9, 6, 0, 0, 5, 0, 0, 0, 0, 5, 3, 0,
    0, 9, 0, 0, 0, 1, 0, 0, 8, 0, 0, 0, 2, 6, 0, 0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1, 0, 0,
    4, 0, 0, 0, 0, 0, 0, 7, 0, 0, 7, 0, 0, 0, 3, 0, 0,
];

#[allow(clippy::zero_prefixed_literal)]
const INKALA_2010: [u32; 81] = [
    0, 0, 5, 3, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 2, 0, 0, 7, 0, 0, 1, 0, 5, 0, 0, 4, 0, 0, 0, 0,
    5, 3, 0, 0, 0, 1, 0, 0, 7, 0, 0, 0, 6, 0, 0, 3, 2, 0, 0, 0, 8, 0, 0, 6, 0, 5, 0, 0, 0, 0, 9, 0,
    0, 4, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 9, 7, 0, 0,
];

#[allow(clippy::zero_prefixed_literal)]
const GOLDEN_NUGGET: [u32; 81] = [
    0, 0, 0, 0, 0, 0, 0, 3, 9, 0, 0, 0, 0, 0, 1, 0, 0, 5, 0, 0, 3, 0, 5, 0, 8, 0, 0, 0, 0, 8, 0, 9,
    0, 0, 0, 6, 0, 7, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 9, 0, 8, 0, 0, 5, 0, 0,
    2, 0, 0, 0, 0, 6, 0, 0, 4, 0, 0, 7, 0, 0, 0, 0, 0,
];

#[allow(clippy::zero_prefixed_literal)]
const PLATINUM_BLONDE: [u32; 81] = [
    0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 3, 5, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 7, 0, 7, 0, 0, 0, 0,
    0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 8, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0,
    0, 0, 1, 8, 0, 0, 0, 0, 2, 5, 0, 0, 0, 0, 0, 0, 0,
];

#[allow(clippy::zero_prefixed_literal)]
const MINIMAL_17: [u32; 81] = [
    0, 0, 0, 0, 0, 0, 0, 1, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5,
    0, 4, 0, 7, 0, 0, 8, 0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 9, 0, 0, 0, 0, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0,
    5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 6, 0, 0, 0,
];

// -----------------------------------------------------------------------
// 39. 9x9 Sudoku: medium puzzle
// -----------------------------------------------------------------------
#[test]
fn test_9x9_sudoku_medium() {
    #[allow(clippy::zero_prefixed_literal)]
    let grid: [u32; 81] = [
        5, 3, 0, 0, 7, 0, 0, 0, 0, 6, 0, 0, 1, 9, 5, 0, 0, 0, 0, 9, 8, 0, 0, 0, 0, 6, 0, 8, 0, 0,
        0, 6, 0, 0, 0, 3, 4, 0, 0, 8, 0, 3, 0, 0, 1, 7, 0, 0, 0, 2, 0, 0, 0, 6, 0, 6, 0, 0, 0, 0,
        2, 8, 0, 0, 0, 0, 4, 1, 9, 0, 0, 5, 0, 0, 0, 0, 8, 0, 0, 7, 9,
    ];

    let (mut csp, given) = build_sudoku_9x9(&grid);

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solutions = csp.solve_with_given(&config, &given);
    assert_eq!(solutions.len(), 1, "Should find exactly one solution");
    validate_sudoku_solution(&solutions[0]);
}

// -----------------------------------------------------------------------
// 40-44. Hard 9x9 Sudoku stress tests
// -----------------------------------------------------------------------
#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_al_escargot() {
    let (mut csp, given) = build_sudoku_9x9(&AL_ESCARGOT);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty(), "Should solve Al Escargot");
    validate_sudoku_solution(&solutions[0]);
}

#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_inkala_2010() {
    let (mut csp, given) = build_sudoku_9x9(&INKALA_2010);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty(), "Should solve Inkala 2010");
    validate_sudoku_solution(&solutions[0]);
}

#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_golden_nugget() {
    let (mut csp, given) = build_sudoku_9x9(&GOLDEN_NUGGET);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty(), "Should solve Golden Nugget");
    validate_sudoku_solution(&solutions[0]);
}

#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_platinum_blonde() {
    let (mut csp, given) = build_sudoku_9x9(&PLATINUM_BLONDE);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty(), "Should solve Platinum Blonde");
    validate_sudoku_solution(&solutions[0]);
}

#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_minimal_17() {
    let (mut csp, given) = build_sudoku_9x9(&MINIMAL_17);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let solutions = csp.solve_with_given(&config, &given);
    assert!(!solutions.is_empty(), "Should solve 17-clue minimal");
    validate_sudoku_solution(&solutions[0]);
}

// -----------------------------------------------------------------------
// 45. Hard sudoku across multiple configs
// -----------------------------------------------------------------------
#[test]
#[ignore = "requires GAC alldiff — too slow with binary FC"]
fn test_hard_sudoku_all_configs() {
    let configs = [
        (Pruning::ForwardChecking, Ordering::FailFirst),
        (Pruning::ForwardChecking, Ordering::Mrv),
        (Pruning::Ac3, Ordering::FailFirst),
    ];

    for (pruning, ordering) in &configs {
        let (mut csp, given) = build_sudoku_9x9(&AL_ESCARGOT);

        let config = SolveConfig {
            pruning: *pruning,
            ordering: *ordering,
            max_solutions: 1,
            ..Default::default()
        };

        let solutions = csp.solve_with_given(&config, &given);
        assert!(
            !solutions.is_empty(),
            "Al Escargot failed with pruning={:?}, ordering={:?}",
            pruning,
            ordering
        );
        validate_sudoku_solution(&solutions[0]);
    }
}

// -----------------------------------------------------------------------
// 46. Regression test: FailFirst <= Chronological backtracks on Sudoku
// -----------------------------------------------------------------------
#[test]
fn test_fail_first_fewer_backtracks_sudoku() {
    #[allow(clippy::zero_prefixed_literal)]
    let grid: [u32; 81] = [
        5, 3, 0, 0, 7, 0, 0, 0, 0, 6, 0, 0, 1, 9, 5, 0, 0, 0, 0, 9, 8, 0, 0, 0, 0, 6, 0, 8, 0, 0,
        0, 6, 0, 0, 0, 3, 4, 0, 0, 8, 0, 3, 0, 0, 1, 7, 0, 0, 0, 2, 0, 0, 0, 6, 0, 6, 0, 0, 0, 0,
        2, 8, 0, 0, 0, 0, 4, 1, 9, 0, 0, 5, 0, 0, 0, 0, 8, 0, 0, 7, 9,
    ];

    let (mut csp_chrono, given_chrono) = build_sudoku_9x9(&grid);
    let config_chrono = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::Chronological,
        max_solutions: 1,
        ..Default::default()
    };
    let _sol = csp_chrono.solve_with_given(&config_chrono, &given_chrono);
    let bt_chrono = csp_chrono.stats().backtracks;

    let (mut csp_ff, given_ff) = build_sudoku_9x9(&grid);
    let config_ff = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    let _sol = csp_ff.solve_with_given(&config_ff, &given_ff);
    let bt_ff = csp_ff.stats().backtracks;

    assert!(
        bt_ff <= bt_chrono,
        "FailFirst ({bt_ff}) should have <= backtracks than Chronological ({bt_chrono})"
    );
}

// -----------------------------------------------------------------------
// 60. BitsetDomain iterator
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_iterator() {
    let d = BitsetDomain::new([5, 2, 8, 1]);
    let vals: Vec<u32> = d.iter().collect();
    // Iterator yields in ascending order (lowest bit first)
    assert_eq!(vals, vec![1, 2, 5, 8]);
    assert_eq!(d.iter().len(), 4);
}

// -----------------------------------------------------------------------
// 61. BitsetDomain range constructor
// -----------------------------------------------------------------------
#[test]
fn test_bitset_domain_range() {
    let d = BitsetDomain::range(5);
    assert_eq!(d.size(), 5);
    assert_eq!(d.values(), vec![0, 1, 2, 3, 4]);

    let d0 = BitsetDomain::range(0);
    assert_eq!(d0.size(), 0);
    assert!(d0.is_empty());
}

// -----------------------------------------------------------------------
// 62. 5-Queens (verifying solution count)
// -----------------------------------------------------------------------
#[test]
fn test_5_queens() {
    let mut csp = build_nqueens(5);

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(
        solutions.len(),
        10,
        "5-Queens should have exactly 10 solutions"
    );
}

// -----------------------------------------------------------------------
// 63. 6-Queens (verifying solution count)
// -----------------------------------------------------------------------
#[test]
fn test_6_queens() {
    let mut csp = build_nqueens(6);

    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 100,
        ..Default::default()
    };

    let solutions = csp.solve(&config);
    assert_eq!(
        solutions.len(),
        4,
        "6-Queens should have exactly 4 solutions"
    );
}
