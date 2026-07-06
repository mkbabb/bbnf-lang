//! Futoshiki convenience API — the PyO3 mirror of `sudoku_api.rs`, adapted to
//! the Latin-square-plus-carets surface.
//!
//! Three entry points, isomorphic to the Sudoku block:
//! - [`create_futoshiki_csp`] — validate wire parts into a [`FutoshikiCSP`],
//!   rejecting non-adjacent / out-of-range inequalities up front through the
//!   existing [`CspError::InvalidInput`] → `InvalidInputError` taxonomy (G3, no
//!   new variant).
//! - [`solve_futoshiki`] — solve in place, GIL released for the whole search.
//! - [`create_random_futoshiki`] — seeded generation at the single shipped
//!   high-density tier (no `Difficulty` parameter — v1 scope).
//!
//! **`board_size`, never bare `size`** (F5): Sudoku's `size`/`N` is the *subgrid*
//! side (`board = size²`); Futoshiki's `board_size` is the board side directly.
//! Sharing the name across the two surfaces is a live footgun, so this module
//! never spells it `size`.

use std::collections::HashMap;

use pyo3::prelude::*;

use super::config::CancelToken;
use crate::error::CspError;
use crate::ordering::Ordering as RustOrdering;
use crate::puzzles::futoshiki::{
    self, FutoshikiPuzzle, create_futoshiki_csp as build_futoshiki_csp,
};
use crate::{Pruning as RustPruning, SolveConfig as RustSolveConfig};

/// A validated Futoshiki puzzle plus the results of the last solve. The
/// analog of `SudokuCSP`; the `puzzle` field holds the already-validated
/// [`FutoshikiPuzzle`] (adjacency + range checked at construction via
/// [`FutoshikiPuzzle::from_parts`]) so [`solve_futoshiki`] can rebuild the
/// Rust CSP inside its GIL-released block without re-validating.
#[pyclass]
#[derive(Clone)]
pub struct FutoshikiCSP {
    puzzle: FutoshikiPuzzle,
    max_solutions: usize,
    /// Board side length (`4..=7` in v1). Never a bare `size` (F5).
    #[pyo3(get)]
    board_size: u32,
    #[pyo3(get)]
    solutions: Vec<HashMap<String, i32>>,
    #[pyo3(get)]
    backtrack_count: u64,
    /// `True` when the last solve hit its node budget and returned best-so-far
    /// rather than a verified result — mirrors `SudokuCSP.budget_exceeded`.
    #[pyo3(get)]
    budget_exceeded: bool,
    /// `True` when the last solve was stopped early via a `CancelToken`.
    #[pyo3(get)]
    cancelled: bool,
}

#[pymethods]
impl FutoshikiCSP {
    #[getter]
    fn backtracks(&self) -> u64 {
        self.backtrack_count
    }

    /// The inequality set as `(a, b)` pairs meaning `cell_a > cell_b`, every
    /// pair orthogonally adjacent (guaranteed by construction).
    #[getter]
    fn inequalities(&self) -> Vec<(usize, usize)> {
        self.puzzle.inequalities.clone()
    }
}

/// Validate wire parts into a [`FutoshikiCSP`] — the network-facing constructor
/// (G3 PyO3 boundary).
///
/// `values` is the same `{position: value}` map shape `SolveRequest.values`
/// uses for Sudoku; blanks (`value <= 0`) are dropped. `inequalities` are
/// `(a, b)` pairs meaning `cell_a > cell_b`. Every fixed-cell index/value and
/// every inequality pair is range- and adjacency-checked by
/// [`FutoshikiPuzzle::from_parts`]; a non-adjacent pair (opposite corners, say)
/// or an out-of-range index raises `InvalidInputError` here, never a silent
/// accept.
#[pyfunction]
#[pyo3(signature = (board_size, values, inequalities, max_solutions=1))]
pub fn create_futoshiki_csp(
    board_size: u32,
    values: HashMap<String, i32>,
    inequalities: Vec<(usize, usize)>,
    max_solutions: usize,
) -> PyResult<FutoshikiCSP> {
    let mut fixed_cells: Vec<(usize, u32)> = Vec::new();
    for (pos_str, val) in &values {
        let pos: usize = pos_str
            .parse()
            .map_err(|_| CspError::invalid_input(format!("invalid position: {pos_str:?}")))?;
        if *val > 0 {
            fixed_cells.push((pos, *val as u32));
        }
    }

    // Adjacency + range validation (G3): `from_parts` is the single Rust
    // boundary that rejects unrenderable inequality pairs with the existing
    // `CspError::InvalidInput` — no new variant.
    let puzzle = FutoshikiPuzzle::from_parts(board_size, fixed_cells, inequalities)?;

    Ok(FutoshikiCSP {
        puzzle,
        max_solutions,
        board_size,
        solutions: Vec::new(),
        backtrack_count: 0,
        budget_exceeded: false,
        cancelled: false,
    })
}

/// Solve a previously-constructed [`FutoshikiCSP`] in place.
///
/// Mirrors `solve_sudoku`: the shipped `Ac3` + `Mrv` production config (the F1
/// override — the library default cannot even solve an empty N≥6 board), GIL
/// released for the whole construction + search so the FastAPI event loop keeps
/// running on its `asyncio.to_thread` worker, and the same budget-exceeded
/// contract — raises `BudgetExceededError` when the budget was hit with zero
/// solutions found, returns `Ok(false)` only for the provably-unsatisfiable
/// case, and exposes `budget_exceeded` for callers that prefer to branch on
/// state rather than catch.
#[pyfunction]
#[pyo3(signature = (csp, cancel=None))]
pub fn solve_futoshiki(
    py: Python<'_>,
    csp: &mut FutoshikiCSP,
    cancel: Option<CancelToken>,
) -> PyResult<bool> {
    let config = RustSolveConfig {
        pruning: RustPruning::Ac3,
        ordering: RustOrdering::Mrv,
        max_solutions: csp.max_solutions,
        cancel: cancel.as_ref().map(|t| t.inner.clone()),
        ..Default::default()
    };

    let puzzle = csp.puzzle.clone();
    let (solutions, stats) = py.allow_threads(|| {
        let mut rust_csp = build_futoshiki_csp(&puzzle);
        // `solve_with_given` (feasibility-only) with an empty given, mirroring
        // the sudoku surface and the wasm wire — the fixed cells are baked as
        // `add_equals`, so this is behaviorally identical to `solve` while
        // keeping the two product surfaces on one shared code path.
        let solutions = rust_csp.solve_with_given(&config, &[]);
        (solutions, rust_csp.stats().clone())
    });

    csp.backtrack_count = stats.backtracks;
    csp.budget_exceeded = stats.budget_exceeded;
    csp.cancelled = stats.cancelled;

    csp.solutions = solutions
        .into_iter()
        .map(|sol| {
            sol.into_iter()
                .enumerate()
                .map(|(i, v)| (i.to_string(), v as i32))
                .collect()
        })
        .collect();

    if csp.solutions.is_empty() && csp.budget_exceeded {
        return Err(CspError::BudgetExceeded.into());
    }

    Ok(!csp.solutions.is_empty())
}

/// A generated Futoshiki board: dense `{position: value}` givens (`0` blanks
/// dropped, matching the API `values` shape) plus its inequality furniture.
#[pyclass]
#[derive(Clone)]
pub struct FutoshikiBoard {
    #[pyo3(get)]
    values: HashMap<String, i32>,
    #[pyo3(get)]
    inequalities: Vec<(usize, usize)>,
    #[pyo3(get)]
    board_size: u32,
}

/// Generate a random Futoshiki puzzle at the single shipped high-density tier.
///
/// v1 ships **no difficulty parameter** (F2/F3 — fabricated tiers with no
/// measured separation are worse than none), so the surface is just
/// `(board_size, seed)`. `seed` makes generation reproducible and is what the
/// wasm parity harness threads for a bit-exact native↔wasm check; omit it to
/// seed from the wall clock. `board_size` outside the v1 `4..=7` band is
/// rejected with `InvalidInputError` (G3 — out-of-range sizes). The GIL is
/// released for the whole generation, including the uniqueness-checked
/// hole-dig — the more direct DoS surface.
#[pyfunction]
#[pyo3(signature = (board_size, seed=None))]
pub fn create_random_futoshiki(
    py: Python<'_>,
    board_size: u32,
    seed: Option<u64>,
) -> PyResult<FutoshikiBoard> {
    if !(4..=7).contains(&board_size) {
        return Err(CspError::invalid_input(format!(
            "board_size {board_size} out of range [4, 7] (v1 Futoshiki scope)"
        ))
        .into());
    }

    let (board, inequalities) = py.allow_threads(|| match seed {
        Some(s) => futoshiki::generate_futoshiki_seeded(board_size, s),
        None => futoshiki::generate_futoshiki(board_size),
    });

    let values = board
        .into_iter()
        .enumerate()
        .filter(|&(_, v)| v != 0)
        .map(|(i, v)| (i.to_string(), v as i32))
        .collect();

    Ok(FutoshikiBoard {
        values,
        inequalities,
        board_size,
    })
}
