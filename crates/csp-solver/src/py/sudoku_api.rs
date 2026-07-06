//! Sudoku convenience API (isomorphic to Python's `csp_solver.solver.sudoku`).

use std::collections::HashMap;

use pyo3::prelude::*;
use pyo3::types::PyType;

use super::config::CancelToken;
use crate::error::CspError;
use crate::ordering::Ordering as RustOrdering;
use crate::sudoku::{self, Difficulty};
use crate::{Pruning as RustPruning, SolveConfig as RustSolveConfig};

#[pyclass]
#[derive(Clone)]
pub enum SudokuDifficulty {
    EASY,
    MEDIUM,
    HARD,
}

#[pymethods]
impl SudokuDifficulty {
    #[classmethod]
    #[pyo3(signature = (key, default=None))]
    fn get(
        _cls: &Bound<'_, PyType>,
        key: &str,
        default: Option<SudokuDifficulty>,
    ) -> Option<SudokuDifficulty> {
        match key {
            "EASY" => Some(SudokuDifficulty::EASY),
            "MEDIUM" => Some(SudokuDifficulty::MEDIUM),
            "HARD" => Some(SudokuDifficulty::HARD),
            _ => default,
        }
    }
}

impl From<SudokuDifficulty> for Difficulty {
    fn from(d: SudokuDifficulty) -> Self {
        match d {
            SudokuDifficulty::EASY => Difficulty::Easy,
            SudokuDifficulty::MEDIUM => Difficulty::Medium,
            SudokuDifficulty::HARD => Difficulty::Hard,
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct SudokuCSP {
    board: Vec<u32>,
    n: u32,
    max_solutions: usize,
    #[pyo3(get)]
    solutions: Vec<HashMap<String, i32>>,
    #[pyo3(get)]
    backtrack_count: u64,
    /// `True` when the last `solve_sudoku`/`solve_sudoku_board` call hit its
    /// node budget and returned best-so-far rather than a verified result
    /// (see `web/api` F12 in the pass-1 audit: previously this was only
    /// reachable via the generic `Csp`/`SolveStats`, not this convenience
    /// type, so `board.py` had no way to distinguish "no solution" from
    /// "search gave up").
    #[pyo3(get)]
    budget_exceeded: bool,
    /// `True` when the last call was stopped early via a `CancelToken`.
    #[pyo3(get)]
    cancelled: bool,
    _given_values: HashMap<String, i32>,
}

#[pymethods]
impl SudokuCSP {
    #[getter]
    fn backtracks(&self) -> u64 {
        self.backtrack_count
    }
}

#[pyfunction]
#[pyo3(signature = (N, values, max_solutions=1))]
pub fn create_sudoku_csp(
    #[allow(non_snake_case)] N: u32,
    values: HashMap<String, i32>,
    max_solutions: usize,
) -> PyResult<SudokuCSP> {
    let n = N;
    let m = n * n;
    let total = (m * m) as usize;

    let mut board = vec![0u32; total];
    let mut given = HashMap::new();
    for (pos_str, val) in &values {
        let pos: usize = pos_str
            .parse()
            .map_err(|_| CspError::invalid_input(format!("invalid position: {pos_str:?}")))?;
        if pos >= total {
            return Err(CspError::invalid_input(format!(
                "position {pos} out of range [0, {total})"
            ))
            .into());
        }
        if *val > 0 {
            board[pos] = *val as u32;
            given.insert(pos_str.clone(), *val);
        }
    }

    Ok(SudokuCSP {
        board,
        n,
        max_solutions,
        solutions: Vec::new(),
        backtrack_count: 0,
        budget_exceeded: false,
        cancelled: false,
        _given_values: given,
    })
}

/// Solve a previously-constructed [`SudokuCSP`] in place.
///
/// Reconciled from grand-audit Pass 2 prototype 14 (`api-error-taxonomy`):
/// previously returned a plain `bool` and could never distinguish "no
/// solution exists" from "the search gave up at its node budget" — both
/// showed up identically as `Ok(false)` / an empty `solutions` list to
/// `board.py` (Pass-1 ledger F4/F12). Now raises the typed
/// `BudgetExceededError` when the budget was hit with zero solutions found,
/// and returns `Ok(false)` (unchanged wire shape) only for the
/// genuinely-exhaustive "provably unsatisfiable" case. Both are also
/// distinguishable via the `budget_exceeded` getter without needing to
/// catch an exception, for callers that prefer to branch on state rather
/// than on control flow.
#[pyfunction]
#[pyo3(signature = (csp, cancel=None))]
pub fn solve_sudoku(
    py: Python<'_>,
    csp: &mut SudokuCSP,
    cancel: Option<CancelToken>,
) -> PyResult<bool> {
    let config = RustSolveConfig {
        pruning: RustPruning::Ac3,
        ordering: RustOrdering::Mrv,
        max_solutions: csp.max_solutions,
        cancel: cancel.as_ref().map(|t| t.inner.clone()),
        ..Default::default()
    };

    let board = &csp.board;
    let n = csp.n;
    // Releases the GIL for the entire construction + search: the FastAPI
    // event loop (and any heartbeat/health-check coroutine on it) keeps
    // running while this executes on its `asyncio.to_thread` worker.
    let (solutions, stats) = py.allow_threads(|| {
        let (mut rust_csp, given) = sudoku::create_sudoku_csp(board, n);
        let solutions = rust_csp.solve_with_given(&config, &given);
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

/// Single-call sketch collapsing `create_sudoku_csp()` + `solve_sudoku()`
/// into one PyO3 entry point, board-construction and search both inside one
/// `py.allow_threads` block.
///
/// The two-call shape (`create_sudoku_csp` then `solve_sudoku`) that
/// `web/api/.../board.py` uses today costs two separate FFI boundary
/// crossings and two separate places a `CancelToken` would need wiring for
/// what is conceptually a single "solve this board" operation. This is the
/// same computation with one boundary crossing and one place to pass the
/// cancellation handle.
#[pyfunction]
#[pyo3(signature = (N, values, max_solutions=1, cancel=None))]
pub fn solve_sudoku_board(
    py: Python<'_>,
    #[allow(non_snake_case)] N: u32,
    values: HashMap<String, i32>,
    max_solutions: usize,
    cancel: Option<CancelToken>,
) -> PyResult<SudokuCSP> {
    let n = N;
    let total = (n * n * n * n) as usize;

    let mut board = vec![0u32; total];
    for (pos_str, val) in &values {
        let pos: usize = pos_str
            .parse()
            .map_err(|_| CspError::invalid_input(format!("invalid position: {pos_str:?}")))?;
        if pos >= total {
            return Err(CspError::invalid_input(format!(
                "position {pos} out of range [0, {total})"
            ))
            .into());
        }
        if *val > 0 {
            board[pos] = *val as u32;
        }
    }

    let config = RustSolveConfig {
        pruning: RustPruning::Ac3,
        ordering: RustOrdering::Mrv,
        max_solutions,
        cancel: cancel.as_ref().map(|t| t.inner.clone()),
        ..Default::default()
    };

    let (solutions, stats) = py.allow_threads(|| {
        let (mut rust_csp, given) = sudoku::create_sudoku_csp(&board, n);
        let solutions = rust_csp.solve_with_given(&config, &given);
        (solutions, rust_csp.stats().clone())
    });

    let solutions: Vec<HashMap<String, i32>> = solutions
        .into_iter()
        .map(|sol| {
            sol.into_iter()
                .enumerate()
                .map(|(i, v)| (i.to_string(), v as i32))
                .collect()
        })
        .collect();

    Ok(SudokuCSP {
        board,
        n,
        max_solutions,
        solutions,
        backtrack_count: stats.backtracks,
        budget_exceeded: stats.budget_exceeded,
        cancelled: stats.cancelled,
        _given_values: values,
    })
}

#[pyfunction]
#[pyo3(signature = (N, difficulty=SudokuDifficulty::EASY, templates=None))]
pub fn create_random_board(
    py: Python<'_>,
    #[allow(non_snake_case)] N: u32,
    difficulty: SudokuDifficulty,
    templates: Option<Vec<HashMap<String, i32>>>,
) -> PyResult<HashMap<String, i32>> {
    // Releases the GIL for the whole generation call — both the embedded
    // fast path and the hole-digging slow path it falls back to, the latter
    // of which has no timeout wrapper at all on the Python side (see pass-1
    // fastapi-service F1a) and is the more direct DoS surface of the two
    // sudoku entry points.
    let board = py.allow_threads(|| -> Result<Vec<u32>, CspError> {
        if let Some(ref tmpls) = templates {
            // Explicit-template path (retained for callers that still marshal
            // their own templates, e.g. the wasm wire). The Python service no
            // longer takes this branch — puzzle data is Rust-owned now.
            let m = (N * N) as usize;
            let total = m * m;
            let flat_templates: Vec<Vec<u32>> = tmpls
                .iter()
                .map(|t| {
                    let mut flat = vec![0u32; total];
                    for (k, v) in t {
                        if let Ok(pos) = k.parse::<usize>()
                            && pos < total
                        {
                            flat[pos] = *v as u32;
                        }
                    }
                    flat
                })
                .collect();
            Ok(sudoku::generate_board_with_templates(
                N,
                difficulty.into(),
                &flat_templates,
            ))
        } else {
            // Rust-owned fast path: the crate serves its own compile-time
            // embedded template bank (`include_dir!` in `puzzles/sudoku/
            // generate.rs`). When no bank is embedded for this size/difficulty
            // (e.g. the locked N=5 Medium/Hard policy), reject explicitly rather
            // than fall through to unbounded hole-digging — the service also
            // gates this up front via `template_count`, so this is defense in
            // depth against a direct caller.
            let diff: Difficulty = difficulty.into();
            let templates = sudoku::embedded_templates(N, diff);
            if templates.is_empty() {
                return Err(CspError::invalid_input(format!(
                    "no embedded template bank for N={N} {diff:?} — not pregenerated \
                     (locked N=5 policy)"
                )));
            }
            Ok(sudoku::generate_board_with_templates(N, diff, &templates))
        }
    })?;
    Ok(board
        .into_iter()
        .enumerate()
        .map(|(i, v)| (i.to_string(), v as i32))
        .collect())
}

/// Number of pregenerated templates embedded for `(N, difficulty)`.
///
/// The Python service calls this to enforce the locked N=5 policy: a request for
/// a size/difficulty with zero embedded templates (N=5 Medium/Hard) is rejected
/// with a `NOT_FOUND` up front, rather than falling through to unbounded
/// hole-digging generation. Counts from the embedded directory listing without
/// parsing any template file.
#[pyfunction]
#[pyo3(signature = (N, difficulty=SudokuDifficulty::EASY))]
pub fn template_count(#[allow(non_snake_case)] N: u32, difficulty: SudokuDifficulty) -> usize {
    sudoku::embedded_template_count(N, difficulty.into())
}
