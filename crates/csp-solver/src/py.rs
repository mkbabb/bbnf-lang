//! PyO3 bindings — general-purpose CSP solver + Sudoku convenience API.
//!
//! Module name: `csp_solver` (drop-in replacement for the Python package).
//!
//! ```python
//! from csp_solver import Csp, Pruning, Ordering, PropagationStrategy, SolveConfig
//! from csp_solver import SudokuDifficulty, create_sudoku_csp, solve_sudoku, create_random_board
//! ```
//!
//! Build: `PYO3_USE_ABI3_FORWARD_COMPATIBILITY=1 maturin develop --release --features py`

use std::collections::HashMap;

use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;
use pyo3::types::PyType;

use crate::constraint::VarId;
use crate::domain::bitset::BitsetDomain;
use crate::ordering::Ordering as RustOrdering;
use crate::sudoku::{self, Difficulty};
use crate::{Csp as RustCsp, Pruning as RustPruning, PropagationStrategy as RustPropagation, SolveConfig as RustSolveConfig};

// ═══════════════════════════════════════════════════════════════════════════
// Core enums
// ═══════════════════════════════════════════════════════════════════════════

#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, PartialEq)]
pub enum Pruning {
    NONE = 0,
    FORWARD_CHECKING = 1,
    AC3 = 2,
    AC_FC = 3,
}

impl From<Pruning> for RustPruning {
    fn from(p: Pruning) -> Self {
        match p {
            Pruning::NONE => RustPruning::None,
            Pruning::FORWARD_CHECKING => RustPruning::ForwardChecking,
            Pruning::AC3 => RustPruning::Ac3,
            Pruning::AC_FC => RustPruning::AcFc,
        }
    }
}

#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, PartialEq)]
pub enum Ordering {
    CHRONOLOGICAL = 0,
    FAIL_FIRST = 1,
    DOM_WDEG = 2,
}

impl From<Ordering> for RustOrdering {
    fn from(o: Ordering) -> Self {
        match o {
            Ordering::CHRONOLOGICAL => RustOrdering::Chronological,
            Ordering::FAIL_FIRST => RustOrdering::FailFirst,
            Ordering::DOM_WDEG => RustOrdering::DomWdeg,
        }
    }
}

#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, PartialEq)]
pub enum PropagationStrategy {
    AUTO = 0,
    AC3 = 1,
    SWEEP = 2,
}

impl From<PropagationStrategy> for RustPropagation {
    fn from(s: PropagationStrategy) -> Self {
        match s {
            PropagationStrategy::AUTO => RustPropagation::Auto,
            PropagationStrategy::AC3 => RustPropagation::Ac3,
            PropagationStrategy::SWEEP => RustPropagation::Sweep,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// SolveConfig + SolveStats
// ═══════════════════════════════════════════════════════════════════════════

#[pyclass]
#[derive(Clone)]
pub struct SolveConfig {
    #[pyo3(get, set)]
    pub pruning: Pruning,
    #[pyo3(get, set)]
    pub ordering: Ordering,
    #[pyo3(get, set)]
    pub max_solutions: usize,
    #[pyo3(get, set)]
    pub backjumping: bool,
    /// Maximum number of search nodes before aborting early.
    /// `None` disables the budget. Defaults to 1_000_000.
    #[pyo3(get, set)]
    pub node_budget: Option<u64>,
}

#[pymethods]
impl SolveConfig {
    #[new]
    #[pyo3(signature = (pruning=Pruning::FORWARD_CHECKING, ordering=Ordering::CHRONOLOGICAL, max_solutions=1, backjumping=false, node_budget=Some(1_000_000)))]
    fn new(
        pruning: Pruning,
        ordering: Ordering,
        max_solutions: usize,
        backjumping: bool,
        node_budget: Option<u64>,
    ) -> Self {
        Self { pruning, ordering, max_solutions, backjumping, node_budget }
    }
}

impl From<&SolveConfig> for RustSolveConfig {
    fn from(c: &SolveConfig) -> Self {
        RustSolveConfig {
            pruning: c.pruning.into(),
            ordering: c.ordering.into(),
            max_solutions: c.max_solutions,
            backjumping: c.backjumping,
            node_budget: c.node_budget,
            ..Default::default()
        }
    }
}

#[pyclass]
#[derive(Clone)]
pub struct SolveStats {
    #[pyo3(get)]
    pub backtracks: u64,
    #[pyo3(get)]
    pub nodes_explored: u64,
    #[pyo3(get)]
    pub propagations: u64,
    /// `True` when the last search hit its `node_budget` and returned
    /// best-so-far rather than optimal results.
    #[pyo3(get)]
    pub budget_exceeded: bool,
}

// ═══════════════════════════════════════════════════════════════════════════
// Generic Csp class (wraps Csp<BitsetDomain>)
// ═══════════════════════════════════════════════════════════════════════════

#[pyclass(unsendable)]
pub struct Csp {
    inner: RustCsp<BitsetDomain>,
}

#[pymethods]
impl Csp {
    #[new]
    fn new() -> Self {
        Self { inner: RustCsp::new() }
    }

    /// Add a variable with the given domain values. Returns its VarId.
    fn add_variable(&mut self, domain: Vec<u32>) -> u32 {
        self.inner.add_variable(BitsetDomain::new(domain))
    }

    /// Add a not-equal constraint between two variables.
    fn add_not_equal(&mut self, x: u32, y: u32) {
        self.inner.add_not_equal(x, y);
    }

    /// Add an all-different constraint over a group of variables.
    fn add_all_different(&mut self, vars: Vec<u32>) {
        self.inner.add_all_different(vars);
    }

    /// Fix a variable to a specific value.
    fn add_equals(&mut self, var: u32, value: u32) {
        self.inner.add_equals(var, value);
    }

    /// Constrain x < y.
    fn add_less_than(&mut self, x: u32, y: u32) {
        self.inner.add_less_than(x, y);
    }

    /// Constrain x > y.
    fn add_greater_than(&mut self, x: u32, y: u32) {
        self.inner.add_greater_than(x, y);
    }

    /// Build the adjacency graph. Required before solve(), optional before propagate().
    fn finalize(&mut self) {
        self.inner.finalize();
    }

    /// Propagate constraints (auto-select strategy).
    fn propagate(&mut self) -> PyResult<bool> {
        self.inner.propagate().map(|()| true).map_err(|_| PyRuntimeError::new_err("Unsatisfiable"))
    }

    /// Propagate with an explicit strategy.
    fn propagate_with(&mut self, strategy: PropagationStrategy) -> PyResult<bool> {
        self.inner
            .propagate_with(strategy.into())
            .map(|()| true)
            .map_err(|_| PyRuntimeError::new_err("Unsatisfiable"))
    }

    /// Solve with the given configuration.
    fn solve(&mut self, config: &SolveConfig) -> Vec<HashMap<u32, u32>> {
        let rust_config: RustSolveConfig = config.into();
        self.inner
            .solve(&rust_config)
            .into_iter()
            .map(|sol| sol.into_iter().enumerate().map(|(i, v)| (i as u32, v)).collect())
            .collect()
    }

    /// Solve with pre-assigned values.
    fn solve_with_given(&mut self, config: &SolveConfig, given: HashMap<u32, u32>) -> Vec<HashMap<u32, u32>> {
        let rust_config: RustSolveConfig = config.into();
        let given_vec: Vec<(VarId, u32)> = given.into_iter().collect();
        self.inner
            .solve_with_given(&rust_config, &given_vec)
            .into_iter()
            .map(|sol| sol.into_iter().enumerate().map(|(i, v)| (i as u32, v)).collect())
            .collect()
    }

    /// Solver statistics from the last run.
    #[getter]
    fn stats(&self) -> SolveStats {
        let s = self.inner.stats();
        SolveStats {
            backtracks: s.backtracks,
            nodes_explored: s.nodes_explored,
            propagations: s.propagations,
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Sudoku convenience API (isomorphic to Python's csp_solver.solver.sudoku)
// ═══════════════════════════════════════════════════════════════════════════

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
    fn get(_cls: &Bound<'_, PyType>, key: &str, default: Option<SudokuDifficulty>) -> Option<SudokuDifficulty> {
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
    _given_values: HashMap<String, i32>,
}

#[pymethods]
impl SudokuCSP {
    #[getter]
    fn backtracks(&self) -> u64 { self.backtrack_count }
}

#[pyfunction]
#[pyo3(signature = (N, values, max_solutions=1))]
fn create_sudoku_csp(
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
            .map_err(|_| PyValueError::new_err(format!("Invalid position: {pos_str}")))?;
        if pos >= total {
            return Err(PyValueError::new_err(format!("Position {pos} out of range")));
        }
        if *val > 0 {
            board[pos] = *val as u32;
            given.insert(pos_str.clone(), *val);
        }
    }

    Ok(SudokuCSP { board, n, max_solutions, solutions: Vec::new(), backtrack_count: 0, _given_values: given })
}

#[pyfunction]
fn solve_sudoku(csp: &mut SudokuCSP) -> PyResult<bool> {
    let config = RustSolveConfig {
        pruning: RustPruning::Ac3,
        ordering: RustOrdering::DomWdeg,
        max_solutions: csp.max_solutions,
        backjumping: false,
        ..Default::default()
    };

    let (mut rust_csp, given) = sudoku::create_sudoku_csp(&csp.board, csp.n);
    let solutions = rust_csp.solve_with_given(&config, &given);
    let stats = rust_csp.stats();
    csp.backtrack_count = stats.backtracks;

    csp.solutions = solutions
        .into_iter()
        .map(|sol| sol.into_iter().enumerate().map(|(i, v)| (i.to_string(), v as i32)).collect())
        .collect();

    Ok(!csp.solutions.is_empty())
}

#[pyfunction]
#[pyo3(signature = (N, difficulty=SudokuDifficulty::EASY, templates=None))]
fn create_random_board(
    #[allow(non_snake_case)] N: u32,
    difficulty: SudokuDifficulty,
    templates: Option<Vec<HashMap<String, i32>>>,
) -> PyResult<HashMap<String, i32>> {
    let board = if let Some(ref tmpls) = templates {
        // Fast path: convert template dicts to flat boards, use template-based generation.
        let m = (N * N) as usize;
        let total = m * m;
        let flat_templates: Vec<Vec<u32>> = tmpls
            .iter()
            .map(|t| {
                let mut flat = vec![0u32; total];
                for (k, v) in t {
                    if let Ok(pos) = k.parse::<usize>() {
                        if pos < total {
                            flat[pos] = *v as u32;
                        }
                    }
                }
                flat
            })
            .collect();
        sudoku::generate_board_with_templates(N, difficulty.into(), &flat_templates)
    } else {
        // Slow path: hole-digging from scratch.
        sudoku::generate_board(N, difficulty.into())
    };
    Ok(board.into_iter().enumerate().map(|(i, v)| (i.to_string(), v as i32)).collect())
}

// ═══════════════════════════════════════════════════════════════════════════
// Module registration
// ═══════════════════════════════════════════════════════════════════════════

#[pymodule]
pub fn csp_solver(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Core
    m.add_class::<Pruning>()?;
    m.add_class::<Ordering>()?;
    m.add_class::<PropagationStrategy>()?;
    m.add_class::<SolveConfig>()?;
    m.add_class::<SolveStats>()?;
    m.add_class::<Csp>()?;
    // Sudoku
    m.add_class::<SudokuDifficulty>()?;
    m.add_class::<SudokuCSP>()?;
    m.add_function(wrap_pyfunction!(create_sudoku_csp, m)?)?;
    m.add_function(wrap_pyfunction!(solve_sudoku, m)?)?;
    m.add_function(wrap_pyfunction!(create_random_board, m)?)?;
    Ok(())
}
