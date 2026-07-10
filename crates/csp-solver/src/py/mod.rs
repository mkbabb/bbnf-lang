//! PyO3 bindings — general-purpose CSP solver + Sudoku/Futoshiki convenience API.
//!
//! Module name: `csp_solver`. This is the only solver the FastAPI backend
//! (`web/api`) imports; there is no Python-side solver behind it.
//!
//! ```python
//! from csp_solver import Csp, Pruning, Ordering, PropagationStrategy, SolveConfig
//! from csp_solver import CancelToken
//! from csp_solver import SudokuDifficulty, create_sudoku_csp, solve_sudoku, create_random_board
//! ```
//!
//! Build: `maturin develop --release --features py`
//!
//! Split into submodules (each well under the 500-line budget) rather than
//! one monolithic file:
//! - `enums` — `Pruning` / `Ordering` / `PropagationStrategy` + their
//!   `From` impls into the Rust-core equivalents.
//! - `config` — `SolveConfig`, `SolveStats`, and the `CancelToken`
//!   cooperative-cancellation handle.
//! - `csp` — the general-purpose `Csp` pyclass (wraps `Csp<BitsetDomain>`).
//! - `sudoku_api` — the Sudoku convenience API (`SudokuCSP`,
//!   `create_sudoku_csp`, `solve_sudoku`, `solve_sudoku_board`,
//!   `create_random_board`).
//! - `errors` — typed exceptions (`UnsatisfiableError`, `BudgetExceededError`,
//!   `InvalidInputError`, `CspTimeoutError`), one per [`crate::error::CspError`]
//!   variant. Reconciled here from grand-audit Pass 2 prototype 14
//!   (`api-error-taxonomy`), which authored it against a since-superseded
//!   monolithic `py.rs` + `py_errors.rs` sibling shape — see the Pass-3
//!   `py-module-reconciliation` report.

use pyo3::prelude::*;

mod config;
mod csp;
mod enums;
pub mod errors;
mod futoshiki_api;
mod sudoku_api;

pub use config::{CancelToken, SolveConfig, SolveStats};
pub use csp::Csp;
pub use enums::{Ordering, PropagationStrategy, Pruning};
pub use errors::{BudgetExceededError, CspTimeoutError, InvalidInputError, UnsatisfiableError};
pub use futoshiki_api::{
    FutoshikiBoard, FutoshikiCSP, create_futoshiki_csp, create_random_futoshiki, solve_futoshiki,
};
pub use sudoku_api::{
    SudokuCSP, SudokuDifficulty, create_random_board, create_sudoku_csp, solve_sudoku,
    solve_sudoku_board, template_count,
};

#[pymodule]
pub fn csp_solver(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Core
    m.add_class::<Pruning>()?;
    m.add_class::<Ordering>()?;
    m.add_class::<PropagationStrategy>()?;
    m.add_class::<SolveConfig>()?;
    m.add_class::<SolveStats>()?;
    m.add_class::<CancelToken>()?;
    m.add_class::<Csp>()?;
    // Sudoku
    m.add_class::<SudokuDifficulty>()?;
    m.add_class::<SudokuCSP>()?;
    m.add_function(wrap_pyfunction!(create_sudoku_csp, m)?)?;
    m.add_function(wrap_pyfunction!(solve_sudoku, m)?)?;
    m.add_function(wrap_pyfunction!(solve_sudoku_board, m)?)?;
    m.add_function(wrap_pyfunction!(create_random_board, m)?)?;
    m.add_function(wrap_pyfunction!(template_count, m)?)?;
    // Futoshiki
    m.add_class::<FutoshikiCSP>()?;
    m.add_class::<FutoshikiBoard>()?;
    m.add_function(wrap_pyfunction!(create_futoshiki_csp, m)?)?;
    m.add_function(wrap_pyfunction!(solve_futoshiki, m)?)?;
    m.add_function(wrap_pyfunction!(create_random_futoshiki, m)?)?;
    // Typed exceptions — `from csp_solver import UnsatisfiableError, ...`
    m.add(
        "UnsatisfiableError",
        m.py().get_type::<UnsatisfiableError>(),
    )?;
    m.add(
        "BudgetExceededError",
        m.py().get_type::<BudgetExceededError>(),
    )?;
    m.add("InvalidInputError", m.py().get_type::<InvalidInputError>())?;
    m.add("CspTimeoutError", m.py().get_type::<CspTimeoutError>())?;
    Ok(())
}
