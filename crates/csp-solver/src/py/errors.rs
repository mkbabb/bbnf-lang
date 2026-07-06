//! Typed PyO3 exceptions — one Python class per [`crate::error::CspError`]
//! variant.
//!
//! Reconciled from grand-audit Pass 2 prototype 14 (`api-error-taxonomy`,
//! originally a `py_errors.rs` sibling of a monolithic `py.rs`) onto the
//! Pass-3 composed `py/` directory shape (prototype 5, `pyo3-liberation`) —
//! see the Pass-3 `py-module-reconciliation` report for the two-shapes
//! finding this resolves.
//!
//! Replaces the prior `PyRuntimeError::new_err("Unsatisfiable")` /
//! `PyValueError::new_err(...)` pattern (Pass-1 ledger R15 — a bare
//! `PyRuntimeError`, distinguishable only by string-matching its message)
//! with four `except`-able classes. The `From<CspError> for PyErr` impl
//! below is the one place that maps variant → class, so every call site in
//! `py/csp.rs` / `py/sudoku_api.rs` that returns `PyResult<T>` can just `?`
//! a `Result<T, CspError>` instead of hand-rolling its own `map_err`.

use pyo3::PyErr;
use pyo3::create_exception;
use pyo3::exceptions::PyException;

use crate::error::CspError;

create_exception!(
    csp_solver,
    UnsatisfiableError,
    PyException,
    "No solution exists under the given constraints."
);
create_exception!(
    csp_solver,
    BudgetExceededError,
    PyException,
    "The search aborted after exhausting its node budget; results (if any) are best-so-far."
);
create_exception!(
    csp_solver,
    InvalidInputError,
    PyException,
    "A caller-supplied argument was structurally invalid."
);
create_exception!(
    csp_solver,
    CspTimeoutError,
    PyException,
    "The search exceeded its wall-clock deadline. Distinct from BudgetExceededError \
     (node-count budget) — reserved for the cooperative wall-clock cancellation path."
);

impl CspError {
    /// Map to the matching typed Python exception, one class per variant —
    /// never a bare `PyRuntimeError` string match.
    fn to_pyerr(&self) -> PyErr {
        let msg = self.to_string();
        match self {
            CspError::Unsatisfiable => UnsatisfiableError::new_err(msg),
            CspError::BudgetExceeded => BudgetExceededError::new_err(msg),
            CspError::InvalidInput { .. } => InvalidInputError::new_err(msg),
            CspError::Timeout => CspTimeoutError::new_err(msg),
        }
    }
}

impl From<CspError> for PyErr {
    fn from(e: CspError) -> PyErr {
        e.to_pyerr()
    }
}
