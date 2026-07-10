//! PyO3-facing enums mirroring the Rust core's `Pruning` / `Ordering` /
//! `PropagationStrategy`.

use pyo3::prelude::*;

use crate::ordering::Ordering as RustOrdering;
use crate::{PropagationStrategy as RustPropagation, Pruning as RustPruning};

// PyO3 exports these variant names verbatim to Python; they intentionally keep
// the SCREAMING_CASE the Python API expects rather than Rust's CamelCase.
#[allow(non_camel_case_types)]
#[pyclass(eq, eq_int, from_py_object)]
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

// PyO3 exports these variant names verbatim to Python; they intentionally keep
// the SCREAMING_CASE the Python API expects rather than Rust's CamelCase.
#[allow(non_camel_case_types)]
#[pyclass(eq, eq_int, from_py_object)]
#[derive(Clone, Copy, PartialEq)]
pub enum Ordering {
    CHRONOLOGICAL = 0,
    FAIL_FIRST = 1,
    MRV = 2,
}

impl From<Ordering> for RustOrdering {
    fn from(o: Ordering) -> Self {
        match o {
            Ordering::CHRONOLOGICAL => RustOrdering::Chronological,
            Ordering::FAIL_FIRST => RustOrdering::FailFirst,
            Ordering::MRV => RustOrdering::Mrv,
        }
    }
}

// PyO3 exports these variant names verbatim to Python; they intentionally keep
// the SCREAMING_CASE the Python API expects rather than Rust's CamelCase.
#[allow(non_camel_case_types)]
#[pyclass(eq, eq_int, from_py_object)]
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
