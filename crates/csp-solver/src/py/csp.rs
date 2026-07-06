//! Generic `Csp` PyO3 class (wraps `Csp<BitsetDomain>`).

use std::collections::HashMap;

use pyo3::prelude::*;

use super::config::{SolveConfig, SolveStats};
use super::enums::PropagationStrategy;
use crate::constraint::VarId;
use crate::domain::bitset::BitsetDomain;
use crate::error::CspError;
use crate::{Csp as RustCsp, SolveConfig as RustSolveConfig};

#[pyclass(unsendable)]
pub struct Csp {
    inner: RustCsp<BitsetDomain>,
}

#[pymethods]
impl Csp {
    #[new]
    fn new() -> Self {
        Self {
            inner: RustCsp::new(),
        }
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
    ///
    /// Releases the GIL for the duration of the native call (`py.allow_threads`)
    /// so a Python-side `asyncio.wait_for` timeout / heartbeat thread keeps
    /// making progress while this runs on a worker thread.
    fn propagate(&mut self, py: Python<'_>) -> PyResult<bool> {
        let inner = &mut self.inner;
        py.allow_threads(|| inner.propagate())
            .map(|()| true)
            .map_err(|e| CspError::from(e).into())
    }

    /// Propagate with an explicit strategy. See `propagate` re: GIL release.
    fn propagate_with(&mut self, py: Python<'_>, strategy: PropagationStrategy) -> PyResult<bool> {
        let inner = &mut self.inner;
        py.allow_threads(|| inner.propagate_with(strategy.into()))
            .map(|()| true)
            .map_err(|e| CspError::from(e).into())
    }

    /// Solve with the given configuration. See `propagate` re: GIL release.
    fn solve(&mut self, py: Python<'_>, config: &SolveConfig) -> Vec<HashMap<u32, u32>> {
        let rust_config: RustSolveConfig = config.into();
        let inner = &mut self.inner;
        py.allow_threads(|| inner.solve(&rust_config))
            .into_iter()
            .map(|sol| {
                sol.into_iter()
                    .enumerate()
                    .map(|(i, v)| (i as u32, v))
                    .collect()
            })
            .collect()
    }

    /// Solve with pre-assigned values. See `propagate` re: GIL release.
    fn solve_with_given(
        &mut self,
        py: Python<'_>,
        config: &SolveConfig,
        given: HashMap<u32, u32>,
    ) -> Vec<HashMap<u32, u32>> {
        let rust_config: RustSolveConfig = config.into();
        let given_vec: Vec<(VarId, u32)> = given.into_iter().collect();
        let inner = &mut self.inner;
        py.allow_threads(|| inner.solve_with_given(&rust_config, &given_vec))
            .into_iter()
            .map(|sol| {
                sol.into_iter()
                    .enumerate()
                    .map(|(i, v)| (i as u32, v))
                    .collect()
            })
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
            budget_exceeded: s.budget_exceeded,
            cancelled: s.cancelled,
        }
    }
}
