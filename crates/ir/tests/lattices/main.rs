//! Aggregated integration test binary — lattice-shaped state.
//!
//! Each sub-module is a formerly free-standing `tests/*.rs` file; folding
//! them into one binary root cuts rustc/linker invocations without changing
//! test semantics. Add new lattice-related tests as additional `mod ...;`
//! declarations here, with the actual test bodies under `tests/lattices/`.

mod csp_components;
mod csp_materialization;
mod csp_types;
mod materialization;
mod materialization_eclass_gate;
mod types;
