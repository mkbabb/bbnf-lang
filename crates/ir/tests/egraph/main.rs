//! Aggregated integration test binary — e-graph substrate + interner.
//!
//! Each sub-module is a formerly free-standing `tests/*.rs` file; folding
//! them into one binary root cuts rustc/linker invocations without changing
//! test semantics. Add new e-graph tests as additional `mod ...;`
//! declarations here, with the actual test bodies under `tests/egraph/`.

mod egraph_analysis;
mod egraph_grammar;
mod egraph_interner;
mod egraph_roundtrip;
mod egraph_suffix;
mod type_desc_interner;
