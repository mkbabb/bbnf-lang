//! Aggregated integration test binary — bytecode compiler, VM, debug adapter,
//! cost-model gate, and cross-rule CSP gate.
//!
//! Each sub-module is a formerly free-standing `tests/*.rs` file; folding
//! them into one binary root cuts rustc/linker invocations without changing
//! test semantics. Add new VM-adjacent tests as additional `mod ...;`
//! declarations here, with the actual test bodies under `tests/vm/`.

mod compiler;
mod cost_weights_unified;
mod cross_rule_csp;
mod debug;
mod interpreter;
