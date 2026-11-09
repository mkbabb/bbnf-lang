//! Shared fixtures for the aggregated `bbnf-ir` integration test binaries
//! (`egraph`, `lattices`, `passes`, `vm`). Each binary root declares it with
//! `#[path = "../support/mod.rs"] mod support;`.

use bbnf_ir::GrammarIR;
use bbnf_ir::dag::GrammarDag;

/// Build `ir.dag` (and the string index) for a hand-built `GrammarIR`, so a
/// single pass can run in isolation. Production builds the DAG exactly once,
/// at the compile pipeline's DAG step (`crates/core/src/pipeline/compile`).
pub fn ensure_dag(ir: &mut GrammarIR) {
    if ir.dag.is_none() {
        ir.dag = Some(GrammarDag::from_ir(ir));
    }
    if ir.string_index.is_empty() && !ir.strings.is_empty() {
        ir.build_string_index();
    }
}
