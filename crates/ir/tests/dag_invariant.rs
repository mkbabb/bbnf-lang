//! Mechanical enforcement of the "DAG built exactly once per
//! compile" invariant.
//!
//! `GrammarDag::from_ir` is the constructor for the durable
//! grammar DAG substrate. It must be called from exactly one
//! production call site — `crates/core/src/pipeline/compile.rs` —
//! plus the `bbnf_ir::dag::ensure_dag` test/bench helper. Any
//! other production call is a bug: the reverse-pointer map inside
//! `GrammarDag` is keyed on `*const IrNode` addresses valid only
//! for the borrowed `&GrammarIR` the DAG was built from, so
//! rebuilding at a second site would produce a DAG whose pointer
//! map races with the first.
//!
//! This test reads the workspace source tree via `include_str!`
//! and asserts the grep invariant at `cargo test` time — no
//! process spawn, no external tools. Run as part of the standard
//! `cargo test -p bbnf-ir` sweep.

const COMPILE_RS: &str = include_str!("../../core/src/pipeline/compile.rs");
const DAG_MOD_RS: &str = include_str!("../src/dag/mod.rs");

/// Count only fully-qualified call sites — `GrammarDag::from_ir(...)`
/// with a paren — not comment / doc mentions and not the function
/// definition line itself.
fn count_call_sites(src: &str) -> usize {
    src.matches("GrammarDag::from_ir(").count()
}

#[test]
fn exactly_one_production_call_site_for_grammar_dag_from_ir() {
    // Production call site: inside compile.rs, there must be
    // exactly one `GrammarDag::from_ir(...)` invocation. It lives
    // inside `compile_ast_common` after the body-mutating facts
    // passes converge and before the non-mutating fact passes.
    let compile_occurrences = count_call_sites(COMPILE_RS);
    assert_eq!(
        compile_occurrences, 1,
        "compile.rs must contain exactly one `GrammarDag::from_ir(...)` \
         call site; found {}. Production code builds the DAG exactly \
         once — adding a second call site breaks the pointer-map \
         identity substrate. Use `bbnf_ir::dag::ensure_dag` in tests \
         instead.",
        compile_occurrences,
    );
}

#[test]
fn ensure_dag_helper_lives_in_dag_mod() {
    // The test/bench helper must exist in dag/mod.rs, not
    // scattered across the codebase. It is the single permitted
    // non-production call site for `GrammarDag::from_ir`.
    assert!(
        DAG_MOD_RS.contains("pub fn ensure_dag"),
        "`bbnf_ir::dag::ensure_dag` helper must be defined in \
         crates/ir/src/dag/mod.rs",
    );
    let dag_mod_occurrences = count_call_sites(DAG_MOD_RS);
    assert_eq!(
        dag_mod_occurrences, 1,
        "dag/mod.rs must contain exactly one `GrammarDag::from_ir(...)` \
         call — inside the `ensure_dag` helper. Found {}.",
        dag_mod_occurrences,
    );
}
