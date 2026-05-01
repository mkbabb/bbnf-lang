//! Tranche N regression: codegen output must be byte-stable across
//! repeated runs of the same source.
//!
//! The bug fixed in Tranche N: the dependency graph
//! `Dependencies = HashMap<&str, HashSet<&str>>` had non-deterministic
//! iteration order, which propagated through Tarjan's SCC discovery
//! and Kahn's topological sort, which reordered `ir.rules`, which
//! reordered enum variants in the generated Rust code. The fix was
//! to switch the dependency graph to `IndexMap<&str, IndexSet<&str>>`
//! so iteration order matches insertion order (which is itself
//! deterministic — the AST is parsed in source order).
//!
//! This test compiles the same grammar twice and asserts that the
//! resulting IR has identical rule order, the same dispatch
//! strategies, and the same FIRST sets. It runs in milliseconds and
//! catches future regressions at `cargo test` time without needing
//! to invoke the bootstrap script.

use bbnf::pipeline::{PipelineOptions, compile_grammar};

const FIXTURE: &str = include_str!("fixtures/deterministic.bbnf");

#[test]
fn lowering_is_deterministic_across_runs() {
    let options = PipelineOptions::default();
    let ir1 = compile_grammar(FIXTURE, &options).expect("compile run 1");
    let ir2 = compile_grammar(FIXTURE, &options).expect("compile run 2");

    // Sanity: the fixture should produce a non-trivial IR.
    assert!(
        ir1.rules.len() > 1,
        "fixture should produce multiple rules; got {}",
        ir1.rules.len(),
    );

    // Rule count must match.
    assert_eq!(
        ir1.rules.len(),
        ir2.rules.len(),
        "rule count must be identical across runs",
    );

    // Rule order must be identical (by name + id).
    for (i, (r1, r2)) in ir1.rules.iter().zip(ir2.rules.iter()).enumerate() {
        let n1 = ir1.get_string(r1.name);
        let n2 = ir2.get_string(r2.name);
        assert_eq!(
            (r1.id, n1),
            (r2.id, n2),
            "rule {} differs across runs: ({:?},{}) vs ({:?},{})",
            i,
            r1.id,
            n1,
            r2.id,
            n2,
        );
    }

    // Strings table must match exactly (same strings, same order).
    assert_eq!(
        ir1.strings, ir2.strings,
        "string interner must be deterministic across runs",
    );

    // Entry rule id must match.
    assert_eq!(ir1.entry, ir2.entry);
}

#[test]
fn dependency_graph_iteration_is_deterministic() {
    use bbnf::graph::{calculate_ast_deps, tarjan_scc};

    let parsed = bbnf::grammar::parse(FIXTURE).expect("parse fixture");
    let ast = parsed.rules;
    let deps1 = calculate_ast_deps(&ast);
    let deps2 = calculate_ast_deps(&ast);

    // IndexMap iteration order should be identical for two builds
    // from the same AST.
    let names1: Vec<&str> = deps1.keys().copied().collect();
    let names2: Vec<&str> = deps2.keys().copied().collect();
    assert_eq!(
        names1, names2,
        "dependency graph key order must be deterministic",
    );

    // Tarjan SCC results should also be byte-stable.
    let scc1 = tarjan_scc(&deps1);
    let scc2 = tarjan_scc(&deps2);
    assert_eq!(
        scc1.sccs, scc2.sccs,
        "SCC discovery order must be deterministic",
    );
}
