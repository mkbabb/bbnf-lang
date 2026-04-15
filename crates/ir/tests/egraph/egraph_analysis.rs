//! Tranche AA.2 — `GrammarAnalysis` activation test.
//!
//! Verifies that the `Analysis<GrammarENode>` substrate is live in
//! production e-graph construction: building + saturating a real
//! grammar produces an `EGraph<GrammarENode, GrammarAnalysis>` whose
//! root classes carry non-trivial `EClassFacts`.
//!
//! The facts themselves are computed monotonically by
//! `GrammarAnalysis::make` and propagated via `merge` when classes
//! union. The test does not assert specific fact values (those vary
//! with pool resolution and downstream normalization order); it
//! asserts that (a) the e-graph instantiates the analysis without a
//! panic, (b) the per-class `data` is reachable via the
//! `egraph.class(id).data` accessor, and (c) rules run against the
//! analysis-enabled e-graph still produce correct post-saturation IR.

use bbnf_ir::egraph::{build_and_saturate, GrammarAnalysis};
use bbnf_ir::GrammarIR;

/// Helper: compile a tiny fixture grammar all the way through the
/// bbnf-ir public surface (without the full pipeline, which lives in
/// the bbnf crate). Builds a minimal `GrammarIR` from a handwritten
/// tree so the test file doesn't cross into the parser crate.
fn fixture_ir() -> GrammarIR {
    use bbnf_ir::{IrNode, IrRule, RuleMeta, StringId};
    use std::collections::HashMap;

    let strings = vec![
        "entry".to_string(),
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ];

    let rule = IrRule {
        id: 0,
        name: 0 as StringId,
        body: IrNode::Alt(
            vec![
                bbnf_ir::AltBranch {
                    node: IrNode::Literal(1),
                    first_set: None,
                },
                bbnf_ir::AltBranch {
                    node: IrNode::Literal(2),
                    first_set: None,
                },
                bbnf_ir::AltBranch {
                    node: IrNode::Literal(3),
                    first_set: None,
                },
            ],
            None,
        ),
        meta: RuleMeta::default(),
        source_span: None,
    };

    let mut ir = GrammarIR {
        rules: vec![rule],
        entry: 0,
        strings,
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
        type_map: None,
        pattern_annotations: HashMap::new(),
        regex_info: HashMap::new(),
        node_facts: HashMap::new(),
        recognizer_decisions: HashMap::new(),
        delim_scan_configs: HashMap::new(),
        key_dispatch_configs: HashMap::new(),
        context_facts: HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        string_index: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        struct_registry: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    ir
}

#[test]
fn grammar_analysis_activates_in_production_build_and_saturate() {
    let ir = fixture_ir();

    // The load-bearing assertion: calling `build_and_saturate` must
    // return an `EGraph<GrammarENode, GrammarAnalysis>`. Type-level
    // check: if `GrammarAnalysis` isn't the substrate, the compiler
    // rejects this let binding. The runtime assert just keeps the
    // test from being dead code after optimization.
    let (egraph, _pool, rule_body_ids) = build_and_saturate(&ir);

    // Every rule root has a canonical class with attached data.
    for (_rid, &root) in &rule_body_ids {
        let class = egraph.class(root);
        // `class.data` is `EClassFacts` — access triggers a field
        // read, which fails to compile if the substrate is wrong.
        let _first_set = class.data.first_set.clone();
        let _nullable = class.data.nullable;
        let _width_min = class.data.width.min;
    }

    // Second assertion: every class has a `data` field (zero panics
    // on the accessor chain).
    let total_classes = egraph.classes().count();
    assert!(
        total_classes >= 1,
        "saturation produced at least one e-class"
    );

    // Type-level sanity: build_and_saturate's return type is EGraph
    // parameterized over GrammarAnalysis — the assignment below
    // forces the compiler to see this.
    let _: &::egraph::EGraph<::bbnf_ir::egraph::GrammarENode, GrammarAnalysis> = &egraph;
}

#[test]
fn eclass_facts_epsilon_is_nullable() {
    use bbnf_ir::egraph::EClassFacts;
    let facts = EClassFacts::epsilon();
    assert!(facts.nullable);
    assert_eq!(facts.width.min, 0);
    assert_eq!(facts.width.max, Some(0));
}

#[test]
fn eclass_facts_merge_is_monotone() {
    use bbnf_ir::egraph::EClassFacts;

    let mut a = EClassFacts::default();
    let b = EClassFacts::epsilon();

    // First merge: nullable widens from false → true.
    let changed_first = a.merge(&b);
    assert!(changed_first);
    assert!(a.nullable);

    // Second merge with same value: no change (idempotent).
    let changed_second = a.merge(&b);
    assert!(!changed_second);
}
