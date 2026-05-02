//! Per-rule unit tests for the AY.W2.3 universal rewrites (G1-G4).
//!
//! - [`alt_of_single_collapses_to_inner`] — G1 `Alt([x]) ≡ x`.
//! - [`alt_of_single_abstains_on_multi_branch`] — G1 negative case.
//! - [`repeat_of_single_collapses_to_inner`] — G2 `Repeat{1,1} ≡ inner`.
//! - [`repeat_of_single_abstains_on_bounds`] — G2 negative case.
//! - [`wrap_of_epsilon_scalar_collapses_to_leaf`] — G3 PRIMARY LEVER.
//! - [`wrap_of_epsilon_with_ref_scalar_collapses`] — G3 via Ref +
//!   TypeDesc side-table.
//! - [`wrap_of_epsilon_non_scalar_abstains`] — G3 composite-type
//!   negative case.
//! - [`wrap_of_epsilon_three_branch_abstains`] — G3 shape guard.
//! - [`concat_literals_fuses_adjacent_run`] — G4 fuses a run.
//! - [`concat_literals_leaves_non_adjacent_alone`] — G4 negative case.
//! - [`concat_literals_fuses_multi_run`] — G4 multi-run fusion.

use rustc_hash::FxHashMap;

use bbnf_ir::egraph::SharedStrings;
use bbnf_ir::egraph::node::GrammarENode;
use bbnf_ir::egraph::rules::{AltOfSingle, ConcatLiterals, RepeatOfSingle, WrapOfEpsilonScalar};
use bbnf_ir::{GrammarIR, RuleId, TypeDesc};
use egraph::{EGraph, NoAnalysis, Rewrite};

// ── G1 ───────────────────────────────────────────────────────────────────────

#[test]
fn alt_of_single_collapses_to_inner() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let x = eg.add(GrammarENode::Literal(0));
    let alt = eg.add(GrammarENode::Alt(Box::new([x]), None));
    eg.rebuild();

    let rule = AltOfSingle;
    let matches = rule.search(&eg);
    assert_eq!(matches.len(), 1, "G1 expected to match single-branch Alt");
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(x),
        "G1: Alt([x]) class should unify with x after rewrite"
    );
}

#[test]
fn alt_of_single_abstains_on_multi_branch() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let x = eg.add(GrammarENode::Literal(0));
    let y = eg.add(GrammarENode::Literal(1));
    let _alt = eg.add(GrammarENode::Alt(Box::new([x, y]), None));
    eg.rebuild();

    let matches = AltOfSingle.search(&eg);
    assert!(
        matches.is_empty(),
        "G1 should ignore multi-branch Alt, got {} matches",
        matches.len()
    );
}

// ── G2 ───────────────────────────────────────────────────────────────────────

#[test]
fn repeat_of_single_collapses_to_inner() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let inner = eg.add(GrammarENode::Literal(0));
    let rep = eg.add(GrammarENode::Repeat {
        inner,
        lo: 1,
        hi: 1,
    });
    eg.rebuild();

    let rule = RepeatOfSingle;
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G2 expected to match Repeat {{lo:1,hi:1}}"
    );
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(rep),
        eg.find_ref(inner),
        "G2: Repeat {{1,1}}(x) class should unify with x after rewrite"
    );
}

#[test]
fn repeat_of_single_abstains_on_bounds() {
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let inner = eg.add(GrammarENode::Literal(0));
    let _rep_opt = eg.add(GrammarENode::Repeat {
        inner,
        lo: 0,
        hi: 1,
    });
    let _rep_star = eg.add(GrammarENode::Repeat {
        inner,
        lo: 0,
        hi: u32::MAX,
    });
    let _rep_plus = eg.add(GrammarENode::Repeat {
        inner,
        lo: 1,
        hi: u32::MAX,
    });
    eg.rebuild();

    let matches = RepeatOfSingle.search(&eg);
    assert!(
        matches.is_empty(),
        "G2 should ignore Repeat with bounds other than (1,1), got {} matches",
        matches.len()
    );
}

// ── G3 ───────────────────────────────────────────────────────────────────────

#[test]
fn wrap_of_epsilon_scalar_collapses_to_leaf() {
    // Alt([Literal("a"), Epsilon]) → Literal("a").
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let leaf = eg.add(GrammarENode::Literal(0));
    let eps = eg.add(GrammarENode::Epsilon);
    let alt = eg.add(GrammarENode::Alt(Box::new([leaf, eps]), None));
    eg.rebuild();

    // Literal branches are scalar by construction — empty TypeDesc
    // table suffices.
    let rule = WrapOfEpsilonScalar::new(FxHashMap::default());
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G3 expected to match Alt([leaf, ε]) with scalar leaf"
    );
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(leaf),
        "G3: Alt([leaf, ε]) class should unify with leaf"
    );
}

#[test]
fn wrap_of_epsilon_with_ref_scalar_collapses() {
    // Alt([Ref(rule_id), Epsilon]) with rule_id → TypeDesc::F64.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let rid: RuleId = 42;
    let leaf = eg.add(GrammarENode::Ref(rid));
    let eps = eg.add(GrammarENode::Epsilon);
    let alt = eg.add(GrammarENode::Alt(Box::new([leaf, eps]), None));
    eg.rebuild();

    let mut rule_types = FxHashMap::default();
    rule_types.insert(rid, TypeDesc::F64);
    let rule = WrapOfEpsilonScalar::new(rule_types);
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G3 expected to match Alt([Ref, ε]) with scalar-typed target"
    );
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();
    assert_eq!(
        eg.find_ref(alt),
        eg.find_ref(leaf),
        "G3: Alt([scalar Ref, ε]) class should unify with the Ref branch"
    );
}

#[test]
fn wrap_of_epsilon_non_scalar_abstains() {
    // Alt([Ref(rule_id), Epsilon]) with rule_id → TypeDesc::Vec(Span)
    // (composite, not scalar). G3 must abstain.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let rid: RuleId = 42;
    let _leaf = eg.add(GrammarENode::Ref(rid));
    let _eps = eg.add(GrammarENode::Epsilon);
    let _alt = eg.add(GrammarENode::Alt(Box::new([_leaf, _eps]), None));
    eg.rebuild();

    let mut rule_types = FxHashMap::default();
    rule_types.insert(rid, TypeDesc::Vec(Box::new(TypeDesc::Span)));
    let rule = WrapOfEpsilonScalar::new(rule_types);
    let matches = rule.search(&eg);
    assert!(
        matches.is_empty(),
        "G3 should abstain on non-scalar projection, got {} matches",
        matches.len()
    );
}

#[test]
fn wrap_of_epsilon_three_branch_abstains() {
    // Alt with 3 branches — G3 only fires on exactly 2.
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let eps = eg.add(GrammarENode::Epsilon);
    let _alt = eg.add(GrammarENode::Alt(Box::new([a, b, eps]), None));
    eg.rebuild();

    let matches = WrapOfEpsilonScalar::new(FxHashMap::default()).search(&eg);
    assert!(
        matches.is_empty(),
        "G3 should abstain on 3-branch Alt, got {} matches",
        matches.len()
    );
}

// ── G4 ───────────────────────────────────────────────────────────────────────

fn make_pool() -> (GrammarIR, SharedStrings) {
    let ir = GrammarIR {
        entry: 0,
        rules: vec![],
        strings: vec!["a".to_string(), "b".to_string(), "c".to_string()],
        fns: vec![],
        types: vec![],
        follow_sets: std::collections::HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
        type_map: None,
        pattern_annotations: std::collections::HashMap::new(),
        regex_info: std::collections::HashMap::new(),
        node_facts: std::collections::HashMap::new(),
        recognizer_decisions: std::collections::HashMap::new(),
        delim_scan_configs: std::collections::HashMap::new(),
        key_dispatch_configs: std::collections::HashMap::new(),
        context_facts: std::collections::HashMap::new(),
        has_family_recognizers: false,
        regex_engine_decisions: std::collections::HashMap::new(),
        dag: None,
        cost_config: bbnf_ir::CostConfig::default(),
        type_desc_interner: bbnf_ir::TypeDescInterner::new(),
        materialization: std::collections::HashMap::new(),
        payload_layouts: std::collections::HashMap::new(),
        string_index: Default::default(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),
        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
    };
    let pool = SharedStrings::from_ir(&ir);
    (ir, pool)
}

#[test]
fn concat_literals_fuses_adjacent_run() {
    // Seq([Lit("a"), Lit("b"), Lit("c")]) → one fused Literal("abc").
    let (_ir, pool) = make_pool();
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let c = eg.add(GrammarENode::Literal(2));
    let seq = eg.add(GrammarENode::Seq(Box::new([a, b, c])));
    eg.rebuild();

    let rule = ConcatLiterals::new(pool.clone());
    let matches = rule.search(&eg);
    assert_eq!(matches.len(), 1, "G4 expected to match 3-literal Seq");
    for (class_id, m) in matches {
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    // The Seq class should now contain a Literal node whose StringId
    // resolves to "abc". Search for it.
    let seq_class = eg.class(eg.find_ref(seq));
    let fused_sid = seq_class.iter().find_map(|n| match n {
        GrammarENode::Literal(sid) => Some(*sid),
        _ => None,
    });
    assert!(
        fused_sid.is_some(),
        "G4: Seq class should contain the fused Literal after rewrite; got {:?}",
        seq_class.iter().collect::<Vec<_>>()
    );
    assert_eq!(
        pool.get(fused_sid.unwrap()),
        "abc",
        "G4: fused literal should concatenate children's strings"
    );
}

#[test]
fn concat_literals_leaves_non_adjacent_alone() {
    // Seq([Lit("a"), Ref, Lit("b")]) — no adjacent run; G4 abstains.
    let (_ir, pool) = make_pool();
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let a = eg.add(GrammarENode::Literal(0));
    let ref_node = eg.add(GrammarENode::Ref(99));
    let b = eg.add(GrammarENode::Literal(1));
    let _seq = eg.add(GrammarENode::Seq(Box::new([a, ref_node, b])));
    eg.rebuild();

    let matches = ConcatLiterals::new(pool).search(&eg);
    assert!(
        matches.is_empty(),
        "G4 should abstain on non-adjacent literals, got {} matches",
        matches.len()
    );
}

#[test]
fn concat_literals_fuses_multi_run() {
    // Seq([Lit("a"), Lit("b"), Ref, Lit("a"), Lit("c")]) — two runs
    // of length 2 each. Both should fuse in one apply.
    let (_ir, pool) = make_pool();
    let mut eg: EGraph<GrammarENode, NoAnalysis> = EGraph::new();
    let a = eg.add(GrammarENode::Literal(0));
    let b = eg.add(GrammarENode::Literal(1));
    let ref_node = eg.add(GrammarENode::Ref(99));
    let a2 = eg.add(GrammarENode::Literal(0));
    let c = eg.add(GrammarENode::Literal(2));
    let seq = eg.add(GrammarENode::Seq(Box::new([a, b, ref_node, a2, c])));
    eg.rebuild();

    let rule = ConcatLiterals::new(pool.clone());
    let matches = rule.search(&eg);
    assert_eq!(
        matches.len(),
        1,
        "G4 expected one match with two internal runs"
    );
    for (class_id, m) in matches {
        assert_eq!(m.runs.len(), 2, "G4 match should carry both literal runs");
        rule.apply(&mut eg, class_id, m);
    }
    eg.rebuild();

    // The Seq class should now contain a Seq node of length 3:
    // [Literal("ab"), Ref, Literal("ac")].
    let seq_class = eg.class(eg.find_ref(seq));
    let fused_seq = seq_class.iter().find_map(|n| match n {
        GrammarENode::Seq(children) if children.len() == 3 => Some(children.clone()),
        _ => None,
    });
    assert!(
        fused_seq.is_some(),
        "G4: Seq class should contain a 3-child rewritten form; got {:?}",
        seq_class.iter().collect::<Vec<_>>()
    );
}
