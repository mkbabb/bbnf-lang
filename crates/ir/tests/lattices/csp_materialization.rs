//! CSP joint strategy + materialization solve.
//!
//! Covers:
//!
//! 1. Materialization cost weights are read from `CostConfig`.
//! 2. `solve_grammar_components` returns a per-rule materialization
//!    map alongside the recognizer decisions.
//! 3. Prettify-pinned rules stay `MustTape` after the CSP refinement
//!    (single-value domain clamp).
//! 4. Debug-pinned rules stay `MustTape`.
//! 5. `preserve_identity` rules stay `MustTape`.
//! 6. Non-pinned literal rules pick `TapeSpanOnly` (cheapest legal
//!    class in a lattice of {MustTape, TapeSpanOnly}).
//! 7. Non-pinned transparent-compatible rules pick
//!    `TransparentElide` (cheapest legal class in a lattice of
//!    {MustTape, TapeSpanOnly, TransparentElide}).
//! 8. `debug_all = true` pins every rule in the grammar.

use std::collections::HashMap;

use bbnf_ir::passes::materialization::{classify_materialization, MaterializationClass};
use bbnf_ir::passes::solve_grammar_components;
use bbnf_ir::{
    AltBranch, CostConfig, GrammarIR, IrNode, IrRule, PrettyHints, RuleDirectives, RuleMeta,
    StringId, TypeDescInterner,
};

fn make_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
    // Sentinel entry: `u32::MAX` does not match any rule id, so the
    // AF.0 "entry rule always MustTape" pin does not fire. Tests that
    // need entry-rule semantics should override `ir.entry` directly.
    let entry = u32::MAX;
    let mut ir = GrammarIR {
        rules,
        entry,
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
        cost_config: CostConfig::default(),
        type_desc_interner: TypeDescInterner::new(),
        materialization: HashMap::new(),
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
    // Run the AB.0 classification first — the CSP consumes its
    // output as the domain prefilter.
    classify_materialization(&mut ir);
    ir
}

fn rule(id: u32, name: StringId, body: IrNode) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn rule_with_meta(id: u32, name: StringId, body: IrNode, meta: RuleMeta) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta,
        source_span: None,
    }
}

/// Run the joint CSP on `ir` and merge the result into
/// `ir.materialization`. Returns the rule body's post-CSP class.
fn solve_and_merge(ir: &mut GrammarIR) {
    let (_decisions, mat_refined) = solve_grammar_components(ir);
    for (node_id, class) in mat_refined {
        ir.materialization.insert(node_id, class);
    }
}

fn body_class(ir: &GrammarIR, rule_id: u32) -> MaterializationClass {
    let dag = ir.dag.as_ref().unwrap();
    let body = &ir.rules[rule_id as usize].body;
    let id = dag.node_for(body).unwrap();
    *ir.materialization.get(&id).expect("body classified")
}

// ── Cost weight plumbing ──────────────────────────────────────────────

#[test]
fn cost_weights_default_shape() {
    let cfg = CostConfig::default();
    assert!(cfg.mat_must_tape > cfg.mat_tape_span_only);
    assert!(cfg.mat_tape_span_only > cfg.mat_transparent_elide);
    assert!(cfg.mat_prettify_pin_penalty > cfg.mat_must_tape * 1000.0);
    assert!(cfg.mat_debug_pin_penalty > cfg.mat_must_tape * 1000.0);
}

// ── Post-CSP classification ──────────────────────────────────────────

#[test]
fn literal_rule_stays_tape_span_only_after_csp() {
    let strings = vec!["entry".to_string(), "hi".to_string()];
    let rules = vec![rule(0, 0, IrNode::Literal(1))];
    let mut ir = make_ir(rules, strings);
    // After AB.0 classification, the literal rule is TapeSpanOnly.
    assert_eq!(body_class(&ir, 0), MaterializationClass::TapeSpanOnly);
    // The CSP should keep it there (cheapest legal class).
    solve_and_merge(&mut ir);
    assert_eq!(body_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn epsilon_rule_stays_transparent_elide_after_csp() {
    let strings = vec!["entry".to_string()];
    let rules = vec![rule(0, 0, IrNode::Epsilon)];
    let mut ir = make_ir(rules, strings);
    assert_eq!(body_class(&ir, 0), MaterializationClass::TransparentElide);
    solve_and_merge(&mut ir);
    // TransparentElide is the cheapest possible — CSP keeps it.
    assert_eq!(body_class(&ir, 0), MaterializationClass::TransparentElide);
}

#[test]
fn pretty_pinned_rule_stays_must_tape_after_csp() {
    let strings = vec!["entry".to_string(), "a".to_string(), "b".to_string()];
    let mut meta = RuleMeta::default();
    meta.directives = RuleDirectives {
        pretty: Some(PrettyHints::default()),
        recover: None,
        token: false,
        debug: false,
    };
    let body = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Literal(1), first_set: None },
            AltBranch { node: IrNode::Literal(2), first_set: None },
        ],
        None,
    );
    let rules = vec![rule_with_meta(0, 0, body, meta)];
    let mut ir = make_ir(rules, strings);
    solve_and_merge(&mut ir);
    // Pinned by @pretty — CSP domain is clamped to {MustTape}.
    assert_eq!(body_class(&ir, 0), MaterializationClass::MustTape);
}

#[test]
fn debug_pinned_rule_stays_must_tape_after_csp() {
    let strings = vec!["entry".to_string(), "hi".to_string()];
    let mut meta = RuleMeta::default();
    meta.directives.debug = true;
    let rules = vec![rule_with_meta(0, 0, IrNode::Literal(1), meta)];
    let mut ir = make_ir(rules, strings);
    solve_and_merge(&mut ir);
    assert_eq!(body_class(&ir, 0), MaterializationClass::MustTape);
}

#[test]
fn preserve_identity_stays_must_tape_after_csp() {
    let strings = vec!["entry".to_string(), "hi".to_string()];
    let mut meta = RuleMeta::default();
    meta.preserve_identity = true;
    let rules = vec![rule_with_meta(0, 0, IrNode::Literal(1), meta)];
    let mut ir = make_ir(rules, strings);
    solve_and_merge(&mut ir);
    assert_eq!(body_class(&ir, 0), MaterializationClass::MustTape);
}

#[test]
fn debug_all_pins_every_rule_after_csp() {
    let strings = vec![
        "r0".to_string(),
        "a".to_string(),
        "r1".to_string(),
        "b".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Literal(1)),
        rule(1, 2, IrNode::Literal(3)),
    ];
    let mut ir = make_ir(rules, strings);
    ir.debug_all = true;
    // Re-run classification with debug_all set so the AB.0 sweep
    // pins every rule; otherwise the initial classification was
    // computed before we set the flag.
    ir.materialization.clear();
    classify_materialization(&mut ir);
    solve_and_merge(&mut ir);
    assert_eq!(body_class(&ir, 0), MaterializationClass::MustTape);
    assert_eq!(body_class(&ir, 1), MaterializationClass::MustTape);
}

#[test]
fn alt_rule_stays_must_tape_after_csp() {
    // Alt carries a variant discriminator — classification gives
    // MustTape and the CSP has no cheaper legal class to pick.
    let strings = vec![
        "entry".to_string(),
        "a".to_string(),
        "b".to_string(),
        "c".to_string(),
    ];
    let rules = vec![rule(
        0,
        0,
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Literal(1), first_set: None },
                AltBranch { node: IrNode::Literal(2), first_set: None },
                AltBranch { node: IrNode::Literal(3), first_set: None },
            ],
            None,
        ),
    )];
    let mut ir = make_ir(rules, strings);
    solve_and_merge(&mut ir);
    // AJ.1: Alt with all-leaf branches is TapeSpanOnly.
    assert_eq!(body_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn solve_returns_decision_map() {
    let strings = vec!["entry".to_string(), "x".to_string()];
    let rules = vec![rule(0, 0, IrNode::Literal(1))];
    let ir = make_ir(rules, strings);
    let (decisions, mat) = solve_grammar_components(&ir);
    // Materialization map has an entry for the rule body NodeId.
    assert!(!mat.is_empty(), "materialization map populated");
    // Decisions map may be empty for a trivial literal rule (no
    // Alt / Wrap / Regex variables) — the test is just that the
    // call doesn't panic and the materialization side fires.
    let _ = decisions;
}
