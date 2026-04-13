//! Tranche AF.1 — `classify_materialization` e-graph fact gate.
//!
//! Covers the dormant-infrastructure activation: before AF.1,
//! `EClassFacts::is_fixed_shape` (and the three sibling bits
//! `elision_safe` / `closure_free` / `all_descendants_elidable`)
//! were computed in the grammar-tier e-graph monotone lattice and
//! never consumed anywhere in the pipeline. AF.1 wires them into
//! `classify_materialization` as a gate on every `TransparentElide`
//! return.
//!
//! The gate is additive:
//!
//! 1. **Agree path** — a rule with a structurally-eligible body
//!    AND all four fact bits set classifies as `TransparentElide`.
//! 2. **Disagree path** — a rule with a structurally-eligible body
//!    but a missing fact bit demotes to `TapeSpanOnly`.
//! 3. **Fallback path** — when the facts map has no entry for a
//!    `NodeId` (e.g. structural mode, isolated unit tests), the
//!    classifier reverts to pre-AF.1 structural behavior.
//!
//! Every test here puts the node under test in a non-entry rule
//! so Tranche AF.0's entry-rule pinning (which unconditionally
//! widens the entry rule's subtree to `MustTape`) does not mask
//! the gate's effect.

use std::collections::HashMap;

use bbnf_ir::passes::materialization::{
    EClassFactsMap, MaterializationClass, classify_materialization,
    classify_materialization_with_facts, compute_eclass_facts,
};
use bbnf_ir::{
    AltBranch, CostConfig, GrammarIR, IrNode, IrRule, RuleMeta, StringId, TypeDescInterner,
};
use bbnf_ir::egraph::analysis::{EClassFacts, WidthBound};
use bbnf_regex::sets::charset::CharSet128;

// ── Fixture grammar helpers ──────────────────────────────────────────

/// Build a minimal `GrammarIR` with the specified entry rule.
fn make_ir_with_entry(rules: Vec<IrRule>, strings: Vec<String>, entry: u32) -> GrammarIR {
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
        string_index: std::collections::HashMap::new(),
        payload_layouts: HashMap::new(),
        struct_registry: Default::default(),
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
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

/// Look up the class assigned to a rule's body.
fn rule_class(ir: &GrammarIR, rule_id: u32) -> MaterializationClass {
    let dag = ir.dag.as_ref().expect("dag present");
    let body = &ir.rules[rule_id as usize].body;
    let id = dag.node_for(body).expect("body interned in dag");
    *ir.materialization
        .get(&id)
        .expect("rule body classified")
}

/// Look up the `NodeId` for a rule's body.
fn rule_body_id(ir: &GrammarIR, rule_id: u32) -> bbnf_ir::dag::NodeId {
    let dag = ir.dag.as_ref().expect("dag present");
    let body = &ir.rules[rule_id as usize].body;
    dag.node_for(body).expect("body interned in dag")
}

/// A fully-agreeing fact record — matches `EClassFacts::epsilon()`
/// exactly, every lattice bit set to the permissive value.
fn all_bits_agree() -> EClassFacts {
    EClassFacts {
        first_set: CharSet128::new(),
        nullable: true,
        width: WidthBound { min: 0, max: Some(0) },
        literal_sid: None,
        regex_sid: None,
        elision_safe: true,
        closure_free: true,
        is_fixed_shape: true,
        all_descendants_elidable: true,
    }
}

/// A fact record with every permissive bit set EXCEPT
/// `is_fixed_shape`. Mirrors the state of a class that saw an `Alt`
/// or a non-fixed `Repeat` — structurally-elidable but shape-
/// variable, which the AF.1 gate must demote.
fn fixed_shape_false_only() -> EClassFacts {
    EClassFacts {
        first_set: CharSet128::new(),
        nullable: true,
        width: WidthBound { min: 0, max: Some(0) },
        literal_sid: None,
        regex_sid: None,
        elision_safe: true,
        closure_free: true,
        is_fixed_shape: false,
        all_descendants_elidable: true,
    }
}

// ── Scalar payload tier — facts agree path ──────────────────────────

/// A rule whose body is structurally `TransparentElide`-eligible
/// (Epsilon) AND whose pre-computed facts agree on all four bits
/// classifies as `TransparentElide`.
#[test]
fn structural_and_facts_agree_yields_transparent_elide() {
    let strings = vec![
        "test".to_string(),  // rule 0
        "entry".to_string(), // rule 1 — pinned by pin_sweep
        "x".to_string(),
    ];
    // Two rules: rule 0 (Epsilon body, tested) + rule 1 (Literal body,
    // entry rule). rule 0 is NOT referenced from rule 1, so pin_sweep
    // leaves it alone.
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    // Happy path: compute facts via the live helper. Since the body is
    // Epsilon, `compute_eclass_facts` produces `EClassFacts::epsilon()`
    // — every bit true — and the gate agrees with the structural
    // `TransparentElide` decision.
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TransparentElide);
}

// ── Scalar payload tier — facts disagree path ───────────────────────

/// A rule whose body is structurally `TransparentElide`-eligible
/// but whose facts map says `is_fixed_shape == false` demotes to
/// `TapeSpanOnly`. Exercises the gate's primary purpose: preventing
/// structurally-eligible classification when the e-graph monotone
/// lattice disagrees.
#[test]
fn is_fixed_shape_false_demotes_transparent_elide_to_tape_span_only() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    // rule 0 body is Epsilon — structurally TransparentElide.
    // rule 1 body is Literal — entry rule, isolates the test.
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    // Manually build a fact map that DISAGREES on is_fixed_shape for
    // rule 0's body. Every other bit is permissive — the only demotion
    // signal is the missing fixed-shape guarantee.
    let rule0_body_id = rule_body_id(&ir, 0);
    let mut facts: EClassFactsMap = HashMap::new();
    facts.insert(rule0_body_id, fixed_shape_false_only());

    classify_materialization_with_facts(&mut ir, &facts);

    // The gate saw `is_fixed_shape == false` → demoted one rank up
    // the lattice to `TapeSpanOnly` instead of the structural
    // `TransparentElide`.
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

/// All four bits — not just `is_fixed_shape` — gate the result. A
/// fact record with `elision_safe == false` also demotes.
#[test]
fn elision_safe_false_demotes_transparent_elide() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    let rule0_body_id = rule_body_id(&ir, 0);
    let mut disagreeing = all_bits_agree();
    disagreeing.elision_safe = false;
    let mut facts: EClassFactsMap = HashMap::new();
    facts.insert(rule0_body_id, disagreeing);

    classify_materialization_with_facts(&mut ir, &facts);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

/// `closure_free == false` also demotes.
#[test]
fn closure_free_false_demotes_transparent_elide() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    let rule0_body_id = rule_body_id(&ir, 0);
    let mut disagreeing = all_bits_agree();
    disagreeing.closure_free = false;
    let mut facts: EClassFactsMap = HashMap::new();
    facts.insert(rule0_body_id, disagreeing);

    classify_materialization_with_facts(&mut ir, &facts);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

/// `all_descendants_elidable == false` also demotes.
#[test]
fn all_descendants_elidable_false_demotes_transparent_elide() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    let rule0_body_id = rule_body_id(&ir, 0);
    let mut disagreeing = all_bits_agree();
    disagreeing.all_descendants_elidable = false;
    let mut facts: EClassFactsMap = HashMap::new();
    facts.insert(rule0_body_id, disagreeing);

    classify_materialization_with_facts(&mut ir, &facts);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

// ── Fallback path (no facts entry) ───────────────────────────────────

/// When the facts map has no entry for the rule body's `NodeId`,
/// the gate is a no-op and the classifier returns the pre-AF.1
/// structural result. This is the load-bearing invariant for
/// structural mode — the pipeline gates `egraph_build_saturate_writeback`
/// out in structural mode, so no facts get computed, and the
/// classifier must still produce a usable classification.
#[test]
fn empty_facts_map_falls_back_to_structural_transparent_elide() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    // Empty facts map → fallback. The structural Epsilon path still
    // returns TransparentElide because the gate skips nodes that lack
    // an entry.
    let facts: EClassFactsMap = HashMap::new();
    classify_materialization_with_facts(&mut ir, &facts);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TransparentElide);
}

/// A partial facts map — populated for other nodes but missing the
/// rule body's own `NodeId` — also takes the fallback path. The
/// gate resolves `facts.get(&id)` per-`NodeId`, so missing entries
/// are independent.
#[test]
fn partial_facts_map_falls_back_per_node() {
    let strings = vec![
        "test".to_string(),
        "entry".to_string(),
        "x".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Epsilon),
        rule(1, 1, IrNode::Literal(2)),
    ];
    let mut ir = make_ir_with_entry(rules, strings, 1);

    // Populate facts for a DIFFERENT node (rule 1's body), leaving
    // rule 0's body absent. The gate must fall back for rule 0 even
    // though the map is non-empty overall.
    let rule1_body_id = rule_body_id(&ir, 1);
    let mut facts: EClassFactsMap = HashMap::new();
    facts.insert(rule1_body_id, fixed_shape_false_only());

    classify_materialization_with_facts(&mut ir, &facts);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TransparentElide);
}

// ── compute_eclass_facts — the bottom-up lattice walk ────────────────

/// The live fact computation produces `EClassFacts::epsilon()`-shaped
/// facts for an Epsilon body (all four permissive bits set).
#[test]
fn compute_eclass_facts_epsilon_all_bits_permissive() {
    let strings = vec!["entry".to_string()];
    let rules = vec![rule(0, 0, IrNode::Epsilon)];
    let ir = make_ir_with_entry(rules, strings, 0);

    let facts = compute_eclass_facts(&ir);
    let body_id = rule_body_id(&ir, 0);
    let fact = facts.get(&body_id).expect("epsilon body in facts");
    assert!(fact.is_fixed_shape);
    assert!(fact.elision_safe);
    assert!(fact.closure_free);
    assert!(fact.all_descendants_elidable);
}

/// A Literal body produces `EClassFacts::literal(sid)` — fixed
/// shape, elision-safe, closure-free. Descendants trivially elidable
/// since there are none.
#[test]
fn compute_eclass_facts_literal_all_bits_permissive() {
    let strings = vec!["entry".to_string(), "hi".to_string()];
    let rules = vec![rule(0, 0, IrNode::Literal(1))];
    let ir = make_ir_with_entry(rules, strings, 0);

    let facts = compute_eclass_facts(&ir);
    let body_id = rule_body_id(&ir, 0);
    let fact = facts.get(&body_id).expect("literal body in facts");
    assert!(fact.is_fixed_shape);
    assert!(fact.elision_safe);
    assert!(fact.closure_free);
    assert!(fact.all_descendants_elidable);
    assert_eq!(fact.literal_sid, Some(1));
}

/// An Alt body produces facts with `is_fixed_shape == false` —
/// the AF.1 gate would correctly refuse to call the Alt
/// `TransparentElide` even if the structural classifier wanted to
/// (it never does; Alt unconditionally returns `MustTape`). This is
/// a sanity check that the fact computation tracks the lattice
/// correctly.
#[test]
fn compute_eclass_facts_alt_is_not_fixed_shape() {
    let strings = vec![
        "entry".to_string(),
        "a".to_string(),
        "b".to_string(),
    ];
    let rules = vec![rule(
        0,
        0,
        IrNode::Alt(
            vec![
                AltBranch { node: IrNode::Literal(1), first_set: None },
                AltBranch { node: IrNode::Literal(2), first_set: None },
            ],
            None,
        ),
    )];
    let ir = make_ir_with_entry(rules, strings, 0);

    let facts = compute_eclass_facts(&ir);
    let body_id = rule_body_id(&ir, 0);
    let fact = facts.get(&body_id).expect("alt body in facts");
    assert!(!fact.is_fixed_shape, "Alt is never fixed-shape");
    // Children are both literal leaves → all_descendants_elidable
    // carries through via the `&&` join.
    assert!(fact.all_descendants_elidable);
}

/// A `Seq` of literals has `is_fixed_shape == true` (every child
/// is fixed), `all_descendants_elidable == true`, and
/// `elision_safe == false` (the Seq itself is not a wrapper). The
/// AF.1 gate would still prevent `TransparentElide` because
/// `elision_safe` is false; the structural classifier also refuses
/// (Seq of literals collapses to `TapeSpanOnly`, not
/// `TransparentElide`). Sanity check on the fact propagation.
#[test]
fn compute_eclass_facts_seq_of_literals_tracks_lattice() {
    let strings = vec![
        "entry".to_string(),
        "hello".to_string(),
        " ".to_string(),
        "world".to_string(),
    ];
    let rules = vec![rule(
        0,
        0,
        IrNode::Seq(vec![
            IrNode::Literal(1),
            IrNode::Literal(2),
            IrNode::Literal(3),
        ]),
    )];
    let ir = make_ir_with_entry(rules, strings, 0);

    let facts = compute_eclass_facts(&ir);
    let body_id = rule_body_id(&ir, 0);
    let fact = facts.get(&body_id).expect("seq body in facts");
    assert!(fact.is_fixed_shape);
    assert!(!fact.elision_safe, "Seq itself is not elision-safe");
    assert!(fact.all_descendants_elidable);
    assert!(fact.closure_free);
}
