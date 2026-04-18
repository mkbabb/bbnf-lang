//! Tranche AB.0 — `MaterializationClass` lattice + `classify_materialization`
//! pass.
//!
//! Covers:
//!
//! 1. Lattice invariants — `mat_join` is idempotent, commutative,
//!    and monotone.
//! 2. Per-variant ordering — `TransparentElide < TapeSpanOnly <
//!    MustTape`.
//! 3. `classify_materialization` on fixture grammars:
//!    - Literal leaf → `TapeSpanOnly`
//!    - Epsilon → `TransparentElide`
//!    - Alt → `MustTape` regardless of branches
//!    - Transparent alias ref → `TransparentElide`
//!    - `@pretty`-pinned subtree → every descendant `MustTape`
//!    - `@debug`-pinned rule → `MustTape`
//! 4. Debug assertion sweep — every rule body has a class.

use std::collections::HashMap;

use bbnf_ir::passes::materialization::{
    classify_materialization, mat_join, MaterializationClass,
};
use bbnf_ir::{
    AltBranch, CostConfig, GrammarIR, IrNode, IrRule, PrettyHints, RuleDirectives, RuleMeta,
    StringId, TypeDescInterner,
};

// ── Lattice invariants ────────────────────────────────────────────────

#[test]
fn mat_join_idempotent() {
    for c in [
        MaterializationClass::MustTape,
        MaterializationClass::TapeSpanOnly,
        MaterializationClass::TransparentElide,
    ] {
        assert_eq!(mat_join(c, c), c, "mat_join({:?}, {:?})", c, c);
    }
}

#[test]
fn mat_join_commutative() {
    let values = [
        MaterializationClass::MustTape,
        MaterializationClass::TapeSpanOnly,
        MaterializationClass::TransparentElide,
    ];
    for a in values {
        for b in values {
            assert_eq!(
                mat_join(a, b),
                mat_join(b, a),
                "mat_join({:?}, {:?}) vs reversed",
                a,
                b
            );
        }
    }
}

#[test]
fn mat_join_widens_toward_must_tape() {
    // TransparentElide join anything → at least that thing.
    assert_eq!(
        mat_join(
            MaterializationClass::TransparentElide,
            MaterializationClass::TapeSpanOnly
        ),
        MaterializationClass::TapeSpanOnly
    );
    assert_eq!(
        mat_join(
            MaterializationClass::TransparentElide,
            MaterializationClass::MustTape
        ),
        MaterializationClass::MustTape
    );
    // TapeSpanOnly join MustTape → MustTape.
    assert_eq!(
        mat_join(
            MaterializationClass::TapeSpanOnly,
            MaterializationClass::MustTape
        ),
        MaterializationClass::MustTape
    );
}

#[test]
fn emits_record_matches_variants() {
    assert!(MaterializationClass::MustTape.emits_record());
    assert!(MaterializationClass::TapeSpanOnly.emits_record());
    assert!(!MaterializationClass::TransparentElide.emits_record());
}

#[test]
fn top_and_bottom() {
    assert_eq!(MaterializationClass::top(), MaterializationClass::MustTape);
    assert_eq!(
        MaterializationClass::bottom(),
        MaterializationClass::TransparentElide
    );
}

// ── Fixture grammar helpers ───────────────────────────────────────────

/// Build a minimal `GrammarIR` from a handwritten rule list. The
/// string table is pre-populated with rule names so the caller can
/// refer to them by `StringId`.
///
/// `ir.entry` is set to `u32::MAX` — a sentinel that does not match
/// any rule's id — so the AF.0 "entry rule always MustTape" pin does
/// not fire for any of the rules under test. Tests that exercise
/// entry-rule semantics explicitly should override `ir.entry` after
/// calling `make_ir`.
fn make_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
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
        string_index: std::collections::HashMap::new(),
        payload_layouts: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
            dedup_eligible_rules: Vec::new(),
        eclass_facts: std::collections::HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
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

fn rule_with_meta(id: u32, name: StringId, body: IrNode, meta: RuleMeta) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta,
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

// ── Classification tests ──────────────────────────────────────────────

#[test]
fn literal_leaf_is_tape_span_only() {
    let strings = vec!["entry".to_string(), "hi".to_string()];
    let rules = vec![rule(0, 0, IrNode::Literal(1))];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn regex_leaf_is_tape_span_only() {
    let strings = vec!["entry".to_string(), r"\d+".to_string()];
    let rules = vec![rule(0, 0, IrNode::Regex(1))];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn epsilon_is_transparent_elide() {
    let strings = vec!["entry".to_string()];
    let rules = vec![rule(0, 0, IrNode::Epsilon)];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TransparentElide);
}

#[test]
fn alt_is_must_tape() {
    // alt → "a" | "b" | "c"
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
    classify_materialization(&mut ir);
    // AJ.1: Alt with all-leaf branches is TapeSpanOnly (variant_idx
    // stored in flags, no children needed).
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn seq_of_literals_collapses_to_tape_span_only() {
    // entry → "hello" " " "world"
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
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn pretty_pinned_rule_must_tape() {
    // entry → "a" | "b", but @pretty pinned.
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
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::MustTape);
    // And every descendant is pinned: the two literal branches must
    // also be MustTape (not their natural TapeSpanOnly class).
    let dag = ir.dag.as_ref().unwrap();
    let body = &ir.rules[0].body;
    if let IrNode::Alt(branches, _) = body {
        for b in branches {
            let id = dag.node_for(&b.node).expect("branch interned");
            assert_eq!(
                ir.materialization.get(&id).copied(),
                Some(MaterializationClass::MustTape),
                "pretty-pinned alt branch must be MustTape"
            );
        }
    }
}

#[test]
fn debug_pinned_rule_must_tape() {
    let strings = vec!["entry".to_string(), "tok".to_string()];
    let mut meta = RuleMeta::default();
    meta.directives = RuleDirectives {
        pretty: None,
        recover: None,
        token: false,
        debug: true,
    };
    let rules = vec![rule_with_meta(0, 0, IrNode::Literal(1), meta)];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    // The literal would normally be TapeSpanOnly; the @debug pin
    // widens to MustTape.
    assert_eq!(rule_class(&ir, 0), MaterializationClass::MustTape);
}

#[test]
fn debug_all_pins_every_rule() {
    let strings = vec![
        "entry".to_string(),
        "a".to_string(),
        "other".to_string(),
        "b".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Literal(1)),
        rule(1, 2, IrNode::Literal(3)),
    ];
    let mut ir = make_ir(rules, strings);
    ir.debug_all = true;
    classify_materialization(&mut ir);
    // @debug * ; pins every rule to MustTape regardless of their
    // natural class.
    assert_eq!(rule_class(&ir, 0), MaterializationClass::MustTape);
    assert_eq!(rule_class(&ir, 1), MaterializationClass::MustTape);
}

#[test]
fn preserve_identity_forces_must_tape() {
    let strings = vec!["entry".to_string(), "x".to_string()];
    let mut meta = RuleMeta::default();
    meta.preserve_identity = true;
    let rules = vec![rule_with_meta(0, 0, IrNode::Literal(1), meta)];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::MustTape);
}

#[test]
fn negate_is_transparent_elide() {
    let strings = vec!["entry".to_string(), "x".to_string()];
    let rules = vec![rule(
        0,
        0,
        IrNode::Negate(Box::new(IrNode::Literal(1))),
    )];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TransparentElide);
}

#[test]
fn optional_whitespace_passes_through_inner_class() {
    // ?w around a literal leaf → same class as the literal (TapeSpanOnly).
    let strings = vec!["entry".to_string(), "x".to_string()];
    let rules = vec![rule(
        0,
        0,
        IrNode::OptionalWhitespace(Box::new(IrNode::Literal(1))),
    )];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    assert_eq!(rule_class(&ir, 0), MaterializationClass::TapeSpanOnly);
}

#[test]
fn every_rule_body_has_a_class() {
    // Debug assertion sweep equivalent: check that every rule body
    // got an entry in `ir.materialization`.
    let strings = vec![
        "r0".to_string(),
        "a".to_string(),
        "r1".to_string(),
        "b".to_string(),
        "r2".to_string(),
    ];
    let rules = vec![
        rule(0, 0, IrNode::Literal(1)),
        rule(1, 2, IrNode::Literal(3)),
        rule(2, 4, IrNode::Epsilon),
    ];
    let mut ir = make_ir(rules, strings);
    classify_materialization(&mut ir);
    for rule in &ir.rules {
        let dag = ir.dag.as_ref().unwrap();
        let id = dag.node_for(&rule.body).expect("rule body interned");
        assert!(
            ir.materialization.contains_key(&id),
            "rule {} body missing from materialization map",
            ir.get_string(rule.name)
        );
    }
}
