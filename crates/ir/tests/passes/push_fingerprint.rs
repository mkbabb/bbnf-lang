//! `compute_push_fingerprint` — per-grammar push-site counts + the
//! capacity-ratio derivation consumed by the Rust emitter.
//!
//! These tests exercise both the counting logic (walks `ir.rules` +
//! `ir.materialization` + `ir.payload_layouts`) and the fingerprint →
//! `(numer, denom)` divisor function that `grammar.rs` emits into
//! `TapeBuilder::with_capacity`.

use std::collections::HashMap;

use bbnf_ir::passes::materialization::MaterializationClass;
use bbnf_ir::passes::{compute_push_fingerprint, PushFingerprint};
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleMeta, TypeDesc};

fn rule_with_body(id: u32, name: u32, body: IrNode) -> IrRule {
    IrRule {
        id,
        name,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    }
}

fn empty_ir(rules: Vec<IrRule>, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        rules,
        entry: 0,
        strings,
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: Vec::new(),
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
        materialization: HashMap::new(),
        string_index: HashMap::new(),
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
    }
}

/// Build a fresh IR, classify every rule body via `ir.materialization`,
/// then run the pass. Returns the stored fingerprint.
fn run_pass(mut ir: GrammarIR, classes: &[(u32, MaterializationClass)]) -> PushFingerprint {
    bbnf_ir::dag::ensure_dag(&mut ir);
    let dag = ir.dag.as_ref().expect("dag");
    for (rid, class) in classes {
        let rule = &ir.rules[*rid as usize];
        let nid = dag.node_for(&rule.body).expect("rule body in dag");
        ir.materialization.insert(nid, *class);
    }
    compute_push_fingerprint(&mut ir);
    ir.push_fingerprint.expect("pass populates the fingerprint")
}

#[test]
fn empty_grammar_falls_back_to_default() {
    // Degenerate shape: zero push sites. The fingerprint records
    // (0, 0, 0) and the capacity ratio falls back to the
    // historical JSON-calibrated divisor (1/2).
    let mut ir = empty_ir(vec![], vec!["entry".into()]);
    compute_push_fingerprint(&mut ir);
    let fp = ir.push_fingerprint.expect("pass populated");
    assert_eq!(fp.total(), 0);
    assert_eq!(fp.capacity_ratio(), (1, 2));
}

#[test]
fn json_like_shape_picks_half_divisor() {
    // JSON-like fingerprint with low compound ratio: 8 compound, 1
    // leaf, 3 leaf_with (the AU pre-W2 expand-output count).
    // Ratio = 8/12 = 66.7% (≤ 70) → (1, 2). The total-rule
    // override would also catch this (12 < 25), but the ratio
    // gate fires first.
    let fp = PushFingerprint {
        compound_pushes: 8,
        leaf_pushes: 1,
        leaf_with_pushes: 3,
    };
    assert_eq!(fp.total(), 12);
    assert_eq!(fp.compound_ratio_pct(), 66);
    assert_eq!(fp.capacity_ratio(), (1, 2));
}

#[test]
fn small_grammar_overrides_high_compound_ratio() {
    // JSON's per-rule classification post-AU.1 is (10, 0, 0) —
    // 100% compound, but only 10 emitted rules. The total-rule
    // gate (< 25) demotes to JSON-like (1/2) instead of the
    // Sheets-like (1/1) the ratio alone would pick. This is the
    // signal that distinguishes JSON's small grammar from
    // Sheets's medium one.
    let fp = PushFingerprint {
        compound_pushes: 10,
        leaf_pushes: 0,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.total(), 10);
    assert_eq!(fp.compound_ratio_pct(), 100);
    assert_eq!(fp.capacity_ratio(), (1, 2));
}

#[test]
fn bbnf_like_shape_picks_five_eighths_divisor() {
    // BBNF fingerprint: 90 compound, 15 leaf, 0 leaf_with.
    // Ratio = 90/105 = 85.7% (in (70, 95) band), total = 105
    // (< 200) → (5, 8).
    let fp = PushFingerprint {
        compound_pushes: 90,
        leaf_pushes: 15,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.total(), 105);
    assert_eq!(fp.compound_ratio_pct(), 85);
    assert_eq!(fp.capacity_ratio(), (5, 8));
}

#[test]
fn sheets_like_shape_picks_unit_divisor() {
    // Sheets fingerprint: 37 compound, 0 leaf, 0 leaf_with.
    // Ratio = 100%, total = 37 (< 200) → (1, 1).
    let fp = PushFingerprint {
        compound_pushes: 37,
        leaf_pushes: 0,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.total(), 37);
    assert_eq!(fp.compound_ratio_pct(), 100);
    assert_eq!(fp.capacity_ratio(), (1, 1));
}

#[test]
fn css_like_shape_picks_unit_divisor() {
    // CSS L4 typed parser fingerprint observed under the AU.6.2
    // probe: 190 compound, 0 leaf, 0 leaf_with — every rule
    // post-prune materializes as MustTape under the current
    // classifier. Ratio = 100%, total = 190 (≥ 150) → (1, 1).
    // The CSS bucket currently shares the Sheets-like (1, 1)
    // ratio because the wider 3/2 over-allocation traded cache
    // locality for first-parse realloc avoidance and net-
    // regressed against the corpus. Reserving the bucket
    // separately keeps a dispatch site for future cost-model
    // tuning once a wider per-grammar profile corpus is in hand.
    let fp = PushFingerprint {
        compound_pushes: 190,
        leaf_pushes: 0,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.total(), 190);
    assert_eq!(fp.compound_ratio_pct(), 100);
    assert_eq!(fp.capacity_ratio(), (1, 1));
}

#[test]
fn must_tape_rule_counts_as_compound_push() {
    // Single `MustTape` rule → fingerprint (1, 0, 0).
    let ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    let fp = run_pass(ir, &[(0, MaterializationClass::MustTape)]);
    assert_eq!(fp.compound_pushes, 1);
    assert_eq!(fp.leaf_pushes, 0);
    assert_eq!(fp.leaf_with_pushes, 0);
}

#[test]
fn tape_span_only_rule_without_payload_counts_as_leaf() {
    // `TapeSpanOnly` with no scalar type + no aggregate layout →
    // fingerprint (0, 1, 0).
    let ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    let fp = run_pass(ir, &[(0, MaterializationClass::TapeSpanOnly)]);
    assert_eq!(fp.compound_pushes, 0);
    assert_eq!(fp.leaf_pushes, 1);
    assert_eq!(fp.leaf_with_pushes, 0);
}

#[test]
fn tape_span_only_rule_with_scalar_type_counts_as_leaf_with() {
    // `TapeSpanOnly` whose TypeDesc is a scalar payload (F64) →
    // fingerprint (0, 0, 1). Mirrors the emitter's gate for
    // `push_leaf_with` + `PayloadData::WideScalar(f64::to_bits())`.
    let mut ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    ir.types.push((0, TypeDesc::F64));
    let fp = run_pass(ir, &[(0, MaterializationClass::TapeSpanOnly)]);
    assert_eq!(fp.compound_pushes, 0);
    assert_eq!(fp.leaf_pushes, 0);
    assert_eq!(fp.leaf_with_pushes, 1);
}

#[test]
fn tape_span_only_rule_with_payload_layout_counts_as_leaf_with() {
    // Aggregate payload layout promotes the rule to `leaf_with`
    // even without a scalar `TypeDesc`. Mirrors the emitter's
    // aggregate-path gate.
    use bbnf_ir::passes::plan_layout;
    let mut ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    let layout = plan_layout(&[TypeDesc::F64, TypeDesc::U8]).expect("layout");
    ir.payload_layouts.insert(0, layout);
    let fp = run_pass(ir, &[(0, MaterializationClass::TapeSpanOnly)]);
    assert_eq!(fp.compound_pushes, 0);
    assert_eq!(fp.leaf_pushes, 0);
    assert_eq!(fp.leaf_with_pushes, 1);
}

#[test]
fn transparent_elide_rule_contributes_zero() {
    // `TransparentElide` rules are inlined at every call site —
    // their body never emits a standalone push.
    let ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    let fp = run_pass(ir, &[(0, MaterializationClass::TransparentElide)]);
    assert_eq!(fp.total(), 0);
}

#[test]
fn preserve_identity_forces_compound_regardless_of_class() {
    // `preserve_identity` rules always push a compound — mirrors
    // the emitter's unconditional `MustTape` dispatch.
    let mut ir = empty_ir(
        vec![rule_with_body(0, 0, IrNode::Literal(1))],
        vec!["entry".into(), "hi".into()],
    );
    ir.rules[0].meta.preserve_identity = true;
    // Even if the materialization table insists the rule is
    // `TransparentElide`, `preserve_identity` wins and the rule
    // counts as a compound push.
    let fp = run_pass(ir, &[(0, MaterializationClass::TransparentElide)]);
    assert_eq!(fp.compound_pushes, 1);
    assert_eq!(fp.total(), 1);
}

#[test]
fn capacity_ratio_under_seventy_pct_picks_half() {
    // Compound-ratio band boundary: 70% stays in the JSON bucket.
    let fp = PushFingerprint {
        compound_pushes: 70,
        leaf_pushes: 30,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.compound_ratio_pct(), 70);
    assert_eq!(fp.capacity_ratio(), (1, 2));
}

#[test]
fn capacity_ratio_just_above_seventy_pct_picks_five_eighths() {
    // 71% ratio → BBNF bucket.
    let fp = PushFingerprint {
        compound_pushes: 71,
        leaf_pushes: 29,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.compound_ratio_pct(), 71);
    assert_eq!(fp.capacity_ratio(), (5, 8));
}

#[test]
fn capacity_ratio_total_override_triggers_at_one_fifty() {
    // A grammar with total = 150 and ratio = 80% routes through
    // the CSS-like override rather than the BBNF bucket. CSS L4's
    // typed parser sits at ~190 emitted rules post-prune which
    // lands solidly inside this band. Currently both the CSS and
    // Sheets buckets pick (1, 1); the dispatch is preserved as a
    // cost-model insertion point.
    let fp = PushFingerprint {
        compound_pushes: 120,
        leaf_pushes: 30,
        leaf_with_pushes: 0,
    };
    assert_eq!(fp.total(), 150);
    assert_eq!(fp.compound_ratio_pct(), 80);
    assert_eq!(fp.capacity_ratio(), (1, 1));
}
