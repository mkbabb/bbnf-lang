//! AZ-IV.W2.2 — golden tests for the inline-trace sidecar.
//!
//! Verifies that the recording wrappers around `inline_acyclic` and
//! `fuse_single_use` produce a deterministic
//! [`bbnf_ir::passes::InlineTrace`] for fixture grammars where the
//! substitution graph is known. The W2.2 sub-gate demands the trace be
//! a deterministic side table; these tests are the gate evidence.

use std::collections::HashMap;

use bbnf_ir::passes::inline_trace::{InlinePass, InlineTrace};
use bbnf_ir::passes::{fuse_single_use_with_trace, inline_acyclic_with_trace};
use bbnf_ir::{
    CostConfig, GrammarIR, IrNode, IrRule, RuleId, RuleMeta, StructRegistry, TypeDescInterner,
};

// ── Fixture infrastructure (mirrors `passes_inline.rs`) ──────────────

fn make_ir(rules: Vec<IrRule>, entry: RuleId, strings: Vec<String>) -> GrammarIR {
    GrammarIR {
        rules,
        entry,
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
        cost_config: CostConfig::default(),
        type_desc_interner: TypeDescInterner::new(),
        materialization: HashMap::new(),
        string_index: HashMap::new(),
        payload_layouts: HashMap::new(),
        structural_alphabet: None,
        push_fingerprint: None,
        dedup_eligible_rules: Vec::new(),
        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
    }
}

// ── Tests ────────────────────────────────────────────────────────────

/// Single-use fuse on a two-rule grammar: `entry = small`,
/// `small = "x"`. The fuse pass inlines `small`'s body into `entry`;
/// the trace records one event with `small` → `entry`.
#[test]
fn fuse_single_use_records_one_substitution() {
    let strings: Vec<String> = vec!["entry".into(), "small".into()];
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(0),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        0,
        strings,
    );

    let mut trace = InlineTrace::new();
    fuse_single_use_with_trace(&mut ir, &mut trace);

    assert_eq!(trace.len(), 1, "expected one substitution event");
    let event = &trace.events[0];
    assert_eq!(event.source_rule_id, 1);
    assert_eq!(event.source_rule_name, "small");
    assert_eq!(event.absorber_rule_id, 0);
    assert_eq!(event.absorber_rule_name, "entry");
    assert_eq!(event.pass, InlinePass::FuseSingleUse);
    // Sanity: rule 0 absorbed rule 1's body.
    assert_eq!(ir.rules[0].body, IrNode::Literal(0));
}

/// `inline_acyclic` records substitutions when small acyclic rules
/// inline. Two callers reference the same small rule; the trace
/// records one event per (source, absorber) pair (i.e. two events
/// total — one for each absorber).
#[test]
fn inline_acyclic_records_per_absorber() {
    let strings: Vec<String> = vec![
        "entry".into(),
        "left".into(),
        "right".into(),
        "small".into(),
    ];
    let mut ir = make_ir(
        vec![
            // entry = left | right ; (Alt with two non-bare-Ref branches keeps
            // the inline machinery active).
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Seq(vec![IrNode::Ref(1), IrNode::Ref(2)]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            // left = small ; (single-Ref body — but Ref's small enough to inline)
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Seq(vec![IrNode::Ref(3), IrNode::Literal(0)]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            // right = small ;
            IrRule {
                id: 2,
                name: 2,
                body: IrNode::Seq(vec![IrNode::Ref(3), IrNode::Literal(1)]),
                meta: RuleMeta::default(),
                source_span: None,
            },
            // small = "x"  -- 1 node, inlinable
            IrRule {
                id: 3,
                name: 3,
                body: IrNode::Literal(0),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        0,
        strings,
    );

    let mut trace = InlineTrace::new();
    inline_acyclic_with_trace(&mut ir, &mut trace);

    // Two events: one for left → small absorbed, one for right →
    // small absorbed. (entry's Seq doesn't directly reference small.)
    let small_events = trace.absorbers_for("small");
    assert_eq!(
        small_events.len(),
        2,
        "expected two absorbers for `small`, got {:?}",
        trace.events
    );

    let absorber_ids: Vec<RuleId> = small_events.iter().map(|e| e.absorber_rule_id).collect();
    assert!(absorber_ids.contains(&1), "missing absorber=left (id=1)");
    assert!(absorber_ids.contains(&2), "missing absorber=right (id=2)");
    for event in &small_events {
        assert_eq!(event.pass, InlinePass::InlineAcyclic);
        assert_eq!(event.source_rule_id, 3);
    }
}

/// Determinism: the same fixture run twice produces a byte-identical
/// trace. The W2.2 golden test gate consumes this property.
#[test]
fn trace_is_deterministic_across_runs() {
    let make = || {
        let strings: Vec<String> = vec!["entry".into(), "small".into()];
        make_ir(
            vec![
                IrRule {
                    id: 0,
                    name: 0,
                    body: IrNode::Ref(1),
                    meta: RuleMeta::default(),
                    source_span: None,
                },
                IrRule {
                    id: 1,
                    name: 1,
                    body: IrNode::Literal(0),
                    meta: RuleMeta::default(),
                    source_span: None,
                },
            ],
            0,
            strings,
        )
    };

    let mut ir_a = make();
    let mut ir_b = make();

    let mut trace_a = InlineTrace::new();
    let mut trace_b = InlineTrace::new();
    fuse_single_use_with_trace(&mut ir_a, &mut trace_a);
    fuse_single_use_with_trace(&mut ir_b, &mut trace_b);

    assert_eq!(trace_a, trace_b);
    assert_eq!(trace_a.len(), 1);
}

/// Recording wrappers preserve the bare-pass behavior. A grammar with
/// no inlinable rules produces an empty trace and an unchanged IR.
#[test]
fn no_inlinable_rules_produces_empty_trace() {
    let strings: Vec<String> = vec!["entry".into(), "cyclic".into()];
    // entry = cyclic ; cyclic = cyclic (cyclic, not inlinable)
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Ref(1),
                meta: RuleMeta {
                    is_cyclic: true,
                    scc_id: Some(0),
                    ..RuleMeta::default()
                },
                source_span: None,
            },
        ],
        0,
        strings,
    );

    let mut trace = InlineTrace::new();
    inline_acyclic_with_trace(&mut ir, &mut trace);
    fuse_single_use_with_trace(&mut ir, &mut trace);
    assert!(trace.is_empty(), "cyclic rules must not be inlined");
}

/// Trace events expose `absorbers_for` and `first_absorber_id` lookups
/// for the W2.2 `path_check` resolver to consume.
#[test]
fn lookup_helpers_resolve_to_absorber() {
    let strings: Vec<String> = vec!["entry".into(), "small".into()];
    let mut ir = make_ir(
        vec![
            IrRule {
                id: 0,
                name: 0,
                body: IrNode::Ref(1),
                meta: RuleMeta::default(),
                source_span: None,
            },
            IrRule {
                id: 1,
                name: 1,
                body: IrNode::Literal(0),
                meta: RuleMeta::default(),
                source_span: None,
            },
        ],
        0,
        strings,
    );

    let mut trace = InlineTrace::new();
    fuse_single_use_with_trace(&mut ir, &mut trace);

    assert_eq!(trace.first_absorber_id("small"), Some(0));
    assert_eq!(trace.first_absorber_id("nonexistent"), None);
    assert_eq!(trace.absorbers_for("small").len(), 1);
}
