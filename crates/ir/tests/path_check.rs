//! AZ-IV.W2.2 — `path_check` IR pass tests.
//!
//! Verifies the resolver built by [`bbnf_ir::passes::run_path_check`]:
//!
//! - source rules whose layouts survived the pipeline bind to
//!   themselves;
//! - source rules that `inline_acyclic` / `fuse_single_use` substituted
//!   bind to the absorber's `RuleId` via the inline trace;
//! - chained substitutions (`A → B → C`) follow through to the
//!   surviving absorber `C` when `B` was itself inlined further;
//! - source rules with no surviving absorber are omitted from the
//!   resolver (the W2.4 macro turns the absence into a compile error).

use bbnf_ir::passes::inline_trace::{InlinePass, InlineSubstitution, InlineTrace};
use bbnf_ir::passes::path_check::{PathCheckResolver, build_resolver};
use bbnf_ir::registry::{LayoutKind, StructLayout, StructRegistry};
use bbnf_ir::types::{IrNode, IrRule, RuleId, RuleMeta, TypeDesc};

/// Build a minimal `(registry, rules, strings)` triple. The registry
/// holds layouts for `surviving_rules`; `inlined_rules` are rules that
/// the pipeline removed (the registry has no entry for them).
fn fixture(
    surviving_rules: &[(RuleId, &str)],
    inlined_rules: &[(RuleId, &str)],
) -> (StructRegistry, Vec<IrRule>, Vec<String>) {
    let mut registry = StructRegistry::new();
    let mut rules = Vec::new();
    let mut strings = Vec::new();

    for (_id, name) in surviving_rules.iter().chain(inlined_rules.iter()) {
        strings.push((*name).to_owned());
    }

    for (idx, (id, name)) in surviving_rules.iter().enumerate() {
        let layout = StructLayout {
            rule_id: *id,
            rule_name: (*name).to_owned(),
            kind: LayoutKind::Struct,
            rule_type: TypeDesc::Span,
            fields: Vec::new(),
        };
        registry.insert(layout);
        rules.push(IrRule {
            id: *id,
            name: idx as u32,
            body: IrNode::Epsilon,
            meta: RuleMeta::default(),
            source_span: None,
        });
    }
    // The IR also carries the inlined rules in some pipeline contexts
    // (before `prune_unreachable`); for the resolver test we omit them
    // — the prune already ran.

    (registry, rules, strings)
}

#[test]
fn surviving_rule_resolves_to_self() {
    let (registry, rules, strings) = fixture(&[(0, "entry"), (1, "value")], &[]);
    let trace = InlineTrace::new();

    let resolver = build_resolver(&registry, &trace, &rules, &strings);

    assert_eq!(resolver.resolve("entry"), Some(0));
    assert_eq!(resolver.resolve("value"), Some(1));
    assert_eq!(resolver.len(), 2);
}

#[test]
fn fused_rule_resolves_through_trace_to_absorber() {
    // `entry` survives; `inner` was fused into `entry`.
    let (registry, rules, strings) = fixture(&[(0, "entry")], &[(1, "inner")]);
    let mut trace = InlineTrace::new();
    trace.record(InlineSubstitution::new(
        1,
        "inner",
        0,
        "entry",
        InlinePass::FuseSingleUse,
    ));

    let resolver = build_resolver(&registry, &trace, &rules, &strings);

    assert_eq!(resolver.resolve("entry"), Some(0));
    // The W2 invariant 8 demands the source name still resolves —
    // through the trace, to the absorber's id.
    assert_eq!(
        resolver.resolve("inner"),
        Some(0),
        "source rule name must resolve through inline trace"
    );
}

#[test]
fn chained_substitution_resolves_to_first_surviving_absorber() {
    // `entry` survives; `mid` was inlined into `entry`; `leaf` was
    // inlined into `mid`. Only `entry`'s layout is in the registry.
    let (registry, rules, strings) = fixture(&[(0, "entry")], &[(1, "mid"), (2, "leaf")]);

    let mut trace = InlineTrace::new();
    trace.record(InlineSubstitution::new(
        2,
        "leaf",
        1,
        "mid",
        InlinePass::InlineAcyclic,
    ));
    trace.record(InlineSubstitution::new(
        1,
        "mid",
        0,
        "entry",
        InlinePass::FuseSingleUse,
    ));

    let resolver = build_resolver(&registry, &trace, &rules, &strings);

    // `mid` chains to `entry` directly.
    assert_eq!(resolver.resolve("mid"), Some(0));
    // `leaf` chains via `mid` to `entry`.
    assert_eq!(resolver.resolve("leaf"), Some(0));
}

#[test]
fn unreachable_source_rule_is_omitted() {
    // `entry` survives. `orphan` was substituted into `mid`, but
    // `mid` is not in the registry (the absorber itself was pruned).
    // The chain has no surviving anchor → no binding.
    let (registry, rules, strings) = fixture(&[(0, "entry")], &[(1, "orphan")]);
    let mut trace = InlineTrace::new();
    trace.record(InlineSubstitution::new(
        1,
        "orphan",
        99, // no rule with this id exists in the registry
        "missing",
        InlinePass::FuseSingleUse,
    ));

    let resolver = build_resolver(&registry, &trace, &rules, &strings);
    assert_eq!(resolver.resolve("orphan"), None);
}

#[test]
fn empty_inputs_produce_empty_resolver() {
    let registry = StructRegistry::new();
    let trace = InlineTrace::new();
    let resolver = build_resolver(&registry, &trace, &[], &[]);
    assert!(resolver.is_empty());
    assert_eq!(resolver.resolve("anything"), None);
}

#[test]
fn resolver_iter_is_btreemap_stable() {
    let (registry, rules, strings) = fixture(&[(0, "alpha"), (1, "beta"), (2, "gamma")], &[]);
    let trace = InlineTrace::new();
    let resolver = build_resolver(&registry, &trace, &rules, &strings);

    let names: Vec<String> = resolver.iter().map(|(n, _)| n.clone()).collect();
    assert_eq!(
        names,
        vec!["alpha".to_string(), "beta".into(), "gamma".into()]
    );
}

#[test]
fn resolver_first_absorber_is_deterministic() {
    // The trace records two events (acyclic-inlined into both `caller_a`
    // and `caller_b`). The resolver picks the first absorber by
    // insertion order so identical (registry, trace) pairs always
    // produce identical resolvers.
    let (registry, rules, strings) = fixture(
        &[(0, "entry"), (1, "caller_a"), (2, "caller_b")],
        &[(3, "small")],
    );
    let mut trace = InlineTrace::new();
    trace.record(InlineSubstitution::new(
        3,
        "small",
        1,
        "caller_a",
        InlinePass::InlineAcyclic,
    ));
    trace.record(InlineSubstitution::new(
        3,
        "small",
        2,
        "caller_b",
        InlinePass::InlineAcyclic,
    ));

    let resolver_a = build_resolver(&registry, &trace, &rules, &strings);
    let resolver_b = build_resolver(&registry, &trace, &rules, &strings);
    assert_eq!(resolver_a, resolver_b);
    // First-absorber picks `caller_a` (insertion order).
    assert_eq!(resolver_a.resolve("small"), Some(1));
}

/// End-to-end: a fixture grammar with a `fuse_single_use`-fused rule
/// produces a resolver whose source-rule binding follows the trace to
/// the absorber. This is the W2.2 sub-gate: a fixture grammar with a
/// rule fused by `fuse_single_use` still resolves a path against the
/// source rule name.
///
/// The structural normalizer loop in `pipeline::compile` runs
/// `prune_unreachable` after `fuse_single_use` to remove dead rules.
/// This test mirrors that production order — fuse, then prune — so
/// the registry only sees the surviving rules.
#[test]
fn fixture_grammar_fused_rule_still_resolves() {
    use bbnf_ir::passes::types::registry::populate_struct_registry;
    use bbnf_ir::passes::{fuse_single_use, prune_unreachable};
    use bbnf_ir::types::TypeDescInterner;
    use rustc_hash::FxHashMap;
    use std::collections::HashMap;

    // entry = small ;  small = "x" ;
    let strings: Vec<String> = vec!["entry".into(), "small".into()];
    let rules = vec![
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
    ];
    let mut ir = bbnf_ir::types::GrammarIR {
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
        path_check_resolver: PathCheckResolver::default(),
    };

    // Run fuse with trace recording, then prune the dead rule. This
    // mirrors the structural normalizer loop's `fuse_single_use →
    // prune_unreachable` sequence.
    let mut trace = InlineTrace::new();
    fuse_single_use(&mut ir, &mut trace);
    prune_unreachable(&mut ir);

    // After prune, only `entry` survives. The trace still carries the
    // substitution event so the resolver can re-bind the source name.
    assert!(
        ir.rules.iter().all(|r| r.id == 0),
        "expected only `entry` to survive prune; got {:?}",
        ir.rules.iter().map(|r| r.id).collect::<Vec<_>>()
    );

    // Populate registry against the post-prune IR. Only `entry`'s
    // layout survives — the registry has no `small` entry.
    let mut rule_types: FxHashMap<RuleId, TypeDesc> = FxHashMap::default();
    rule_types.insert(0, TypeDesc::Span);
    let type_map = bbnf_ir::passes::TypeMap::default();
    populate_struct_registry(&mut ir, &rule_types, &type_map);

    // Run path_check.
    let resolver = bbnf_ir::passes::run_path_check(&ir, &trace);

    // The post-fuse `entry` rule absorbed `small`'s body. The W2
    // invariant 8 demands `path!(..., "small", ...)` resolve — the
    // resolver maps `"small" → entry's RuleId` through the trace.
    assert_eq!(resolver.resolve("entry"), Some(0));
    assert_eq!(
        resolver.resolve("small"),
        Some(0),
        "fused source rule must resolve through inline trace"
    );
}
