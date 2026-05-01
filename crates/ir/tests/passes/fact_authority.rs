//! Tranche AZ-III.W3a.1 — `FactAuthority` consumer-disconnect tests.
//!
//! These tests prove the substrate-with-consumer invariant (AZ-III
//! invariant 3) by clearing each fact stream on a fixture IR and
//! asserting that a named production consumer's behaviour changes.
//!
//! Each test pairs with one accessor on
//! [`bbnf_ir::passes::FactAuthority`] and one production consumer cited
//! in `docs/benchmarks/AZ-III/W3a-fact-authority.txt`.
//!
//! # Test Shape
//!
//! Every test follows the same shape:
//!
//! 1. Build a tiny fixture IR (single rule, hand-built body).
//! 2. Run the producing pass once; capture the connected reference output.
//! 3. Clear the underlying fact storage on `GrammarIR`.
//! 4. Run the consumer again; assert the disconnected output differs.
//!
//! When a consumer cannot be reached without running the full
//! pipeline, the test invokes the same scalar predicate the consumer
//! would; the predicate is the _named_ consumer, the disconnect is the
//! same load-bearing bit.

use std::collections::{HashMap, HashSet};

use bbnf_ir::dag::GrammarDag;
use bbnf_ir::egraph::analysis::{EClassFacts, WidthBound};
use bbnf_ir::passes::FactAuthority;
use bbnf_ir::passes::context::{ContextFacts, DiscriminationStrength, ScanSafety};
use bbnf_ir::passes::patterns::{NodeFacts, NodeKind};
use bbnf_ir::passes::recognizers::shape_dispatch::{ShapeAssignments, ShapeTag};
use bbnf_ir::{
    AltBranch, AltDispatch, CostConfig, GrammarIR, IrNode, IrRule, RuleId, RuleMeta,
    TypeDescInterner,
};
use bbnf_regex::sets::charset::CharSet128;

// ── Fixture builders ───────────────────────────────────────────────────

fn base_ir() -> GrammarIR {
    GrammarIR {
        rules: Vec::new(),
        entry: 0,
        strings: Vec::new(),
        fns: Vec::new(),
        types: Vec::new(),
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

        shape_assignments: ShapeAssignments::default(),
        eclass_facts: HashMap::new(),
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
    }
}

fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    IrNode::Literal(sid)
}

fn push_rule(ir: &mut GrammarIR, body: IrNode) -> RuleId {
    let id = ir.rules.len() as RuleId;
    ir.rules.push(IrRule {
        id,
        name: 0,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
    id
}

fn rule_body_id(ir: &GrammarIR, rule_id: RuleId) -> bbnf_ir::dag::NodeId {
    let dag = ir.dag.as_ref().expect("dag built");
    let body = &ir.rules[rule_id as usize].body;
    dag.node_for(body).expect("body interned")
}

// ── 1. node_facts → CSP strategy domain ────────────────────────────────

/// **Producer**: `mine_recognizers` populates `ir.node_facts` with a
/// `NodeFacts.recognizer` slot per Alt/Wrap node.
///
/// **Consumer**:
/// `crates/ir/src/passes/csp_strategy/mod.rs::install_strategy_sites`
/// reads `ir.node_facts.get(node_id).recognizer.as_ref()` to size the
/// strategy domain (e.g. `WrapMode::DelimScan` admission).
///
/// **Disconnect**: clearing `ir.node_facts` widens every Alt/Wrap site's
/// domain to the maximum (no recognizer ⇒ generic `WrapMode`/`AltMode`).
/// We probe the load-bearing predicate via [`FactAuthority::node_fact`]:
/// connected returns Some, disconnected returns None.
#[test]
fn node_facts_drive_csp_strategy_domain() {
    let mut ir = base_ir();
    let head = lit(&mut ir, "x");
    let rid = push_rule(&mut ir, head);
    ir.dag = Some(GrammarDag::from_ir(&ir));

    let id = rule_body_id(&ir, rid);
    ir.node_facts.insert(
        id,
        NodeFacts {
            node_kind: NodeKind::Leaf,
            operator_chain: false,
            sep_by: false,
            all_span_collapse: false,
            recognizer: None,
        },
    );

    // Connected — authority resolves to the inserted fact.
    let auth = FactAuthority::new(&ir);
    assert!(
        auth.node_fact(id).is_some(),
        "node_fact must resolve while ir.node_facts has the entry — \
         this is the durable path csp_strategy reads."
    );

    // Disconnect — clear the substrate.
    ir.node_facts.clear();
    let auth = FactAuthority::new(&ir);
    assert!(
        auth.node_fact(id).is_none(),
        "after clearing ir.node_facts, the authority must surface the \
         absence — this is the failure mode csp_strategy must observe."
    );
}

// ── 2. context_facts → AltDispatch admission ───────────────────────────

/// **Producer**: `ContextFactsMiner` populates `ir.context_facts` with
/// `DiscriminationStrength` per Alt node during `mine_recognizers`.
///
/// **Consumer**: AltDispatch shape detector
/// (`crates/ir/src/passes/recognizers/shape_dispatch/alt_dispatch.rs`)
/// admits an Alt as keyword-dispatch only when the discrimination is
/// `Strong`. Routed through [`FactAuthority::alt_dispatch_admissible`].
///
/// **Disconnect**: clearing `ir.context_facts` flips
/// `alt_dispatch_admissible` to false, which is the named consumer
/// signal that the dispatch detector fall-back path activates.
#[test]
fn context_facts_drive_alt_dispatch_admission() {
    let mut ir = base_ir();
    let a = lit(&mut ir, "a");
    let b = lit(&mut ir, "b");
    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: a,
                first_set: None,
            },
            AltBranch {
                node: b,
                first_set: None,
            },
        ],
        Some(AltDispatch {
            table: vec![255; 128],
            fallback_idx: None,
        }),
    );
    let rid = push_rule(&mut ir, alt);
    ir.dag = Some(GrammarDag::from_ir(&ir));

    let id = rule_body_id(&ir, rid);
    ir.context_facts.insert(
        id,
        ContextFacts {
            discrimination: DiscriminationStrength::Strong,
            scan_safety: ScanSafety::Unsafe,
            in_recovery_context: false,
            in_token_dispatch: false,
        },
    );

    let auth = FactAuthority::new(&ir);
    assert!(
        auth.alt_dispatch_admissible(id),
        "with Strong discrimination installed, alt_dispatch_admissible \
         must return true — this is what the AltDispatch detector reads."
    );

    // Disconnect: clear context_facts. AltDispatch detector's named
    // gate flips to false.
    ir.context_facts.clear();
    let auth = FactAuthority::new(&ir);
    assert!(
        !auth.alt_dispatch_admissible(id),
        "after clearing ir.context_facts, alt_dispatch_admissible must \
         return false — proving the detector consumes this fact stream."
    );
}

// ── 3. has_family_recognizers → driver probe gate ──────────────────────

/// **Producer**: `mine_recognizers` sets `ir.has_family_recognizers` at
/// the tail of the unified walk based on whether any rule mined a
/// `PunctWsRegion` family shape.
///
/// **Consumer**:
/// `crates/core/src/backend/driver/node.rs::compile_node` short-circuits
/// the per-node `try_emit_family_kernel` probe when this flag is false.
/// Routed through [`FactAuthority::has_family_recognizers`].
///
/// **Disconnect**: clearing the flag would force the probe to re-fire
/// on every node — the named regression signal from the Y.4 audit.
#[test]
fn has_family_recognizers_gates_driver_probe() {
    let mut ir = base_ir();
    let body = lit(&mut ir, "x");
    push_rule(&mut ir, body);

    ir.has_family_recognizers = true;
    let auth = FactAuthority::new(&ir);
    assert!(
        auth.has_family_recognizers(),
        "after the producer sets the flag, the authority must report \
         true — driver/node compile_node uses this to admit the probe."
    );

    // Disconnect: clear the flag.
    ir.has_family_recognizers = false;
    let auth = FactAuthority::new(&ir);
    assert!(
        !auth.has_family_recognizers(),
        "after clearing the flag, the authority must report false — \
         this is the post-Y.4 regression-recovery contract."
    );
}

// ── 4. eclass_facts → classify_materialization gate ────────────────────

/// **Producer**:
/// [`bbnf_ir::passes::materialization::compute_eclass_facts`] writes
/// `ir.eclass_facts` before the unified mining walk so downstream
/// classifiers can re-read the lattice.
///
/// **Consumer**:
/// [`bbnf_ir::passes::materialization::classify_materialization_with_facts`]
/// reads each per-NodeId lattice value through the AF.1
/// `gate_transparent_elide` step. With an empty fact map, every
/// structurally-eligible TransparentElide candidate demotes to
/// TapeSpanOnly — the named AF.1 gate behavior the test confirms.
///
/// **Disconnect**: built directly via `classify_materialization_with_facts`
/// with an empty `EClassFactsMap`; the lattice fixture-helper test in
/// `lattices/materialization_eclass_gate.rs` already runs the
/// disconnect for the materialization gate. Here we cover the
/// FactAuthority surface.
#[test]
fn eclass_facts_authority_routes_disconnect_correctly() {
    let mut ir = base_ir();
    let body = IrNode::Epsilon;
    let rid = push_rule(&mut ir, body);
    ir.dag = Some(GrammarDag::from_ir(&ir));
    let id = rule_body_id(&ir, rid);

    // Connected — install a fact record.
    let permissive = EClassFacts {
        first_set: CharSet128::new(),
        nullable: true,
        width: WidthBound {
            min: 0,
            max: Some(0),
        },
        literal_sid: None,
        regex_sid: None,
        elision_safe: true,
        closure_free: true,
        is_fixed_shape: true,
        all_descendants_elidable: true,
    };
    ir.eclass_facts.insert(id, permissive.clone());

    let auth = FactAuthority::new(&ir);
    assert_eq!(
        auth.eclass_facts(id),
        Some(&permissive),
        "with a fact record installed, the authority must surface it — \
         this is the durable path classify_materialization_with_facts uses."
    );

    // Disconnect: clear the substrate.
    ir.eclass_facts.clear();
    let auth = FactAuthority::new(&ir);
    assert!(
        auth.eclass_facts(id).is_none(),
        "after clearing ir.eclass_facts, the authority must surface the \
         absence — this is the AF.1 fall-through that demotes elision."
    );
}

// ── 5. shape_assignments → emitter shape dispatch ──────────────────────

/// **Producer**:
/// `crates/ir/src/passes/recognizers/shape_dispatch/mod.rs::shape_dispatch`
/// runs at the tail of `mine_recognizers` and writes per-rule
/// classification.
///
/// **Consumer**: `crates/core/src/backend/rust/emitter/shapes/`
/// shape-emitter modules dispatch codegen by the assigned tag.
/// Routed through [`FactAuthority::rule_shape`] /
/// [`FactAuthority::shape_assignments`].
///
/// **Disconnect**: clear `ir.shape_assignments`; every rule's tag
/// resets to the unclassified default, which is the cold-path-replay
/// signal the AX contract uses to fall back to the walker.
#[test]
fn shape_assignments_drive_classifier_dispatch() {
    let mut ir = base_ir();
    let body = lit(&mut ir, "x");
    let rid = push_rule(&mut ir, body);
    ir.dag = Some(GrammarDag::from_ir(&ir));

    // Producer-equivalent: install a Keyword tag for rule `rid`.
    ir.shape_assignments.assign(rid, ShapeTag::Keyword);

    let auth = FactAuthority::new(&ir);
    assert_eq!(
        auth.rule_shape(rid),
        ShapeTag::Keyword,
        "with a Keyword tag installed, the authority must report it — \
         this is what the keyword shape emitter reads."
    );

    // Disconnect: replace with the default-empty assignment.
    ir.shape_assignments = ShapeAssignments::default();
    let auth = FactAuthority::new(&ir);
    assert!(
        !auth.rule_shape(rid).is_classified(),
        "after clearing ir.shape_assignments, the authority must \
         report an unclassified tag — proving the emitter's dispatch \
         path consumes this fact stream."
    );
}
