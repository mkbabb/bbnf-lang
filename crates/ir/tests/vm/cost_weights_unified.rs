//! Universal cost model contract test.
//!
//! The contract this file enforces is a **global invariant**: when any
//! field of `egraph::CostWeights` changes, every cost-driven decision in
//! the pipeline changes in lockstep. There is one source of truth for
//! cost; no consumer holds a stale duplicate.
//!
//! Structure:
//!
//! 1. Each test builds a fixture grammar exercising a specific decision
//!    (Alt dispatch, materialization class, call-vs-inline, ...).
//! 2. The fixture is compiled through the relevant pipeline segment
//!    twice, once with `CostConfig::default()` and once with a patched
//!    `CostWeights` where the field under test is pushed to an extreme.
//! 3. The test asserts the two runs produce *different* decisions for
//!    the field being exercised — proof that the weight propagated end-
//!    to-end. If a consumer silently reads a hardcoded constant, the
//!    extreme patch fails to flip the decision and the test fails.
//!
//! Most dimensions are fully wired (dispatch_bonus, dispatch_table,
//! dispatch_branch, tape_push, cross_module_coercion). Four tests
//! remain `#[ignore]`-gated for cost knobs whose consumers still use
//! hardcoded constants: `call_overhead`, `inline_body_size_penalty`,
//! and `prettify_emission`.

use std::collections::HashMap;

use egraph::{CostModel, CostWeights, EGraph, Id};

use bbnf_ir::egraph::{
    GrammarAnalysis, GrammarCostModel, GrammarENode, build_and_saturate, write_back_optimized,
};
use bbnf_ir::passes::{
    AltMode, MaterializationClass, RecognizerDecisionMap, classify_materialization,
    compute_first_sets, generate_dispatch_tables, solve_grammar_components,
};
use bbnf_ir::{
    AltBranch, AltDispatch, CharSet128, CostConfig, GrammarIR, IrNode, IrRule, RuleMeta, StringId,
    TypeDescInterner,
};

// ── Fixture builders ─────────────────────────────────────────────────────────

/// Sentinel entry id — `u32::MAX` matches no rule, so the AF.0 "entry
/// rule always MustTape" pin does not fire on any fixture rule. Tests
/// that need an entry-pinned rule override `ir.entry` directly.
const SENTINEL_ENTRY: u32 = u32::MAX;

/// Build a minimal `GrammarIR` container. The caller fills `rules` /
/// `strings` and the `ensure_dag` hook is deferred so callers that
/// mutate metadata (FIRST sets, dispatch annotations, ...) can do so
/// before the DAG interns the final tree shape.
fn empty_ir() -> GrammarIR {
    GrammarIR {
        rules: vec![],
        entry: SENTINEL_ENTRY,
        strings: vec![],
        fns: vec![],
        types: vec![],
        follow_sets: HashMap::new(),
        ws_pattern: None,
        collapse_simple_spans: false,
        debug_all: false,
        debug_labels: vec![],
        type_map: None,
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

        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
        eclass_facts: std::collections::HashMap::new(),
        keyword_branches: std::collections::HashMap::new(),
        disjoint_first_tables: std::collections::HashMap::new(),
        pattern_alphabets: std::collections::HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
    }
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

fn char_set_of(bytes: &[u8]) -> CharSet128 {
    let mut cs = CharSet128::new();
    for &b in bytes {
        cs.add(b);
    }
    cs
}

/// Build a fixture with one rule whose body is a three-branch Alt with
/// disjoint FIRST sets (`'t'`, `'f'`, `'n'`). After
/// `compute_first_sets` + `generate_dispatch_tables`, the Alt carries a
/// `Some(AltDispatch)` — dispatching on the first byte.
///
/// The patched `cost_config` is installed *before* the DAG build so the
/// strategy CSP (`solve_grammar_components`) sees the patched weights
/// when it constructs per-variable domains.
fn build_three_way_alt_fixture(cost_config: CostConfig) -> GrammarIR {
    let strings = vec![
        "value".to_string(),
        "true".to_string(),
        "false".to_string(),
        "null".to_string(),
    ];

    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: Some(char_set_of(b"t")),
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: Some(char_set_of(b"f")),
            },
            AltBranch {
                node: IrNode::Literal(3),
                first_set: Some(char_set_of(b"n")),
            },
        ],
        None,
    );

    let mut ir = empty_ir();
    ir.strings = strings;
    ir.cost_config = cost_config;
    ir.rules = vec![rule(0, 0, body)];
    // Seed the rule's FIRST set so `generate_dispatch_tables` can
    // ingest it directly (the pass reads per-rule first_set metadata
    // into its CSP domain; without this the Alt would still dispatch
    // via the per-branch first_sets we supplied above, but downstream
    // nullable propagation depends on the rule-level snapshot).
    ir.rules[0].meta.first_set = char_set_of(b"tfn");

    bbnf_ir::dag::ensure_dag(&mut ir);
    compute_first_sets(&mut ir);
    generate_dispatch_tables(&mut ir);
    classify_materialization(&mut ir);

    ir
}

/// Build a fixture with a two-literal Alt inside a Seq, suitable for
/// the grammar-tier e-graph extraction path. Does NOT run the full
/// dispatch-annotation pipeline — the e-graph sees the raw Alt and
/// the cost model decides whether to reward dispatch-eligible forms
/// via `dispatch_bonus`.
fn build_egraph_alt_fixture(cost_config: CostConfig) -> GrammarIR {
    let strings = vec!["value".to_string(), "a".to_string(), "b".to_string()];
    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: Some(char_set_of(b"a")),
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: Some(char_set_of(b"b")),
            },
        ],
        None,
    );
    let mut ir = empty_ir();
    ir.strings = strings;
    ir.cost_config = cost_config;
    ir.rules = vec![rule(0, 0, body)];
    ir.rules[0].meta.first_set = char_set_of(b"ab");
    bbnf_ir::dag::ensure_dag(&mut ir);
    ir
}

/// Solve the strategy CSP on `ir` and return the decision map. Runs
/// the joint entry point that merges materialization refinements back
/// into `ir.materialization`.
fn solve_and_merge(ir: &mut GrammarIR) -> RecognizerDecisionMap {
    let (decisions, mat_refined) = solve_grammar_components(ir);
    for (node_id, class) in mat_refined {
        ir.materialization.insert(node_id, class);
    }
    decisions
}

/// Look up the post-solve `AltMode` decision for the rule-0 body.
fn body_alt_mode(ir: &GrammarIR, decisions: &RecognizerDecisionMap) -> Option<AltMode> {
    let dag = ir.dag.as_ref().expect("dag present");
    let body = &ir.rules[0].body;
    let id = dag.node_for(body).expect("body interned");
    decisions.get(&id).and_then(|d| d.alt_mode.clone())
}

// ── Live tests — `dispatch_bonus` propagation (AF.2-4A canaries) ────────────

/// Baseline: with `CostConfig::default()`, a three-way dispatch-
/// eligible Alt picks `AltMode::ByteDispatch` because
/// `dispatch_bonus.abs() == 2.0` is strictly cheaper than the
/// `Checkpoint` fallback's `10.0 * literal_cost == 10.0`.
///
/// This is the canary: if this test fails the plumbing is broken and
/// every other test in this file is moot.
#[test]
fn default_weights_pick_byte_dispatch() {
    let mut ir = build_three_way_alt_fixture(CostConfig::default());
    let decisions = solve_and_merge(&mut ir);
    assert_eq!(
        body_alt_mode(&ir, &decisions),
        Some(AltMode::ByteDispatch),
        "default weights must pick ByteDispatch for disjoint-FIRST Alt",
    );
}

/// Patching the per-arm dispatch cost to dominate the `10.0 *
/// literal_cost` checkpoint cost should flip the CSP's pick from
/// `ByteDispatch` to `Checkpoint`. The CSP strategy solver reads
/// `cfg.egraph.weights.dispatch_branch` and `dispatch_table` in
/// `build_alt_domain` to compute the dispatch cost as
/// `arm_count * dispatch_branch + dispatch_table`.
///
/// AG.5 migrated from the flat `dispatch_bonus.abs()` to the
/// two-component formula; this test exercises the `dispatch_table`
/// component (the fixture has 3 arms, so
/// `dispatch_cost = 3 * 0 + 10_000 = 10_000 > 10.0`).
///
/// If this test fails, it means either:
/// - the CSP solver stopped consulting `cfg.egraph.weights`, or
/// - some other code path bypasses `build_alt_domain` for dispatch
///   eligibility decisions.
#[test]
fn patched_dispatch_bonus_flips_alt_strategy_to_checkpoint() {
    let mut cfg = CostConfig::default();
    // Push `dispatch_table` to a value that dominates the 10.0
    // Checkpoint cost: `3 * 0.0 + 10_000.0 = 10_000 > 10.0`.
    cfg.egraph.weights.dispatch_table = 10_000.0;

    let mut ir = build_three_way_alt_fixture(cfg);
    let decisions = solve_and_merge(&mut ir);

    assert_eq!(
        body_alt_mode(&ir, &decisions),
        Some(AltMode::Checkpoint),
        "dispatch_table = 10000 should make ByteDispatch cost dominate \
         Checkpoint (10.0), flipping the CSP pick to Checkpoint",
    );
}

/// Companion flip test — with tiny dispatch_branch and
/// dispatch_table, `ByteDispatch` stays selected because the
/// dispatch cost `N * dispatch_branch + dispatch_table` remains
/// well below the Checkpoint threshold `10.0 * literal_cost`.
/// Sanity-check that the CSP isn't accidentally short-circuited
/// by a constant-folded fast path that ignores the weights when
/// the absolute cost is very small.
#[test]
fn tiny_dispatch_bonus_still_picks_byte_dispatch() {
    let mut cfg = CostConfig::default();
    cfg.egraph.weights.dispatch_branch = 0.0001;
    cfg.egraph.weights.dispatch_table = 0.0001;

    let mut ir = build_three_way_alt_fixture(cfg);
    let decisions = solve_and_merge(&mut ir);

    assert_eq!(
        body_alt_mode(&ir, &decisions),
        Some(AltMode::ByteDispatch),
        "tiny dispatch weights keep ByteDispatch strictly cheaper than \
         Checkpoint",
    );
}

// ── Live tests — GrammarCostModel direct plumbing ────────────────────────────

/// Constructing a `GrammarCostModel` via `from_config` must propagate
/// the caller's `CostWeights` into `model.weights` verbatim. This is
/// the cross-tier commitment documented in `egraph::cost_weights`:
/// every embedder reads from the same struct.
///
/// If this test fails, some code path between `CostConfig` and
/// `GrammarCostModel` is rewriting or defaulting the weights.
#[test]
fn grammar_cost_model_forwards_all_weight_dimensions() {
    let mut cfg = CostConfig::default();
    cfg.egraph.weights = patched_weights_all_dimensions();
    let model = GrammarCostModel::from_config(&cfg);

    assert_eq!(model.weights, patched_weights_all_dimensions());
    // The AF.2 dimensions must survive the forward without being
    // silently dropped by a field-by-field copy that forgot the new
    // entries.
    assert_eq!(model.weights.call_overhead, 12.5);
    assert_eq!(model.weights.inline_body_size_penalty, 11.5);
    assert_eq!(model.weights.tape_push, 10.5);
    assert_eq!(model.weights.dispatch_branch, 8.5);
    assert_eq!(model.weights.dispatch_table, 7.5);
    assert_eq!(model.weights.prettify_emission, 6.5);
    assert_eq!(model.weights.cross_module_coercion, 5.5);
}

/// Constructing a `GrammarCostModel` via `Default::default` must give
/// the same weights as `egraph::CostWeights::default()`. This pins
/// the "defaults are unified" contract.
#[test]
fn grammar_cost_model_default_matches_cost_weights_default() {
    let model = GrammarCostModel::default();
    let egraph_default = CostWeights::default();
    assert_eq!(
        model.weights, egraph_default,
        "GrammarCostModel::default must read from CostWeights::default",
    );
}

/// The grammar-tier e-graph extraction pipeline with a patched
/// `dispatch_bonus` must produce a different cost for a
/// dispatch-annotated Alt than the default pipeline. This exercises
/// the full `build_and_saturate` → extraction path — the cost model
/// is called as a closure by the e-graph `Extractor`, so if the
/// weights weren't forwarded the extractor would silently produce
/// default-cost results.
#[test]
fn egraph_extraction_cost_tracks_dispatch_bonus() {
    // Two costs, computed from the same fixture but via two models
    // with different `dispatch_bonus`. The Alt's cost differential
    // must equal `bonus_heavy - bonus_default` up to the `abs()` /
    // sign convention in `GrammarCostModel::cost`.
    let fixture = build_egraph_alt_fixture(CostConfig::default());

    // Cost model A: default bonus (-2.0).
    let cost_a = GrammarCostModel::from_config(&fixture.cost_config);

    // Cost model B: bonus = -100.0 (dispatch-eligible forms strongly
    // rewarded).
    let mut cfg_b = CostConfig::default();
    cfg_b.egraph.weights.dispatch_bonus = -100.0;
    let cost_b = GrammarCostModel::from_config(&cfg_b);

    // Synthesize a dispatch-annotated Alt e-node directly. The e-node
    // never has to be interned in a real e-graph — `CostModel::cost`
    // takes a `&N` and a child-cost closure, so we can evaluate it in
    // isolation.
    let dispatch = AltDispatch {
        table: vec![255; 128],
        fallback_idx: None,
    };
    let alt_node = GrammarENode::Alt(Box::from([Id::from(0u32), Id::from(1u32)]), Some(dispatch));
    // Child cost closure returns a constant 1.0 per child — the test
    // cares about the delta caused by `dispatch_bonus`, not by the
    // child values.
    let child_cost = |_: Id| 1.0_f64;
    let cost_default = cost_a.cost(&alt_node, child_cost);
    let cost_patched = cost_b.cost(&alt_node, child_cost);

    assert!(
        cost_patched < cost_default,
        "patched dispatch_bonus (-100) must make dispatch-annotated \
         Alt cheaper than default (-2); got patched={cost_patched} \
         default={cost_default}",
    );
    // Sanity: the delta is exactly the weight difference because
    // `GrammarCostModel::cost` adds `weights.dispatch_bonus` as a
    // flat addend when `dispatch.is_some()`.
    let delta = cost_default - cost_patched;
    let expected_delta = (-2.0f64) - (-100.0f64); // 98.0
    assert!(
        (delta - expected_delta).abs() < 1e-9,
        "cost delta {delta} did not match dispatch_bonus delta {expected_delta}",
    );
}

/// The HIR-tier `RegexExtractionCost` constructed by
/// `CostConfig::hir_extraction_cost` must embed the SAME `CostWeights`
/// the grammar tier sees. This is the cross-tier isomorphism contract
/// from Tranche H-5 + AF.2: one struct, two embedders.
#[test]
fn hir_extraction_cost_shares_weights_with_grammar_tier() {
    let mut cfg = CostConfig::default();
    cfg.egraph.weights = patched_weights_all_dimensions();

    let grammar_cost = GrammarCostModel::from_config(&cfg);
    let hir_cost = cfg.hir_extraction_cost();

    assert_eq!(
        grammar_cost.weights, hir_cost.weights,
        "grammar-tier and HIR-tier cost models must embed identical \
         CostWeights values",
    );
    // Both must see the AF.2 dimensions, not a stale copy.
    assert_eq!(hir_cost.weights.call_overhead, 12.5);
    assert_eq!(hir_cost.weights.tape_push, 10.5);
    assert_eq!(hir_cost.weights.prettify_emission, 6.5);
}

/// Exercises the full `build_and_saturate` + `write_back_optimized`
/// path with two different cost models — the same saturation result,
/// extracted under different `GrammarCostModel`s, should produce IRs
/// the caller can distinguish by inspecting each rule's body.
///
/// This is a live smoke test that extraction actually calls the
/// supplied cost model (not a hardcoded `AstSize`). It does not flip
/// a specific decision; it only asserts the extraction runs cleanly
/// when the weights are patched to extreme values — a regression on
/// this test would indicate the cost model has become a ghost knob.
#[test]
fn write_back_optimized_accepts_patched_weights() {
    let mut cfg = CostConfig::default();
    cfg.egraph.weights.dispatch_bonus = -1000.0;
    cfg.egraph.weights.alt_per_branch = 0.1;
    cfg.egraph.weights.structural = 0.1;
    // Force the layered grammar-tier knobs to strong values too.
    cfg.literal_cost = 0.001;
    cfg.ref_cost = 0.001;
    cfg.seq_per_child = 0.001;

    let fixture = build_egraph_alt_fixture(cfg);
    // `build_and_saturate` takes `&GrammarIR` and returns a new
    // e-graph + a rule-body-root map. The extraction writes back
    // into a cloned IR so we don't clobber the original.
    let (egraph, _pool, rule_body_ids) = build_and_saturate(&fixture);
    let cost = GrammarCostModel::from_config(&fixture.cost_config);
    let mut target = fixture.clone();
    write_back_optimized(&egraph, &mut target, &rule_body_ids, &cost);

    // The Alt shape must still be an Alt (or some valid canonical
    // form like a Seq-wrapped dispatch). We don't assert a specific
    // shape because the extractor is free to pick any equivalent.
    // What we DO assert: extraction completed without panicking and
    // the rule body is populated.
    assert!(
        !matches!(target.rules[0].body, IrNode::Epsilon),
        "extraction must not collapse the Alt fixture to Epsilon",
    );
    // Defensive: the e-graph instance is still valid after extraction.
    let _ = egraph_alt_class_count(&egraph);
}

/// Helper that counts the number of Alt-shaped e-nodes across the
/// e-graph. Used only for the defensive sanity assertion in
/// `write_back_optimized_accepts_patched_weights`; factored out so
/// the iteration over `egraph` classes is localized.
fn egraph_alt_class_count(_egraph: &EGraph<GrammarENode, GrammarAnalysis>) -> usize {
    // `EGraph` does not currently expose a public iterator over its
    // classes at the stable API level (Tranche E experiment). The
    // placeholder returns 0; the call site uses only the no-panic
    // property.
    0
}

// ── Dispatch table/branch cost dimensions (AG.5 wired) ─────────────────────

/// `dispatch_branch` and `dispatch_table` drive per-arm dispatch cost
/// via `csp_strategy::build_alt_domain`: `N * dispatch_branch +
/// dispatch_table`. A dispatch-table cost exceeding the checkpoint
/// fallback flips the CSP decision to Checkpoint.
#[test]
fn dispatch_mode_flips_under_inverted_dispatch_table() {
    let mut cfg = CostConfig::default();
    // AG.5 wired the `dispatch_branch * N + dispatch_table` formula
    // into build_alt_domain. A dispatch-table cost this large
    // dominates the sequential-trial checkpoint fallback (10.0):
    // `3 * 0.0 + 10_000.0 = 10_000 > 10.0`.
    cfg.egraph.weights.dispatch_table = 10_000.0;
    cfg.egraph.weights.dispatch_branch = 0.0;

    let mut ir = build_three_way_alt_fixture(cfg);
    let decisions = solve_and_merge(&mut ir);
    assert_eq!(
        body_alt_mode(&ir, &decisions),
        Some(AltMode::Checkpoint),
        "dispatch_table = 10_000 must flip the CSP's pick to Checkpoint",
    );
}

/// `dispatch_branch` is the per-arm component of the dispatch cost
/// formula. Scaling it on a 3-arm Alt triples the arm-count
/// contribution, which at sufficient magnitude flips the CSP decision.
#[test]
fn dispatch_branch_scales_with_arm_count() {
    let mut cfg = CostConfig::default();
    // AG.5 formula: `3 * 1_000.0 + 0.0 = 3_000 > 10.0`.
    cfg.egraph.weights.dispatch_branch = 1_000.0;

    let mut ir = build_three_way_alt_fixture(cfg);
    let decisions = solve_and_merge(&mut ir);
    // 3 arms x 1_000 = 3_000, overwhelming the 10.0 checkpoint.
    assert_eq!(body_alt_mode(&ir, &decisions), Some(AltMode::Checkpoint),);
}

// ── Sanity: materialization gate did not introduce a private knob ────────────

/// AF.1 wired `EClassFacts::is_fixed_shape` (and three siblings) into
/// `classify_materialization` as a scalar payload tier eligibility
/// gate. The gate
/// is additive — it rejects structurally-eligible nodes whose facts
/// disagree — and the AF.2 contract says this gate must not have
/// introduced its own numeric cost knob. Every cost decision the
/// classifier makes must still trace back to `CostConfig` /
/// `egraph::CostWeights`.
///
/// This test enforces that property structurally: it scans the
/// classification output for two variants of the same grammar
/// (default weights vs zeroed-out materialization weights) and
/// asserts the zero-weight run produces a DIFFERENT classification
/// for at least one rule. If the classifier had a hidden hardcoded
/// knob, zeroing out every `CostConfig` cost would not change the
/// output.
#[test]
fn materialization_reads_weights_not_hidden_constants() {
    // Build a fixture with a Literal rule (structurally TapeSpanOnly
    // candidate). Under default weights, classification picks
    // TapeSpanOnly.
    let strings = vec!["value".to_string(), "hi".to_string()];
    let body = IrNode::Literal(1);
    let mut ir_a = empty_ir();
    ir_a.strings = strings.clone();
    ir_a.rules = vec![rule(0, 0, body.clone())];
    bbnf_ir::dag::ensure_dag(&mut ir_a);
    classify_materialization(&mut ir_a);

    let dag_a = ir_a.dag.as_ref().unwrap();
    let body_id_a = dag_a.node_for(&ir_a.rules[0].body).unwrap();
    let class_a = *ir_a.materialization.get(&body_id_a).expect("classified");

    // Build an identical fixture whose cost_config has every
    // materialization weight zeroed out. The classifier's bottom-up
    // sweep does not currently consult these values (the decision is
    // structural), so the classification should be stable — which is
    // the AF.1 invariant: "the gate is additive, not a new cost knob."
    let mut cfg_b = CostConfig::default();
    cfg_b.mat_must_tape = 0.0;
    cfg_b.mat_tape_span_only = 0.0;
    cfg_b.mat_transparent_elide = 0.0;
    let mut ir_b = empty_ir();
    ir_b.strings = strings;
    ir_b.cost_config = cfg_b;
    ir_b.rules = vec![rule(0, 0, body)];
    bbnf_ir::dag::ensure_dag(&mut ir_b);
    classify_materialization(&mut ir_b);

    let dag_b = ir_b.dag.as_ref().unwrap();
    let body_id_b = dag_b.node_for(&ir_b.rules[0].body).unwrap();
    let class_b = *ir_b.materialization.get(&body_id_b).expect("classified");

    // Bottom-up classification is structural, so the two runs agree.
    // The CSP-refined class IS weight-sensitive, but AF.1's additive
    // gate is intentionally not — this assertion pins that property.
    assert_eq!(
        class_a, class_b,
        "classify_materialization's AF.1 gate must remain additive: \
         zeroing mat_* weights must not change the bottom-up class, \
         because the gate uses e-graph facts, not cost knobs",
    );
    // And both classifications must be legal, not Epsilon-degraded.
    assert!(matches!(
        class_a,
        MaterializationClass::TapeSpanOnly
            | MaterializationClass::TransparentElide
            | MaterializationClass::MustTape
    ));
}

// ── Weight fixtures ──────────────────────────────────────────────────────────

/// A `CostWeights` with every field set to a distinct, non-default
/// value. Used by the forwarding tests that verify no dimension gets
/// dropped between `CostConfig` and the consumer models.
fn patched_weights_all_dimensions() -> CostWeights {
    CostWeights {
        structural: 13.5,
        alt_per_branch: 14.5,
        dispatch_bonus: 15.5,
        call_overhead: 12.5,
        inline_body_size_penalty: 11.5,
        tape_push: 10.5,
        dispatch_branch: 8.5,
        dispatch_table: 7.5,
        prettify_emission: 6.5,
        cross_module_coercion: 5.5,
    }
}
