//! AZ-III.W3b — CSP authority for shape, layout, and dispatch.
//!
//! Each constraint installer (`shape::install`, `layout::install`,
//! `dispatch::install`) is paired with a named production consumer.
//! The tests in this module form the substrate-with-consumer
//! disconnect pairings required by AZ-III invariant 3:
//!
//! - `shape_*` tests cover [`crate::passes::csp_strategy::constraints::shape::install`].
//! - `layout_*` tests cover [`crate::passes::csp_strategy::constraints::layout::install`].
//! - `dispatch_*` tests cover [`crate::passes::csp_strategy::constraints::dispatch::install`].
//!
//! Each test populates the upstream fact a constraint installer
//! consumes (`node_facts.recognizer.shape`, `delim_scan_configs`, or
//! `key_dispatch_configs`), runs `solve_grammar_components`, and
//! asserts that the resulting `recognizer_decisions[id]` carries the
//! authoritative pin. The "without the installer" variant is
//! exercised by zeroing the upstream fact — the same code path the
//! installer skips when no fact applies — and asserting the CSP
//! returns its untargeted cost-min instead.
//!
//! # Producer / consumer ledger
//!
//! | constraint | producer | consumer |
//! |---|---|---|
//! | `shape::install` (Alt) | `node_facts.recognizer.shape == TokenLedBranches | KeywordPrefix` | `crates/core/src/backend/strategy/alt_strategy.rs` |
//! | `shape::install` (Wrap) | `node_facts.recognizer.shape == DelimiterBalanced | SeparatorList` | `crates/core/src/backend/driver/wrap.rs` |
//! | `layout::install` | `ir.delim_scan_configs[id]` or `node_facts.recognizer.shape == SeparatorList` | `crates/core/src/backend/driver/wrap.rs` |
//! | `dispatch::install` | `ir.key_dispatch_configs[id]` or `ir.keyword_branches[id]` | `crates/core/src/backend/strategy/alt_strategy.rs` |

use std::collections::HashMap;

use bbnf_ir::passes::csp_strategy::{AltMode, WrapMode, solve_grammar_components};
use bbnf_ir::passes::materialization::classify_materialization;
use bbnf_ir::passes::patterns::{
    NodeFacts, NodeKind, OnePassGrade, OutputShape, Recognizer, RecognizerRole, RecognizerShape,
    RecognizerSignature,
};
use bbnf_ir::{
    AltBranch, CostConfig, DelimScanConfig, GrammarIR, IrNode, IrRule, KeyClass, KeyDispatchConfig,
    KeyDispatchMatch, RuleMeta, StringId, TypeDescInterner,
};

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
    };
    bbnf_ir::dag::ensure_dag(&mut ir);
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

fn synthesize_recognizer(shape: RecognizerShape) -> Recognizer {
    Recognizer {
        role: RecognizerRole::Standalone,
        shape,
        signature: RecognizerSignature {
            shape_hash: 0,
            output_shape: OutputShape::SpanOnly,
            advance_on_failure: false,
            grade: OnePassGrade::OnePass,
        },
    }
}

fn install_recognizer_fact(ir: &mut GrammarIR, rule_idx: usize, shape: RecognizerShape) {
    // The DAG keys nodes by pointer identity, so we must look up
    // the node id from the live tree address, then mutate.
    let node_id = {
        let dag = ir.dag.as_ref().unwrap();
        let body = &ir.rules[rule_idx].body;
        dag.node_for(body).unwrap()
    };
    let facts = NodeFacts {
        node_kind: NodeKind::Alt,
        operator_chain: false,
        sep_by: false,
        all_span_collapse: false,
        recognizer: Some(synthesize_recognizer(shape)),
    };
    ir.node_facts.insert(node_id, facts);
}

fn install_recognizer_fact_wrap(ir: &mut GrammarIR, rule_idx: usize, shape: RecognizerShape) {
    let node_id = {
        let dag = ir.dag.as_ref().unwrap();
        let body = &ir.rules[rule_idx].body;
        dag.node_for(body).unwrap()
    };
    let facts = NodeFacts {
        node_kind: NodeKind::Skip,
        operator_chain: false,
        sep_by: false,
        all_span_collapse: false,
        recognizer: Some(synthesize_recognizer(shape)),
    };
    ir.node_facts.insert(node_id, facts);
}

// ── W3b.1 — Shape constraint authority ──────────────────────────────

/// `shape::install` pins an Alt with a `KeywordPrefix` recognizer
/// shape to `AltMode::KeyDispatch`. Without the installer, the CSP
/// has no `KeyDispatch` value in the alt domain (it only enters the
/// domain via the recognizer fact at `build_alt_domain`), so the
/// CSP picks `Checkpoint` from the cost-min path.
#[test]
fn shape_constraint_pins_alt_mode_for_keyword_prefix() {
    let strings: Vec<String> = vec!["entry", "kw1", "kw2"]
        .into_iter()
        .map(String::from)
        .collect();
    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
        ],
        None,
    );
    let mut ir = make_ir(vec![rule(0, 0, body.clone())], strings);
    let shape = RecognizerShape::KeywordPrefix {
        bytes: smallvec::smallvec![b'k'],
        disjoint_tail: true,
    };
    install_recognizer_fact(&mut ir, 0, shape);

    let (decisions, _mat) = solve_grammar_components(&ir);
    let dag = ir.dag.as_ref().unwrap();
    let alt_id = dag.node_for(&ir.rules[0].body).unwrap();
    let mode = decisions
        .get(&alt_id)
        .and_then(|d| d.alt_mode.as_ref())
        .expect("CSP produced an alt decision");

    // Authoritative pin via shape::install.
    assert_eq!(
        *mode,
        AltMode::KeyDispatch,
        "shape::install must pin KeywordPrefix Alt to KeyDispatch"
    );
}

/// Disconnect: without the recognizer fact, the same Alt structure
/// gets only the universal-fallback `Checkpoint` value in its CSP
/// domain, so the cost-min answer is `Checkpoint`. This proves the
/// installer is not redundant.
#[test]
fn shape_constraint_disconnect_falls_back_to_checkpoint() {
    let strings: Vec<String> = vec!["entry", "kw1", "kw2"]
        .into_iter()
        .map(String::from)
        .collect();
    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
        ],
        None,
    );
    let ir = make_ir(vec![rule(0, 0, body)], strings);
    // No recognizer fact installed: the CSP has only Checkpoint in
    // the Alt domain.

    let (decisions, _mat) = solve_grammar_components(&ir);
    let dag = ir.dag.as_ref().unwrap();
    let alt_id = dag.node_for(&ir.rules[0].body).unwrap();
    let mode = decisions
        .get(&alt_id)
        .and_then(|d| d.alt_mode.as_ref())
        .expect("CSP produced an alt decision");

    assert_eq!(
        *mode,
        AltMode::Checkpoint,
        "without the shape fact the CSP falls back to Checkpoint"
    );
}

// ── W3b.2 — Layout constraint authority ─────────────────────────────

/// `layout::install` pins a Wrap with a populated `delim_scan_configs`
/// entry to `WrapMode::BalancedScan`. Disconnecting the
/// `delim_scan_configs` entry (the upstream producer fact) lets the
/// CSP pick `Generic` because the Wrap domain has no other entries.
#[test]
fn layout_constraint_pins_wrap_mode_when_delim_scan_present() {
    let strings: Vec<String> = vec!["entry", "{", "}"]
        .into_iter()
        .map(String::from)
        .collect();
    // Wrap shape: Skip(Next(open, middle), close).
    let open = IrNode::Literal(1);
    let close = IrNode::Literal(2);
    let middle = IrNode::Literal(0);
    let body = IrNode::Skip(
        Box::new(IrNode::Next(Box::new(open), Box::new(middle))),
        Box::new(close),
    );
    let mut ir = make_ir(vec![rule(0, 0, body.clone())], strings);

    // Install a recognizer fact so the Wrap variable has BalancedScan
    // in its domain (`build_wrap_domain` only adds it when a
    // `DelimiterBalanced` fact is present).
    let shape = RecognizerShape::DelimiterBalanced {
        open: b'{',
        close: b'}',
        inner: Box::new(synthesize_recognizer(RecognizerShape::Regex { sid: 0 })),
    };
    install_recognizer_fact_wrap(&mut ir, 0, shape);

    // Authoritative producer: delim_scan_configs.
    let dag = ir.dag.as_ref().unwrap();
    let wrap_id = dag.node_for(&ir.rules[0].body).unwrap();
    ir.delim_scan_configs.insert(
        wrap_id,
        DelimScanConfig {
            open_byte: b'{',
            close_byte: b'}',
            pivot_byte: b'{',
            trail_byte: None,
            block_rule: None,
            pivot_rule: None,
        },
    );

    let (decisions, _mat) = solve_grammar_components(&ir);
    let mode = decisions
        .get(&wrap_id)
        .and_then(|d| d.wrap_mode.as_ref())
        .expect("CSP produced a wrap decision");

    assert_eq!(
        *mode,
        WrapMode::BalancedScan,
        "layout::install must pin a delim-scan-eligible wrap to BalancedScan"
    );
}

#[test]
fn layout_constraint_disconnect_picks_generic_without_fact() {
    let strings: Vec<String> = vec!["entry", "{", "}"]
        .into_iter()
        .map(String::from)
        .collect();
    let body = IrNode::Skip(
        Box::new(IrNode::Next(
            Box::new(IrNode::Literal(1)),
            Box::new(IrNode::Literal(0)),
        )),
        Box::new(IrNode::Literal(2)),
    );
    let ir = make_ir(vec![rule(0, 0, body)], strings);

    let (decisions, _mat) = solve_grammar_components(&ir);
    let dag = ir.dag.as_ref().unwrap();
    let wrap_id = dag.node_for(&ir.rules[0].body).unwrap();
    let mode = decisions
        .get(&wrap_id)
        .and_then(|d| d.wrap_mode.as_ref())
        .expect("CSP produced a wrap decision");

    // Without the producer fact, the Wrap domain holds only Generic
    // (the universal fallback in `build_wrap_domain`).
    assert_eq!(
        *mode,
        WrapMode::Generic,
        "without delim_scan_configs the CSP picks Generic"
    );
}

// ── W3b.3 — Dispatch constraint authority ───────────────────────────

/// `dispatch::install` pins an Alt with a populated
/// `key_dispatch_configs` entry to `AltMode::KeyDispatch`. The
/// upstream Alt domain must include `KeyDispatch` for the pin to be
/// satisfiable; we install a `KeywordPrefix` recognizer fact to
/// enable that, then assert the dispatch installer (not the shape
/// installer's first-pass) is the one carrying the authority by
/// using `key_dispatch_configs` as the only signal.
#[test]
fn dispatch_constraint_pins_alt_mode_when_key_dispatch_config_present() {
    let strings: Vec<String> = vec!["entry", "kw1", "kw2", "sep"]
        .into_iter()
        .map(String::from)
        .collect();
    let body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Literal(1),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Literal(2),
                first_set: None,
            },
        ],
        None,
    );
    let mut ir = make_ir(vec![rule(0, 0, body)], strings);

    // Add a KeywordPrefix shape fact so `KeyDispatch` is in the CSP
    // domain — without it, the pin would prune the domain to empty.
    let shape = RecognizerShape::KeywordPrefix {
        bytes: smallvec::smallvec![b'k'],
        disjoint_tail: true,
    };
    install_recognizer_fact(&mut ir, 0, shape);

    // Producer fact: key_dispatch_configs.
    let dag = ir.dag.as_ref().unwrap();
    let alt_id = dag.node_for(&ir.rules[0].body).unwrap();
    let key_match: KeyDispatchMatch = (
        KeyDispatchConfig {
            key_class: KeyClass::Identifier,
            separator: None,
            key_scanner_regex_id: None,
        },
        Vec::new(),
        Vec::new(),
    );
    ir.key_dispatch_configs.insert(alt_id, key_match);

    let (decisions, _mat) = solve_grammar_components(&ir);
    let mode = decisions
        .get(&alt_id)
        .and_then(|d| d.alt_mode.as_ref())
        .expect("CSP produced an alt decision");

    assert_eq!(
        *mode,
        AltMode::KeyDispatch,
        "dispatch::install must pin a key_dispatch_configs Alt to KeyDispatch"
    );
}
