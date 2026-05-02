//! AZ-I.W0.2 — typed-`->` coverage audit pass tests.
//!
//! Verifies that [`bbnf_ir::passes::audit_payload_coverage`] produces
//! the documented per-grammar shape on synthetic IR fixtures of
//! JSON, CSS L4, and Sheets — the three data grammars whose
//! direct-to-struct migration AZ-I owns.
//!
//! The fixtures here are intentionally minimal. They construct only
//! the IR pieces the audit pass reads: rules with bodies that
//! contain `IrNode::Map` nodes whose `FnDescriptor` carries an
//! explicit return type or one of the compiler-internal typed-leaf
//! specialisations (`NumberConvert`, `HexConvert`, `SpanCapture`,
//! `EnumWrap`, `BoxWrap`). Per the IR crate's convention
//! (`tests/structural_alphabet_extended.rs`) the full pipeline-built
//! IR is overkill for an audit-pass test; wire-contract tests against
//! real grammars live in `bbnf-core` where the pipeline is in scope.
//!
//! Each fixture seeds at least one typed `->` site so the audit's
//! denominator is non-zero. Initial assertion under
//! [`AbsentRegistryProbe`] (the W0 baseline): every marker projects
//! as `Pending`. A second assertion under [`PayloadLayoutsProbe`]
//! with a hand-populated layout map demonstrates the `Mapped` state
//! materialising once a registry-equivalent surface admits a rule.
//! A third assertion populates a layout that fails to cover the
//! marker's typed leaf and verifies the `Missing` (red) state.
//! A final write-out exercises [`write_coverage_report`] and emits
//! `docs/benchmarks/AZ-I/W0/audit-coverage.json` for the build gate.

use std::collections::HashMap;
use std::path::PathBuf;

use bbnf_ir::passes::{
    AbsentRegistryProbe, AuditCoverageReport, GrammarAuditTag, PayloadField, PayloadLayout,
    PayloadLayoutsProbe, StructRegistryProbe, audit_payload_coverage, write_coverage_report,
};
use bbnf_ir::{
    AltBranch, CharSet128, CostConfig, FnDescriptor, FnId, GrammarIR, IrNode, IrRule, MapExpr,
    RuleId, RuleMeta, StringId, TypeDesc, TypeDescInterner,
};

// ── Fixture infrastructure ───────────────────────────────────────────────

const SENTINEL_ENTRY: u32 = u32::MAX;

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
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
    }
}

fn intern(ir: &mut GrammarIR, s: &str) -> StringId {
    if let Some(idx) = ir.strings.iter().position(|x| x == s) {
        return idx as StringId;
    }
    ir.strings.push(s.to_string());
    (ir.strings.len() - 1) as StringId
}

fn add_fn(ir: &mut GrammarIR, fd: FnDescriptor) -> FnId {
    let fid = ir.fns.len() as FnId;
    ir.fns.push(fd);
    fid
}

fn add_literal(ir: &mut GrammarIR, lit: &str) -> IrNode {
    let sid = intern(ir, lit);
    IrNode::Literal(sid)
}

fn add_regex_node(ir: &mut GrammarIR, pat: &str) -> IrNode {
    let sid = intern(ir, pat);
    IrNode::Regex(sid)
}

fn add_rule(ir: &mut GrammarIR, name: &str, body: IrNode) -> RuleId {
    let name_sid = intern(ir, name);
    let rule_id = ir.rules.len() as RuleId;
    ir.rules.push(IrRule {
        id: rule_id,
        name: name_sid,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
    rule_id
}

fn map_node(inner: IrNode, fn_id: FnId) -> IrNode {
    IrNode::Map {
        inner: Box::new(inner),
        fn_id,
    }
}

fn alt(branches: Vec<IrNode>) -> IrNode {
    IrNode::Alt(
        branches
            .into_iter()
            .map(|node| AltBranch {
                node,
                first_set: Some(CharSet128::new()),
            })
            .collect(),
        None,
    )
}

fn seq(children: Vec<IrNode>) -> IrNode {
    IrNode::Seq(children)
}

fn expr_typed(return_type: TypeDesc) -> FnDescriptor {
    FnDescriptor::Expr {
        expr: MapExpr::Input,
        return_type: Some(return_type),
    }
}

// ── Per-grammar fixture builders ─────────────────────────────────────────

/// Synthetic JSON IR seed — `value` rule with five typed `->`
/// markers (true / false / null / number / string), faithful in
/// shape to `grammar/json/json.bbnf`.
fn json_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let f_true = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_false = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_null = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_num = add_fn(
        &mut ir,
        FnDescriptor::NumberConvert {
            allow_leading_dot: false,
        },
    );
    let f_str = add_fn(&mut ir, FnDescriptor::SpanCapture);
    let l_true = add_literal(&mut ir, "true");
    let l_false = add_literal(&mut ir, "false");
    let l_null = add_literal(&mut ir, "null");
    let r_num = add_regex_node(&mut ir, "[0-9]+");
    let r_str = add_regex_node(&mut ir, "\"[^\"]*\"");
    let body = alt(vec![
        map_node(l_true, f_true),
        map_node(l_false, f_false),
        map_node(l_null, f_null),
        map_node(r_num, f_num),
        map_node(r_str, f_str),
    ]);
    add_rule(&mut ir, "value", body);
    ir
}

/// Synthetic Sheets IR seed — five typed `->` markers on the
/// scalar primitives Sheets converts inline (`add_op`, `mul_op`,
/// `boolean`, `number`, `reference`).
fn sheets_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let f_plus = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_minus = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_times = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_bool_t = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_bool_f = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_num = add_fn(
        &mut ir,
        FnDescriptor::NumberConvert {
            allow_leading_dot: true,
        },
    );
    let f_ref = add_fn(&mut ir, FnDescriptor::SpanCapture);

    let l_plus = add_literal(&mut ir, "+");
    let l_minus = add_literal(&mut ir, "-");
    let l_times = add_literal(&mut ir, "*");
    let l_true = add_literal(&mut ir, "TRUE");
    let l_false = add_literal(&mut ir, "FALSE");
    let r_num = add_regex_node(&mut ir, "[0-9]+(\\.[0-9]+)?");
    let r_ref = add_regex_node(&mut ir, "\\$?[A-Z]+\\$?[0-9]+");

    add_rule(
        &mut ir,
        "add_op",
        alt(vec![map_node(l_plus, f_plus), map_node(l_minus, f_minus)]),
    );
    add_rule(&mut ir, "mul_op", map_node(l_times, f_times));
    add_rule(
        &mut ir,
        "boolean",
        alt(vec![
            map_node(l_true, f_bool_t),
            map_node(l_false, f_bool_f),
        ]),
    );
    add_rule(&mut ir, "number", map_node(r_num, f_num));
    add_rule(&mut ir, "reference", map_node(r_ref, f_ref));
    ir
}

/// Synthetic CSS L4 IR seed — typed `->` markers on `<length>`
/// units, the `<color>` hex form, and the `<percentage>` form. Six
/// markers total — enough to exercise the per-rule grouping and
/// the `Missing` red signal in the third assertion.
fn css_l4_fixture() -> GrammarIR {
    let mut ir = empty_ir();
    let f_px = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_em = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_rem = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let f_pct = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let hex_path_sid = intern(&mut ir, "color::from_hex");
    let f_hex = add_fn(
        &mut ir,
        FnDescriptor::HexConvert {
            fn_path: hex_path_sid,
        },
    );
    let color_enum_sid = intern(&mut ir, "ColorEnum");
    let f_named = add_fn(&mut ir, expr_typed(TypeDesc::Named(color_enum_sid)));

    let l_px = add_literal(&mut ir, "px");
    let l_em = add_literal(&mut ir, "em");
    let l_rem = add_literal(&mut ir, "rem");
    let l_pct = add_literal(&mut ir, "%");
    let r_hex = add_regex_node(&mut ir, "#[0-9a-fA-F]{3,8}");
    let l_red = add_literal(&mut ir, "red");
    let r_num = add_regex_node(&mut ir, "[0-9]+");

    add_rule(
        &mut ir,
        "length_unit",
        alt(vec![
            map_node(l_px, f_px),
            map_node(l_em, f_em),
            map_node(l_rem, f_rem),
        ]),
    );
    add_rule(&mut ir, "percentage", map_node(l_pct, f_pct));
    add_rule(
        &mut ir,
        "color",
        alt(vec![map_node(r_hex, f_hex), map_node(l_red, f_named)]),
    );
    add_rule(&mut ir, "length", seq(vec![r_num, IrNode::Ref(0)]));
    ir
}

// ── Probe helpers ────────────────────────────────────────────────────────

/// Probe with a hand-populated layout map — used to demonstrate the
/// `Mapped` and `Missing` states without depending on a real
/// `StructRegistry` (W1's substrate).
struct StaticLayoutProbe {
    layouts: HashMap<RuleId, PayloadLayout>,
}

impl StructRegistryProbe for StaticLayoutProbe {
    fn layout(&self, rule_id: RuleId) -> Option<&PayloadLayout> {
        self.layouts.get(&rule_id)
    }
}

fn scalar_layout(ty: TypeDesc) -> PayloadLayout {
    let bytes = ty.payload_size_bytes().unwrap_or(8);
    PayloadLayout {
        fields: vec![PayloadField { ty, offset: 0 }],
        total_bytes: bytes,
    }
}

// ── W0-baseline assertions ───────────────────────────────────────────────

#[test]
fn w0_baseline_pending_status_for_every_grammar() {
    let absent = AbsentRegistryProbe;
    for (tag, ir) in [
        (GrammarAuditTag::Json, json_fixture()),
        (GrammarAuditTag::CssL4, css_l4_fixture()),
        (GrammarAuditTag::Sheets, sheets_fixture()),
    ] {
        let coverage = audit_payload_coverage(&ir, tag.clone(), &absent);
        assert!(
            coverage.total_markers > 0,
            "grammar {} has zero typed markers — fixture is bad",
            tag.key()
        );
        assert_eq!(
            coverage.mapped_markers,
            0,
            "grammar {} should be 0% mapped under AbsentRegistryProbe",
            tag.key()
        );
        assert_eq!(
            coverage.pending_markers,
            coverage.total_markers,
            "grammar {} should report every marker as Pending under \
             AbsentRegistryProbe",
            tag.key()
        );
        assert_eq!(
            coverage.missing_markers,
            0,
            "grammar {} should report no Missing markers in W0 baseline",
            tag.key()
        );
        assert_eq!(coverage.pending.len(), coverage.pending_markers);
        assert!(coverage.is_clean(), "no Missing → audit is clean");
        assert_eq!(coverage.ratio(), 0.0);
    }
}

#[test]
fn json_per_rule_grouping_lists_value_with_five_markers() {
    let ir = json_fixture();
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &AbsentRegistryProbe);
    assert_eq!(coverage.total_markers, 5);
    assert_eq!(coverage.markers_per_rule.len(), 1);
    let (rule_name, count) = &coverage.markers_per_rule[0];
    assert_eq!(rule_name, "value");
    assert_eq!(*count, 5);
}

#[test]
fn sheets_fixture_produces_seven_markers_across_five_rules() {
    let ir = sheets_fixture();
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Sheets, &AbsentRegistryProbe);
    assert_eq!(coverage.total_markers, 7);
    assert_eq!(coverage.markers_per_rule.len(), 5);
    let names: Vec<&str> = coverage
        .markers_per_rule
        .iter()
        .map(|(n, _)| n.as_str())
        .collect();
    assert!(names.contains(&"add_op"));
    assert!(names.contains(&"boolean"));
    assert!(names.contains(&"number"));
    assert!(names.contains(&"reference"));
    assert!(names.contains(&"mul_op"));
}

#[test]
fn css_l4_fixture_groups_color_and_length_unit_separately() {
    let ir = css_l4_fixture();
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::CssL4, &AbsentRegistryProbe);
    // 3 length_unit + 1 percentage + 2 color = 6.
    assert_eq!(coverage.total_markers, 6);
    let length_unit_count: usize = coverage
        .markers_per_rule
        .iter()
        .find(|(n, _)| n == "length_unit")
        .map(|(_, c)| *c)
        .expect("length_unit row");
    assert_eq!(length_unit_count, 3);
    let color_count: usize = coverage
        .markers_per_rule
        .iter()
        .find(|(n, _)| n == "color")
        .map(|(_, c)| *c)
        .expect("color row");
    assert_eq!(color_count, 2);
}

// ── W1-rehearsal assertions — Mapped + Missing ──────────────────────────

#[test]
fn mapped_status_when_layout_admits_marker_type() {
    let ir = sheets_fixture();
    // Map every Sheets rule to a scalar layout that covers the
    // marker's typed leaf.
    let mut layouts = HashMap::new();
    for rule in &ir.rules {
        layouts.insert(rule.id, scalar_layout(TypeDesc::U8));
    }
    let probe = StaticLayoutProbe { layouts };
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Sheets, &probe);
    assert_eq!(coverage.mapped_markers, coverage.total_markers);
    assert_eq!(coverage.pending_markers, 0);
    assert_eq!(coverage.missing_markers, 0);
    assert!(coverage.is_clean());
    assert_eq!(coverage.ratio(), 1.0);
}

#[test]
fn pending_and_missing_are_distinguished_in_same_grammar() {
    let ir = sheets_fixture();
    // Cover only the `add_op` rule (rule id 0); leave the other rules
    // unmapped → mix of Mapped + Pending.
    let mut layouts = HashMap::new();
    layouts.insert(0u32, scalar_layout(TypeDesc::U8));
    let probe = StaticLayoutProbe { layouts };
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Sheets, &probe);
    assert!(coverage.mapped_markers > 0);
    assert!(coverage.pending_markers > 0);
    assert_eq!(coverage.missing_markers, 0);
}

#[test]
fn missing_status_when_layout_fails_to_cover_marker_type() {
    let ir = json_fixture();
    // Construct a layout whose only field is a non-scalar
    // `TypeDesc::Enum` — the `layout_covers` default-impl scalar
    // arm rejects it, producing the Missing red signal for any
    // marker whose typed leaf is a scalar (Bool, U8, F64, Span).
    //
    // Because `TypeDesc::Enum` is *not* `is_scalar_payload`, the
    // probe's covers() returns false on every JSON marker. Every
    // marker therefore projects as Missing.
    let mut layouts = HashMap::new();
    for rule in &ir.rules {
        layouts.insert(
            rule.id,
            PayloadLayout {
                fields: vec![PayloadField {
                    ty: TypeDesc::Enum,
                    offset: 0,
                }],
                total_bytes: 8,
            },
        );
    }
    let probe = StaticLayoutProbe { layouts };
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &probe);
    // 3 markers (true/false/null with typed leaf U8 / Bool) project
    // Missing under the default `layout_covers`. The number /
    // string markers (NumberConvert → F64, SpanCapture → Span) are
    // also scalar typed leaves — so all 5 project Missing.
    assert!(
        coverage.missing_markers >= 3,
        "expected ≥3 Missing markers; got {}",
        coverage.missing_markers
    );
    assert_eq!(coverage.pending_markers, 0);
    assert!(!coverage.is_clean(), "Missing markers ⇒ not clean");
}

// ── Multi-grammar JSON output ────────────────────────────────────────────

#[test]
fn write_audit_coverage_json_to_w0_path() {
    let absent = AbsentRegistryProbe;
    let mut report = AuditCoverageReport::new();
    for (tag, ir) in [
        (GrammarAuditTag::Json, json_fixture()),
        (GrammarAuditTag::CssL4, css_l4_fixture()),
        (GrammarAuditTag::Sheets, sheets_fixture()),
    ] {
        let coverage = audit_payload_coverage(&ir, tag, &absent);
        report.push(coverage);
    }
    assert_eq!(report.grammars.len(), 3);
    assert!(report.grammars.contains_key("json"));
    assert!(report.grammars.contains_key("css_l4"));
    assert!(report.grammars.contains_key("sheets"));
    assert!(report.total_markers > 0);
    assert_eq!(report.pending_markers, report.total_markers);
    assert_eq!(report.missing_markers, 0);

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path =
        PathBuf::from(manifest_dir).join("../../docs/benchmarks/AZ-I/W0/audit-coverage.json");
    let bytes_written = write_coverage_report(&report, &path).expect("write coverage report");
    assert!(bytes_written > 0);
    assert!(path.exists());

    // Round-trip the JSON to confirm the output is valid and the
    // coverage rows survive serialise → deserialise unchanged.
    let raw = std::fs::read_to_string(&path).expect("read written report");
    let round: AuditCoverageReport = serde_json::from_str(&raw).expect("deserialize round trip");
    assert_eq!(round.grammars.len(), report.grammars.len());
    assert_eq!(round.total_markers, report.total_markers);
}

#[test]
fn payload_layouts_probe_falls_back_to_pending_when_layouts_empty() {
    let ir = json_fixture();
    let probe = PayloadLayoutsProbe::new(&ir);
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &probe);
    // Empty payload_layouts on the synthetic IR ⇒ every marker
    // projects as Pending — same as AbsentRegistryProbe.
    assert_eq!(coverage.mapped_markers, 0);
    assert_eq!(coverage.pending_markers, coverage.total_markers);
    assert_eq!(coverage.missing_markers, 0);
}
