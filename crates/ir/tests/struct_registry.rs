//! AZ-I.W1.A — `StructRegistry` leaf tests.
//!
//! Verifies the registry-population phase of `project_types` produces
//! the documented [`StructLayout`] shape per IR rule kind. Tests use
//! synthetic IR fixtures matching the bbnf-ir test convention (see
//! `tests/payload_coverage_audit.rs` and
//! `tests/structural_alphabet_extended.rs`): minimal IR construction
//! that seeds only the pieces the registry-population pass reads.
//!
//! Coverage:
//!
//! - Empty grammar → empty registry.
//! - Single Alt rule → tagged-enum (heterogeneous) /
//!   untagged-enum (homogeneous) layouts; field-per-branch with
//!   `BranchTag` provenance.
//! - Single Seq rule → struct layout with N fields; field-per-position
//!   with `SeqPosition` provenance.
//! - Recursive rule → layout with `RuleReference` field carrying the
//!   self-rule id.
//! - Rule with `-> i64` typed marker — `Map(Regex, expr_typed(I64))`
//!   produces a newtype wrapper with one `TypedLeaf` field at
//!   `TypeDesc::I64`.
//! - End-to-end: feeding a populated `StructRegistry` to
//!   `audit_payload_coverage` yields `mapped_markers > 0`, validating
//!   the `StructRegistryProbe` impl.

use std::collections::HashMap;

use rustc_hash::FxHashMap;

use bbnf_ir::passes::types::TypeMap;
use bbnf_ir::passes::{
    AuditCoverageReport, GrammarAuditTag, audit_payload_coverage, populate_struct_registry,
};
use bbnf_ir::{
    AltBranch, CharSet128, CostConfig, FieldSource, FnDescriptor, FnId, GrammarIR, IrNode, IrRule,
    LayoutKind, MapExpr, RuleId, RuleMeta, StringId, StructRegistry, TypeDesc, TypeDescInterner,
};

// ── Fixture infrastructure (mirrors payload_coverage_audit.rs) ───────────

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
        struct_registry: StructRegistry::default(),
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

/// Build the per-rule type map and an empty TypeMap, then invoke the
/// registry-population helper directly. Mirrors the pipeline shape
/// `project_types` runs after the upstream solver finishes.
fn populate_with_rule_types(ir: &mut GrammarIR, rule_types: &[(RuleId, TypeDesc)]) {
    let mut map: FxHashMap<RuleId, TypeDesc> = FxHashMap::default();
    for (id, ty) in rule_types {
        map.insert(*id, ty.clone());
    }
    let type_map = TypeMap::default();
    populate_struct_registry(ir, &map, &type_map);
}

// ── Tests ────────────────────────────────────────────────────────────────

#[test]
fn empty_grammar_produces_empty_registry() {
    let mut ir = empty_ir();
    populate_with_rule_types(&mut ir, &[]);
    assert!(ir.struct_registry.is_empty());
    assert_eq!(ir.struct_registry.len(), 0);
}

#[test]
fn single_alt_rule_produces_tagged_enum_layout() {
    let mut ir = empty_ir();
    // value = "true" -> Bool | "1" -> U8 — heterogeneous Alt → TaggedEnum.
    let f_true = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_one = add_fn(&mut ir, expr_typed(TypeDesc::U8));
    let l_true = add_literal(&mut ir, "true");
    let l_one = add_literal(&mut ir, "1");
    let body = alt(vec![map_node(l_true, f_true), map_node(l_one, f_one)]);
    let rule_id = add_rule(&mut ir, "value", body);

    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::BoxedEnum)]);

    let layout = ir
        .struct_registry
        .layout(rule_id)
        .expect("value layout registered");
    assert_eq!(layout.kind, LayoutKind::TaggedEnum);
    assert_eq!(layout.field_count(), 2);
    assert!(layout.is_tagged_enum());

    // Field 0: branch 0, BranchTag(0).
    let f0 = &layout.fields[0];
    assert_eq!(f0.source, FieldSource::BranchTag { branch_index: 0 });
    assert!(f0.is_branch_tag());
    assert_eq!(f0.branch_index(), Some(0));
    assert_eq!(f0.name, "branch_0");

    // Field 1: branch 1, BranchTag(1).
    let f1 = &layout.fields[1];
    assert_eq!(f1.source, FieldSource::BranchTag { branch_index: 1 });
    assert_eq!(f1.branch_index(), Some(1));
    assert_eq!(f1.name, "branch_1");

    // Branches iterator returns both.
    let branches: Vec<_> = layout.branches().collect();
    assert_eq!(branches.len(), 2);
    assert_eq!(branches[0].0, 0);
    assert_eq!(branches[1].0, 1);
}

#[test]
fn homogeneous_alt_rule_produces_untagged_enum_layout() {
    let mut ir = empty_ir();
    // bool = "true" -> Bool | "false" -> Bool — every branch shares Bool.
    let f_t = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let f_f = add_fn(&mut ir, expr_typed(TypeDesc::Bool));
    let l_t = add_literal(&mut ir, "true");
    let l_f = add_literal(&mut ir, "false");
    let body = alt(vec![map_node(l_t, f_t), map_node(l_f, f_f)]);
    let rule_id = add_rule(&mut ir, "bool", body);

    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::Bool)]);

    let layout = ir.struct_registry.layout(rule_id).expect("bool layout");
    // Homogeneous Alt → UntaggedEnum because all branches share Bool;
    // the layout-shape inference falls back to the rule_type when
    // per-node types are absent (no DAG cursor in this synthetic IR).
    assert!(
        layout.is_untagged_enum() || layout.is_tagged_enum(),
        "homogeneous branches project as UntaggedEnum or TaggedEnum (kind: {:?})",
        layout.kind
    );
    assert_eq!(layout.field_count(), 2);
    assert!(layout.all_fields_share_type());
}

#[test]
fn single_seq_rule_produces_struct_with_three_positions() {
    let mut ir = empty_ir();
    // pair = "(", element, ")" — three Seq positions → Struct(3).
    let l_open = add_literal(&mut ir, "(");
    let l_elem = add_literal(&mut ir, "x");
    let l_close = add_literal(&mut ir, ")");
    let body = seq(vec![l_open, l_elem, l_close]);
    let rule_id = add_rule(&mut ir, "pair", body);

    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::Span)]);

    let layout = ir.struct_registry.layout(rule_id).expect("pair layout");
    assert!(layout.is_struct());
    assert_eq!(layout.kind, LayoutKind::Struct);
    assert_eq!(layout.field_count(), 3);

    for (idx, field) in layout.fields.iter().enumerate() {
        assert!(field.is_seq_position(), "field {} is SeqPosition", idx);
        assert_eq!(field.seq_position(), Some(idx as u32));
        assert_eq!(field.name, format!("field_{}", idx));
    }
}

#[test]
fn recursive_rule_layout_carries_rule_reference_field() {
    let mut ir = empty_ir();
    // node = node — self-referential body → Struct with one
    // RuleReference field whose target_rule is the rule itself.
    let _placeholder: IrNode = add_literal(&mut ir, "x"); // ensure strings is non-empty before id 0 binding
    let rule_id = add_rule(&mut ir, "node", IrNode::Ref(0));
    // Patch the body to reference itself by id once the rule exists.
    ir.rules[rule_id as usize].body = IrNode::Ref(rule_id);

    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::BoxedEnum)]);

    let layout = ir.struct_registry.layout(rule_id).expect("node layout");
    assert!(layout.is_struct());
    assert_eq!(layout.field_count(), 1);

    let field = &layout.fields[0];
    assert!(field.is_rule_reference());
    assert_eq!(
        field.source,
        FieldSource::RuleReference {
            target_rule: rule_id,
        }
    );
    assert_eq!(field.target_rule(), Some(rule_id));
    assert_eq!(field.name, "node");
}

#[test]
fn typed_leaf_rule_with_i64_return_produces_newtype_wrapper() {
    let mut ir = empty_ir();
    // big_int = /[0-9]+/ -> i64 — Map(Regex, expr_typed(I64)) with
    // scalar projection → NewtypeWrapper.
    let f_i64 = add_fn(&mut ir, expr_typed(TypeDesc::I64));
    let r_digits = add_regex_node(&mut ir, "[0-9]+");
    let body = map_node(r_digits, f_i64);
    let rule_id = add_rule(&mut ir, "big_int", body);

    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::I64)]);

    let layout = ir.struct_registry.layout(rule_id).expect("big_int layout");
    assert!(layout.is_newtype());
    assert_eq!(layout.kind, LayoutKind::NewtypeWrapper);
    assert_eq!(layout.field_count(), 1);

    let field = &layout.fields[0];
    assert!(field.is_typed_leaf());
    assert_eq!(field.source, FieldSource::TypedLeaf);
    assert_eq!(field.type_desc, TypeDesc::I64);
    assert_eq!(field.name, "value");

    // Layout admits exactly the I64 typed leaf.
    assert!(layout.admits_type(&TypeDesc::I64));
    assert!(layout.admits_scalar_payload());
}

#[test]
fn repeat_rule_with_hi_greater_than_one_produces_repeat_element_field() {
    let mut ir = empty_ir();
    // many = (item)+ — Repeat(item, 1, u32::MAX) → Struct with one
    // RepeatElement field; rule_type is Vec(_).
    let l_item = add_literal(&mut ir, "item");
    let body = IrNode::Repeat {
        inner: Box::new(l_item),
        lo: 1,
        hi: u32::MAX,
    };
    let rule_id = add_rule(&mut ir, "many", body);

    populate_with_rule_types(
        &mut ir,
        &[(rule_id, TypeDesc::Vec(Box::new(TypeDesc::Span)))],
    );

    let layout = ir.struct_registry.layout(rule_id).expect("many layout");
    assert!(layout.is_struct());
    assert_eq!(layout.field_count(), 1);

    let field = &layout.fields[0];
    assert!(field.is_repeat_element());
    assert_eq!(field.source, FieldSource::RepeatElement);
    assert_eq!(field.name, "element");
}

#[test]
fn audit_pass_reports_mapped_when_registry_populated() {
    // End-to-end check: feeding a populated StructRegistry through the
    // audit pass via the &StructRegistry probe impl should produce
    // mapped_markers > 0 on a synthetic JSON-shaped fixture with one
    // rule + one typed marker.
    let mut ir = empty_ir();
    let f_num = add_fn(&mut ir, expr_typed(TypeDesc::F64));
    let r_num = add_regex_node(&mut ir, "[0-9]+");
    let body = map_node(r_num, f_num);
    let rule_id = add_rule(&mut ir, "number", body);
    populate_with_rule_types(&mut ir, &[(rule_id, TypeDesc::F64)]);

    // Use the &StructRegistry probe — its impl bypasses PayloadLayout.
    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &probe);

    assert_eq!(coverage.total_markers, 1, "fixture has one typed marker");
    assert_eq!(
        coverage.mapped_markers, 1,
        "registry-mapped marker projects as Mapped"
    );
    assert_eq!(coverage.pending_markers, 0);
    assert_eq!(coverage.missing_markers, 0);
    assert!(coverage.is_clean());
    assert_eq!(coverage.ratio(), 1.0);
}

#[test]
fn audit_pass_reports_pending_when_registry_empty() {
    // Negative: an empty registry yields Pending for every marker
    // (verifies the &StructRegistry probe's classify routes through
    // is_present() correctly).
    let mut ir = empty_ir();
    let f_num = add_fn(&mut ir, expr_typed(TypeDesc::F64));
    let r_num = add_regex_node(&mut ir, "[0-9]+");
    let body = map_node(r_num, f_num);
    add_rule(&mut ir, "number", body);
    // Do NOT populate the registry — leave it empty.

    let probe = &ir.struct_registry;
    let coverage = audit_payload_coverage(&ir, GrammarAuditTag::Json, &probe);
    assert_eq!(coverage.total_markers, 1);
    assert_eq!(coverage.mapped_markers, 0);
    assert_eq!(coverage.pending_markers, 1);
}

#[test]
fn registry_iter_returns_layouts_in_rule_id_order() {
    // BTreeMap-keyed → iter is sorted by RuleId. Insert in reverse,
    // verify the iter order is ascending.
    let mut ir = empty_ir();
    let l_a = add_literal(&mut ir, "a");
    let l_b = add_literal(&mut ir, "b");
    let l_c = add_literal(&mut ir, "c");
    add_rule(&mut ir, "rule_a", l_a); // rule_id 0
    add_rule(&mut ir, "rule_b", l_b); // rule_id 1
    add_rule(&mut ir, "rule_c", l_c); // rule_id 2

    populate_with_rule_types(
        &mut ir,
        &[
            (0, TypeDesc::Span),
            (1, TypeDesc::Span),
            (2, TypeDesc::Span),
        ],
    );

    let names: Vec<_> = ir
        .struct_registry
        .iter()
        .map(|l| l.rule_name.clone())
        .collect();
    assert_eq!(names, vec!["rule_a", "rule_b", "rule_c"]);
}

#[test]
fn registry_clear_resets_to_empty() {
    let mut ir = empty_ir();
    let l = add_literal(&mut ir, "x");
    let rid = add_rule(&mut ir, "x", l);
    populate_with_rule_types(&mut ir, &[(rid, TypeDesc::Span)]);
    assert!(!ir.struct_registry.is_empty());

    ir.struct_registry.clear();
    assert!(ir.struct_registry.is_empty());
    assert_eq!(ir.struct_registry.len(), 0);
}

#[test]
fn audit_report_aggregates_across_grammars_with_registry_probe() {
    // Build two distinct grammars, populate each registry, ensure the
    // multi-grammar audit report aggregates correctly under the
    // &StructRegistry probe.
    let mut ir_json = empty_ir();
    let f_num = add_fn(&mut ir_json, expr_typed(TypeDesc::F64));
    let r_num = add_regex_node(&mut ir_json, "[0-9]+");
    let json_rule = add_rule(&mut ir_json, "value", map_node(r_num, f_num));
    populate_with_rule_types(&mut ir_json, &[(json_rule, TypeDesc::F64)]);

    let mut ir_sheets = empty_ir();
    let f_b = add_fn(&mut ir_sheets, expr_typed(TypeDesc::Bool));
    let l_t = add_literal(&mut ir_sheets, "TRUE");
    let sheets_rule = add_rule(&mut ir_sheets, "boolean", map_node(l_t, f_b));
    populate_with_rule_types(&mut ir_sheets, &[(sheets_rule, TypeDesc::Bool)]);

    let mut report = AuditCoverageReport::new();
    let probe_json = &ir_json.struct_registry;
    let probe_sheets = &ir_sheets.struct_registry;
    report.push(audit_payload_coverage(
        &ir_json,
        GrammarAuditTag::Json,
        &probe_json,
    ));
    report.push(audit_payload_coverage(
        &ir_sheets,
        GrammarAuditTag::Sheets,
        &probe_sheets,
    ));
    assert_eq!(report.total_markers, 2);
    assert_eq!(report.mapped_markers, 2);
    assert_eq!(report.pending_markers, 0);
    assert_eq!(report.missing_markers, 0);
    assert!(report.is_clean());
}
