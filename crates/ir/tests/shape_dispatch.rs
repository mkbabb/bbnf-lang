//! AW-V.W3.1 — Shape-dispatch classifier tests.
//!
//! Validates the per-shape detectors against synthetic IRs that
//! mirror JSON's rule shapes — the hard gate from AW-V.md §W3.1
//! requires JSON's 6 value-producing rules (object / array / string
//! / number / bool-keyword / null-keyword) to receive the expected
//! tags. End-to-end coverage over the full pipeline (CSS / Sheets /
//! BBNF classifications) is verified at the bbnf crate level by
//! downstream parity tests; these synthetic tests keep the bbnf-ir
//! crate's dependency boundary clean while still exercising the
//! per-grammar MECHANISM the §6 invariant requires.
//!
//! # Test strategy
//!
//! Each JSON rule is constructed via minimal IR builders that
//! reproduce the exact `IrNode` topology the lowering pass emits
//! for `grammar/json/json.bbnf`:
//!
//! - `null = "null"` → `Literal(...)`
//! - `bool = "true" | "false"` → `Alt([Literal, Literal])`
//! - `number = /…/` → `Regex(...)` with Numeric classification
//! - `string = /"…"/` → `Regex(...)` with QuotedString classification
//! - `array = "[" >> (value<<comma?)* << "]"` → Wrap over Repeat(value)
//! - `object = "{" >> (pair<<comma?)* << "}"` → Wrap over Repeat(pair)
//! - `pair = string, colon >> value` → Seq([key, colon, value])
//! - `value = object | array | string | number | bool | null` → Alt
//!
//! The classifier runs after `mine_recognizers` + `compute_regex_info`
//! so every detector reads authoritative miner outputs.

use std::collections::HashMap;

use bbnf_ir::dag::GrammarDag;
use bbnf_ir::passes::{
    compute_regex_info, mine_recognizers,
    recognizers::shape_dispatch::{ShapeTag, shape_dispatch},
};
use bbnf_ir::{
    AltBranch, CostConfig, GrammarIR, IrNode, IrRule, RuleId, RuleMeta, TypeDescInterner,
};

// ─── Fixture builders ────────────────────────────────────────────────

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
        eclass_facts: HashMap::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        struct_registry: bbnf_ir::StructRegistry::default(),
        type_obligations: Vec::new(),
        inline_trace: bbnf_ir::passes::inline_trace::InlineTrace::default(),
        path_check_resolver: bbnf_ir::passes::path_check::PathCheckResolver::default(),
        shape_assignments: bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(
        ),
    }
}

/// Intern a string and return its `StringId`.
fn intern(ir: &mut GrammarIR, s: &str) -> u32 {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    sid
}

/// Build a `Literal(sid)` node after interning `s`.
fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = intern(ir, s);
    IrNode::Literal(sid)
}

/// Build a `Regex(sid)` node after interning the pattern.
fn regex(ir: &mut GrammarIR, pattern: &str) -> IrNode {
    let sid = intern(ir, pattern);
    IrNode::Regex(sid)
}

/// Push a rule with the given body; returns its `RuleId`.
fn push_rule(ir: &mut GrammarIR, name: &str, body: IrNode) -> RuleId {
    let name_id = intern(ir, name);
    let id = ir.rules.len() as RuleId;
    ir.rules.push(IrRule {
        id,
        name: name_id,
        body,
        meta: RuleMeta::default(),
        source_span: None,
    });
    id
}

/// Wrap an inner node in `Map { inner, fn_id: 0 }` to mimic `->` typed
/// annotation without actually declaring a real fn.
fn mapped(inner: IrNode) -> IrNode {
    IrNode::Map {
        inner: Box::new(inner),
        fn_id: 0,
    }
}

/// Run the post-W3.1 orchestration: build DAG, classify regex
/// patterns, run the recognizer miner (which internally runs
/// `shape_dispatch` as its tail pass).
fn finalise_and_classify(ir: &mut GrammarIR) {
    ir.dag = Some(GrammarDag::from_ir(ir));
    compute_regex_info(ir);
    mine_recognizers(ir);
}

// ─── JSON grammar fixture ────────────────────────────────────────────

/// Build a GrammarIR matching the structure of `grammar/json/json.bbnf`.
///
/// Returns the tuple of rule ids for every named rule so tests can
/// assert per-rule classifications without re-looking them up.
struct JsonRules {
    null: RuleId,
    bool_rule: RuleId,
    number: RuleId,
    comma: RuleId,
    colon: RuleId,
    string: RuleId,
    pair: RuleId,
    array: RuleId,
    object: RuleId,
    value: RuleId,
}

fn build_json_ir() -> (GrammarIR, JsonRules) {
    let mut ir = base_ir();

    // null = "null" -> 0u8
    let null_body = mapped(lit(&mut ir, "null"));
    let null = push_rule(&mut ir, "null", null_body);

    // bool = "true" -> true | "false" -> false
    let true_lit = mapped(lit(&mut ir, "true"));
    let false_lit = mapped(lit(&mut ir, "false"));
    let bool_body = IrNode::Alt(
        vec![
            AltBranch {
                node: true_lit,
                first_set: None,
            },
            AltBranch {
                node: false_lit,
                first_set: None,
            },
        ],
        None,
    );
    let bool_rule = push_rule(&mut ir, "bool", bool_body);

    // number = /-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?/ -> f64
    let num_body = mapped(regex(&mut ir, r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"));
    let number = push_rule(&mut ir, "number", num_body);

    // comma = "," ?w
    let comma_body = IrNode::OptionalWhitespace(Box::new(lit(&mut ir, ",")));
    let comma = push_rule(&mut ir, "comma", comma_body);

    // colon = ":" ?w
    let colon_body = IrNode::OptionalWhitespace(Box::new(lit(&mut ir, ":")));
    let colon = push_rule(&mut ir, "colon", colon_body);

    // string = /"..."/  -> decode_json_string(input)
    let str_body = mapped(regex(
        &mut ir,
        r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
    ));
    let string = push_rule(&mut ir, "string", str_body);

    // Value forward decl: we need the rule id before building pair /
    // array / object. Use a placeholder body and patch later.
    let value = push_rule(&mut ir, "value", IrNode::Epsilon);

    // pair = string, colon >> value  —  Seq(string, Next(colon, value))
    let pair_body = IrNode::Seq(vec![
        IrNode::Ref(string),
        IrNode::Next(Box::new(IrNode::Ref(colon)), Box::new(IrNode::Ref(value))),
    ]);
    let pair = push_rule(&mut ir, "pair", pair_body);

    // array = "[" >> ((value << comma?)*)?w << "]"
    //       = Next(Literal("["), Skip(Repeat(Skip(Ref(value), Repeat(comma,0,1)), 0, inf), Literal("]")))
    let arr_open = lit(&mut ir, "[");
    let arr_close = lit(&mut ir, "]");
    let arr_repeat_inner = IrNode::Skip(
        Box::new(IrNode::Ref(value)),
        Box::new(IrNode::Repeat {
            inner: Box::new(IrNode::Ref(comma)),
            lo: 0,
            hi: 1,
        }),
    );
    let arr_repeat = IrNode::Repeat {
        inner: Box::new(arr_repeat_inner),
        lo: 0,
        hi: u32::MAX,
    };
    let arr_middle = IrNode::OptionalWhitespace(Box::new(arr_repeat));
    let array_body = IrNode::Next(
        Box::new(arr_open),
        Box::new(IrNode::Skip(Box::new(arr_middle), Box::new(arr_close))),
    );
    let array = push_rule(&mut ir, "array", array_body);

    // object = "{" >> ((pair << comma?)*)?w << "}"
    let obj_open = lit(&mut ir, "{");
    let obj_close = lit(&mut ir, "}");
    let obj_repeat_inner = IrNode::Skip(
        Box::new(IrNode::Ref(pair)),
        Box::new(IrNode::Repeat {
            inner: Box::new(IrNode::Ref(comma)),
            lo: 0,
            hi: 1,
        }),
    );
    let obj_repeat = IrNode::Repeat {
        inner: Box::new(obj_repeat_inner),
        lo: 0,
        hi: u32::MAX,
    };
    let obj_middle = IrNode::OptionalWhitespace(Box::new(obj_repeat));
    let object_body = IrNode::Next(
        Box::new(obj_open),
        Box::new(IrNode::Skip(Box::new(obj_middle), Box::new(obj_close))),
    );
    let object = push_rule(&mut ir, "object", object_body);

    // value = object | array | string | number | bool | null
    ir.rules[value as usize].body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(object),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(array),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(string),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(number),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(bool_rule),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(null),
                first_set: None,
            },
        ],
        None,
    );

    ir.entry = value;

    let rules = JsonRules {
        null,
        bool_rule,
        number,
        comma,
        colon,
        string,
        pair,
        array,
        object,
        value,
    };

    finalise_and_classify(&mut ir);

    (ir, rules)
}

// ─── JSON hard-gate tests ────────────────────────────────────────────

#[test]
fn json_object_classified_as_object() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.object),
        ShapeTag::Object,
        "JSON `object` rule must classify as Object-shape"
    );
}

#[test]
fn json_array_classified_as_array() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.array),
        ShapeTag::Array,
        "JSON `array` rule must classify as Array-shape"
    );
}

#[test]
fn json_string_classified_as_string() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.string),
        ShapeTag::String,
        "JSON `string` rule must classify as String-shape"
    );
}

#[test]
fn json_number_classified_as_number() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.number),
        ShapeTag::Number,
        "JSON `number` rule must classify as Number-shape"
    );
}

#[test]
fn json_bool_classified_as_keyword() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.bool_rule),
        ShapeTag::Keyword,
        "JSON `bool` rule must classify as Keyword-shape"
    );
}

#[test]
fn json_null_classified_as_keyword() {
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.null),
        ShapeTag::Keyword,
        "JSON `null` rule must classify as Keyword-shape"
    );
}

#[test]
fn json_six_primary_rules_cover_hard_gate() {
    // AW-V.md §W3.1 hard gate: JSON's six primary rules get expected
    // tags. The dispatcher rule `value` plus the structural `comma` /
    // `colon` / `pair` rules stay on the walker fallback (W4 admits
    // them via Wrap / Flat).
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.get(rules.object), ShapeTag::Object);
    assert_eq!(ir.shape_assignments.get(rules.array), ShapeTag::Array);
    assert_eq!(ir.shape_assignments.get(rules.string), ShapeTag::String);
    assert_eq!(ir.shape_assignments.get(rules.number), ShapeTag::Number);
    assert_eq!(ir.shape_assignments.get(rules.bool_rule), ShapeTag::Keyword);
    assert_eq!(ir.shape_assignments.get(rules.null), ShapeTag::Keyword);
    assert!(
        ir.shape_assignments.classified_count() >= 6,
        "classified_count < 6 — hard gate requires six primary rules"
    );
}

#[test]
fn json_value_classifies_as_wrap_under_w4() {
    // The `value` rule's Alt-of-Refs dispatcher is W4's `Wrap` shape.
    // W4.1 activates the detector; the classifier now admits it.
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.value),
        ShapeTag::Wrap,
        "`value` is W4's Wrap shape (Alt of Refs to shape rules)"
    );
}

#[test]
fn json_pair_classifies_as_flat_under_w4_fix() {
    // AW-V.W4-fix — `pair = string, colon >> value` is a typed Seq
    // of three Refs. The W4.1 Flat detector rejected Ref-headed Seqs;
    // the W4-fix extends admission to include them, so `pair` now
    // classifies as Flat. This is harmless under the current W3
    // emitter gate — W4 emitters are not consumed while emitter
    // bodies are being specialised, so `pair` continues to route
    // through the walker at parse time.
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.pair),
        ShapeTag::Flat,
        "W4-fix: Ref-headed typed Seq classifies as Flat"
    );
}

#[test]
fn json_structural_separators_classify_as_keyword_or_scalar() {
    // comma / colon are `?w`-wrapped single-byte literals — their
    // body unwraps to a bare Literal, which the Keyword detector
    // admits (single-literal case). Scalar is the fallback.
    let (ir, rules) = build_json_ir();
    let comma_tag = ir.shape_assignments.get(rules.comma);
    let colon_tag = ir.shape_assignments.get(rules.colon);
    assert!(
        matches!(comma_tag, ShapeTag::Keyword | ShapeTag::Scalar),
        "`comma` classified as {comma_tag:?}; expected Keyword or Scalar"
    );
    assert!(
        matches!(colon_tag, ShapeTag::Keyword | ShapeTag::Scalar),
        "`colon` classified as {colon_tag:?}; expected Keyword or Scalar"
    );
}

// ─── W4 detector activation markers ──────────────────────────────────
//
// JSON exercises Wrap via its `value = object | array | ...`
// dispatcher; it has no Pratt / Unordered / ArgList / Flat / HRegex
// rules. These tests assert the W4 detectors fire (or not) per the
// JSON grammar's shape. Per-grammar coverage for Sheets / CSS / BBNF
// lands in the bbnf crate's parity tests.

#[test]
fn w4_pratt_no_hits_on_json_grammar() {
    // An operator-chain head is W4's `Pratt` shape. The JSON grammar
    // has no operator chains.
    let (ir, _) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Pratt),
        0,
        "JSON grammar has no Pratt rules"
    );
}

#[test]
fn w4_unordered_no_hits_on_json_grammar() {
    // CSS `compoundSelector`-shaped Repeat-over-disjoint-FIRST-Alt
    // is `Unordered`. JSON has none.
    let (ir, _) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Unordered),
        0,
        "JSON grammar has no Unordered rules"
    );
}

#[test]
fn w4_arglist_no_hits_on_json_grammar() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::ArgList), 0);
}

#[test]
fn w4_flat_classifies_json_pair() {
    // AW-V.W4-fix — the Flat detector now admits Ref-headed typed
    // Seqs. JSON's `pair = string, colon >> value` is the only
    // rule in the JSON fixture that matches, so exactly one Flat
    // hit.
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Flat), 1);
    assert_eq!(ir.shape_assignments.get(rules.pair), ShapeTag::Flat);
}

#[test]
fn w4_wrap_classifies_json_value_dispatcher() {
    // JSON's `value = object | array | string | number | bool | null`
    // is the canonical Wrap shape — exactly one rule in the grammar.
    let (ir, rules) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Wrap), 1);
    assert_eq!(
        ir.shape_assignments.get(rules.value),
        ShapeTag::Wrap,
        "`value` classifies as Wrap"
    );
}

#[test]
fn w4_hregex_no_hits_on_json_grammar() {
    // JSON's sole Regex leaves are `string` (QuotedString → String
    // shape) and `number` (Numeric → Number shape). No HRegex.
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::HRegex), 0);
}

// ─── Direct `shape_dispatch` invocation (without orchestrator) ───────

#[test]
fn shape_dispatch_is_idempotent() {
    // Running the classifier twice over the same IR yields identical
    // assignments — it's a pure projection.
    let (mut ir, _) = build_json_ir();
    let direct = shape_dispatch(&mut ir);
    assert_eq!(direct.per_rule.len(), ir.shape_assignments.per_rule.len());
    for (rule, tag) in &ir.shape_assignments.per_rule {
        assert_eq!(
            direct.per_rule.get(rule).copied(),
            Some(*tag),
            "shape_dispatch is not idempotent at rule {rule}: got {:?}, expected {:?}",
            direct.per_rule.get(rule),
            Some(tag)
        );
    }
}

// ─── W4 per-shape synthetic fixtures ─────────────────────────────────
//
// Each test builds a minimal IR with one rule of the canonical shape
// and asserts the classifier tags it correctly. Synthetic fixtures
// mirror the grammar sources cited in the per-shape detector modules'
// doc-comments.

/// Build a Pratt-shape IR: one rule whose body matches the canonical
/// operator-chain rung `Seq(operand_ref, Repeat(Seq(op_ref, operand_ref)))`
/// — mined by [`node_facts::recognize_tree`] as `operator_chain: true`.
fn build_pratt_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let op_body = IrNode::Alt(
        vec![
            AltBranch {
                node: lit(&mut ir, "+"),
                first_set: None,
            },
            AltBranch {
                node: lit(&mut ir, "-"),
                first_set: None,
            },
        ],
        None,
    );
    let op_rule = push_rule(&mut ir, "op", op_body);
    let operand_body = mapped(regex(&mut ir, r"[0-9]+"));
    let operand_rule = push_rule(&mut ir, "operand", operand_body);
    // expr = operand, (op, operand)*
    let repeat_inner = IrNode::Seq(vec![IrNode::Ref(op_rule), IrNode::Ref(operand_rule)]);
    let expr_body = IrNode::Seq(vec![
        IrNode::Ref(operand_rule),
        IrNode::Repeat {
            inner: Box::new(repeat_inner),
            lo: 0,
            hi: u32::MAX,
        },
    ]);
    let expr_rule = push_rule(&mut ir, "expr", expr_body);
    ir.entry = expr_rule;
    finalise_and_classify(&mut ir);
    (ir, expr_rule)
}

#[test]
fn w4_pratt_detector_admits_operator_chain_rung() {
    let (ir, expr_rule) = build_pratt_fixture();
    assert_eq!(
        ir.shape_assignments.get(expr_rule),
        ShapeTag::Pratt,
        "operator-chain rung body must classify as Pratt"
    );
}

/// CSS math operator chains use `(op >> operand)` inside the Repeat —
/// i.e. `IrNode::Next`, not `IrNode::Seq`. Before AW-V.W4-fix the
/// detector's `is_operator_chain_tail` required a bare `Seq` and
/// rejected every CSS math chain. This fixture mirrors CSS's
/// `mathProduct = mathValue , (("*"|"/") >> mathValue)*` topology —
/// `Next(Alt_op, Ref(operand))` inside the outer `Repeat`.
fn build_pratt_next_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let operand_body = mapped(regex(&mut ir, r"[0-9]+"));
    let operand_rule = push_rule(&mut ir, "operand", operand_body);
    // expr = operand, (("*" | "/") >> operand)*
    let op_alt = IrNode::Alt(
        vec![
            AltBranch {
                node: lit(&mut ir, "*"),
                first_set: None,
            },
            AltBranch {
                node: lit(&mut ir, "/"),
                first_set: None,
            },
        ],
        None,
    );
    let repeat_inner = IrNode::Next(Box::new(op_alt), Box::new(IrNode::Ref(operand_rule)));
    let expr_body = IrNode::Seq(vec![
        IrNode::Ref(operand_rule),
        IrNode::Repeat {
            inner: Box::new(repeat_inner),
            lo: 0,
            hi: u32::MAX,
        },
    ]);
    let expr_rule = push_rule(&mut ir, "expr", expr_body);
    ir.entry = expr_rule;
    finalise_and_classify(&mut ir);
    (ir, expr_rule)
}

#[test]
fn w4_pratt_detector_admits_next_based_operator_chain() {
    // AW-V.W4-fix regression: the detector must admit `(op >> operand)`
    // shape — CSS's mathProduct / mathExpr body — in addition to the
    // Sheets-style `(op, operand)` Seq shape. Pre-fix the detector's
    // is_operator_chain_tail required `IrNode::Seq(_)` inside the
    // Repeat; CSS math chains failed admission because their `>>`
    // combinator lowers to `IrNode::Next`, not a bare Seq.
    let (ir, expr_rule) = build_pratt_next_fixture();
    assert_eq!(
        ir.shape_assignments.get(expr_rule),
        ShapeTag::Pratt,
        "Next-based operator chain (`(op >> operand)*`) must classify as Pratt — \
         mirrors CSS mathProduct / mathExpr body topology"
    );
}

/// `Skip` is the dual of `Next` (`a << b` keeps left); the Pratt
/// detector must admit it via the same projection. No canonical
/// grammar uses Skip inside an operator-chain repeat, but the
/// structural admission is symmetric and cheap to verify.
#[test]
fn w4_pratt_detector_admits_skip_based_operator_chain() {
    let mut ir = base_ir();
    let operand_body = mapped(regex(&mut ir, r"[0-9]+"));
    let operand_rule = push_rule(&mut ir, "operand", operand_body);
    let op_body = mapped(lit(&mut ir, "+"));
    let op_rule = push_rule(&mut ir, "op", op_body);
    let repeat_inner = IrNode::Skip(
        Box::new(IrNode::Ref(op_rule)),
        Box::new(IrNode::Ref(operand_rule)),
    );
    let expr_body = IrNode::Seq(vec![
        IrNode::Ref(operand_rule),
        IrNode::Repeat {
            inner: Box::new(repeat_inner),
            lo: 0,
            hi: u32::MAX,
        },
    ]);
    let expr_rule = push_rule(&mut ir, "expr", expr_body);
    ir.entry = expr_rule;
    finalise_and_classify(&mut ir);
    assert_eq!(
        ir.shape_assignments.get(expr_rule),
        ShapeTag::Pratt,
        "Skip-based operator chain must classify as Pratt"
    );
}

/// Build an Unordered-shape IR: `Repeat(lo: 1)` over an Alt whose
/// branches have disjoint FIRST byte sets (mirrors CSS
/// `compoundSelector`).
///
/// The compound is a non-entry rule so the Array detector's entry-list
/// admission (mine_list_rules) doesn't fire. An outer entry rule
/// references the compound.
fn build_unordered_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    // Five "branches" each a Ref to a rule starting with a distinct
    // literal. Each branch rule's body is a Literal so
    // DisjointFirstMiner computes disjoint FIRST sets trivially.
    let branch_rules: Vec<RuleId> = ["a", "b", "c", "d", "e"]
        .iter()
        .map(|&s| {
            let body = lit(&mut ir, s);
            push_rule(&mut ir, &format!("{s}_rule"), body)
        })
        .collect();
    let alt = IrNode::Alt(
        branch_rules
            .iter()
            .map(|&rid| AltBranch {
                node: IrNode::Ref(rid),
                first_set: None,
            })
            .collect(),
        None,
    );
    let compound_body = IrNode::Repeat {
        inner: Box::new(alt),
        lo: 1,
        hi: u32::MAX,
    };
    let compound = push_rule(&mut ir, "compound", compound_body);
    // Outer entry — non-list structural wrapper so the Array detector's
    // entry-list admission doesn't fire on `compound`.
    let entry_body = IrNode::Seq(vec![
        lit(&mut ir, "("),
        IrNode::Ref(compound),
        lit(&mut ir, ")"),
    ]);
    let entry = push_rule(&mut ir, "entry", entry_body);
    ir.entry = entry;
    finalise_and_classify(&mut ir);
    (ir, compound)
}

#[test]
fn w4_unordered_detector_admits_kleene_plus_over_disjoint_alt() {
    let (ir, compound) = build_unordered_fixture();
    assert_eq!(
        ir.shape_assignments.get(compound),
        ShapeTag::Unordered,
        "Repeat(lo: 1) over disjoint-FIRST Alt must classify as Unordered"
    );
}

/// Build an Unordered fixture mimicking CSS `compoundSelector`:
/// `Repeat(lo=1)` over an Alt whose branches are Refs to Alt-bodied
/// rules. The [`bbnf_ir::passes::recognizers::disjoint_first::
/// DisjointFirstMiner`]'s `branch_first_bytes` rejects Alt targets,
/// so a W4.1 detector keyed on [`DisjointFirstTable`] never fires;
/// the W4-fix detector admits via structural FIRST walk.
///
/// Four branches:
/// - `a_chain = "a" | "A"` — Alt-bodied rule, FIRST ⊆ {'a', 'A'}.
/// - `b_chain = "b" | "B"` — Alt-bodied rule, FIRST ⊆ {'b', 'B'}.
/// - `c_lit   = "c"`       — Literal-bodied rule, FIRST = {'c'}.
/// - `d_lit   = "d"`       — Literal-bodied rule, FIRST = {'d'}.
fn build_unordered_alt_branch_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let a_lo = lit(&mut ir, "a");
    let a_up = lit(&mut ir, "A");
    let a_chain_body = IrNode::Alt(
        vec![
            AltBranch {
                node: a_lo,
                first_set: None,
            },
            AltBranch {
                node: a_up,
                first_set: None,
            },
        ],
        None,
    );
    let a_chain = push_rule(&mut ir, "a_chain", a_chain_body);

    let b_lo = lit(&mut ir, "b");
    let b_up = lit(&mut ir, "B");
    let b_chain_body = IrNode::Alt(
        vec![
            AltBranch {
                node: b_lo,
                first_set: None,
            },
            AltBranch {
                node: b_up,
                first_set: None,
            },
        ],
        None,
    );
    let b_chain = push_rule(&mut ir, "b_chain", b_chain_body);

    let c_body = lit(&mut ir, "c");
    let c_lit = push_rule(&mut ir, "c_lit", c_body);
    let d_body = lit(&mut ir, "d");
    let d_lit = push_rule(&mut ir, "d_lit", d_body);

    // compound = (a_chain | b_chain | c_lit | d_lit)+
    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(a_chain),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(b_chain),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(c_lit),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(d_lit),
                first_set: None,
            },
        ],
        None,
    );
    let compound_body = IrNode::Repeat {
        inner: Box::new(alt),
        lo: 1,
        hi: u32::MAX,
    };
    let compound = push_rule(&mut ir, "compound", compound_body);
    let entry_body = IrNode::Seq(vec![
        lit(&mut ir, "("),
        IrNode::Ref(compound),
        lit(&mut ir, ")"),
    ]);
    let entry = push_rule(&mut ir, "entry", entry_body);
    ir.entry = entry;
    finalise_and_classify(&mut ir);
    (ir, compound)
}

#[test]
fn w4_unordered_admits_alt_branch_rules() {
    // Mirrors CSS compoundSelector: Alt branches reference Alt-bodied
    // rules (typeSelector / colonSelector). The W4.1 detector rejected
    // the pattern because `DisjointFirstMiner::branch_first_bytes`
    // returns `None` on `IrNode::Alt`. The W4-fix structural walk
    // admits it.
    let (ir, compound) = build_unordered_alt_branch_fixture();
    assert_eq!(
        ir.shape_assignments.get(compound),
        ShapeTag::Unordered,
        "Repeat over Alt-of-Refs-to-Alt-bodied-rules must classify as Unordered"
    );
}

#[test]
fn w4_unordered_rejects_overlapping_first_alt() {
    // Two branches start with the same byte — disjointness fails.
    let mut ir = base_ir();
    let a_body = lit(&mut ir, "a");
    let a_rule = push_rule(&mut ir, "a_rule", a_body);
    // `aa_rule` also starts with 'a' — its FIRST overlaps `a_rule`'s.
    let aa_body = lit(&mut ir, "ab");
    let aa_rule = push_rule(&mut ir, "aa_rule", aa_body);
    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(a_rule),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(aa_rule),
                first_set: None,
            },
        ],
        None,
    );
    let compound_body = IrNode::Repeat {
        inner: Box::new(alt),
        lo: 1,
        hi: u32::MAX,
    };
    let compound = push_rule(&mut ir, "compound", compound_body);
    let entry_body = IrNode::Seq(vec![
        lit(&mut ir, "("),
        IrNode::Ref(compound),
        lit(&mut ir, ")"),
    ]);
    let entry = push_rule(&mut ir, "entry", entry_body);
    ir.entry = entry;
    finalise_and_classify(&mut ir);
    assert_ne!(
        ir.shape_assignments.get(compound),
        ShapeTag::Unordered,
        "Alt branches with overlapping FIRST bytes must NOT classify as Unordered"
    );
}

#[test]
fn w4_unordered_rejects_optional_repeat() {
    // Repeat lo = 0 — the Unordered emitter's iteration-floor
    // invariant (≥ 1 iter) rejects this as an optional pattern.
    let mut ir = base_ir();
    let a_body = lit(&mut ir, "a");
    let a_rule = push_rule(&mut ir, "a_rule", a_body);
    let b_body = lit(&mut ir, "b");
    let b_rule = push_rule(&mut ir, "b_rule", b_body);
    let alt = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(a_rule),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(b_rule),
                first_set: None,
            },
        ],
        None,
    );
    // Repeat{lo=0} — Kleene-star, not plus.
    let compound_body = IrNode::Repeat {
        inner: Box::new(alt),
        lo: 0,
        hi: u32::MAX,
    };
    let compound = push_rule(&mut ir, "compound", compound_body);
    let entry_body = IrNode::Seq(vec![
        lit(&mut ir, "("),
        IrNode::Ref(compound),
        lit(&mut ir, ")"),
    ]);
    let entry = push_rule(&mut ir, "entry", entry_body);
    ir.entry = entry;
    finalise_and_classify(&mut ir);
    assert_ne!(
        ir.shape_assignments.get(compound),
        ShapeTag::Unordered,
        "Repeat{{lo: 0}} over disjoint-FIRST Alt must NOT classify as Unordered"
    );
}

#[test]
fn w4_unordered_rejects_single_branch_alt() {
    // Single-branch Alt degenerates into a structural wrapper; the
    // detector rejects it so the classifier doesn't emit pointless
    // Unordered tags.
    let mut ir = base_ir();
    let a_body = lit(&mut ir, "a");
    let a_rule = push_rule(&mut ir, "a_rule", a_body);
    let alt = IrNode::Alt(
        vec![AltBranch {
            node: IrNode::Ref(a_rule),
            first_set: None,
        }],
        None,
    );
    let compound_body = IrNode::Repeat {
        inner: Box::new(alt),
        lo: 1,
        hi: u32::MAX,
    };
    let compound = push_rule(&mut ir, "compound", compound_body);
    let entry_body = IrNode::Seq(vec![
        lit(&mut ir, "("),
        IrNode::Ref(compound),
        lit(&mut ir, ")"),
    ]);
    let entry = push_rule(&mut ir, "entry", entry_body);
    ir.entry = entry;
    finalise_and_classify(&mut ir);
    assert_ne!(
        ir.shape_assignments.get(compound),
        ShapeTag::Unordered,
        "Single-branch Alt must NOT classify as Unordered"
    );
}

/// Build an ArgList-shape IR: `"calc" , "(" >> inner << ")"` — the
/// canonical CSS-style function-call shape.
fn build_arglist_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let inner_body = mapped(regex(&mut ir, r"[0-9]+"));
    let inner = push_rule(&mut ir, "inner", inner_body);
    // calcFunction = "calc", "(", Ref(inner), ")"
    let calc_body = IrNode::Seq(vec![
        lit(&mut ir, "calc"),
        lit(&mut ir, "("),
        IrNode::Ref(inner),
        lit(&mut ir, ")"),
    ]);
    let calc = push_rule(&mut ir, "calcFunction", calc_body);
    ir.entry = calc;
    finalise_and_classify(&mut ir);
    (ir, calc)
}

#[test]
fn w4_arglist_detector_admits_name_paren_body_paren() {
    let (ir, calc) = build_arglist_fixture();
    assert_eq!(
        ir.shape_assignments.get(calc),
        ShapeTag::ArgList,
        "`calc(...)` shape must classify as ArgList"
    );
}

/// Build a Flat-shape IR: typed Seq with a literal head — mirrors CSS
/// `displayDecl = "display" , ":" ?w , value , ";"?`.
fn build_flat_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let value_body = mapped(regex(&mut ir, r"[a-z]+"));
    let value = push_rule(&mut ir, "value", value_body);
    // displayDecl = "display", ":", value, ";"
    let decl_body = IrNode::Seq(vec![
        lit(&mut ir, "display"),
        lit(&mut ir, ":"),
        IrNode::Ref(value),
        lit(&mut ir, ";"),
    ]);
    let decl = push_rule(&mut ir, "displayDecl", decl_body);
    ir.entry = decl;
    finalise_and_classify(&mut ir);
    (ir, decl)
}

#[test]
fn w4_flat_detector_admits_literal_headed_typed_seq() {
    let (ir, decl) = build_flat_fixture();
    assert_eq!(
        ir.shape_assignments.get(decl),
        ShapeTag::Flat,
        "`\"display\" , \":\" , value , \";\"` shape must classify as Flat"
    );
}

/// Build a Wrap-shape IR: `Alt(Ref, Ref, Ref)` transparent dispatcher
/// whose branches resolve to *compound* shape rules (not bare
/// literals — those would classify as Keyword first).
///
/// Mirrors JSON's `value = object | array | string | number | bool |
/// null` where every branch is a Ref to a compound / regex rule.
fn build_wrap_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    // Three Ref targets: a number regex (non-literal), a string regex
    // (non-literal), and an array (Wrap) compound.
    let num_body = mapped(regex(&mut ir, r"[0-9]+"));
    let num_rule = push_rule(&mut ir, "num", num_body);
    let str_body = mapped(regex(&mut ir, r#""[^"]*""#));
    let str_rule = push_rule(&mut ir, "str", str_body);
    // arr = "[" >> num << "]" — a delimited Wrap shape.
    let arr_body = IrNode::Next(
        Box::new(lit(&mut ir, "[")),
        Box::new(IrNode::Skip(
            Box::new(IrNode::Ref(num_rule)),
            Box::new(lit(&mut ir, "]")),
        )),
    );
    let arr_rule = push_rule(&mut ir, "arr", arr_body);
    // dispatcher = num | str | arr
    let disp_body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(num_rule),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(str_rule),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Ref(arr_rule),
                first_set: None,
            },
        ],
        None,
    );
    let disp = push_rule(&mut ir, "dispatcher", disp_body);
    ir.entry = disp;
    finalise_and_classify(&mut ir);
    (ir, disp)
}

#[test]
fn w4_wrap_detector_admits_all_ref_alt() {
    let (ir, disp) = build_wrap_fixture();
    assert_eq!(
        ir.shape_assignments.get(disp),
        ShapeTag::Wrap,
        "Alt of Refs must classify as Wrap"
    );
}

/// Build an HRegex-shape IR: a single regex leaf whose class is NOT
/// QuotedString / Numeric — an Identifier-class regex is the
/// canonical case.
fn build_hregex_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    // identifier = /[A-Za-z_][A-Za-z0-9_]*/ -> Span
    let ident_body = mapped(regex(&mut ir, r"[A-Za-z_][A-Za-z0-9_]*"));
    let ident = push_rule(&mut ir, "identifier", ident_body);
    ir.entry = ident;
    finalise_and_classify(&mut ir);
    (ir, ident)
}

#[test]
fn w4_hregex_detector_admits_identifier_regex_leaf() {
    let (ir, ident) = build_hregex_fixture();
    assert_eq!(
        ir.shape_assignments.get(ident),
        ShapeTag::HRegex,
        "identifier regex leaf must classify as HRegex"
    );
}

#[test]
fn w4_hregex_rejects_quoted_string_regex_leaf() {
    // A QuotedString-class regex must route to String, not HRegex.
    let mut ir = base_ir();
    let str_body = mapped(regex(&mut ir, r#""(?:[^"\\]|\\[\s\S])*""#));
    let string_rule = push_rule(&mut ir, "string", str_body);
    ir.entry = string_rule;
    finalise_and_classify(&mut ir);
    assert_eq!(
        ir.shape_assignments.get(string_rule),
        ShapeTag::String,
        "QuotedString regex must classify as String, not HRegex"
    );
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::HRegex),
        0,
        "no HRegex tags when all regex leaves are typed"
    );
}

// ─── W4-fix detector extension tests ─────────────────────────────────
//
// The AW-V.W4-fix wave broadens four detectors beyond the W4.1
// scaffolding. Each extension gets a synthetic fixture asserting the
// admission fires.

/// Build a Ref-headed Flat fixture: `decl = kwGroup, ":", valueRef,
/// ";"` where `kwGroup` is a Keyword-classified Alt-of-literal rule.
/// Mirrors CSS `colorDecl = colorProps, ":", value, ";"` shape.
fn build_ref_headed_flat_fixture() -> (GrammarIR, RuleId, RuleId) {
    let mut ir = base_ir();
    // kwGroup = "color" | "background-color" — Alt-of-literal →
    // classifies as Keyword.
    let kw_alt = IrNode::Alt(
        vec![
            AltBranch {
                node: lit(&mut ir, "color"),
                first_set: None,
            },
            AltBranch {
                node: lit(&mut ir, "background-color"),
                first_set: None,
            },
        ],
        None,
    );
    let kw_group = push_rule(&mut ir, "kwGroup", kw_alt);
    // value = /[a-z]+/
    let value_body = mapped(regex(&mut ir, r"[a-z]+"));
    let value = push_rule(&mut ir, "value", value_body);
    // decl = kwGroup, ":", value, ";"
    let decl_body = IrNode::Seq(vec![
        IrNode::Ref(kw_group),
        lit(&mut ir, ":"),
        IrNode::Ref(value),
        lit(&mut ir, ";"),
    ]);
    let decl = push_rule(&mut ir, "decl", decl_body);
    ir.entry = decl;
    finalise_and_classify(&mut ir);
    (ir, decl, kw_group)
}

#[test]
fn w4fix_flat_admits_ref_to_keyword_head() {
    let (ir, decl, kw_group) = build_ref_headed_flat_fixture();
    assert_eq!(
        ir.shape_assignments.get(kw_group),
        ShapeTag::Keyword,
        "kwGroup's Alt-of-literal body classifies as Keyword"
    );
    assert_eq!(
        ir.shape_assignments.get(decl),
        ShapeTag::Flat,
        "W4-fix: Ref-to-Keyword head admits Flat classification"
    );
}

/// Build a typed-dimension Flat fixture: `length = number, unit`
/// where both positions are Refs. Mirrors CSS `length = number,
/// lengthUnit`.
fn build_typed_dimension_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let num_body = mapped(regex(&mut ir, r"-?\d+"));
    let num = push_rule(&mut ir, "num", num_body);
    let unit_alt = IrNode::Alt(
        vec![
            AltBranch {
                node: lit(&mut ir, "px"),
                first_set: None,
            },
            AltBranch {
                node: lit(&mut ir, "em"),
                first_set: None,
            },
        ],
        None,
    );
    let unit = push_rule(&mut ir, "unit", unit_alt);
    let length_body = IrNode::Seq(vec![IrNode::Ref(num), IrNode::Ref(unit)]);
    let length = push_rule(&mut ir, "length", length_body);
    ir.entry = length;
    finalise_and_classify(&mut ir);
    (ir, length)
}

#[test]
fn w4fix_flat_admits_typed_dimension() {
    let (ir, length) = build_typed_dimension_fixture();
    assert_eq!(
        ir.shape_assignments.get(length),
        ShapeTag::Flat,
        "W4-fix: typed dimension Seq(Ref, Ref) admits Flat"
    );
}

/// Build an optional Seq Flat fixture: `importantSuffix = ("!",
/// "important") ?`. Mirrors CSS `importantSuffix`.
fn build_optional_seq_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let inner = IrNode::Seq(vec![lit(&mut ir, "!"), lit(&mut ir, "important")]);
    let body = IrNode::Repeat {
        inner: Box::new(inner),
        lo: 0,
        hi: 1,
    };
    let suffix = push_rule(&mut ir, "importantSuffix", body);
    ir.entry = suffix;
    finalise_and_classify(&mut ir);
    (ir, suffix)
}

#[test]
fn w4fix_flat_admits_optional_seq() {
    let (ir, suffix) = build_optional_seq_fixture();
    assert_eq!(
        ir.shape_assignments.get(suffix),
        ShapeTag::Flat,
        "W4-fix: Repeat(0, 1, Seq(...)) admits Flat via optional-Seq"
    );
}

/// Build a BBNF-style ArgList with a Ref head + explicit paren:
/// `fnCall = idPath, "(", args, ")"`. Mirrors BBNF `value_fn_call =
/// value_path , "(" , args , ")"`.
fn build_ref_head_arglist_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    // idPath = /[a-z]+(::[a-z]+)*/
    let ident_body = mapped(regex(&mut ir, r"[a-z]+(::[a-z]+)*"));
    let id_path = push_rule(&mut ir, "idPath", ident_body);
    // args = /[^)]*/
    let args_body = mapped(regex(&mut ir, r"[^)]*"));
    let args = push_rule(&mut ir, "args", args_body);
    // fnCall = idPath, "(", args, ")"
    let body = IrNode::Seq(vec![
        IrNode::Ref(id_path),
        lit(&mut ir, "("),
        IrNode::Ref(args),
        lit(&mut ir, ")"),
    ]);
    let fn_call = push_rule(&mut ir, "fnCall", body);
    ir.entry = fn_call;
    finalise_and_classify(&mut ir);
    (ir, fn_call)
}

#[test]
fn w4fix_arglist_admits_ref_head_with_separate_paren() {
    let (ir, fn_call) = build_ref_head_arglist_fixture();
    assert_eq!(
        ir.shape_assignments.get(fn_call),
        ShapeTag::ArgList,
        "W4-fix: Ref head + separate `(` admits ArgList"
    );
}

// ─── AX.W0a.2.b — AltDispatch + detector-widening tests ──────────────

/// Fixture: a three-branch mixed Alt dispatcher —
/// `target = subA | "literal" | /[regex]/` where `subA` is
/// classified. Mirrors CSS `value`'s mixed-leaf pattern.
fn build_alt_dispatch_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    // subA: Literal("sub") — classifies as Keyword.
    let sub_body = mapped(lit(&mut ir, "sub"));
    let sub_a = push_rule(&mut ir, "subA", sub_body);
    // target: Alt of Ref(subA) | Literal("only") | Regex("\\d+")
    let target_body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Ref(sub_a),
                first_set: None,
            },
            AltBranch {
                node: lit(&mut ir, "only"),
                first_set: None,
            },
            AltBranch {
                node: regex(&mut ir, r"\d+"),
                first_set: None,
            },
        ],
        None,
    );
    let target = push_rule(&mut ir, "target", target_body);
    ir.entry = target;
    finalise_and_classify(&mut ir);
    (ir, target)
}

#[test]
fn axw0a2b_alt_dispatch_admits_mixed_leaf_alt() {
    let (ir, target) = build_alt_dispatch_fixture();
    assert_eq!(
        ir.shape_assignments.get(target),
        ShapeTag::AltDispatch,
        "AltDispatch admits Alt mixing Ref-to-classified + Literal + Regex"
    );
}

/// Fixture: a single-Ref body — `alias = target` where `target` is
/// classified. Mirrors BbnfBootstrap `lhs = identifier`.
fn build_scalar_ref_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let target_body = mapped(lit(&mut ir, "x"));
    let target = push_rule(&mut ir, "target", target_body);
    let alias_body = IrNode::Ref(target);
    let alias = push_rule(&mut ir, "alias", alias_body);
    ir.entry = alias;
    finalise_and_classify(&mut ir);
    (ir, alias)
}

#[test]
fn axw0a2b_scalar_admits_transparent_ref_body() {
    let (ir, alias) = build_scalar_ref_fixture();
    assert_eq!(
        ir.shape_assignments.get(alias),
        ShapeTag::Scalar,
        "Scalar admits single-Ref transparent-alias bodies \
         when the target is classified"
    );
}

/// Fixture: a non-entry Repeat-rooted rule — `list = inner +`
/// where `list` is NOT the entry rule. Mirrors BBNF `alternation =
/// (concatenation ?w, "|"?) +` (a non-entry rule reachable from the
/// entry `grammar`).
///
/// An entry-rule Repeat body is claimed by Array's Shape 2
/// (list-rule detection); Flat's Repeat-rooted admission covers
/// non-entry cases.
fn build_repeat_rooted_flat_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let item_body = mapped(lit(&mut ir, "a"));
    let item = push_rule(&mut ir, "item", item_body);
    let list_body = IrNode::Repeat {
        inner: Box::new(IrNode::Seq(vec![IrNode::Ref(item), lit(&mut ir, "|")])),
        lo: 1,
        hi: u32::MAX,
    };
    let list = push_rule(&mut ir, "list", list_body);
    // Wrap `list` in a separate entry rule so `list` is NOT the
    // list-rule entry candidate.
    let outer_body = IrNode::Ref(list);
    let outer = push_rule(&mut ir, "outer", outer_body);
    ir.entry = outer;
    finalise_and_classify(&mut ir);
    (ir, list)
}

#[test]
fn axw0a2b_flat_admits_repeat_rooted_body() {
    let (ir, list) = build_repeat_rooted_flat_fixture();
    assert_eq!(
        ir.shape_assignments.get(list),
        ShapeTag::Flat,
        "Flat admits Repeat-rooted bodies after AX.W0a.2.b"
    );
}

/// Fixture: a Seq with a Repeat head — `expr = prefix* , core`.
/// Mirrors Sheets `unary_expr = unary_prefix * , postfix_expr`.
fn build_repeat_head_flat_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let prefix_body = mapped(lit(&mut ir, "-"));
    let prefix = push_rule(&mut ir, "prefix", prefix_body);
    let core_body = mapped(lit(&mut ir, "x"));
    let core = push_rule(&mut ir, "core", core_body);
    let expr_body = IrNode::Seq(vec![
        IrNode::Repeat {
            inner: Box::new(IrNode::Ref(prefix)),
            lo: 0,
            hi: u32::MAX,
        },
        IrNode::Ref(core),
    ]);
    let expr = push_rule(&mut ir, "expr", expr_body);
    ir.entry = expr;
    finalise_and_classify(&mut ir);
    (ir, expr)
}

#[test]
fn axw0a2b_flat_admits_repeat_max_head() {
    let (ir, expr) = build_repeat_head_flat_fixture();
    assert_eq!(
        ir.shape_assignments.get(expr),
        ShapeTag::Flat,
        "Flat admits Seq with Repeat(lo, MAX) head after AX.W0a.2.b"
    );
}

/// Fixture: a delimited Seq — `block = "[" , ref , "]"`. Previously
/// rejected by Flat's head-byte exclusion; now admitted since
/// Object/Array claim their narrow cases first.
fn build_delimited_seq_flat_fixture() -> (GrammarIR, RuleId) {
    let mut ir = base_ir();
    let body_body = mapped(lit(&mut ir, "x"));
    let body = push_rule(&mut ir, "body", body_body);
    let block_body = IrNode::Seq(vec![
        lit(&mut ir, "["),
        IrNode::Ref(body),
        lit(&mut ir, "]"),
    ]);
    let block = push_rule(&mut ir, "block", block_body);
    ir.entry = block;
    finalise_and_classify(&mut ir);
    (ir, block)
}

#[test]
fn axw0a2b_flat_admits_bracketed_seq() {
    let (ir, block) = build_delimited_seq_flat_fixture();
    assert_eq!(
        ir.shape_assignments.get(block),
        ShapeTag::Flat,
        "Flat admits bracketed `[ ... ]` Seqs after AX.W0a.2.b \
         (Array/Object/ArgList/Wrap claim their narrow cases first)"
    );
}
