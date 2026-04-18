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
    compute_regex_info,
    recognizers::shape_dispatch::{shape_dispatch, ShapeTag},
    mine_recognizers,
};
use bbnf_ir::{
    AltBranch, CostConfig, GrammarIR, IrNode, IrRule, RuleId, RuleMeta,
    TypeDescInterner,
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
        shape_dict_templates: Vec::new(),
        shape_dict_selection: Vec::new(),
        keyword_branches: HashMap::new(),
        disjoint_first_tables: HashMap::new(),
        pattern_alphabets: HashMap::new(),
        ctns_lifts: std::collections::HashSet::new(),
        shape_assignments:
            bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments::default(),
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
    let num_body = mapped(regex(
        &mut ir,
        r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?",
    ));
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
        IrNode::Next(
            Box::new(IrNode::Ref(colon)),
            Box::new(IrNode::Ref(value)),
        ),
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
    assert_eq!(
        ir.shape_assignments.get(rules.object),
        ShapeTag::Object
    );
    assert_eq!(ir.shape_assignments.get(rules.array), ShapeTag::Array);
    assert_eq!(
        ir.shape_assignments.get(rules.string),
        ShapeTag::String
    );
    assert_eq!(
        ir.shape_assignments.get(rules.number),
        ShapeTag::Number
    );
    assert_eq!(
        ir.shape_assignments.get(rules.bool_rule),
        ShapeTag::Keyword
    );
    assert_eq!(ir.shape_assignments.get(rules.null), ShapeTag::Keyword);
    assert!(
        ir.shape_assignments.classified_count() >= 6,
        "classified_count < 6 — hard gate requires six primary rules"
    );
}

#[test]
fn json_value_dispatcher_defers_to_walker_in_w3() {
    // The `value` rule's Wrap-over-Refs dispatcher is W4's `Wrap`
    // shape; W3 leaves it as `None` so it routes through the walker.
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.value),
        ShapeTag::None,
        "`value` is W4's Wrap shape; W3 defers to walker fallback"
    );
}

#[test]
fn json_pair_defers_to_walker_in_w3() {
    // The `pair` rule's `key, pivot, value` Seq is W4's `Flat` shape.
    let (ir, rules) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.get(rules.pair),
        ShapeTag::None,
        "`pair` is W4's Flat shape; W3 defers to walker fallback"
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

// ─── W4 deferral markers ─────────────────────────────────────────────
//
// These tests document the W4 detectors' deferred status: the
// detectors exist in the taxonomy but their bodies are stubs today.
// When W4 lands, these tests get replaced by shape-positive
// assertions; today they guard the stub contract so a partial W4
// landing doesn't silently claim coverage the mechanism hasn't
// produced.

#[test]
fn w4_pratt_stub_returns_none_for_w3_inputs() {
    // An operator-chain head is W4's `Pratt` shape. The JSON grammar
    // has none, so this is vacuously true; W4 ships concrete tests
    // against Sheets / CSS math towers.
    let (ir, _) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Pratt),
        0,
        "JSON grammar has no Pratt rules; W3 must not emit `Pratt` tags"
    );
}

#[test]
fn w4_unordered_stub_returns_none_for_w3_inputs() {
    // CSS `compoundSelector`-shaped Repeat-over-disjoint-FIRST-Alt
    // is `Unordered`. JSON has none.
    let (ir, _) = build_json_ir();
    assert_eq!(
        ir.shape_assignments.count_of(ShapeTag::Unordered),
        0,
        "JSON grammar has no Unordered rules; W3 must not emit `Unordered` tags"
    );
}

#[test]
fn w4_arglist_stub_returns_none_for_w3_inputs() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::ArgList), 0);
}

#[test]
fn w4_flat_stub_returns_none_for_w3_inputs() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Flat), 0);
}

#[test]
fn w4_wrap_stub_returns_none_for_w3_inputs() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::Wrap), 0);
}

#[test]
fn w4_hregex_stub_returns_none_for_w3_inputs() {
    let (ir, _) = build_json_ir();
    assert_eq!(ir.shape_assignments.count_of(ShapeTag::HRegex), 0);
}

// ─── Direct `shape_dispatch` invocation (without orchestrator) ───────

#[test]
fn shape_dispatch_is_idempotent() {
    // Running the classifier twice over the same IR yields identical
    // assignments — it's a pure projection.
    let (ir, _) = build_json_ir();
    let direct = shape_dispatch(&ir);
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
