//! Synthetic JSON `GrammarIR` fixture shared by AW-V.W3.3 tests.
//!
//! Mirrors `crates/ir/tests/shape_dispatch.rs`'s W3.1 fixture — the
//! same `IrNode` topology the lowering pass produces for
//! `grammar/json/json.bbnf`, so the classifier results match
//! production end-to-end with zero host-function lookup.
//!
//! Returns the tuple of [`RuleId`]s for every named rule so tests
//! can assert per-rule classifications without re-resolving names.
//!
//! The fixture runs the same `finalise_and_classify` pipeline that
//! production grammars exercise: `GrammarDag::from_ir` + `compute_regex_info`
//! + `mine_recognizers` (which invokes `shape_dispatch` internally).

use std::collections::HashMap;

use bbnf_ir::dag::GrammarDag;
use bbnf_ir::passes::recognizers::shape_dispatch::ShapeAssignments;
use bbnf_ir::passes::{compute_regex_info, mine_recognizers};
use bbnf_ir::{
    AltBranch, CostConfig, FnDescriptor, GrammarIR, IrNode, IrRule, MapExpr, RuleId, RuleMeta,
    TypeDescInterner,
};

/// A fully-classified JSON-shaped fixture plus the rule ids for each
/// named rule. The IR carries populated `shape_assignments`,
/// `regex_info`, and every miner sidecar `mine_recognizers` computes.
pub struct JsonRules {
    pub null: RuleId,
    pub bool_rule: RuleId,
    pub number: RuleId,
    pub comma: RuleId,
    pub colon: RuleId,
    pub string: RuleId,
    pub pair: RuleId,
    pub array: RuleId,
    pub object: RuleId,
    pub value: RuleId,
}

/// Build an empty `GrammarIR` with every sidecar initialised.
pub fn base_ir() -> GrammarIR {
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
        struct_registry: bbnf_ir::StructRegistry::default(),
        shape_assignments: ShapeAssignments::default(),
    }
}

/// Intern a string and return its interned id.
pub fn intern(ir: &mut GrammarIR, s: &str) -> u32 {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    sid
}

/// Build a `Literal(sid)` node after interning `s`.
pub fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = intern(ir, s);
    IrNode::Literal(sid)
}

/// Build a `Regex(sid)` node after interning the pattern.
pub fn regex(ir: &mut GrammarIR, pattern: &str) -> IrNode {
    let sid = intern(ir, pattern);
    IrNode::Regex(sid)
}

/// Push a rule with the given body; returns its [`RuleId`].
pub fn push_rule(ir: &mut GrammarIR, name: &str, body: IrNode) -> RuleId {
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

/// Push a [`FnDescriptor::Expr`] carrying a [`MapExpr`]; returns its
/// fn id. The Keyword emitter reads the `-> expr` annotation for
/// branch payload inference.
pub fn push_fn(ir: &mut GrammarIR, expr: MapExpr) -> u32 {
    let id = ir.fns.len() as u32;
    ir.fns.push(FnDescriptor::Expr {
        expr,
        return_type: None,
    });
    id
}

/// Wrap `inner` in `Map { inner, fn_id }` — the production form for
/// `-> <expr>` typed annotation.
pub fn mapped(inner: IrNode, fn_id: u32) -> IrNode {
    IrNode::Map {
        inner: Box::new(inner),
        fn_id,
    }
}

/// Run the post-W3.1 orchestration: build DAG, classify regex
/// patterns, run the recognizer miner (which internally runs
/// `shape_dispatch` as its tail pass).
pub fn finalise_and_classify(ir: &mut GrammarIR) {
    ir.dag = Some(GrammarDag::from_ir(ir));
    compute_regex_info(ir);
    mine_recognizers(ir);
}

/// Build a `GrammarIR` matching the structure of
/// `grammar/json/json.bbnf`. Every named rule is lowered to the same
/// `IrNode` topology the production lowering pass produces — the
/// shape-dispatch classifier matches production tags with zero drift.
pub fn build_json_ir() -> (GrammarIR, JsonRules) {
    let mut ir = base_ir();

    // null = "null" -> 0u8
    let null_expr = push_fn(&mut ir, MapExpr::IntLit(0));
    let null_body = mapped(lit(&mut ir, "null"), null_expr);
    let null = push_rule(&mut ir, "null", null_body);

    // bool = "true" -> true | "false" -> false
    let true_expr = push_fn(&mut ir, MapExpr::BoolLit(true));
    let false_expr = push_fn(&mut ir, MapExpr::BoolLit(false));
    let true_lit = mapped(lit(&mut ir, "true"), true_expr);
    let false_lit = mapped(lit(&mut ir, "false"), false_expr);
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
    let num_fn = push_fn(&mut ir, MapExpr::Input);
    let num_body = mapped(
        regex(&mut ir, r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"),
        num_fn,
    );
    let number = push_rule(&mut ir, "number", num_body);

    // comma = "," ?w
    let comma_body = IrNode::OptionalWhitespace(Box::new(lit(&mut ir, ",")));
    let comma = push_rule(&mut ir, "comma", comma_body);

    // colon = ":" ?w
    let colon_body = IrNode::OptionalWhitespace(Box::new(lit(&mut ir, ":")));
    let colon = push_rule(&mut ir, "colon", colon_body);

    // string = /"..."/  -> decode_json_string(input)
    let str_fn = push_fn(&mut ir, MapExpr::Input);
    let str_body = mapped(
        regex(
            &mut ir,
            r#""(?:[^"\\]|\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4}))*""#,
        ),
        str_fn,
    );
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
    //       = Next(Literal("["), Skip(OptionalWhitespace(Repeat(Skip(Ref(value), Repeat(comma,0,1)), 0, inf)), Literal("]")))
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
