//! SK-V19 W6c — structural keying of the BBNF shape recognizer.
//!
//! `mine_bbnf_shape_templates` recognizes two BBNF rule-body patterns
//! and emits `BbnfShapeTemplate` records. The recognizer is keyed
//! purely on STRUCTURE plus the defining concrete-syntax literals
//! (`/*`/`*/` for the block comment, `->` for the map arrow) — never on
//! the grammar rule name. These tests pin that contract:
//!
//! 1. A `Seq[lit "/*", regex, lit "*/"]` body yields a `BigComment`
//!    template regardless of the rule's name.
//! 2. A structurally identical `Seq[lit, regex, lit]` whose delimiters
//!    are NOT `/*`/`*/` does NOT match (no over-matching).
//! 3. `Seq[Ref, Repeat(0,1){ Seq[lit "->", ...] }]` yields a
//!    `MappedFactorEmpty` template regardless of name.
//! 4. A generic `Seq[Ref, Repeat(0,1){ Seq[lit ",", ...] }]` (any
//!    optional suffix that is not the map arrow) does NOT match.

use bbnf_ir::passes::recognizers::shape_dict_bbnf::{BbnfShapeKind, mine_bbnf_shape_templates};
use bbnf_ir::{GrammarIR, IrNode, IrRule, RuleId, RuleMeta};

/// Intern a string and return its `StringId`.
fn intern(ir: &mut GrammarIR, s: &str) -> u32 {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    sid
}

fn lit(ir: &mut GrammarIR, s: &str) -> IrNode {
    let sid = intern(ir, s);
    IrNode::Literal(sid)
}

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

/// `( open , regex , close )` Seq body — the block-comment topology.
fn comment_like_body(ir: &mut GrammarIR, open: &str, close: &str) -> IrNode {
    let open = lit(ir, open);
    let body = regex(ir, "[^*]*");
    let close = lit(ir, close);
    IrNode::Seq(vec![open, body, close])
}

/// `Ref(0) , ( lead , value )?` Seq body — the mapped-factor topology,
/// with the optional branch leading on `lead`.
fn mapped_factor_like_body(ir: &mut GrammarIR, lead: &str) -> IrNode {
    let arrow = lit(ir, lead);
    let value = regex(ir, "value");
    let optional = IrNode::Repeat {
        inner: Box::new(IrNode::Seq(vec![arrow, value])),
        lo: 0,
        hi: 1,
    };
    IrNode::Seq(vec![IrNode::Ref(0), optional])
}

// ── BigComment recognition ─────────────────────────────────────────

#[test]
fn big_comment_recognized_by_delimiters_not_name() {
    let mut ir = GrammarIR::default();
    // Deliberately NOT named "big_comment": recognition must be structural.
    let body = comment_like_body(&mut ir, "/*", "*/");
    push_rule(&mut ir, "totally_unrelated_rule", body);

    let templates = mine_bbnf_shape_templates(&ir);
    assert_eq!(templates.len(), 1, "tightened shape must still recognize");
    assert_eq!(templates[0].kind, BbnfShapeKind::BigComment);
    assert_eq!(templates[0].payload_bytes, 8);
    // rule_name is recorded as DATA — the actual name, not a literal.
    assert_eq!(templates[0].rule_name, "totally_unrelated_rule");
}

#[test]
fn comment_shape_with_wrong_delimiters_does_not_match() {
    let mut ir = GrammarIR::default();
    // Structurally Seq[literal, regex, literal] — but the delimiters are
    // NOT the block-comment delimiters. Must NOT over-match.
    let body = comment_like_body(&mut ir, "<<", ">>");
    push_rule(&mut ir, "big_comment", body); // even the canonical name

    let templates = mine_bbnf_shape_templates(&ir);
    assert!(
        templates.is_empty(),
        "generic Seq[lit, regex, lit] must not yield BigComment without /* */"
    );
}

#[test]
fn comment_shape_with_one_wrong_delimiter_does_not_match() {
    let mut ir = GrammarIR::default();
    // Correct open, wrong close.
    let body = comment_like_body(&mut ir, "/*", "**");
    push_rule(&mut ir, "big_comment", body);
    assert!(mine_bbnf_shape_templates(&ir).is_empty());
}

// ── MappedFactorEmpty recognition ──────────────────────────────────

#[test]
fn mapped_factor_recognized_by_arrow_not_name() {
    let mut ir = GrammarIR::default();
    let body = mapped_factor_like_body(&mut ir, "->");
    push_rule(&mut ir, "some_other_name", body);

    let templates = mine_bbnf_shape_templates(&ir);
    assert_eq!(templates.len(), 1);
    assert_eq!(templates[0].kind, BbnfShapeKind::MappedFactorEmpty);
    assert_eq!(templates[0].payload_bytes, 0);
    assert_eq!(templates[0].rule_name, "some_other_name");
}

#[test]
fn mapped_factor_shape_with_wrong_lead_does_not_match() {
    let mut ir = GrammarIR::default();
    // Seq[Ref, Repeat(0,1)] but the optional branch leads with "," not "->".
    let body = mapped_factor_like_body(&mut ir, ",");
    push_rule(&mut ir, "mapped_factor", body);

    let templates = mine_bbnf_shape_templates(&ir);
    assert!(
        templates.is_empty(),
        "Repeat(0,1) not leading with -> must not yield MappedFactorEmpty"
    );
}
