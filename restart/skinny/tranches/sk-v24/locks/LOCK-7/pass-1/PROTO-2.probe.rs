// PROTO-2 (LOCK-7 pass-1, probe 2) — construction census anchor.
// Empirically discriminates the two load-bearing claims of the 8-step census:
//   (A) the ExprId arena is APPEND-ONLY (a tree), not a structural interner  -> step 4 is a SOFT host
//   (B) the cross-rule Ref->RuleId edge is DEFERRED to a separate global pass -> step 5 is a HARD host
// Throwaway probe; removed after the run. Live source is NOT mutated.
use grammar::parse_grammar;
use ir::ExprKind;

// PROOF A — add_expr (ir:51) is an append-only arena, NOT a dedup interner.
// Two byte-identical literals must occupy DISTINCT ExprIds. An interner would
// collapse them; an append-only fold keeps them distinct. Distinct => the built
// IR is a TREE (no structural sharing), so step 4's arena is fold-reducible in
// principle (a SOFT host), even though it threads &mut GrammarIr today.
#[test]
fn add_expr_is_append_only_not_interned() {
    let g = parse_grammar("t", "a = \"x\" , \"x\" ;").unwrap();
    let lits: Vec<_> = g
        .exprs
        .iter()
        .filter(|e| matches!(&e.kind, ExprKind::Literal { bytes, .. } if bytes == b"x"))
        .map(|e| e.id)
        .collect();
    assert_eq!(lits.len(), 2, "two identical literals must NOT be interned");
    assert_ne!(lits[0], lits[1], "distinct ExprIds => append-only arena, not a dedup interner");
}

// PROOF A' — ExprIds are strict post-order (child before parent): a synthesized
// attribute numbering. Confirms the arena is a post-order flatten of a tree.
#[test]
fn arena_ids_are_post_order_synthesized() {
    let g = parse_grammar("t", "a = \"x\" , \"y\" ;").unwrap();
    let seq = g
        .exprs
        .iter()
        .find(|e| matches!(&e.kind, ExprKind::Seq(_)))
        .expect("seq node");
    let ExprKind::Seq(children) = &seq.kind else { unreachable!() };
    for c in children {
        assert!(c.0 < seq.id.0, "child ExprId {} must precede parent Seq id {}", c.0, seq.id.0);
    }
}

// PROOF B — construction leaves cross-rule Refs UNRESOLVED; the name->RuleId link
// is a SEPARATE global pass (resolve_refs, ir:84). A forward+cyclic ref resolves
// (=> resolution needs the WHOLE rule table, not a synthesized child value), and
// an undefined ref is a LINK error, not a parse/construct error. This is step 5's
// HARD host: a global-environment edge no synthesized `->` can mint.
#[test]
fn cross_rule_link_is_a_separate_global_pass() {
    let g = parse_grammar("t", "a = b ;\nb = a | \"x\" ;").unwrap();
    let a = g.rule_by_name("a").unwrap();
    let ExprKind::Ref { target, .. } = g.expr(a.body).kind else { panic!("expected ref") };
    assert!(target.is_some(), "resolve_refs filled the cross-rule edge post-construction");
    assert!(
        parse_grammar("t", "a = undefined_rule ;").is_err(),
        "undefined ref must be a LINK error surfaced by the global pass, not a construct error"
    );
}
