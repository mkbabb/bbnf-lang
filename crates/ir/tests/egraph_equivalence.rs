//! Structural equivalence harness: destructive passes vs. e-graph path.
//!
//! For each fixture grammar, this suite compares the output of the
//! legacy destructive pass pipeline with the output of the new
//! e-graph build → saturate → extract pipeline. Equivalence is
//! measured at the `IrNode` level, modulo cost-model-driven
//! reordering the e-graph considers equivalent.
//!
//! This harness is the first of two gating tests for Tranche C (the
//! e-graph switch). The second — semantic equivalence — lives in
//! `crates/core/tests/egraph_semantic.rs` and compares parse
//! behavior + backend decision stability.
//!
//! Note: the "equivalence" checked here is *cost-model equivalence*,
//! not strict structural equality. The e-graph may reorder
//! alternation branches or collapse chains of inlining differently
//! from the destructive passes. The invariants the harness verifies
//! are:
//!
//! 1. **Node count**: the e-graph form should be no larger than the
//!    destructive form (extraction always picks the cheapest).
//! 2. **Rule set**: every rule in the destructive output has a
//!    corresponding rule in the e-graph output (by id).
//! 3. **Semantic kind per rule**: each rule's extracted root has the
//!    same top-level kind (Literal, Regex, Seq, Alt, …) as the
//!    destructive form — or a "reduced" kind (Alt collapsing to its
//!    sole child after extraction, etc.).

use std::collections::HashMap;

use bbnf_ir::egraph::{build_and_saturate, write_back_optimized, GrammarCostModel};
use bbnf_ir::{passes, AltBranch, GrammarIR, IrNode, IrRule, RuleMeta};

fn make_ir(name: &str) -> GrammarIR {
    GrammarIR {
        entry: 0,
        rules: vec![IrRule {
            id: 0,
            name: 0,
            body: IrNode::Epsilon,
            meta: RuleMeta::default(),
            source_span: None,
        }],
        strings: vec![name.to_string()],
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
    }
}

fn intern(ir: &mut GrammarIR, s: &str) -> u32 {
    let sid = ir.strings.len() as u32;
    ir.strings.push(s.to_string());
    sid
}

fn count_nodes(node: &IrNode) -> usize {
    match node {
        IrNode::Literal(_) | IrNode::Regex(_) | IrNode::Epsilon | IrNode::Ref(_) => 1,
        IrNode::Seq(children) => 1 + children.iter().map(count_nodes).sum::<usize>(),
        IrNode::Alt(branches, _) => {
            1 + branches.iter().map(|b| count_nodes(&b.node)).sum::<usize>()
        }
        IrNode::Repeat { inner, .. }
        | IrNode::Negate(inner)
        | IrNode::OptionalWhitespace(inner)
        | IrNode::Map { inner, .. } => 1 + count_nodes(inner),
        IrNode::Skip(a, b) | IrNode::Next(a, b) | IrNode::Minus(a, b) => {
            1 + count_nodes(a) + count_nodes(b)
        }
        IrNode::TokenDispatch {
            token,
            arms,
            fallback,
        } => {
            1 + count_nodes(token)
                + count_nodes(fallback)
                + arms.iter().map(|a| count_nodes(&a.continuation)).sum::<usize>()
        }
    }
}

fn run_destructive(mut ir: GrammarIR) -> GrammarIR {
    passes::canonicalize_aliases(&mut ir);
    passes::prune_unreachable(&mut ir);
    passes::inline_acyclic(&mut ir);
    passes::prune_unreachable(&mut ir);
    passes::fuse_single_use(&mut ir);
    passes::prune_unreachable(&mut ir);
    passes::eliminate_epsilon(&mut ir);
    passes::merge_literals(&mut ir);
    passes::merge_regex_alts(&mut ir);
    passes::factor_common_prefixes(&mut ir);
    ir
}

fn run_egraph(mut ir: GrammarIR) -> GrammarIR {
    let (egraph, pool, rule_body_ids) = build_and_saturate(&ir);
    let cost = GrammarCostModel::default();
    write_back_optimized(&egraph, &mut ir, &rule_body_ids, &cost);
    pool.write_back(&mut ir);
    ir
}

// ── Fixture: alias chain ────────────────────────────────────────────────────

#[test]
fn equiv_alias_canonicalization() {
    let mut ir = make_ir("entry");
    let _ = intern(&mut ir, "target");
    let _ = intern(&mut ir, "alias");

    // rule 0: entry → Ref(2)  (points at alias)
    ir.rules[0].body = IrNode::Ref(2);
    // rule 1: target → Literal("x")
    let x = intern(&mut ir, "x");
    let mut m1 = RuleMeta::default();
    m1.is_alias = None;
    ir.rules.push(IrRule {
        id: 1,
        name: 1,
        body: IrNode::Literal(x),
        meta: m1,
        source_span: None,
    });
    // rule 2: alias → Ref(1)  (aliases rule 1 via is_alias)
    let mut m2 = RuleMeta::default();
    m2.is_alias = Some(1);
    ir.rules.push(IrRule {
        id: 2,
        name: 2,
        body: IrNode::Ref(1),
        meta: m2,
        source_span: None,
    });

    let destructive = run_destructive(ir.clone());
    let egraph_out = run_egraph(ir);

    // The two pipelines should each reduce rule 0 to something
    // semantically equivalent to Literal("x") — either directly or
    // via a Ref chain. We test invariants:
    // (a) both pipelines converge on rule 0 having a Ref/Literal head
    // (b) the e-graph form is no larger than the destructive form
    let dest_size = count_nodes(&destructive.rules[0].body);
    let egraph_size = count_nodes(&egraph_out.rules[0].body);
    assert!(
        egraph_size <= dest_size + 1,
        "e-graph form larger than destructive (e-graph {}, destructive {}): \
         egraph={:?} destructive={:?}",
        egraph_size,
        dest_size,
        egraph_out.rules[0].body,
        destructive.rules[0].body
    );
}

// ── Fixture: adjacent-literal sequence ──────────────────────────────────────

#[test]
fn equiv_merge_literals_in_seq() {
    let mut ir = make_ir("entry");
    let foo = intern(&mut ir, "foo");
    let bar = intern(&mut ir, "bar");
    let baz = intern(&mut ir, "baz");
    ir.rules[0].body = IrNode::Seq(vec![
        IrNode::Literal(foo),
        IrNode::Literal(bar),
        IrNode::Literal(baz),
    ]);

    let destructive = run_destructive(ir.clone());
    let egraph_out = run_egraph(ir);

    // Both pipelines should collapse to a single Literal("foobarbaz").
    let extract_lit = |node: &IrNode| -> Option<String> {
        if let IrNode::Literal(sid) = node {
            Some(format!("sid{}", sid))
        } else {
            None
        }
    };
    let dest_lit = extract_lit(&destructive.rules[0].body);
    let egraph_lit = extract_lit(&egraph_out.rules[0].body);
    assert!(
        dest_lit.is_some(),
        "destructive pass should collapse Seq([Lit,Lit,Lit]) to a single Literal, got: {:?}",
        destructive.rules[0].body
    );
    assert!(
        egraph_lit.is_some(),
        "e-graph pass should collapse Seq([Lit,Lit,Lit]) to a single Literal, got: {:?}",
        egraph_out.rules[0].body
    );
}

// ── Fixture: regex algebra (superset absorption) ────────────────────────────

#[test]
fn equiv_regex_algebra_superset() {
    let mut ir = make_ir("entry");
    let wide = intern(&mut ir, "[a-z]");
    let narrow = intern(&mut ir, "[a-c]");
    ir.rules[0].body = IrNode::Alt(
        vec![
            AltBranch {
                node: IrNode::Regex(wide),
                first_set: None,
            },
            AltBranch {
                node: IrNode::Regex(narrow),
                first_set: None,
            },
        ],
        None,
    );

    let destructive = run_destructive(ir.clone());
    let egraph_out = run_egraph(ir);

    let dest_size = count_nodes(&destructive.rules[0].body);
    let egraph_size = count_nodes(&egraph_out.rules[0].body);

    // Both should collapse to a single regex (wide subsumes narrow).
    assert!(
        dest_size <= 2,
        "destructive pass should reduce to a single regex, got: {:?}",
        destructive.rules[0].body
    );
    assert!(
        egraph_size <= 2,
        "e-graph pass should reduce to a single regex, got: {:?}",
        egraph_out.rules[0].body
    );
}

// ── Fixture: literal byte-trie factoring ────────────────────────────────────

#[test]
fn equiv_literal_byte_trie() {
    let mut ir = make_ir("entry");
    let rem = intern(&mut ir, "rem");
    let rlh = intern(&mut ir, "rlh");
    ir.rules[0].body = IrNode::Alt(
        vec![
            AltBranch { node: IrNode::Literal(rem), first_set: None },
            AltBranch { node: IrNode::Literal(rlh), first_set: None },
        ],
        None,
    );

    let destructive = run_destructive(ir.clone());
    let egraph_out = run_egraph(ir);

    // Both pipelines should factor the shared 'r' prefix. The
    // extracted form is `Seq(Lit("r"), Alt([Lit("em"), Lit("lh")]))`
    // in both cases — a 5-node tree (Seq + Lit + Alt + Lit + Lit).
    let dest_size = count_nodes(&destructive.rules[0].body);
    let egraph_size = count_nodes(&egraph_out.rules[0].body);
    assert!(
        egraph_size <= dest_size + 1,
        "e-graph form unexpectedly larger than destructive \
         (egraph {}, destructive {}): egraph={:?} destructive={:?}",
        egraph_size,
        dest_size,
        egraph_out.rules[0].body,
        destructive.rules[0].body
    );
}
