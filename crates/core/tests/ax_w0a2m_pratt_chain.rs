//! AX.W0a.2.m probe — reducer-chain layout characterization.
//!
//! After W0a.2.l landed per-rule PRECEDENCE_LUT + reducer-compound
//! emission under structural dispatch, `<<` / `>>` / `-` bytes in
//! binary_factor routes through `parse_pratt_BbnfBootstrap_binary_factor`.
//! That emits an outer Rule compound whose `child_off` points at the
//! LAST reducer in a chain; each reducer's own `child_off` points at
//! its LHS (a previous reducer or the initial operand). Between
//! reducers sits a `TapeKind::Span` op-leaf carrying the operator
//! byte's discriminant via the 1-byte arena payload API.
//!
//! This probe parses a BBNF snippet containing `">> "` / `"<< "` / `"-"`
//! and asserts the exact tape shape `collect_binary_operands` must
//! recognize. Keeping the assertions structural (kinds + variant_idx +
//! span text) keeps the probe stable under span-position renumbers.
//!
//! NB the bbnf crate's dev-dep `gorgeous` currently fails derive on
//! `json.bbnf` via a pre-existing `parse_pratt` skip-space defect
//! (missing `skip_space` at the top of the Pratt loop makes the initial
//! operand's trailing whitespace look like an EOF-op). The test file is
//! valid BBNF tape ground-truth; once the parse bug is resolved the
//! test runs as written.

use bbnf::grammar::generated::{BbnfBootstrap, BbnfBootstrapRuleKind};

/// Depth-first search for a `binary_factor` Rule compound.
fn find_binary_factor<'p>(
    cursor: ::bbnf::runtime::tape::TapeCursor<'p>,
) -> Option<::bbnf::runtime::tape::TapeCursor<'p>> {
    if cursor.variant_idx() == 34u8 && cursor.kind() == ::bbnf::runtime::tape::TapeKind::Rule
    {
        // A binary_factor *with* operators — detected by exactly one
        // direct child whose own structure looks like a reducer (see
        // below). A binary_factor with zero operators collapses to the
        // single operand and is not the shape we care about for this
        // probe.
        let child_count = cursor.children().count();
        if child_count == 1 {
            return Some(cursor);
        }
    }
    for child in cursor.children() {
        if let Some(found) = find_binary_factor(child) {
            return Some(found);
        }
    }
    None
}

#[test]
fn pratt_reducer_chain_three_ops() {
    // BBNF snippet: `foo = "<" >> identifier << "x" - bar ;`
    // binary_factor chain with three operators (>> , << , -).
    let input = "foo = \"<\" >> identifier << \"x\" - bar ;\n";
    let parsed = BbnfBootstrap::parse(input).expect("parse must succeed");

    let root_cursor = ::bbnf::runtime::tape::TapeCursor::new(
        parsed.tape(),
        parsed.root_offset(),
    );
    let bf = find_binary_factor(root_cursor)
        .expect("binary_factor compound must be reachable under the grammar tree");

    // ── Invariant 1: outer compound has exactly one direct child ──
    let children: Vec<_> = bf.children().collect();
    assert_eq!(
        children.len(),
        1,
        "outer binary_factor compound must have exactly one direct child (the last reducer); \
         got {} (outer kind={:?}, variant_idx={})",
        children.len(),
        bf.kind(),
        bf.variant_idx()
    );

    let last_reducer = children[0];

    // ── Invariant 2: last reducer is a Rule with variant_idx ∈ {0, 1, 2} ──
    assert_eq!(
        last_reducer.kind(),
        ::bbnf::runtime::tape::TapeKind::Rule,
        "last reducer must be TapeKind::Rule; got {:?}",
        last_reducer.kind()
    );
    assert!(
        matches!(last_reducer.variant_idx(), 0 | 1 | 2),
        "last reducer variant_idx must be op_discriminant ∈ 0..=2 for binary_factor; got {}",
        last_reducer.variant_idx()
    );

    // ── Invariant 3: reducer has exactly 3 direct children ──
    //     [LHS_compound, Span_op_leaf (variant_idx=0), RHS_compound]
    let reducer_children: Vec<_> = last_reducer.children().collect();
    assert_eq!(
        reducer_children.len(),
        3,
        "reducer compound must have exactly 3 children [LHS, op_leaf, RHS]; got {}",
        reducer_children.len()
    );

    // Middle child = op-leaf (TapeKind::Span, variant_idx=0).
    let op_leaf = reducer_children[1];
    assert_eq!(
        op_leaf.kind(),
        ::bbnf::runtime::tape::TapeKind::Span,
        "middle child of reducer must be TapeKind::Span (op-leaf); got {:?}",
        op_leaf.kind()
    );
    assert_eq!(
        op_leaf.variant_idx(),
        0u8,
        "op-leaf must carry variant_idx=0 (Pratt op-leaf marker)"
    );

    // Span text of the op-leaf must be one of the fixed operator tokens.
    let op_span = {
        let (lo, hi) = op_leaf.span();
        &input[lo as usize..hi as usize]
    };
    assert!(
        matches!(op_span, "<<" | ">>" | "-"),
        "op-leaf span text must be one of <</>>/-, got {:?}",
        op_span
    );

    // ── Invariant 4: walk backward, every intermediate LHS is a reducer ──
    let mut chain_depth = 0usize;
    let mut current = last_reducer;
    loop {
        let kids: Vec<_> = current.children().collect();
        if kids.len() != 3 {
            panic!(
                "intermediate reducer must have 3 children [LHS, op_leaf, RHS]; got {}",
                kids.len()
            );
        }
        let lhs = kids[0];
        let op_leaf = kids[1];
        let _rhs = kids[2];
        assert_eq!(
            op_leaf.kind(),
            ::bbnf::runtime::tape::TapeKind::Span,
            "intermediate reducer op-leaf must be Span"
        );
        chain_depth += 1;
        // Is LHS another reducer?
        let lhs_is_reducer = lhs.kind() == ::bbnf::runtime::tape::TapeKind::Rule
            && matches!(lhs.variant_idx(), 0 | 1 | 2)
            && lhs.children().count() == 3
            && lhs.children().nth(1).map(|m| m.kind()) == Some(::bbnf::runtime::tape::TapeKind::Span);
        if lhs_is_reducer {
            current = lhs;
        } else {
            // Initial operand — not a reducer. For 3 operators we
            // expect chain_depth == 3 at this point (last + 2
            // intermediate + 1 leading operand seen at LHS).
            break;
        }
    }
    assert_eq!(
        chain_depth, 3,
        "3-operator chain must walk exactly 3 reducers (got {})",
        chain_depth
    );
}

#[test]
fn pratt_reducer_chain_single_op() {
    // Single-operator chain: `foo = "a" >> "b" ;`
    let input = "foo = \"a\" >> \"b\" ;\n";
    let parsed = BbnfBootstrap::parse(input).expect("parse must succeed");

    let root_cursor = ::bbnf::runtime::tape::TapeCursor::new(
        parsed.tape(),
        parsed.root_offset(),
    );
    let bf = find_binary_factor(root_cursor)
        .expect("binary_factor compound must be reachable");

    let children: Vec<_> = bf.children().collect();
    assert_eq!(children.len(), 1, "outer compound has one direct child");
    let only_reducer = children[0];
    let kids: Vec<_> = only_reducer.children().collect();
    assert_eq!(kids.len(), 3, "single reducer has 3 children");
    // The LHS of the only reducer is the initial operand — NOT another reducer.
    let lhs = kids[0];
    // LHS should NOT be a pratt reducer (no nested [LHS, Span, RHS] shape).
    let lhs_is_reducer = lhs.kind() == ::bbnf::runtime::tape::TapeKind::Rule
        && matches!(lhs.variant_idx(), 0 | 1 | 2)
        && lhs.children().count() == 3
        && lhs.children().nth(1).map(|m| m.kind()) == Some(::bbnf::runtime::tape::TapeKind::Span)
        && lhs.children().nth(1).map(|m| m.variant_idx()) == Some(0u8)
        && matches!(
            lhs.children().nth(1).map(|m| {
                let (lo, hi) = m.span();
                &input[lo as usize..hi as usize]
            }),
            Some("<<") | Some(">>") | Some("-")
        );
    assert!(
        !lhs_is_reducer,
        "the single reducer's LHS must be the initial operand, not another reducer"
    );
}

// Suppress unused-import warning if BbnfBootstrapRuleKind ever
// unnecessary at test body level — kept imported for diagnostic use.
#[allow(dead_code)]
fn _unused_rulekind_reference() {
    let _ = BbnfBootstrapRuleKind::binary_factor;
}
