//! Precedence-layer descriptors and operator-chain folding.
//!
//! Holds the static `LAYER_*` constants describing each precedence
//! level's operator-symbol → `MapBinOp` map, the generic
//! `fold_value_chain` left-associative reducer, the chain operand
//! collector, and the anonymous-wrapper-descent helpers the chain
//! collector and `unwrap` module both rely on.

use bbnf_ir::{MapBinOp, MapExpr};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::LowerCtx;
use super::dispatch_value_expr;

// ─── Precedence layer descriptors ─────────────────────────────────────────────

/// One precedence layer: the set of operator symbols at this level
/// mapped to `MapBinOp` constructors. Symbols are listed
/// **longest-first** so prefix matching against a recovered byte
/// gap doesn't return `<` when the actual operator was `<=`.
pub(super) struct PrecedenceLayer {
    pub(super) ops: &'static [(&'static str, MapBinOp)],
}

pub(super) const LAYER_OR: PrecedenceLayer = PrecedenceLayer {
    ops: &[("||", MapBinOp::Or)],
};
pub(super) const LAYER_AND: PrecedenceLayer = PrecedenceLayer {
    ops: &[("&&", MapBinOp::And)],
};
pub(super) const LAYER_CMP: PrecedenceLayer = PrecedenceLayer {
    ops: &[
        ("==", MapBinOp::Eq),
        ("!=", MapBinOp::Ne),
        ("<=", MapBinOp::Le),
        (">=", MapBinOp::Ge),
        ("<", MapBinOp::Lt),
        (">", MapBinOp::Gt),
    ],
};
pub(super) const LAYER_ADD: PrecedenceLayer = PrecedenceLayer {
    ops: &[("+", MapBinOp::Add), ("-", MapBinOp::Sub)],
};
pub(super) const LAYER_MUL: PrecedenceLayer = PrecedenceLayer {
    ops: &[
        ("*", MapBinOp::Mul),
        ("/", MapBinOp::Div),
        ("%", MapBinOp::Mod),
    ],
};

// ─── Operator-chain folding ──────────────────────────────────────────────────

/// Generic left-associative fold over a precedence layer.
///
/// Under structural-mode emission, every `value_X` chain rule pushes
/// a compound whose direct children are:
///   - the lower-precedence operand (one rule compound)
///   - a single `Repeat` compound containing each remaining operand
///     as its own rule compound (the operator tokens between them
///     consume bytes but push nothing)
///
/// The operator at position `i` is recovered from the source bytes
/// between `operands[i].span().1` and `operands[i+1].span().0`. We
/// match the longest valid operator prefix from the layer table —
/// the table lists multi-char operators first to avoid `<` shadowing
/// `<=`.
pub(super) fn fold_value_chain<'a>(
    node: BbnfBootstrapNodeView<'a>,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let operands: Vec<BbnfBootstrapNodeView<'a>> = collect_chain_operands(node);
    debug_assert!(
        !operands.is_empty(),
        "fold_value_chain: chain compound {:?} produced zero operands \
         (text = {:?})",
        node.rule_kind(),
        node.span_text(),
    );

    let mut iter = operands.into_iter();
    let first = iter.next().expect("fold_value_chain: missing first operand");
    let mut prev_end = first.span().1;
    let mut result = dispatch_value_expr(first, ctx);

    let input = node.input();
    for operand in iter {
        let op_text = recover_op_between(input, prev_end, operand.span().0, layer)
            .unwrap_or_else(|| {
                panic!(
                    "lower/value_expr.rs: failed to recover operator for layer \
                     in source gap {:?} (chain rule_kind = {:?})",
                    &input[prev_end as usize..operand.span().0 as usize],
                    node.rule_kind(),
                )
            });
        let op = layer
            .ops
            .iter()
            .find(|(t, _)| *t == op_text)
            .map(|(_, o)| *o)
            .expect("recover_op_between returned a token outside the layer table");
        prev_end = operand.span().1;
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(dispatch_value_expr(operand, ctx)),
        };
    }
    result
}

/// Collect all operand views from a chain compound. The shape under
/// structural mode is `[first, Repeat([rest...])]`; under flattened
/// (non-structural) mode the optimizer may have inlined the Repeat
/// wrapper, in which case the chain's children are already flat.
/// Under DTA the entire body may be wrapped in an anonymous Seq
/// compound — descend through anonymous wrappers first to reach the
/// true operand layout.
///
/// # Pratt cousin-leak guard (B3.W0.η)
///
/// Pratt-shape `value_X` rules emit a pre-order outer Rule compound
/// whose body sits at depth `parent + 1` while subsequent post-order
/// Seq/Repeat wrappers around the rule's outer iteration body push
/// records that, after their own `end_compound_post_order` bumps
/// cascade, settle at the same `parent + 1` depth. The finaliser's
/// depth-only sib_skip computation then erroneously chains
/// `value_X`'s direct child to a cousin record sitting AFTER
/// `value_X`'s span_hi (the type-annotation iteration's Seq wrapper,
/// for example). Bound the children walk by the parent compound's
/// span: any child whose span_lo lies at or beyond `node.span().1`
/// is NOT an operand of `node` — discard it. The check is a strict
/// span containment, so contiguous operand spans inside the chain
/// remain admitted.
///
/// Note: a single-operand chain compound (no operators) collapses
/// to one element; the loop in `fold_value_chain` simply returns
/// that operand's lowering unchanged.
pub(super) fn collect_chain_operands<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use crate::runtime::tape::TapeKind;

    // Under DTA the chain body may sit inside one or more anonymous
    // Seq wrappers. Descend through those wrappers to reach the
    // compound whose direct children are the semantic operand
    // layout `[first, iter_wrapper]`.
    let body = descend_anonymous_wrappers(node);
    let body_hi = body.span().1;
    let in_scope = |c: &BbnfBootstrapNodeView<'a>| {
        let (lo, hi) = c.span();
        hi > lo && lo < body_hi
    };

    let mut children = body.children().filter(in_scope);
    let Some(first) = children.next() else {
        return Vec::new();
    };
    // The remaining children are either zero (single-operand
    // chain), one `Repeat` compound (fn-per-rule structural mode),
    // or — under DTA — one `Rule` compound that's actually the
    // Repeat frame (the walker emits `frame_to_tape_kind(Repeat)
    // == Rule` per the AW-I substrate). Peel a single trailing
    // compound child whose children are the iteration operands.
    let mut operands = vec![first];
    let rest: Vec<BbnfBootstrapNodeView<'a>> = children.collect();
    let is_iteration_wrapper = |c: &BbnfBootstrapNodeView<'a>| {
        matches!(c.kind(), TapeKind::Repeat | TapeKind::Rule)
    };
    if rest.len() == 1 && is_iteration_wrapper(&rest[0]) {
        for child in rest[0].children().filter(in_scope) {
            operands.push(child);
        }
    } else {
        for child in rest {
            operands.push(child);
        }
    }
    operands
}

/// Descend through anonymous Seq/Alt/Repeat wrappers (those with
/// `rule_kind ∈ {Unknown, int_lit}`, the walker's sentinel for
/// compounds never stamped by a `DtaState::Ref`) until reaching a
/// compound whose direct children are the caller's semantic body.
///
/// Returns the innermost anonymous-wrapper view whose direct-child
/// count either exceeds one, or equals one but the sole child is
/// itself a semantic-rule compound. Single-anonymous-child chains
/// get collapsed; semantic content is preserved.
///
/// Intended for use by chain-operand / call-arg / body-content
/// collectors that walk direct children but whose caller's compound
/// is a DTA-wrapped rule body.
pub(super) fn descend_anonymous_wrappers<'a>(
    mut view: BbnfBootstrapNodeView<'a>,
) -> BbnfBootstrapNodeView<'a> {
    loop {
        let children: Vec<BbnfBootstrapNodeView<'a>> = view.children().collect();
        if children.len() != 1 {
            return view;
        }
        let only_child = children[0];
        // Only descend if the child is itself an anonymous wrapper
        // (no semantic rule identity); otherwise stop — the child is
        // the semantic content the caller is after.
        if !is_anonymous_wrapper(only_child) {
            return view;
        }
        view = only_child;
    }
}

/// Whether `view` is an anonymous structural wrapper under DTA. Kept
/// in-file to avoid cross-module coupling; mirrors the helper in
/// `lower/tape_walk.rs` that gates sibling descents.
fn is_anonymous_wrapper(view: BbnfBootstrapNodeView<'_>) -> bool {
    use crate::runtime::tape::TapeKind;
    if !matches!(
        view.kind(),
        TapeKind::Rule | TapeKind::Seq | TapeKind::Alt | TapeKind::Repeat,
    ) {
        return false;
    }
    matches!(
        view.rule_kind(),
        BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
    )
}

/// Recover the operator token from the byte gap between two
/// adjacent operand spans. Skips leading whitespace and matches the
/// longest valid operator prefix from the layer table.
fn recover_op_between<'a>(
    input: &'a str,
    lhs_end: u32,
    rhs_start: u32,
    layer: &PrecedenceLayer,
) -> Option<&'a str> {
    let gap = &input[lhs_end as usize..rhs_start as usize];
    let trimmed = gap.trim_start();
    for &(op, _) in layer.ops {
        if trimmed.starts_with(op) {
            return Some(op);
        }
    }
    None
}
