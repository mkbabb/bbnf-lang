//! Precedence-layer descriptors and operator-chain folding.
//!
//! Holds the static `LAYER_*` constants describing each precedence
//! level's operator-symbol → `MapBinOp` map, the generic
//! `fold_value_chain` left-associative reducer, and the chain
//! operand collector.

use bbnf_ir::{MapBinOp, MapExpr};

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

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
///   - a single iteration sub-tree containing each remaining operand
///     as its own rule compound (the operator tokens between them
///     consume bytes but project no payload)
///
/// The operator at position `i` is recovered from the source bytes
/// between `operands[i].span().1` and `operands[i+1].span().0`. We
/// match the longest valid operator prefix from the layer table —
/// the table lists multi-char operators first to avoid `<` shadowing
/// `<=`.
///
/// Each operand's source extent is recovered via
/// [`BbnfView::span_bounds`], the recursive byte-range projection
/// over the focused subtree's `Span` descendants. When a chain
/// operand resolves entirely to typed-projected leaves (`Int` /
/// `Float` / `Bool`) without any `Span` descendant, the byte-gap
/// recovery falls back to a parent-span linear scan that splits on
/// the layer's operator tokens at the appropriate skip-depth.
pub(super) fn fold_value_chain<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let mut operands: Vec<BbnfView<'a, 'p>> = collect_chain_operands(node);
    debug_assert!(
        !operands.is_empty(),
        "fold_value_chain: chain compound {:?} produced zero operands \
         (text = {:?})",
        node.compound_kind(),
        node.span_text(),
    );

    // Single-operand chain — no operators to recover. Dispatch the
    // lone operand directly; this short-circuit is mandatory when the
    // operand resolves entirely to typed-projected leaves
    // (`BbnfValue::Bool` / `Int` / `Float`) whose source-text was
    // dropped at parse time, so neither per-operand spans nor parent-
    // span splitting can recover anything from the empty extent.
    if operands.len() == 1 {
        return dispatch_value_expr(operands.remove(0), ctx);
    }

    // Try the per-operand span path first; fall back to parent-span
    // splitting when any operand has no recoverable span.
    if let Some(folded) = fold_via_per_operand_spans(node, &operands, layer, ctx) {
        return folded;
    }
    fold_via_parent_span_split(node, operands, layer, ctx)
}

/// Per-operand span path — recover each operand's `(lo, hi)` byte
/// range and read the operator from the byte gap between adjacent
/// operands. Returns `None` when any operand lacks a recoverable
/// span, signalling the caller to fall back to parent-span splitting.
fn fold_via_per_operand_spans<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    operands: &[BbnfView<'a, 'p>],
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'p>,
) -> Option<MapExpr> {
    let input = RuntimeView::input(&node);
    let mut iter = operands.iter().copied();
    let first = iter.next()?;
    let (_, mut prev_end) = first.span_bounds()?;
    let mut result = dispatch_value_expr(first, ctx);

    for operand in iter {
        let (op_start, op_end) = operand.span_bounds()?;
        let op_text = recover_op_between(input, prev_end, op_start, layer)?;
        let op = layer
            .ops
            .iter()
            .find(|(t, _)| *t == op_text)
            .map(|(_, o)| *o)?;
        prev_end = op_end;
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(dispatch_value_expr(operand, ctx)),
        };
    }
    Some(result)
}

/// Fallback chain-folding path used when per-operand span recovery
/// failed (operands resolved to typed-projected leaves with no `Span`
/// descendant). Splits the parent compound's source span on the
/// layer's operator tokens at depth-zero (skipping nested parens /
/// quotes), then re-pairs each split with its operand view by
/// document order.
fn fold_via_parent_span_split<'a, 'p: 'a>(
    node: BbnfView<'a, 'p>,
    operands: Vec<BbnfView<'a, 'p>>,
    layer: &PrecedenceLayer,
    ctx: &mut LowerCtx<'p>,
) -> MapExpr {
    let parent_span = node.span_text_opt().unwrap_or_else(|| {
        panic!(
            "fold_value_chain: chain compound has no recoverable parent \
             span — every value-expression compound's source extent must \
             reach at least one `Span` descendant. Compound kind: {:?}",
            node.compound_kind(),
        )
    });

    let split = split_chain_at_operators(parent_span, layer);

    // Operator count must be exactly `operands - 1`.
    debug_assert_eq!(
        split.operators.len() + 1,
        operands.len(),
        "fold_value_chain: parent-span split produced {} operators \
         for {} operands (chain kind = {:?}, span = {:?})",
        split.operators.len(),
        operands.len(),
        node.compound_kind(),
        parent_span,
    );

    let mut iter = operands.into_iter();
    let first = iter
        .next()
        .expect("fold_value_chain: missing first operand");
    let mut result = dispatch_value_expr(first, ctx);

    for (operand, op_text) in iter.zip(split.operators.into_iter()) {
        let op = layer
            .ops
            .iter()
            .find(|(t, _)| *t == op_text)
            .map(|(_, o)| *o)
            .unwrap_or_else(|| {
                panic!(
                    "fold_value_chain: parent-span split returned a token \
                     {:?} not in the layer's op table",
                    op_text,
                )
            });
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(dispatch_value_expr(operand, ctx)),
        };
    }
    result
}

/// Result of splitting a chain's parent span on its layer operators.
struct ChainSplit<'p> {
    /// Operator tokens in document order, each one of the layer's
    /// declared symbols (e.g. `"+"` / `"-"`).
    operators: Vec<&'p str>,
}

/// Split `text` on the precedence-layer's operator tokens at
/// depth-zero (skipping bytes inside parens, single-quoted strings,
/// double-quoted strings). The returned `operators` list contains
/// each operator token in document order.
fn split_chain_at_operators<'p>(text: &'p str, layer: &PrecedenceLayer) -> ChainSplit<'p> {
    let bytes = text.as_bytes();
    let mut operators: Vec<&'p str> = Vec::new();
    let mut i = 0;
    let mut depth_paren: i32 = 0;
    while i < bytes.len() {
        let b = bytes[i];
        // Skip strings.
        if b == b'"' || b == b'\'' {
            let quote = b;
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' && i + 1 < bytes.len() {
                    i += 2;
                    continue;
                }
                if bytes[i] == quote {
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }
        if b == b'(' {
            depth_paren += 1;
            i += 1;
            continue;
        }
        if b == b')' {
            depth_paren -= 1;
            i += 1;
            continue;
        }
        if depth_paren > 0 {
            i += 1;
            continue;
        }
        // Try operator match at this position.
        let rest = &text[i..];
        let mut matched = None;
        for &(op, _) in layer.ops {
            if rest.starts_with(op) {
                matched = Some(op);
                break;
            }
        }
        if let Some(op) = matched {
            operators.push(op);
            i += op.len();
            continue;
        }
        i += 1;
    }
    ChainSplit { operators }
}

/// Collect all operand views from a chain compound. The shape under
/// structural mode is `[first, (rest...)]`; under flattened
/// (non-structural) mode the optimizer may have inlined the rest
/// wrapper, in which case the chain's children are already flat.
///
/// Note: a single-operand chain compound (no operators) collapses
/// to one element; the loop in `fold_value_chain` simply returns
/// that operand's lowering unchanged.
pub(super) fn collect_chain_operands<'a, 'p: 'a>(node: BbnfView<'a, 'p>) -> Vec<BbnfView<'a, 'p>> {
    let mut children = RuntimeView::children(&node);
    let Some(first) = children.next() else {
        return Vec::new();
    };

    // The remaining children are either zero (single-operand chain),
    // one structural sub-tree carrying the iteration operands, or a
    // flat sequence of operands. Detect the iteration-wrapper case
    // by checking whether the second child is a compound (in which
    // case its own children are the rest-of-operands) versus a leaf
    // (in which case the chain children are already flat).
    let mut operands = vec![first];
    let rest: Vec<BbnfView<'a, 'p>> = children.collect();
    if rest.len() == 1 && rest[0].kind() == BbnfKind::Compound && is_iteration_wrapper(rest[0]) {
        for child in RuntimeView::children(&rest[0]) {
            operands.push(child);
        }
    } else {
        for child in rest {
            operands.push(child);
        }
    }
    operands
}

/// Whether a compound child looks like the chain's iteration sub-tree
/// (a structural `(op , operand)*` Repeat-like grouping) rather than
/// a semantic operand. Iteration wrappers under struct-direct emission
/// land on `BbnfCompoundKind::Other` (they are not named rules) — a
/// semantic operand's compound kind is one of the value-layer arms.
fn is_iteration_wrapper(view: BbnfView<'_, '_>) -> bool {
    match view.compound_kind() {
        Some(BbnfCompoundKind::Other) => true,
        // A semantic value-layer compound is always an operand, not
        // a wrapper.
        Some(_) => false,
        None => {
            // Leaf focus — never an iteration wrapper.
            let _ = view.focus();
            false
        }
    }
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
    let gap = input.get(lhs_end as usize..rhs_start as usize)?;
    let trimmed = gap.trim_start();
    for &(op, _) in layer.ops {
        if trimmed.starts_with(op) {
            return Some(op);
        }
    }
    None
}
