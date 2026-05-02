//! Binary-factor Pratt operator-precedence lowering.
//!
//! `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *`
//!
//! Two emitted shapes reach this entry, dispatched on structure:
//!
//! 1. **Pratt reducer chain** — the shape-authoritative emitter
//!    produces an outer compound whose children form a right-leaning
//!    chain. Each reducer is itself a compound stamped with the
//!    op_discriminant in its [`crate::runtime::bbnf::BbnfView::branch_tag`]
//!    and three children `[LHS, Span_op_leaf, RHS]`. The walk below
//!    flattens the chain into source order.
//! 2. **Walker-era iteration-pair layout** — `[first_operand,
//!    iteration_wrapper]` where the wrapper holds per-iteration `Seq`
//!    compounds each wrapping `(operator, optional_ws, operand)`.

use bbnf_ir::IrNode;

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

use super::super::LowerCtx;
use super::alt::{is_iteration_pair_wrapper, iter_pair_children};
use super::dispatch_expression;

/// Lower a `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *` view.
///
/// [`collect_binary_operands`] flattens the iteration-pair wrapping so
/// the partition loop sees a linear `[operand, operator, operand,
/// operator, operand, ...]` sequence with operator compounds surfaced
/// either as a `BinaryOperators`-stamped compound or as a span-leaf
/// fallback (any compound whose trimmed span matches the fixed
/// `<<` / `>>` / `-` set).
///
/// Single-operand chains (no operators) collapse to the bare
/// operand without wrapping.
pub(crate) fn lower_binary_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let all_children: Vec<BbnfView<'a, 'a>> = collect_binary_operands(node);
    debug_assert!(
        !all_children.is_empty(),
        "binary_factor: chain compound produced zero children (text = {:?})",
        node.span_text(),
    );

    // Partition children into (operators, operands). An operator
    // child is recognised either by its compound shape or by its
    // trimmed span text matching one of the fixed operators.
    let mut operands: Vec<BbnfView<'a, 'a>> = Vec::new();
    let mut inline_ops: Vec<&'a str> = Vec::new();
    for child in &all_children {
        if let Some(op) = recognize_binary_operator(*child) {
            inline_ops.push(op);
        } else {
            operands.push(*child);
        }
    }

    if operands.len() <= 1 {
        let only = operands
            .into_iter()
            .next()
            .expect("binary_factor: no operands after partition");
        return dispatch_expression(only, ctx);
    }

    let input = node.input();
    let mut iter = operands.into_iter();
    let first = iter.next().unwrap();
    let mut prev_end = first.byte_span().map(|(_, hi)| hi).unwrap_or(0);
    let mut result = dispatch_expression(first, ctx);
    let mut op_iter = inline_ops.into_iter();

    for operand in iter {
        let lo = operand.byte_span().map(|(lo, _)| lo).unwrap_or(prev_end);
        let op_text = op_iter
            .next()
            .or_else(|| recover_binary_op(input, prev_end, lo))
            .unwrap_or_else(|| {
                panic!(
                    "lower/expression: binary_factor could not resolve \
                 operator — no binary_operators child and source gap \
                 {:?} contains no recognized token (chain = {:?})",
                    &input[prev_end as usize..lo as usize],
                    node.span_text(),
                )
            });
        prev_end = operand.byte_span().map(|(_, hi)| hi).unwrap_or(lo);
        result = apply_binary_op(result, op_text, operand, ctx);
    }
    result
}

/// Recognise a `binary_factor` operator child. Returns the operator
/// token (`"<<"` / `">>"` / `"-"`) if the child represents an
/// operator compound, `None` if it is an operand.
fn recognize_binary_operator<'a>(child: BbnfView<'a, 'a>) -> Option<&'a str> {
    let trimmed = child.span_text().trim();
    if matches!(trimmed, "<<" | ">>" | "-") {
        return Some(trimmed);
    }
    None
}

/// Collect the flattened child sequence of a `binary_factor`
/// compound as `[first_operand, op, operand, op, operand, ...]`.
fn collect_binary_operands<'a>(node: BbnfView<'a, 'a>) -> Vec<BbnfView<'a, 'a>> {
    // Pratt-shape branch: detect a reducer-chain outer and walk it
    // into the flat `[operand, op_leaf, operand, op_leaf, …]`
    // sequence.
    if let Some(chain) = collect_pratt_reducer_chain(node) {
        return chain;
    }

    // Walker-era iteration-pair branch.
    let mut children = node.children();
    let Some(first) = children.next() else {
        return Vec::new();
    };
    let mut operands = vec![first];
    let rest: Vec<BbnfView<'a, 'a>> = children.collect();

    // Collect iteration-pair compounds. Under structural mode there
    // is always a single anonymous wrapper compound; under inline
    // mode the wrapper may be absent and operands sit directly
    // after the first mapped_factor.
    let pairs: Vec<BbnfView<'a, 'a>> =
        if rest.len() == 1 && matches!(rest[0].compound_kind(), Some(BbnfCompoundKind::Other)) {
            rest[0].children().collect()
        } else {
            rest
        };

    for pair in pairs {
        if is_iteration_pair_wrapper(pair) {
            for grandchild in iter_pair_children(pair) {
                operands.push(grandchild);
            }
        } else {
            operands.push(pair);
        }
    }

    operands
}

/// Detect a Pratt reducer-chain outer and walk it into the flat
/// `[initial_operand, op_leaf, operand, op_leaf, operand, …]`
/// sequence expected by [`lower_binary_factor`]'s partition loop.
fn collect_pratt_reducer_chain<'a>(outer: BbnfView<'a, 'a>) -> Option<Vec<BbnfView<'a, 'a>>> {
    // Three entry shapes reach this detector:
    //
    //  (a) `outer` is the proper `binary_factor` Rule compound. Its
    //      `children()` yield EXACTLY ONE direct child, the tail
    //      reducer.
    //  (b) `outer` is the tail reducer itself.
    //  (c) `outer` is a concatenation-iteration wrapper whose own
    //      `children()` surfaces `[lhs, op_leaf, rhs]` directly.
    let tail_reducer = if is_pratt_reducer(outer) {
        // Case (b)
        outer
    } else {
        let mut iter = outer.children();
        let first = iter.next()?;
        let second = iter.next();
        let third = iter.next();
        let fourth = iter.next();

        if second.is_none() && fourth.is_none() && is_pratt_reducer(first) {
            // Case (a)
            first
        } else if fourth.is_none() {
            // Case (c)
            let op_leaf = second?;
            let _rhs = third?;
            if !op_leaf_has_pratt_shape(op_leaf) {
                return None;
            }
            outer
        } else {
            return None;
        }
    };

    let mut reversed: Vec<BbnfView<'a, 'a>> = Vec::new();
    let mut current = tail_reducer;
    loop {
        let kids: Vec<BbnfView<'a, 'a>> = current.children().collect();
        if kids.len() != 3 {
            return None;
        }
        let lhs = kids[0];
        let op_leaf = kids[1];
        let rhs = kids[2];
        reversed.push(rhs);
        reversed.push(op_leaf);
        if is_pratt_reducer(lhs) {
            current = lhs;
        } else {
            reversed.push(lhs);
            break;
        }
    }
    reversed.reverse();
    Some(reversed)
}

/// Shape check: `view` is a Pratt op-leaf — a Span leaf whose
/// trimmed span text is one of the fixed binary_operators tokens
/// (`<<` / `>>` / `-`).
fn op_leaf_has_pratt_shape<'a>(view: BbnfView<'a, 'a>) -> bool {
    if view.kind() != BbnfKind::Span {
        return false;
    }
    matches!(view.span_text().trim(), "<<" | ">>" | "-")
}

/// Structural check: `view` matches the shape of a Pratt reducer
/// compound — a compound with op_discriminant `branch_tag` and
/// three children whose middle is the op-leaf Span.
///
/// Accepts `branch_tag ∈ {0,1,2}` to cover every binary operator's
/// op_discriminant within `PRECEDENCE_ENTRIES_binary_factor`.
pub(super) fn is_pratt_reducer<'a>(view: BbnfView<'a, 'a>) -> bool {
    if view.kind() != BbnfKind::Compound {
        return false;
    }
    let branch = view.branch_tag();
    if !matches!(branch, Some(0) | Some(1) | Some(2)) {
        return false;
    }
    let mut kids = view.children();
    let _lhs = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    let op_leaf = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    let _rhs = match kids.next() {
        Some(v) => v,
        None => return false,
    };
    if kids.next().is_some() {
        return false;
    }
    // Middle child must be the op-leaf: Span kind, trimmed span
    // text is one of the fixed operator tokens.
    if op_leaf.kind() != BbnfKind::Span {
        return false;
    }
    matches!(op_leaf.span_text().trim(), "<<" | ">>" | "-")
}

/// Structural check: `view`'s direct children surface as a flat
/// `[operand, op_leaf, operand, op_leaf, …]` Pratt sequence.
pub(super) fn looks_like_pratt_flat<'a>(view: BbnfView<'a, 'a>) -> bool {
    for child in view.children() {
        if child.kind() == BbnfKind::Span && matches!(child.span_text().trim(), "<<" | ">>" | "-") {
            return true;
        }
    }
    false
}

/// Recover a binary-factor operator (`<<` / `>>` / `-`) from the
/// source slice between two adjacent operand spans.
///
/// **AZ-IV.W1.6 carry**: the alt_dispatch typed-leaf substrate at
/// `shapes/alt_dispatch/branches.rs:227-298` pushes the matched
/// operator as a typed Span via `push_leaf_with_str`, but the
/// pre-W1.6 `iter_pair_children` walk in `alt::iter_pair_children`
/// surfaces the iteration-pair WRAPPER (whose span_text covers both
/// the operator and the trailing operand). The structural
/// extraction needed to peel through the wrapper to the operator
/// Span itself remains future work; until that lands, the
/// source-gap recovery preserves JSON `object` / `array` parity
/// (`"{" >> (( pair << comma ? ) *)?w << "}"`).
///
/// The gap may contain trailing modifier tokens (`?`, `?w`, `*`,
/// `+`) and `)` close-delimiters that the codegen alt_dispatch path
/// doesn't surface as Span leaves — so the operand's recovered
/// byte_span ends at its last source-leaf descendant rather than at
/// the actual end of the operand parse. Scan the gap forward for
/// the first occurrence of an unquoted operator token (skipping
/// quoted strings and regex literals via `super::find_unquoted` so
/// a literal like `"<<"` inside a string doesn't shadow a real
/// operator). Two-character operators are tried first so `-` doesn't
/// shadow `->` or `<<` doesn't shadow `<`.
fn recover_binary_op<'a>(input: &'a str, lhs_end: u32, rhs_start: u32) -> Option<&'a str> {
    if rhs_start < lhs_end {
        return None;
    }
    let gap = &input[lhs_end as usize..rhs_start as usize];
    for &op in &["<<", ">>"] {
        if super::find_unquoted(gap, op).is_some() {
            return Some(op);
        }
    }
    let mut search_from = 0usize;
    while let Some(off) = super::find_unquoted(&gap[search_from..], "-") {
        let abs = search_from + off;
        let next = gap.as_bytes().get(abs + 1).copied();
        if next != Some(b'>') {
            return Some("-");
        }
        search_from = abs + 1;
    }
    None
}

fn apply_binary_op<'a>(
    lhs: IrNode,
    op_text: &str,
    operand: BbnfView<'a, 'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let rhs = dispatch_expression(operand, ctx);
    match op_text {
        "<<" => IrNode::Skip(Box::new(lhs), Box::new(rhs)),
        ">>" => IrNode::Next(Box::new(lhs), Box::new(rhs)),
        "-" => IrNode::Minus(Box::new(lhs), Box::new(rhs)),
        other => panic!(
            "lower/expression/pratt: apply_binary_op saw an unknown \
             binary_operator token {:?} (recovered from operand gap)",
            other,
        ),
    }
}
