//! Binary-factor Pratt operator-precedence lowering.
//!
//! The Pratt emitter projects
//! `[operand, semantic_tag, operand, semantic_tag, operand, ...]`.
//! Each tag is the emitted `PrattEntry::op_discriminant`; the generated
//! per-rule precedence entries are the sole tag-to-operator authority.

use bbnf_ir::IrNode;

use crate::grammar::generated::bbnf::PRECEDENCE_ENTRIES_binary_factor;
use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfKind, BbnfView};

use super::super::LowerCtx;
use super::dispatch_expression;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinaryOperator {
    Skip,
    Next,
    Minus,
}

/// Lower the semantic Pratt projection for
/// `binary_factor = mapped_factor , (binary_operators ?w , mapped_factor)*`.
pub(crate) fn lower_binary_factor<'a>(node: BbnfView<'a, 'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let chain = semantic_pratt_children(node).unwrap_or_else(|| {
        panic!(
            "lower/expression/pratt: binary_factor has no semantic Tag chain: {:?}",
            node.span_text(),
        )
    });
    let mut children = chain.into_iter();
    let first = children
        .next()
        .expect("lower/expression/pratt: binary_factor has no first operand");
    let mut result = dispatch_expression(first, ctx);

    while let Some(tag) = children.next() {
        let operand = children.next().unwrap_or_else(|| {
            panic!(
                "lower/expression/pratt: dangling operator tag in {:?}",
                node.span_text(),
            )
        });
        let tag = tag.tag_value().unwrap_or_else(|| {
            panic!(
                "lower/expression/pratt: expected semantic Tag between operands, got {:?}",
                tag.kind(),
            )
        });
        result = apply_binary_op(result, operator_for_tag(tag), operand, ctx);
    }

    result
}

/// Return the direct semantic Pratt sequence. A single transparent
/// wrapper is admitted because the lowering dispatcher can enter at
/// either the rule compound or its wrapper; both carry the same Tag
/// projection and neither depends on structural branch identity.
fn semantic_pratt_children<'a>(node: BbnfView<'a, 'a>) -> Option<Vec<BbnfView<'a, 'a>>> {
    let direct = node.children().collect::<Vec<_>>();
    if is_semantic_pratt_sequence(&direct) {
        return Some(direct);
    }
    if direct.len() == 1 {
        let nested = direct[0].children().collect::<Vec<_>>();
        if is_semantic_pratt_sequence(&nested) {
            return Some(nested);
        }
    }
    None
}

fn is_semantic_pratt_sequence(children: &[BbnfView<'_, '_>]) -> bool {
    !children.is_empty()
        && children.len() % 2 == 1
        && children
            .iter()
            .enumerate()
            .all(|(index, child)| (index % 2 == 1) == (child.kind() == BbnfKind::Tag))
}

fn operator_for_tag(tag: u8) -> BinaryOperator {
    let entry = PRECEDENCE_ENTRIES_binary_factor
        .iter()
        .find(|entry| entry.op_discriminant == tag)
        .unwrap_or_else(|| {
            panic!("lower/expression/pratt: unknown binary_factor operator tag {tag}")
        });
    match (entry.byte, entry.second_byte) {
        (b'<', Some(b'<')) => BinaryOperator::Skip,
        (b'>', Some(b'>')) => BinaryOperator::Next,
        (b'-', None) => BinaryOperator::Minus,
        other => {
            panic!("lower/expression/pratt: unsupported binary_factor operator bytes {other:?}")
        }
    }
}

/// Dispatcher predicate for a semantic Pratt chain.
pub(super) fn is_pratt_reducer<'a>(view: BbnfView<'a, 'a>) -> bool {
    semantic_pratt_children(view).is_some_and(|children| children.len() >= 3)
}

/// Whether a view directly exposes the semantic Pratt chain.
pub(super) fn looks_like_pratt_flat<'a>(view: BbnfView<'a, 'a>) -> bool {
    let children = view.children().collect::<Vec<_>>();
    children.len() >= 3 && is_semantic_pratt_sequence(&children)
}

fn apply_binary_op<'a>(
    lhs: IrNode,
    operator: BinaryOperator,
    operand: BbnfView<'a, 'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let rhs = dispatch_expression(operand, ctx);
    match operator {
        BinaryOperator::Skip => IrNode::Skip(Box::new(lhs), Box::new(rhs)),
        BinaryOperator::Next => IrNode::Next(Box::new(lhs), Box::new(rhs)),
        BinaryOperator::Minus => IrNode::Minus(Box::new(lhs), Box::new(rhs)),
    }
}
