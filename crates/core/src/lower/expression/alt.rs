//! Alternation / concatenation lowering.
//!
//! `alternation = ( concatenation ?w , "|" ? ) +`
//! `concatenation = ( binary_factor ?w , "," ? ) +`
//!
//! Both bodies share the same iteration shape: pairs of `(content,
//! optional_separator)` under a possibly-wrapped Repeat. The
//! single-part case collapses to the bare expression (no `Alt` /
//! `Seq` wrapper).

use bbnf_ir::{AltBranch, IrNode};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::super::LowerCtx;
use super::super::tape_walk::{find_descendant_by_kind, iter_rep_children};
use super::dispatch_expression;
use super::pratt::looks_like_pratt_flat;

/// Lower an `alternation = ( concatenation ?w , "|" ? ) +` view.
///
/// Iteration children come in `(content, optional_pipe)` pairs;
/// the `+` quantifier may be wrapped in an explicit
/// `TapeKind::Repeat` compound under structural mode. The
/// `iter_rep_children` helper unwraps that wrapper transparently.
/// The optional pipe wrapper is ignored — only the content child
/// of each pair is lowered.
pub(crate) fn lower_alternation<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let branches: Vec<BbnfBootstrapNodeView<'a>> =
        iter_iteration_pairs(node).collect();
    if branches.len() == 1 {
        return dispatch_expression(branches[0], ctx);
    }
    let alts: Vec<AltBranch> = branches
        .into_iter()
        .map(|branch| AltBranch {
            node: dispatch_expression(branch, ctx),
            first_set: None,
        })
        .collect();
    IrNode::Alt(alts, None)
}

/// Lower a `concatenation = ( binary_factor ?w , "," ? ) +` view.
///
/// Same iteration shape as `alternation`: pairs of `(content,
/// optional_comma)` under a possibly-wrapped Repeat. Single-part
/// concatenations collapse to the bare expression (no `Seq`
/// wrapper).
pub(crate) fn lower_concatenation<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let parts: Vec<BbnfBootstrapNodeView<'a>> =
        iter_iteration_pairs(node).collect();
    if parts.len() == 1 {
        return dispatch_expression(parts[0], ctx);
    }
    let children: Vec<IrNode> = parts
        .into_iter()
        .map(|part| dispatch_expression(part, ctx))
        .collect();
    IrNode::Seq(children)
}

/// Iterate the operand views of an iteration-pair compound. The
/// view passed in is an `alternation` / `concatenation` / `call_arg`
/// rule compound; the body is `(operand ?w , sep ?) +` where the
/// quantifier wraps each iteration in a `Repeat` compound and the
/// trailing optional separator (`|` / `,`) consumes bytes without
/// pushing.
///
/// Tape shape under structural mode (the post-AC.2 default):
///
///   `node.children() == [Repeat([operand_1, operand_2, ...])]`
///
/// Each operand is a `Rule` compound for the lower-precedence
/// expression layer (e.g. `binary_factor`); separators don't push.
/// `iter_rep_children` peels the wrapping Repeat transparently and
/// yields the operand compounds directly.
///
/// Under non-structural mode (legacy optimizer flattening), an
/// iteration's body Seq may push its own compound carrying
/// `[operand, optional_sep]`. We detect that case by inspecting
/// the per-iteration view's tape kind: a `TapeKind::Seq` wrapper is
/// the legacy shape and we descend to its `child(0)`; everything
/// else (every `TapeKind::Rule`) is the operand directly.
fn iter_iteration_pairs<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> impl Iterator<Item = BbnfBootstrapNodeView<'a>> + 'a {
    use crate::runtime::tape::TapeKind;
    iter_rep_children(node).filter_map(|pair| {
        // Peel an explicit Seq wrapper around `(content, optional_sep)` —
        // the legacy shape before structural-mode emission flattened it.
        //
        // AX.W0a.2.o exception: under shape-authoritative Pratt emission
        // the concatenation-iteration Seq wrapper's `children()` sib-skip
        // walk can leak the tail-reducer's three direct children
        // `[lhs, op_leaf, rhs]` past the binary_factor outer compound,
        // leaving the wrapper presenting a flat Pratt layout rather than
        // a single-content-child `(content, optional_sep)` shape. Peeling
        // `child(0)` in that case discards `op_leaf` and `rhs` — the
        // `<<` / `>>` / `-` operator + trailing operand — and
        // `lower_binary_factor` reconstructs only the leading portion of
        // the chain. Detect the leak via [`looks_like_pratt_flat`] and
        // hand the WRAPPER down untouched; `dispatch_expression` then
        // routes it through `lower_binary_factor` which processes the
        // wrapper as a virtual tail reducer.
        let candidate = match pair.kind() {
            TapeKind::Seq if !looks_like_pratt_flat(pair) => pair.child(0)?,
            _ => pair,
        };
        // Reject separator / whitespace placeholder compounds that sit
        // alongside the content inside each iteration body. bbnf's
        // iteration shape `(X ?w , "|" ?) +` / `(X ?w , "," ?) +` pushes
        // an empty-span placeholder for the optional `?w`, and the
        // optional `"|"` / `","` separator pushes either an empty
        // placeholder (when absent) or a single punctuation byte (when
        // present). Neither is an alternation / concatenation operand;
        // yielding them would produce phantom `Alt` branches whose span
        // text is empty or a lone `|` / `,`. Only the content compound
        // is kept.
        let span = candidate.span_text().trim();
        if span.is_empty() {
            return None;
        }
        if span == "|" || span == "," {
            return None;
        }
        Some(candidate)
    })
}

/// Whether `view` is an iteration-pair wrapper compound — a `Seq`
/// whose own `rule_kind` is neither `mapped_factor` nor
/// `binary_operators` and whose trimmed span is not itself an
/// operator token. Such wrappers hold the `(operator, operand)`
/// pair emitted by the walker for each iteration of the
/// `( binary_operators ?w , mapped_factor ) *` body.
pub(super) fn is_iteration_pair_wrapper<'a>(view: BbnfBootstrapNodeView<'a>) -> bool {
    use crate::runtime::tape::TapeKind;
    if view.rule_kind() == BbnfBootstrapRuleKind::mapped_factor
        || view.rule_kind() == BbnfBootstrapRuleKind::binary_operators
    {
        return false;
    }
    let trimmed = view.span_text().trim();
    if matches!(trimmed, "<<" | ">>" | "-") {
        return false;
    }
    matches!(view.kind(), TapeKind::Seq | TapeKind::Rule)
}

/// Iterate the substantive children of an iteration-pair wrapper:
/// skip empty-span placeholders and whitespace-only artefacts, and
/// peel any intermediate anonymous `Seq` wrapper around the
/// operator Alt so the operator compound surfaces at the top level.
pub(super) fn iter_pair_children<'a>(
    view: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use crate::runtime::tape::TapeKind;
    let mut out: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
    for child in view.children() {
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        // Peel an anonymous Seq wrapper whose own `rule_kind` is
        // `int_lit` (the DTA sentinel for non-rule structural
        // compounds) and whose trimmed span IS the operator token.
        // The walker stamps `binary_operators` on the inner Alt;
        // descending through any intervening Seq wrappers surfaces
        // it regardless of DTA nesting depth.
        if child.rule_kind() == BbnfBootstrapRuleKind::int_lit
            && matches!(trimmed, "<<" | ">>" | "-")
            && matches!(child.kind(), TapeKind::Seq)
        {
            if let Some(inner) = find_descendant_by_kind(
                child,
                BbnfBootstrapRuleKind::binary_operators,
            ) {
                out.push(inner);
                continue;
            }
            // Fall back to the wrapper itself — `recognize_binary_operator`
            // matches by span text.
        }
        out.push(child);
    }
    out
}
