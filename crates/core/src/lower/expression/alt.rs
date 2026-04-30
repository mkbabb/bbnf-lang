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

use crate::runtime::RuntimeView;
use crate::runtime::bbnf::{BbnfCompoundKind, BbnfKind, BbnfView};

use super::super::LowerCtx;
use super::super::view_walk::iter_rep_children;
use super::dispatch_expression;
use super::pratt::looks_like_pratt_flat;

/// Lower an `alternation = ( concatenation ?w , "|" ? ) +` view.
///
/// Iteration children come in `(content, optional_pipe)` pairs;
/// the `+` quantifier may be wrapped in an explicit anonymous
/// compound under structural mode. The `iter_rep_children` helper
/// unwraps that wrapper transparently. The optional pipe wrapper
/// is ignored — only the content child of each pair is lowered.
pub(crate) fn lower_alternation<'a>(
    node: BbnfView<'a, 'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let branches: Vec<BbnfView<'a, 'a>> = iter_iteration_pairs(node).collect();
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
    node: BbnfView<'a, 'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let parts: Vec<BbnfView<'a, 'a>> = iter_iteration_pairs(node).collect();
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
/// quantifier wraps each iteration in a Repeat-shaped compound and
/// the trailing optional separator (`|` / `,`) consumes bytes
/// without pushing.
///
/// The `iter_rep_children` helper transparently peels one layer of
/// anonymous wrapper around the iteration body, yielding either
/// the operand compound directly (when the optimiser inlined the
/// per-iteration Seq wrapper) or the per-iteration Seq wrapper
/// itself (legacy shape pre-inline). For the latter case we
/// descend to `child(0)` (the content slot) so callers receive the
/// operand head without the optional-separator placeholder.
fn iter_iteration_pairs<'a>(
    node: BbnfView<'a, 'a>,
) -> impl Iterator<Item = BbnfView<'a, 'a>> + 'a {
    iter_rep_children(node).filter_map(|pair| {
        // Peel an explicit Seq wrapper around `(content, optional_sep)` —
        // legacy iteration shape. Detected via the wrapper being an
        // anonymous compound (`BbnfCompoundKind::Other`).
        //
        // AX.W0a.2.o exception: under shape-authoritative Pratt emission
        // the concatenation-iteration Seq wrapper's `children()` sib-skip
        // walk can leak the tail-reducer's three direct children
        // `[lhs, op_leaf, rhs]` past the binary_factor outer compound.
        // Detect via [`looks_like_pratt_flat`] and hand the WRAPPER down
        // untouched; `dispatch_expression` then routes it through
        // `lower_binary_factor`.
        let candidate = match pair.compound_kind() {
            Some(BbnfCompoundKind::Other) if !looks_like_pratt_flat(pair) => pair.child(0)?,
            _ => pair,
        };
        // Reject separator / whitespace placeholder compounds that sit
        // alongside the content inside each iteration body. bbnf's
        // iteration shape pushes empty-span placeholders for absent
        // optionals and a single punctuation byte (when present) for
        // the separator; neither is an alternation / concatenation
        // operand — yielding them would produce phantom branches whose
        // span text is empty or a lone `|` / `,`.
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

/// Whether `view` is an iteration-pair wrapper compound — a
/// non-`MappedFactor`-kind compound whose trimmed span is not
/// itself a binary-operator token. Such wrappers hold the
/// `(operator, operand)` pair emitted by the walker for each
/// iteration of the `( binary_operators ?w , mapped_factor ) *`
/// body.
pub(super) fn is_iteration_pair_wrapper<'a>(view: BbnfView<'a, 'a>) -> bool {
    if matches!(view.compound_kind(), Some(BbnfCompoundKind::MappedFactor)) {
        return false;
    }
    let trimmed = view.span_text().trim();
    if matches!(trimmed, "<<" | ">>" | "-") {
        return false;
    }
    matches!(view.kind(), BbnfKind::Compound)
}

/// Iterate the substantive children of an iteration-pair wrapper:
/// skip empty-span placeholders and whitespace-only artefacts, and
/// peel any intermediate anonymous wrapper around the operator Alt
/// so the operator compound surfaces at the top level.
pub(super) fn iter_pair_children<'a>(
    view: BbnfView<'a, 'a>,
) -> Vec<BbnfView<'a, 'a>> {
    let mut out: Vec<BbnfView<'a, 'a>> = Vec::new();
    for child in view.children() {
        let span = child.span_text();
        let trimmed = span.trim();
        if trimmed.is_empty() {
            continue;
        }
        // Anonymous wrapper compound whose trimmed span IS the
        // binary-operator token. Pass through directly — the
        // op-leaf classifier matches by span text on the Span
        // sub-leaf.
        out.push(child);
    }
    out
}
