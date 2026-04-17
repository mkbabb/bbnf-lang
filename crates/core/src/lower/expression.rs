//! Lowering: BbnfBootstrapNodeView → IrNode.
//!
//! Layered descent through the grammar hierarchy:
//!   rhs → alternation → concatenation → binary_factor → mapped_factor →
//!   factor → term → leaf
//!
//! Produces IrNode directly from the tape-first bootstrap parse tree —
//! no intermediate Expression AST.
//!
//! Beta reduction is environment-driven, not walker-driven: when a grammar
//! closure is applied, we push a frame on `LowerCtx.env` mapping each param
//! to its argument CST view, lower the body recursively, and pop. Identifier
//! resolution (`resolve_name`) checks the env stack first before the rule
//! table. This eliminates the parallel `substitute_and_lower` walker.
//!
//! Tranche AE: shape-agnostic walking. Layer functions iterate
//! children via [`super::tape_walk::iter_rep_children`] (peels a
//! single `TapeKind::Repeat` wrapper produced by the `+` / `*`
//! quantifier under preserve_identity mode), select positional
//! children by `rule_kind()` rather than by index, and panic on
//! unhandled rule_kinds — silent `IrNode::Epsilon` fallbacks are
//! forbidden because they corrupt every downstream rule body
//! invisibly.

use std::collections::HashMap;

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapExpr, TypeDesc};
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::LowerCtx;
use super::tape_walk::{
    collect_siblings_by_kind, find_descendant_by_kind, find_sibling_by_kind,
    iter_rep_children, peel_transparent,
};
use super::value_expr::{
    deep_unwrap_value, extract_value_func_name, is_type_name, lower_value_expr,
    split_numeric_suffix, unwrap_value_ident_str,
};

// ─── Top-level entry ──────────────────────────────────────────────────────────

/// Lower the RHS of a rule: `rhs = closure | alternation`.
///
/// The caller (`host.rs::extract_grammar` or `lower_to_ir`) hands
/// us whatever non-`identifier` child of the rule compound it
/// found. That child may be the `rhs` wrapper itself (preserved
/// under structural mode), one of the `grammar_item` / `directive`
/// transparent wrappers (peeled defensively), the rule's
/// `closure`, or directly the alternation/concatenation/factor
/// expression head (when the optimizer flattened intervening
/// wrappers in non-structural mode). Peel transparent wrappers,
/// then dispatch on the head's `rule_kind()`.
pub(crate) fn lower_rhs<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    let node = peel_transparent(node);
    dispatch_expression(node, ctx)
}

/// Dispatch any expression view to the appropriate layer based on
/// its `rule_kind()`. The single source of truth for the layered
/// descent — every layer function calls back into this dispatcher
/// when it needs to lower a child whose role is "another
/// expression of unknown layer".
///
/// Unknown rule_kinds panic with a descriptive message; silent
/// `Epsilon` fallbacks would corrupt every rule body downstream
/// without any error. The bbnf.bbnf grammar is a closed schema —
/// every reachable rule_kind has an explicit handler.
fn dispatch_expression<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Peel named transparent wrappers (`grammar_item`, `directive`,
    // `lhs`) at the dispatch entry so layer functions can assume
    // their input is the semantic head.
    let node = peel_transparent(node);

    // Leaf fast-path: only when the node's span is a SINGLE closed
    // token — a bare identifier, an unquoted epsilon keyword, a
    // regex literal bounded by `/ ... /`, or a string literal
    // bounded by matching quotes with no interior punctuation that
    // would indicate a compound expression. The guard prevents a
    // multi-branch alternation whose full-source span happens to
    // start and end with the same quote byte (e.g. `literal`'s body)
    // from being swallowed as a single `Literal` IR node.
    if is_single_token_span(node) {
        if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
            return leaf;
        }
    }

    // Anonymous wrapper compounds — `Rule` / `Repeat` compounds
    // whose own `variant_idx` either isn't mapped in the current
    // `BbnfBootstrapRuleKind` enum (Unknown) or maps to the
    // sentinel `int_lit` because variant_idx=0 (the catch-all
    // Repeat / Optional sentinel) collides with `int_lit`'s
    // rule_id slot. Walk substantive Rule children (skipping
    // every Repeat / Optional wrapper child — separators,
    // optional placeholders, and the like) and re-dispatch:
    //
    // - Zero substantive Rule children → `Epsilon` (empty
    //   placeholder).
    // - Exactly one substantive Rule child → peel and recurse.
    // - Multiple substantive Rule children → treat as a `Seq`
    //   (the wrapper carries iteration content separated by
    //   non-pushing literals like `,` or `|`; under HEAD's
    //   hand-patched generated.rs the separator commas push
    //   single-byte Repeat compounds we ignore).
    //
    // After AE.4's clean regen these wrappers will have proper
    // enum entries and the peel becomes mostly unreachable.
    use ::bbnf::runtime::tape::TapeKind;
    let kind = node.rule_kind();
    let is_unknown_or_sentinel = matches!(
        kind,
        BbnfBootstrapRuleKind::Unknown | BbnfBootstrapRuleKind::int_lit,
    );
    let is_wrapper_kind = matches!(
        node.kind(),
        TapeKind::Rule | TapeKind::Repeat | TapeKind::Seq | TapeKind::Alt,
    );
    if is_unknown_or_sentinel && is_wrapper_kind {
        let parent_offset = node.cursor().offset();
        let substantive: Vec<BbnfBootstrapNodeView<'a>> = node
            .children()
            .filter(|c| {
                matches!(
                    c.kind(),
                    TapeKind::Rule | TapeKind::Seq | TapeKind::Alt,
                )
            })
            // Cycle guard: drop any child whose tape offset
            // equals the parent's. A malformed compound whose
            // children include itself would otherwise re-enter
            // dispatch_expression at the same view and produce
            // an infinitely-nested IrNode tree, which the
            // codegen later SIGBUS-es while flattening.
            .filter(|c| c.cursor().offset() != parent_offset)
            .collect();
        match substantive.len() {
            0 => return IrNode::Epsilon,
            1 => return dispatch_expression(substantive[0], ctx),
            _ => {
                // Multiple substantive children — treat as a
                // concatenation (Seq) and lower each.
                let parts: Vec<IrNode> = substantive
                    .into_iter()
                    .map(|c| dispatch_expression(c, ctx))
                    .collect();
                return IrNode::Seq(parts);
            }
        }
    }

    match node.rule_kind() {
        BbnfBootstrapRuleKind::closure => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            // closure = "|", first_param, rest_params, "|", body
            let body = node
                .child(4)
                .expect("closure: missing body child");
            lower_rhs(body, ctx)
        }
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            lower_alternation(node, ctx)
        }
        BbnfBootstrapRuleKind::concatenation => lower_concatenation(node, ctx),
        BbnfBootstrapRuleKind::binary_factor => lower_binary_factor(node, ctx),
        BbnfBootstrapRuleKind::mapped_factor => lower_mapped_factor(node, ctx),
        BbnfBootstrapRuleKind::factor => lower_factor(node, ctx),

        // Term layer — `term` is the canonical rule_kind after the
        // AF substrate-break closure; `literal` / `regex` /
        // `identifier` surface directly when the optimizer inlines
        // the term wrapper. All four route through the same
        // content-dispatched `lower_term` entry.
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex
        | BbnfBootstrapRuleKind::identifier => lower_term(node, ctx),

        // Comments and directives are grammar-level metadata — they
        // produce no IR contribution. Directives (@recover, @import,
        // @pretty, @ws, @token, @debug, @host, @no_collapse) are
        // consumed by host.rs during grammar extraction; expression
        // lowering treats them as Epsilon.
        //
        // Every directive variant must be listed: under DTA the
        // fallback `lower_term` descends into the compound's span
        // text and reports `unknown leading byte '@'` (or the
        // terminator `';'` when the sub-rule happens to carry its
        // terminator Alt as a direct descendant). `host_directive`
        // was dropped pre-AW-II; re-included here to close the
        // gap.
        //
        // AU.2.5: the old `_0` sub-variants (`import_directive_0`,
        // `pretty_directive_0`, `debug_directive_0`) vanished once
        // Ref-scalar projection collapsed their owning Alt to
        // homogeneous `Span`. The variants dispatched here anyway,
        // so dropping the stale names closes the loop without
        // losing any lowering capability.
        BbnfBootstrapRuleKind::comment
        | BbnfBootstrapRuleKind::big_comment
        | BbnfBootstrapRuleKind::recover_directive
        | BbnfBootstrapRuleKind::import_directive
        | BbnfBootstrapRuleKind::pretty_directive
        | BbnfBootstrapRuleKind::ws_directive
        | BbnfBootstrapRuleKind::token_directive
        | BbnfBootstrapRuleKind::debug_directive
        | BbnfBootstrapRuleKind::host_directive
        | BbnfBootstrapRuleKind::directive
        | BbnfBootstrapRuleKind::directive_0
        | BbnfBootstrapRuleKind::grammar_item_0 => {
            IrNode::Epsilon
        }

        // Fallback: the bbnf grammar is closed at the expression
        // hierarchy layers above, so anything else is a term-shaped
        // node whose `rule_kind` was dropped by a sub-variant dedupe
        // pass in the generated schema. `lower_term` content-dispatches
        // by the span's leading byte and panics if it cannot classify
        // the shape — no silent `Epsilon` fallthrough.
        _ => lower_term(node, ctx),
    }
}

// ─── Grammar expression hierarchy ─────────────────────────────────────────────

/// Lower an `alternation = ( concatenation ?w , "|" ? ) +` view.
///
/// Iteration children come in `(content, optional_pipe)` pairs;
/// the `+` quantifier may be wrapped in an explicit
/// `TapeKind::Repeat` compound under structural mode. The
/// `iter_rep_children` helper unwraps that wrapper transparently.
/// The optional pipe wrapper is ignored — only the content child
/// of each pair is lowered.
fn lower_alternation<'a>(
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
fn lower_concatenation<'a>(
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
    use ::bbnf::runtime::tape::TapeKind;
    iter_rep_children(node).filter_map(|pair| {
        // Peel an explicit Seq wrapper around `(content, optional_sep)` —
        // the legacy shape before structural-mode emission flattened it.
        let candidate = match pair.kind() {
            TapeKind::Seq => pair.child(0)?,
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

/// Lower a `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *` view.
///
/// Tape shape under DTA:
///
/// ```text
/// Seq (binary_factor)
///   [0]: Seq (mapped_factor)                  -- first operand
///   [1]: Rule (iteration-pair wrapper)
///     [0]: Seq (int_lit)                      -- iteration pair 1
///       [0]: Seq (int_lit)                    -- operator wrapper
///         [0]: Alt (binary_operators)         -- operator itself
///       [1]: Seq (mapped_factor)              -- pair operand
///     [1]: Seq (int_lit)                      -- iteration pair 2
///       ... (same shape)
/// ```
///
/// [`collect_binary_operands`] flattens the iteration-pair wrapping so
/// the partition loop sees a linear `[operand, operator, operand,
/// operator, operand, ...]` sequence with operator compounds surfaced
/// as `rule_kind=binary_operators` Alts (emitted by the walker's
/// variant_idx stamping) — or, as a belt-and-braces fallback, any
/// compound whose trimmed span matches the fixed `<<` / `>>` / `-`
/// set.
///
/// Single-operand chains (no operators) collapse to the bare
/// operand without wrapping.
fn lower_binary_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let all_children: Vec<BbnfBootstrapNodeView<'a>> = collect_binary_operands(node);
    debug_assert!(
        !all_children.is_empty(),
        "binary_factor: chain compound produced zero children (text = {:?})",
        node.span_text(),
    );

    // Partition children into (operators, operands). An operator child
    // is recognized by any of:
    //
    //  1. `rule_kind() == binary_operators` — the walker stamped the
    //     Alt compound with the binary_operators rule id. This is the
    //     structurally-rich signal under DTA when the walker's
    //     variant_idx stamping reaches the Alt.
    //  2. Trimmed span text matches one of `<<` / `>>` / `-` —
    //     lightweight belt-and-braces recognition that catches the
    //     case where the operator compound was lifted/inlined to a
    //     bare Seq whose only meaningful content is the literal
    //     punctuation. The fixed alphabet is safe: the surrounding
    //     grammar rules out operand compounds whose full trimmed span
    //     is exactly one of these three tokens.
    //
    // Every other child is an operand.
    let mut operands: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
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
    let mut prev_end = first.span().1;
    let mut result = dispatch_expression(first, ctx);
    let mut op_iter = inline_ops.into_iter();

    for operand in iter {
        // Prefer structurally-identified operator children; fall back
        // to source-gap recovery when the operator compound was fully
        // span-elided (no record to recognize).
        let op_text = op_iter.next().or_else(|| {
            recover_binary_op(input, prev_end, operand.span().0)
        }).unwrap_or_else(|| {
            panic!(
                "lower/expression.rs: binary_factor could not resolve \
                 operator — no binary_operators child and source gap \
                 {:?} contains no recognized token (chain = {:?})",
                &input[prev_end as usize..operand.span().0 as usize],
                node.span_text(),
            )
        });
        prev_end = operand.span().1;
        result = apply_binary_op(result, op_text, operand, ctx);
    }
    result
}

/// Recognize a `binary_factor` operator child. Returns the operator
/// token (`"<<"` / `">>"` / `"-"`) if the child represents an operator
/// compound, `None` if it is an operand.
///
/// Matches either:
///  * `child.rule_kind() == binary_operators` — the walker's
///    variant_idx stamping reached the Alt compound; its trimmed
///    span carries the operator literal directly.
///  * A compound whose trimmed span is exactly one of the fixed
///    operator tokens — the DTA lifter may wrap the Alt in an
///    anonymous `Seq` whose own `rule_kind` is not `binary_operators`
///    but whose full text is still just the operator punctuation.
///
/// The fixed-alphabet span check is safe because operand compounds
/// are never literal `<<` / `>>` / `-` — the grammar's
/// `mapped_factor` layer produces quoted literals, identifiers,
/// regex atoms, or bracket-grouped sub-expressions, none of which
/// trim down to one of these three tokens.
fn recognize_binary_operator<'a>(
    child: BbnfBootstrapNodeView<'a>,
) -> Option<&'a str> {
    if child.rule_kind() == BbnfBootstrapRuleKind::binary_operators {
        return Some(child.span_text().trim());
    }
    let trimmed = child.span_text().trim();
    if matches!(trimmed, "<<" | ">>" | "-") {
        return Some(trimmed);
    }
    None
}

/// Collect the flattened child sequence of a `binary_factor`
/// compound as `[first_operand, op, operand, op, operand, ...]`.
///
/// The DTA tape wraps the iteration body
/// `( binary_operators ?w , mapped_factor )` in two layers:
///   1. An outer Rule/Repeat compound that holds every iteration.
///   2. Per iteration, a `Seq` whose children include the
///      `binary_operators` Alt and the `mapped_factor` operand.
///
/// This function peels both layers so the partition loop in
/// [`lower_binary_factor`] sees a flat sequence. Under the legacy
/// fn-per-rule emission (pre-DTA) the Repeat wrapper held operand
/// compounds directly; the flattening degrades gracefully — a child
/// that is already `mapped_factor` / `binary_operators` / an
/// operator-shaped compound passes through unchanged.
fn collect_binary_operands<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use ::bbnf::runtime::tape::TapeKind;
    let mut children = node.children();
    let Some(first) = children.next() else {
        return Vec::new();
    };
    let mut operands = vec![first];
    let rest: Vec<BbnfBootstrapNodeView<'a>> = children.collect();

    // Collect iteration-pair compounds. Under DTA there is always a
    // single Rule/Repeat wrapper; under the legacy fn-per-rule tape
    // the wrapper may be absent and operands sit directly after the
    // first mapped_factor.
    let pairs: Vec<BbnfBootstrapNodeView<'a>> =
        if rest.len() == 1 && matches!(rest[0].kind(), TapeKind::Repeat | TapeKind::Rule) {
            rest[0].children().collect()
        } else {
            rest
        };

    for pair in pairs {
        // Each pair is either a direct operand (legacy shape) or a
        // Seq compound wrapping `(operator, optional_ws, operand)`.
        // Descend one level and flatten when the pair is a wrapper;
        // keep it whole otherwise.
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

/// Whether `view` is an iteration-pair wrapper compound — a `Seq`
/// whose own `rule_kind` is neither `mapped_factor` nor
/// `binary_operators` and whose trimmed span is not itself an
/// operator token. Such wrappers hold the `(operator, operand)`
/// pair emitted by the walker for each iteration of the
/// `( binary_operators ?w , mapped_factor ) *` body.
fn is_iteration_pair_wrapper<'a>(view: BbnfBootstrapNodeView<'a>) -> bool {
    use ::bbnf::runtime::tape::TapeKind;
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
fn iter_pair_children<'a>(
    view: BbnfBootstrapNodeView<'a>,
) -> Vec<BbnfBootstrapNodeView<'a>> {
    use ::bbnf::runtime::tape::TapeKind;
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

/// Recover a binary-factor operator (`<<` / `>>` / `-`) from the
/// source slice between two adjacent operand spans. Skips leading
/// whitespace and matches the longest valid operator prefix; the
/// two-character operators are listed first so `<` doesn't shadow
/// `<<`.
fn recover_binary_op<'a>(input: &'a str, lhs_end: u32, rhs_start: u32) -> Option<&'a str> {
    let gap = &input[lhs_end as usize..rhs_start as usize];
    let trimmed = gap.trim_start();
    for &op in &["<<", ">>", "-"] {
        if trimmed.starts_with(op) {
            return Some(op);
        }
    }
    None
}

fn apply_binary_op<'a>(
    lhs: IrNode,
    op_text: &str,
    operand: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let rhs = dispatch_expression(operand, ctx);
    match op_text {
        "<<" => IrNode::Skip(Box::new(lhs), Box::new(rhs)),
        ">>" => IrNode::Next(Box::new(lhs), Box::new(rhs)),
        "-" => IrNode::Minus(Box::new(lhs), Box::new(rhs)),
        other => panic!(
            "lower/expression.rs: apply_binary_op saw an unknown \
             binary_operator token {:?} (recovered from operand gap)",
            other,
        ),
    }
}

/// Lower a `mapped_factor = factor ( "->" value_expr type? )?` view.
///
/// The first child is the underlying factor. The optional `->`
/// mapping is detected via span emptiness: when present, the
/// mapping carries `(arrow_keyword, value_expr_view, type_view?)`
/// as its children (regardless of whether the optimizer wrapped
/// them in a sub-compound).
fn lower_mapped_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Under the clean regen, `factor` is inlined into
    // `mapped_factor`, so this compound's children are
    //   `[big_comment?, term, modifier?, big_comment?, mapping?]`
    // with each optional slot represented by an empty-span
    // placeholder. Classify children by span content rather than
    // by positional index: the modifier is the child whose trimmed
    // span is one of `?` / `?w` / `*` / `+`; the mapping group is
    // the child whose trimmed span starts with `->` / `=>`; the
    // term is the first remaining substantive child.
    let mut term_node: Option<BbnfBootstrapNodeView<'a>> = None;
    let mut modifier_text: Option<String> = None;
    let mut mapping_node: Option<BbnfBootstrapNodeView<'a>> = None;
    for c in node.children() {
        let span_text = c.span_text();
        let trimmed = span_text.trim();
        if trimmed.is_empty() {
            continue;
        }
        if matches!(trimmed, "?" | "?w" | "*" | "+") {
            modifier_text = Some(trimmed.to_string());
            continue;
        }
        if trimmed.starts_with("->") || trimmed.starts_with("=>") {
            mapping_node = Some(c);
            continue;
        }
        if term_node.is_none() {
            term_node = Some(c);
        }
    }
    // Detect bracket-delimited group from the span text. Under the
    // tape-first parser, the opening `[`, `{`, `(`, `@{` delimiters
    // are consumed span-only and don't appear as child records. The
    // group kind is recoverable from the leading byte of the
    // compound's span.
    let leading_byte = node.span_text().trim_start().as_bytes().first().copied();
    let group_kind = match leading_byte {
        Some(b'[') => Some(GroupKind::Optional),
        Some(b'{') => Some(GroupKind::Many),
        Some(b'@') if node.span_text().trim_start().starts_with("@{") => {
            Some(GroupKind::SpanCapture)
        }
        Some(b'(') => Some(GroupKind::Paren),
        _ => None,
    };

    let mut base = if let Some(term) = term_node {
        let inner = dispatch_expression(term, ctx);
        // Apply group wrapping when the mapped_factor's span has a
        // bracket delimiter that wasn't handled by the term dispatch
        // (because the brackets are span-only, not child records).
        match group_kind {
            Some(GroupKind::Optional) => IrNode::Repeat {
                inner: Box::new(inner),
                lo: 0,
                hi: 1,
            },
            Some(GroupKind::Many) => IrNode::Repeat {
                inner: Box::new(inner),
                lo: 0,
                hi: u32::MAX,
            },
            Some(GroupKind::SpanCapture) => {
                let fn_id = ctx.fns.push(bbnf_ir::FnDescriptor::SpanCapture);
                IrNode::Map {
                    inner: Box::new(inner),
                    fn_id,
                }
            }
            Some(GroupKind::Paren) | None => inner,
        }
    } else {
        // No tape-level term child — the compound's body is a bare
        // leaf (identifier, literal, regex) that consumed bytes
        // without pushing a record. Recover the leaf from the
        // compound's own span_text after stripping any trailing
        // modifier (`?w`/`?`/`*`/`+`) and any trailing mapping
        // group (`-> ...` / `=> ...`).
        let raw = node.span_text();
        let mut stripped: &str = raw.trim();
        // Strip trailing modifier first (it's closest to the term).
        if let Some(modifier) = &modifier_text {
            stripped = stripped
                .strip_suffix(modifier.as_str())
                .unwrap_or(stripped)
                .trim();
        }
        // Strip mapping group — everything from `->` / `=>` onward.
        // Use find_unquoted to avoid matching `->` inside quoted
        // literals like `"->"`.
        if let Some(idx) = find_unquoted(stripped, "->") {
            stripped = stripped[..idx].trim();
        } else if let Some(idx) = find_unquoted(stripped, "=>") {
            stripped = stripped[..idx].trim();
        }
        lower_leaf_by_span_text_str(stripped, ctx).unwrap_or_else(|| {
            panic!(
                "mapped_factor: no tape term child and span_text {:?} (after stripping \
                 modifier {:?} + mapping) resolved to {:?} which is not a recognisable leaf",
                raw, modifier_text, stripped
            )
        })
    };
    if let Some(modifier) = &modifier_text {
        base = apply_modifier(base, modifier);
    }
    let Some(mapping_node) = mapping_node else {
        return base;
    };
    // Extract the value_expr + optional type_annotation. The
    // mapping group's children are normally [arrow_kw, value_expr,
    // type_annotation?]; under flattened shapes, walk the children
    // and pluck the value_expr / type_annotation by rule_kind.
    let value_expr = find_value_expr_child(mapping_node)
        .expect("mapped_factor mapping: missing value expression");
    let type_ann = find_type_annotation_child(mapping_node);
    let fn_id = lower_map_arrow(value_expr, type_ann, ctx);
    let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
    IrNode::Map {
        inner: Box::new(base),
        fn_id,
    }
}

fn find_value_expr_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // Outermost-first descendant search. Each call to
    // `find_descendant_by_kind` returns the first occurrence in
    // document order, so the lookups run from the outermost class
    // (`value_expr`) down through each precedence wrapper to the
    // inlined atoms. First hit wins.
    //
    // Under DTA, structurally-anonymous groupings (the mapping's
    // `( "->" ?w , ( value_expr , ... ) )` parent) emit Seq compounds
    // whose `variant_idx` was never stamped by a `DtaState::Ref`
    // dispatch — they surface via the `int_lit`/`Unknown` sentinel on
    // `rule_kind()`. Iterating direct children and admitting the
    // whole value-layer set (including `int_lit` for the real literal
    // leaf case) would return the sentinel Seq before ever reaching
    // the real `value_expr` inside. Targeting specific kinds in
    // outermost-first priority order avoids the sentinel trap — no
    // descendant of a mapping Seq has `rule_kind == value_expr`
    // unless it's a genuine `value_expr` record.
    const OUTER_KINDS: &[BbnfBootstrapRuleKind] = &[
        BbnfBootstrapRuleKind::value_expr,
        BbnfBootstrapRuleKind::value_closure,
        BbnfBootstrapRuleKind::value_or,
        BbnfBootstrapRuleKind::value_and,
        BbnfBootstrapRuleKind::value_cmp,
        BbnfBootstrapRuleKind::value_add,
        BbnfBootstrapRuleKind::value_mul,
        BbnfBootstrapRuleKind::value_unary,
        BbnfBootstrapRuleKind::value_atom,
        BbnfBootstrapRuleKind::value_fn_call,
        BbnfBootstrapRuleKind::value_path,
        BbnfBootstrapRuleKind::value_input,
        BbnfBootstrapRuleKind::value_ident,
        BbnfBootstrapRuleKind::float_lit,
        BbnfBootstrapRuleKind::bool_lit,
        BbnfBootstrapRuleKind::string_lit,
        // `int_lit` goes last — real int_lit leaves carry their
        // numeric literal span; if a sentinel-tagged Seq wrapper also
        // carries `int_lit` its span will overlap (or exceed) a real
        // value_expr descendant, which the outermost lookups above
        // resolve first.
        BbnfBootstrapRuleKind::int_lit,
    ];

    for &kind in OUTER_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            return Some(v);
        }
    }
    None
}

fn find_type_annotation_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // The `type_annotation` compound lives at unpredictable depths
    // under DTA's Seq wrappers (the mapping grouping may nest one
    // or more structurally-anonymous Seq layers between the
    // `mapping_node` and the inner `type_annotation`). A descendant
    // search handles both fn-per-rule and DTA shapes uniformly.
    find_descendant_by_kind(node, BbnfBootstrapRuleKind::type_annotation)
}

/// Lower a `factor = big_comment? term ?w modifier? big_comment?` view.
///
/// Children are positionally `[big_comment?, term, modifier?,
/// big_comment?]`, but positional reads are unreliable under
/// structural mode (optional comment / modifier slots push zero-
/// width placeholders that shift later indices). Dispatch by role:
///
/// 1. Find the term child via `find_descendant_by_kind(term)` — the
///    canonical clean-regen shape.
/// 2. Fall back to the first non-metadata, non-placeholder child
///    under HEAD's hand-patched schema where the term may surface
///    under a dedupe-dropped rule_kind or inline directly as a
///    `literal` / `regex` / `identifier` child.
/// 3. Collect the optional `modifier` and apply its quantifier to the
///    base term. Two shapes are handled:
///
///    a) Direct modifier child with `rule_kind() == modifier` (when
///       the modifier's tape record carries its own variant_idx).
///    b) Modifier wrapped in an optional `Repeat(vi=0)` placeholder
///       (the common case under clean-regen: the `?` optional wrapper
///       pushes a `TapeKind::Repeat` compound with `variant_idx = 0`
///       which maps to `int_lit` in the RuleKind enum, masking the
///       inner modifier's identity). Detected by span-text
///       classification: a non-empty child whose trimmed text is one
///       of `?w`, `?`, `*`, `+`.
fn lower_factor<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Under DTA the factor body `big_comment? , term ?w , modifier? ,
    // big_comment?` is emitted inside one or more anonymous Seq /
    // Alt / Repeat wrappers by the lifter's tape-shape requirement;
    // `term` and `modifier` surface as sibling body components one or
    // two anonymous-wrapper levels deeper than the factor compound
    // itself. `find_sibling_by_kind` descends only through those
    // anonymous wrappers — crucially NOT into the sibling `term`'s
    // own subtree — so the modifier returned belongs to THIS factor,
    // not to some nested expression inside the term.
    let term = find_sibling_by_kind(node, BbnfBootstrapRuleKind::term)
        .or_else(|| find_term_child_by_elimination(node))
        .unwrap_or_else(|| {
            panic!(
                "factor: missing term child in span {:?}",
                node.span_text(),
            )
        });
    let base = lower_term(term, ctx);

    // Modifier detection: first try rule_kind-based lookup (works when
    // the modifier compound carries its own variant_idx). Fall back to
    // span-text classification for the clean-regen shape where the
    // modifier sits inside a Repeat(vi=0) optional wrapper whose
    // rule_kind maps to `int_lit` instead of `modifier`.
    if let Some(mod_node) = find_sibling_by_kind(node, BbnfBootstrapRuleKind::modifier)
        && mod_node.span().1 > mod_node.span().0
    {
        return apply_modifier(base, mod_node.span_text());
    }
    // Span-text fallback: scan children for a modifier token.
    for child in node.children() {
        let trimmed = child.span_text().trim();
        if matches!(trimmed, "?w" | "?" | "*" | "+") {
            return apply_modifier(base, trimmed);
        }
    }
    base
}

/// Locate the term child of a `factor` compound by eliminating the
/// known metadata / placeholder children.
///
/// The factor body is `big_comment? term ?w modifier? big_comment?`,
/// so any child whose rule_kind is not `big_comment` / `comment` /
/// `modifier` and whose span is non-empty carries the term. This
/// path is the substrate-break fallback under HEAD's hand-patched
/// schema where `term` may surface under a dedupe-dropped rule_kind
/// or inline directly as a `literal` / `regex` / `identifier` leaf.
fn find_term_child_by_elimination<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    for child in node.children() {
        match child.rule_kind() {
            BbnfBootstrapRuleKind::big_comment
            | BbnfBootstrapRuleKind::comment
            | BbnfBootstrapRuleKind::modifier => continue,
            _ => {
                if is_empty_placeholder(child) {
                    continue;
                }
                return Some(child);
            }
        }
    }
    None
}

fn apply_modifier(base: IrNode, text: &str) -> IrNode {
    match text {
        "?" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: 1,
        },
        "*" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 0,
            hi: u32::MAX,
        },
        "+" => IrNode::Repeat {
            inner: Box::new(base),
            lo: 1,
            hi: u32::MAX,
        },
        "?w" => IrNode::OptionalWhitespace(Box::new(base)),
        _ => base,
    }
}

/// True if `view` is an empty placeholder compound — a Repeat
/// (or Rule) compound whose span has zero width (`lo == hi`) AND
/// whose child run is empty. These arise from the post-AC.2
/// emission of missing optional groups in `factor` / `mapped_factor`
/// (e.g. an absent leading `big_comment?` or trailing modifier
/// `?w`): the emitter pushes a Repeat compound with `(state.offset,
/// state.offset)` and zero children to keep positional layout
/// stable, and downstream consumers detect the placeholder by its
/// empty span. Used by `dispatch_expression`'s wrapper-peel to
/// skip over these placeholders when finding the single
/// substantive child of an anonymous wrapper compound.
fn is_empty_placeholder(view: BbnfBootstrapNodeView<'_>) -> bool {
    let (lo, hi) = view.span();
    lo == hi && view.children().next().is_none()
}

/// Span-text leaf classifier — the shape-agnostic fallback when
/// `dispatch_expression` can't route a view by its `rule_kind`.
///
/// Inspects the node's source slice (after trimming surrounding
/// whitespace) and matches against the closed bbnf leaf vocabulary:
///
/// - `/regex/` — regex literal (delimited by forward slashes,
///   matching the bbnf grammar's `regex` rule)
/// - `"text"` / `'text'` / `` `text` `` — string literal in any of
///   bbnf's three quote styles (matching the `literal` rule)
/// - `epsilon` / `ε` — epsilon
/// - bare identifier — nonterminal reference resolved against the
///   rule table or the closure environment
///
/// Returns `None` when the span text doesn't look like a leaf —
/// the caller falls through to its rule_kind-based dispatch (or
/// panics if there's no handler).
///
/// This fallback exists because the bootstrap regen runs against a
/// hand-patched `generated.rs` whose `BbnfBootstrapRuleKind` enum
/// doesn't map every rule's `variant_idx` (some pre-AC.2 inlining
/// dropped enum entries while leaving the rule functions in place).
/// Source-slice classification is the only stable signal under
/// that mismatch. After AE.4's clean regen the rule_kind dispatch
/// becomes complete and this path becomes mostly unreachable for
/// the bbnf bootstrap path.
/// Whether `node`'s trimmed span is a single closed bbnf leaf
/// token — a bare identifier, `epsilon` / `ε`, a regex literal,
/// or a quoted string with no interior break into a compound
/// expression.
///
/// The gate stops the leaf fast-path in `dispatch_expression`
/// from swallowing a multi-branch alternation whose full-source
/// span happens to start and end with the same quote / bracket
/// byte (e.g. `literal`'s body, which begins with `"` and ends
/// with another `"` on the last branch after a run of `,` / `|`
/// compounds in between).
fn is_single_token_span(node: BbnfBootstrapNodeView<'_>) -> bool {
    let trimmed = node.span_text().trim();
    if trimmed.is_empty() {
        return false;
    }
    let bytes = trimmed.as_bytes();
    // Regex literal `/ ... /` — forbid a `/` inside the body that
    // would imply multiple regex literals concatenated.
    if bytes[0] == b'/' && bytes.len() >= 2 && bytes[bytes.len() - 1] == b'/' {
        let interior = &trimmed[1..trimmed.len() - 1];
        let mut escaped = false;
        for ch in interior.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == '/' {
                return false;
            }
        }
        return true;
    }
    // String literal `"..."` / `'...'` / `` `...` `` — forbid
    // unescaped interior quotes.
    if let first @ (b'"' | b'\'' | b'`') = bytes[0] {
        if bytes.len() < 2 || bytes[bytes.len() - 1] != first {
            return false;
        }
        let quote = first as char;
        let interior = &trimmed[1..trimmed.len() - 1];
        let mut escaped = false;
        for ch in interior.chars() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                return false;
            }
        }
        return true;
    }
    // Epsilon keyword.
    if trimmed == "epsilon" || trimmed == "ε" {
        return true;
    }
    // Bare identifier — matches the bbnf `identifier` regex.
    if (bytes[0].is_ascii_alphabetic() || bytes[0] == b'_')
        && bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return true;
    }
    false
}

/// Span-text variant of [`lower_leaf_by_span_text`] that operates
/// on a bare `&str` (rather than a view). Used by
/// [`lower_mapped_factor`] to recover an identifier / literal /
/// regex that was consumed by the parser without pushing its own
/// tape record.
fn lower_leaf_by_span_text_str<'a>(
    raw: &'a str,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.len() >= 2
        && trimmed.starts_with('/')
        && trimmed.ends_with('/')
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        let id = ctx.strings.intern(inner);
        return Some(IrNode::Regex(id));
    }
    if trimmed.len() >= 2 {
        let bytes = trimmed.as_bytes();
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if first == last && (first == b'"' || first == b'\'' || first == b'`') {
            let inner = &trimmed[1..trimmed.len() - 1];
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            return Some(IrNode::Literal(id));
        }
    }
    if trimmed == "epsilon" || trimmed == "ε" {
        return Some(IrNode::Epsilon);
    }
    let id_bytes = trimmed.as_bytes();
    if !id_bytes.is_empty()
        && (id_bytes[0].is_ascii_alphabetic() || id_bytes[0] == b'_')
        && id_bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return Some(resolve_name(trimmed, ctx));
    }
    None
}

/// Search for `needle` in `haystack`, skipping over quoted strings
/// (`"..."`, `'...'`, `` `...` ``) and regex literals (`/.../`).
/// Returns the byte offset of the first unquoted occurrence, or `None`.
///
/// This prevents matching `->` inside a quoted literal like `"->"`.
fn find_unquoted(haystack: &str, needle: &str) -> Option<usize> {
    let bytes = haystack.as_bytes();
    let needle_bytes = needle.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        // Skip quoted strings and regex literals.
        if b == b'"' || b == b'\'' || b == b'`' || b == b'/' {
            let quote = b;
            i += 1;
            while i < bytes.len() {
                if bytes[i] == b'\\' {
                    i += 2; // skip escape sequence
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
        // Check for needle match at this position.
        if i + needle_bytes.len() <= bytes.len()
            && &bytes[i..i + needle_bytes.len()] == needle_bytes
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

fn lower_leaf_by_span_text<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    use ::bbnf::runtime::tape::TapeKind;
    // Classify by the node's span text regardless of kind — under
    // DTA a `/regex/` or `"string"` leaf may be wrapped in a Seq/Alt
    // compound whose span still encodes the full token. The
    // `is_single_token_span` caller gate upstream guarantees we
    // only reach here when the span is a single closed bbnf token.
    match node.kind() {
        TapeKind::Rule
        | TapeKind::Span
        | TapeKind::Literal
        | TapeKind::Regex
        | TapeKind::Seq
        | TapeKind::Alt => {}
        _ => return None,
    }
    let raw = node.span_text();
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }

    // Regex literal: starts and ends with `/`, length ≥ 2.
    if trimmed.len() >= 2
        && trimmed.starts_with('/')
        && trimmed.ends_with('/')
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        let id = ctx.strings.intern(inner);
        return Some(IrNode::Regex(id));
    }

    // String literal in any of the three bbnf quote styles.
    if trimmed.len() >= 2 {
        let bytes = trimmed.as_bytes();
        let first = bytes[0];
        let last = bytes[bytes.len() - 1];
        if first == last && (first == b'"' || first == b'\'' || first == b'`') {
            let inner = &trimmed[1..trimmed.len() - 1];
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            return Some(IrNode::Literal(id));
        }
    }

    // Epsilon keyword.
    if trimmed == "epsilon" || trimmed == "ε" {
        return Some(IrNode::Epsilon);
    }

    // Bare identifier — must match `[_a-zA-Z][_a-zA-Z0-9-]*` per
    // the bbnf `identifier` rule. Trailing alphanumerics / `-` /
    // `_` only; bail on anything else.
    let id_bytes = trimmed.as_bytes();
    if !id_bytes.is_empty()
        && (id_bytes[0].is_ascii_alphabetic() || id_bytes[0] == b'_')
        && id_bytes
            .iter()
            .all(|b| b.is_ascii_alphanumeric() || *b == b'_' || *b == b'-')
    {
        return Some(resolve_name(trimmed, ctx));
    }

    None
}

/// Lower a `term` compound.
///
/// The bbnf.bbnf `term` rule is a heterogeneous alternation:
///
/// ```bbnf
/// term = "ε" | "epsilon"
///      | identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?
///      | literal | regex
///      | "@{" , rhs ?w , "}"
///      | "(" , rhs ?w , ")"
///      | "[" , rhs ?w , "]"
///      | "{" , rhs ?w , "}" ;
/// ```
///
/// Every branch dedupes into the same `(Span, children, Span)` tape shape,
/// so the generated enum cannot express which branch hit — dispatch by
/// **content**, not by an enum sub-variant. The single source of truth is
/// the leading byte of the compound's source span (or the leading byte of
/// its first substantive child): `(` / `[` / `{` / `@` discriminate the
/// grouped forms, `"` / `'` / `` ` `` a literal, `/` a regex, `ε` / `e`
/// epsilon, anything else an identifier (possibly followed by grammar-call
/// argument parentheses).
///
/// This is the closed-schema entry point for the term layer. Every caller —
/// `dispatch_expression`, `lower_factor`, the implicit cascade under
/// `peel_transparent` — routes through here; there is no other
/// term-lowering path.
fn lower_term<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Plain leaf — literal, regex, identifier, epsilon — classified
    // directly from the span text. Covers every term branch whose
    // source span IS the leaf token (no inner expression to descend into).
    if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
        return leaf;
    }

    let raw = node.span_text();
    let trimmed = raw.trim_start();
    let bytes = trimmed.as_bytes();
    if bytes.is_empty() {
        panic!(
            "lower_term: empty span for rule_kind {:?} (full span = {:?})",
            node.rule_kind(),
            raw,
        );
    }

    // Grouped forms: `"(" rhs ")"`, `"[" rhs "]"`, `"{" rhs "}"`, `"@{" rhs "}"`.
    // The opening byte (plus a look-ahead for the two-byte `@{`) is the
    // only discriminator — the four forms all have the same child layout
    // `[open_delim, inner, close_delim]` at the tape level.
    match bytes[0] {
        b'(' => lower_grouped_term(node, GroupKind::Paren, ctx),
        b'[' => lower_grouped_term(node, GroupKind::Optional, ctx),
        b'{' => lower_grouped_term(node, GroupKind::Many, ctx),
        b'@' if bytes.len() >= 2 && bytes[1] == b'{' => {
            lower_grouped_term(node, GroupKind::SpanCapture, ctx)
        }
        // An identifier head with optional grammar-call argument
        // parentheses. The leaf classifier already handled the bare-identifier
        // case; reaching here means the span carries trailing `(...)` call args.
        b if b.is_ascii_alphabetic() || b == b'_' => {
            lower_identifier_with_optional_call(node, ctx)
        }
        other => panic!(
            "lower_term: unknown leading byte {:?} for rule_kind {:?} (span = {:?})",
            other as char,
            node.rule_kind(),
            raw,
        ),
    }
}

/// The four grouped-term flavors, discriminated by the opening delimiter
/// byte of the term compound's span.
#[derive(Clone, Copy)]
enum GroupKind {
    /// `"(" rhs ")"` — plain grouping.
    Paren,
    /// `"[" rhs "]"` — optional group, lowered to `Repeat { lo: 0, hi: 1 }`.
    Optional,
    /// `"{" rhs "}"` — many-group, lowered to `Repeat { lo: 0, hi: u32::MAX }`.
    Many,
    /// `"@{" rhs "}"` — span-capture, lowered to `Map + FnDescriptor::SpanCapture`.
    SpanCapture,
}

/// Descend into the inner expression of a grouped term compound and apply
/// the grouping operator.
fn lower_grouped_term<'a>(
    node: BbnfBootstrapNodeView<'a>,
    kind: GroupKind,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let inner_view = find_inner_expression(node).unwrap_or_else(|| {
        panic!(
            "lower_term (grouped): missing inner expression in span {:?}",
            node.span_text(),
        )
    });
    // `lower_rhs` peels `rhs`/`grammar_item`/`directive`/`lhs` wrappers
    // before dispatching, matching the top-level rule body entry point.
    let expr = lower_rhs(inner_view, ctx);
    match kind {
        GroupKind::Paren => expr,
        GroupKind::Optional => IrNode::Repeat {
            inner: Box::new(expr),
            lo: 0,
            hi: 1,
        },
        GroupKind::Many => IrNode::Repeat {
            inner: Box::new(expr),
            lo: 0,
            hi: u32::MAX,
        },
        GroupKind::SpanCapture => {
            let fn_id = ctx.fns.push(bbnf_ir::FnDescriptor::SpanCapture);
            IrNode::Map {
                inner: Box::new(expr),
                fn_id,
            }
        }
    }
}

/// Locate the substantive inner child of a grouped term compound —
/// the `rhs` (or collapsed descendant) expression between the
/// `(...)` / `[...]` / `{...}` / `@{...}` delimiters.
///
/// Under DTA the walker emits the opening and closing delimiters
/// as `TapeKind::Literal` / `Span` leaves alongside a Seq compound
/// that carries the body's semantic children. Fn-per-rule emission
/// placed the body compound directly as a child of the grouped
/// term; DTA's structural lifter wraps it one Seq deeper, so a
/// direct-children scan misses the inner expression.
///
/// Strategy (in order):
///
/// 1. **Primary**: `find_descendant_by_kind` for each expression-
///    layer rule kind in outermost-first order (`rhs`, `alternation`,
///    `concatenation`, `binary_factor`, `mapped_factor`, `factor`,
///    `term`, `closure`). First match wins — the outermost class
///    that surfaces in the descendants is the root of the inner
///    subtree. Returning at the first hit avoids picking a nested
///    `term` when the body is a multi-branch alternation.
/// 2. **Fallback**: iterate descendants in document order; stop at
///    the first compound whose span is non-empty and whose
///    `rule_kind()` is a body-expression class (the same set as
///    above). Handles tape shapes where the outermost wrapper was
///    inlined away by the lifter.
///
/// Bracket Literal leaves are skipped implicitly — their `rule_kind`
/// is not in the expression-layer set.
fn find_inner_expression<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    // Outermost-first: a nested `term` inside a multi-branch
    // alternation must not pre-empt the alternation itself.
    const EXPRESSION_KINDS: &[BbnfBootstrapRuleKind] = &[
        BbnfBootstrapRuleKind::rhs,
        BbnfBootstrapRuleKind::closure,
        BbnfBootstrapRuleKind::alternation,
        BbnfBootstrapRuleKind::concatenation,
        BbnfBootstrapRuleKind::binary_factor,
        BbnfBootstrapRuleKind::mapped_factor,
        BbnfBootstrapRuleKind::factor,
        BbnfBootstrapRuleKind::term,
    ];

    for &kind in EXPRESSION_KINDS {
        if let Some(v) = find_descendant_by_kind(node, kind) {
            let (lo, hi) = v.span();
            if hi > lo {
                return Some(v);
            }
        }
    }

    // Fallback: document-order descent looking for any compound
    // with an expression-layer rule_kind (covers tape shapes the
    // outermost-first scan missed — e.g. if the lifter inlined
    // every layer down to an alternation sub-branch without an
    // `alternation` kind surviving).
    find_body_expression_descendant(node, EXPRESSION_KINDS)
}

/// Document-order descent helper — returns the first compound under
/// `view` (inclusive) whose `rule_kind()` is in `kinds` and whose
/// span is non-empty. Used as the fallback arm of
/// [`find_inner_expression`].
fn find_body_expression_descendant<'a>(
    view: BbnfBootstrapNodeView<'a>,
    kinds: &[BbnfBootstrapRuleKind],
) -> Option<BbnfBootstrapNodeView<'a>> {
    let kind = view.rule_kind();
    let (lo, hi) = view.span();
    if hi > lo && kinds.contains(&kind) {
        return Some(view);
    }
    for child in view.children() {
        if let Some(found) = find_body_expression_descendant(child, kinds) {
            return Some(found);
        }
    }
    None
}

/// Lower an `identifier ( "(" call_arg ("," call_arg)* ")" )?` term.
///
/// The identifier is the first substantive child (either a `TapeKind::Rule`
/// compound for the `identifier` rule, or directly surfacing as a `Span`/
/// `Literal` leaf when the optimizer inlined the wrapper). Call arguments,
/// when present, are zero or more child compounds with `rule_kind = call_arg`.
fn lower_identifier_with_optional_call<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // Under DTA the term's identifier branch body
    // `identifier , ( "(" , call_arg ?w , ( "," ?w , call_arg ?w ) * , ")" ) ?`
    // is emitted inside anonymous Seq / Alt wrappers; the `identifier`
    // record and each `call_arg` surface as sibling body components
    // one or more wrapper levels below the term compound. Use the
    // sibling-scoped descent so the search doesn't step past a
    // sibling boundary into a nested expression's own identifier or
    // call_arg list. Fallback first-substantive-child handles the
    // optimizer-inlined shape where the identifier leaf surfaces
    // directly.
    let ident = find_sibling_by_kind(node, BbnfBootstrapRuleKind::identifier)
        .or_else(|| {
            node.children().find(|c| {
                let (lo, hi) = c.span();
                hi > lo
            })
        })
        .unwrap_or_else(|| {
            panic!(
                "lower_term (identifier): no identifier child in span {:?}",
                node.span_text(),
            )
        });
    let name = ident.span_text();
    // Positional `call_arg` siblings under the term compound, gathered
    // with the same scoping rule: no descent past the nested
    // expressions inside any individual arg's body.
    let mut call_args: Vec<BbnfBootstrapNodeView<'a>> = Vec::new();
    collect_siblings_by_kind(node, BbnfBootstrapRuleKind::call_arg, &mut call_args);
    if call_args.is_empty() {
        resolve_name(name, ctx)
    } else {
        lower_grammar_call(name, &call_args, ctx)
    }
}

/// Resolve a bare nonterminal name to an `IrNode`.
///
/// Lookup order:
/// 1. **Beta-reduction environment** — if the name is bound by an enclosing
///    grammar closure application, lower the bound CST view in the current
///    context (which itself sees the same env, supporting nested closures).
/// 2. **Rule table** — emit `IrNode::Ref(rule_id)`.
/// 3. **Recovery fallback** — emit `Epsilon` if `recovery_mode`, else panic.
fn resolve_name<'a>(name: &'a str, ctx: &mut LowerCtx<'a>) -> IrNode {
    if let Some(bound) = lookup_env(name, &ctx.env) {
        return lower_rhs(bound, ctx);
    }
    match ctx.name_to_rule_id.get(name) {
        Some(&rule_id) => IrNode::Ref(rule_id),
        None if ctx.recovery_mode => IrNode::Epsilon,
        None => panic!(
            "unknown nonterminal `{}` — should have been caught by validate_ast()",
            name,
        ),
    }
}

/// Beta-reduction: apply a grammar closure call.
///
/// Pushes a fresh env frame mapping each parameter to its argument CST view,
/// lowers the closure body in the augmented context (so identifier sites
/// inside the body see the bindings via `resolve_name`), then pops the
/// frame.  If `name` doesn't refer to a closure, falls back to a normal
/// nonterminal reference.
fn lower_grammar_call<'a>(
    name: &'a str,
    args: &[BbnfBootstrapNodeView<'a>],
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let Some(closure) = ctx.closures.get(name) else {
        return resolve_name(name, ctx);
    };
    // Snapshot params + body so we can take `&mut ctx` for env push/pop.
    let params: Vec<&'a str> = closure.params.clone();
    let body: BbnfBootstrapNodeView<'a> = closure.body;

    assert_eq!(
        args.len(),
        params.len(),
        "arity mismatch: `{}` expects {} args, got {}",
        name,
        params.len(),
        args.len(),
    );

    let mut frame: HashMap<&'a str, BbnfBootstrapNodeView<'a>> =
        HashMap::with_capacity(args.len());
    for (param, arg) in params.iter().zip(args.iter()) {
        frame.insert(*param, *arg);
    }
    ctx.env.push(frame);
    let result = lower_rhs(body, ctx);
    ctx.env.pop();
    result
}

/// Walk the env stack from innermost to outermost, returning the first
/// binding for `name` (if any).
fn lookup_env<'a>(
    name: &str,
    env: &[HashMap<&'a str, BbnfBootstrapNodeView<'a>>],
) -> Option<BbnfBootstrapNodeView<'a>> {
    for frame in env.iter().rev() {
        if let Some(&bound) = frame.get(name) {
            return Some(bound);
        }
    }
    None
}

// ─── MapArrow / ValueExpr lowering ─────────────────────────────────────────────

/// Resolve a type name string to a `TypeDesc`, preferring concrete scalar
/// variants (`TypeDesc::F64`, `TypeDesc::U8`, etc.) over the generic
/// `TypeDesc::Named`. Falls back to `Named` for unknown type names so
/// backend-specific resolution can still occur.
fn resolve_type_name(name: &str, ctx: &mut LowerCtx<'_>) -> TypeDesc {
    TypeDesc::from_scalar_name(name).unwrap_or_else(|| {
        let sid = ctx.strings.intern(name);
        TypeDesc::Named(sid)
    })
}

/// Lower a `->` mapping to a `FnId`.
///
/// `value_expr` is the value expression node, `type_ann` is the
/// optional type annotation node.
fn lower_map_arrow<'a>(
    value_expr: BbnfBootstrapNodeView<'a>,
    type_ann: Option<BbnfBootstrapNodeView<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_ann.and_then(|ann| {
        if ann.rule_kind() == BbnfBootstrapRuleKind::type_annotation {
            // type_annotation = (":", type_node) — child(1) is the name.
            let type_node = ann.child(1)?;
            let name = match type_node.rule_kind() {
                BbnfBootstrapRuleKind::type_name | BbnfBootstrapRuleKind::identifier => {
                    type_node.span_text()
                }
                _ => return None,
            };
            Some(resolve_type_name(name, ctx))
        } else {
            None
        }
    });

    // Type-shorthand: bare type name like `-> f64`.
    // unwrap_value_ident_str recursively peels value expression wrappers.
    if let Some(name) = unwrap_value_ident_str(value_expr) {
        if is_type_name(name) && return_type.is_none() {
            let td = resolve_type_name(name, ctx);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(td),
            });
        }
    }

    // Extract type suffix from integer/float literals when no explicit type annotation.
    // The tape-rewrite may fold `int_lit`/`float_lit` into `value_atom`,
    // so when the leaf rule_kind is `value_atom` we recover the type
    // suffix from the span text — the span IS the authoritative source
    // of the literal's textual form.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let text = match leaf.rule_kind() {
            BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::float_lit => {
                Some(leaf.span_text())
            }
            BbnfBootstrapRuleKind::value_atom => {
                // value_atom inlined the leaf — inspect leading byte to
                // decide if this is a numeric literal.
                let t = leaf.span_text().trim_start();
                match t.as_bytes().first() {
                    Some(b'0'..=b'9') | Some(b'.') => Some(t),
                    _ => None,
                }
            }
            _ => None,
        };
        text.and_then(|t| {
            let (_, suffix) = split_numeric_suffix(t);
            if suffix.is_empty() {
                None
            } else {
                Some(TypeDesc::from_scalar_name(suffix).unwrap_or_else(|| {
                    let sid = ctx.strings.intern(suffix);
                    TypeDesc::Named(sid)
                }))
            }
        })
    });

    // Bool literal → bool type.
    // Same recovery: when `bool_lit` has been folded into `value_atom`,
    // detect `true`/`false` from the span text.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let is_bool = match leaf.rule_kind() {
            BbnfBootstrapRuleKind::bool_lit => true,
            BbnfBootstrapRuleKind::value_atom => {
                let t = leaf.span_text().trim_start();
                let is_word_boundary = |s: &str, len: usize| {
                    !s.as_bytes()
                        .get(len)
                        .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
                };
                (t.starts_with("true") && is_word_boundary(t, 4))
                    || (t.starts_with("false") && is_word_boundary(t, 5))
            }
            _ => false,
        };
        if is_bool {
            Some(TypeDesc::Bool)
        } else {
            None
        }
    });

    // @host return type propagation.
    let return_type = return_type.or_else(|| {
        let func_name = extract_value_func_name(deep_unwrap_value(value_expr));
        func_name.and_then(|name| {
            ctx.host_fns
                .and_then(|hosts| hosts.get(name.as_str()))
                .and_then(|opt_type| opt_type.as_ref())
                .map(|type_name| resolve_type_name(type_name, ctx))
        })
    });

    let map_expr = lower_value_expr(value_expr, ctx);

    ctx.fns.push(FnDescriptor::Expr {
        expr: map_expr,
        return_type,
    })
}

// ─── Specialization ────────────────────────────────────────────────────────────

fn try_specialize_map_fn(inner: &IrNode, fn_id: FnId, ctx: &mut LowerCtx<'_>) -> FnId {
    let desc = &ctx.fns.fns[fn_id as usize];

    // Extract the expression and type name for specialization.
    // Handles both concrete scalar TypeDescs (TypeDesc::F64, etc.) and
    // legacy Named("f64") — the latter may still appear from explicit
    // type annotations using unknown names.
    //
    // AU.2.4: The type annotation is optional — when it's absent, we
    // attempt structural inference from the (regex, expr) pair: a
    // HexDigits regex paired with `FnCall(_, [Input])` is a hex
    // converter, even without an explicit `: u32` annotation. This
    // side-steps a bootstrap-grammar surface-syntax quirk where
    // `-> fn_call(input) : type_name` loses the type annotation
    // between the function-call arg list and the type name.
    let desc_clone = desc.clone();
    let (expr, type_name_owned) = match &desc_clone {
        FnDescriptor::Expr {
            expr,
            return_type: Some(td),
        } => {
            let name = match td {
                TypeDesc::Named(sid) => Some(ctx.strings.resolve(*sid).to_owned()),
                TypeDesc::F64 => Some("f64".to_owned()),
                TypeDesc::U32 => Some("u32".to_owned()),
                _ => None,
            };
            match name {
                Some(n) => (expr.clone(), n),
                None => return fn_id,
            }
        }
        FnDescriptor::Expr {
            expr,
            return_type: None,
        } => {
            // Type-free specialization: infer from (regex, expr) shape.
            // Currently only the hex-converter pattern is inferred;
            // number-conversion already requires the `-> f64` shorthand
            // to carry type information explicitly.
            let IrNode::Regex(sid) = inner else {
                return fn_id;
            };
            let pattern = ctx.strings.resolve(*sid).to_owned();
            if let MapExpr::FnCall { name, args } = expr {
                if args.len() == 1
                    && matches!(args[0], MapExpr::Input | MapExpr::InputProp { .. })
                    && matches!(classify_regex(&pattern), RegexClass::HexDigits)
                {
                    let fn_path_str = ctx.strings.resolve(*name).to_owned();
                    let path_sid = ctx.strings.intern(&fn_path_str);
                    return ctx
                        .fns
                        .push(FnDescriptor::HexConvert { fn_path: path_sid });
                }
            }
            return fn_id;
        }
        _ => return fn_id,
    };

    let regex_sid = match inner {
        IrNode::Regex(sid) => *sid,
        _ => return fn_id,
    };

    let pattern = ctx.strings.resolve(regex_sid).to_owned();

    match type_name_owned.as_str() {
        "f64" => {
            if matches!(expr, MapExpr::Input) {
                if let RegexClass::Numeric { allow_leading_dot, .. } = classify_regex(&pattern) {
                    ctx.fns
                        .push(FnDescriptor::NumberConvert { allow_leading_dot })
                } else {
                    fn_id
                }
            } else {
                fn_id
            }
        }
        "u32" => {
            if let MapExpr::FnCall { name, args } = &expr {
                if args.len() == 1
                    && matches!(args[0], MapExpr::Input | MapExpr::InputProp { .. })
                    && matches!(classify_regex(&pattern), RegexClass::HexDigits)
                {
                    let fn_path_str = ctx.strings.resolve(*name).to_owned();
                    let path_sid = ctx.strings.intern(&fn_path_str);
                    ctx.fns.push(FnDescriptor::HexConvert { fn_path: path_sid })
                } else {
                    fn_id
                }
            } else {
                fn_id
            }
        }
        _ => fn_id,
    }
}
