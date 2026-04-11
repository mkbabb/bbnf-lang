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
use super::tape_walk::{find_child_by_kind, iter_rep_children, peel_transparent};
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

    // Try to classify as a closed leaf vocabulary first: regex
    // literal, string literal, identifier, epsilon. The
    // span-text classifier is the shape-agnostic recognizer of
    // simple leaves, used both as the named-rule_kind
    // path's fast-path (literal/regex/identifier hits here when
    // they would otherwise need to walk the term sub-variants)
    // and as the fallback when the rule_kind dispatch can't find
    // a handler.
    if let Some(leaf) = lower_leaf_by_span_text(node, ctx) {
        return leaf;
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
        TapeKind::Rule | TapeKind::Repeat,
    );
    if is_unknown_or_sentinel && is_wrapper_kind {
        let parent_offset = node.cursor().offset();
        let substantive: Vec<BbnfBootstrapNodeView<'a>> = node
            .children()
            .filter(|c| c.kind() == TapeKind::Rule)
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
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::term_0
        | BbnfBootstrapRuleKind::term_1
        | BbnfBootstrapRuleKind::term_2
        | BbnfBootstrapRuleKind::value_atom_0
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex
        | BbnfBootstrapRuleKind::identifier => lower_term_dispatch(node, ctx),

        // Comments are skipped at the rule body level — they
        // produce no IR contribution.
        BbnfBootstrapRuleKind::comment | BbnfBootstrapRuleKind::big_comment => {
            IrNode::Epsilon
        }

        other => panic!(
            "lower/expression.rs: dispatch_expression called on \
             unhandled rule_kind {:?} (span = {:?}, text = {:?}). \
             Add an explicit handler for this rule_kind.",
            other,
            node.span(),
            node.span_text(),
        ),
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
    iter_rep_children(node).filter_map(|pair| match pair.kind() {
        TapeKind::Seq => pair.child(0),
        _ => Some(pair),
    })
}

/// Lower a `binary_factor = mapped_factor , ( binary_operators ?w , mapped_factor ) *` view.
///
/// Tape shape under structural mode:
///
///   `node.children() == [mapped_factor_1, Repeat([mapped_factor_2, mapped_factor_3, ...])]`
///
/// The `binary_operators` rule is inlined (its body is just an
/// alternation of literal punctuation tokens), so the operator
/// bytes are consumed but never push a tape compound. To recover
/// each operator, inspect the source slice between
/// `operands[i].span().1` and `operands[i+1].span().0` and match
/// against the fixed `<<` / `>>` / `-` set (longest-first).
///
/// Single-operand chains (no operators) collapse to the bare
/// operand without wrapping.
fn lower_binary_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let operands: Vec<BbnfBootstrapNodeView<'a>> = collect_binary_operands(node);
    debug_assert!(
        !operands.is_empty(),
        "binary_factor: chain compound produced zero operands (text = {:?})",
        node.span_text(),
    );

    let mut iter = operands.into_iter();
    let first = iter
        .next()
        .expect("binary_factor: missing first operand");
    let input = node.input();
    let mut prev_end = first.span().1;
    let mut result = dispatch_expression(first, ctx);
    for operand in iter {
        let op_text = recover_binary_op(input, prev_end, operand.span().0)
            .unwrap_or_else(|| {
                panic!(
                    "lower/expression.rs: binary_factor failed to recover \
                     operator from source gap {:?} (chain text = {:?})",
                    &input[prev_end as usize..operand.span().0 as usize],
                    node.span_text(),
                )
            });
        prev_end = operand.span().1;
        result = apply_binary_op(result, op_text, operand, ctx);
    }
    result
}

/// Collect operand views from a `binary_factor` compound. The
/// shape under structural mode is `[first, Repeat([rest...])]`;
/// under non-structural (legacy) mode the optimizer may have
/// flattened the Repeat wrapper, in which case the operands appear
/// as direct children. The same `iter_rep_children`-style handling
/// applies as for `iter_iteration_pairs`, but we need the first
/// (non-wrapped) operand alongside the rest, so it's spelled out
/// here.
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
    if rest.len() == 1 && rest[0].kind() == TapeKind::Repeat {
        operands.extend(rest[0].children());
    } else {
        operands.extend(rest);
    }
    operands
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
    let inner = node
        .child(0)
        .expect("mapped_factor: missing inner factor child");
    let base = dispatch_expression(inner, ctx);
    let Some(mapping_node) = node.child(1) else {
        return base;
    };
    // Optional mapping group is empty when absent — span(lo, lo).
    if mapping_node.span().1 <= mapping_node.span().0 {
        return base;
    }
    // Disambiguate: under HEAD's hand-patched generated.rs the
    // mapped_factor compound's children may collapse positions
    // (factor's modifier slot can land at child(1) instead of
    // the mapping group). The mapping group always carries the
    // `->` arrow keyword in its source slice; if `child(1)` is a
    // bare modifier (`?w` / `?` / `*` / `+`), it's not the
    // mapping group and we have no `->` mapping.
    if !mapping_node.span_text().contains("->") {
        return base;
    }
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
    // Search for a value-expression-rooted child.
    for c in node.children() {
        match c.rule_kind() {
            BbnfBootstrapRuleKind::value_expr
            | BbnfBootstrapRuleKind::value_or
            | BbnfBootstrapRuleKind::value_and
            | BbnfBootstrapRuleKind::value_cmp
            | BbnfBootstrapRuleKind::value_add
            | BbnfBootstrapRuleKind::value_mul
            | BbnfBootstrapRuleKind::value_unary
            | BbnfBootstrapRuleKind::value_unary_0
            | BbnfBootstrapRuleKind::value_atom
            | BbnfBootstrapRuleKind::value_atom_0
            | BbnfBootstrapRuleKind::value_fn_call
            | BbnfBootstrapRuleKind::value_path
            | BbnfBootstrapRuleKind::value_ident
            | BbnfBootstrapRuleKind::value_input
            | BbnfBootstrapRuleKind::value_closure
            | BbnfBootstrapRuleKind::int_lit
            | BbnfBootstrapRuleKind::float_lit
            | BbnfBootstrapRuleKind::bool_lit
            | BbnfBootstrapRuleKind::string_lit => return Some(c),
            _ => {
                // Recurse into single-child wrapper compounds (the
                // mapping's inner Seq may push its own compound
                // before reaching the value_expr).
                if let Some(found) = find_value_expr_child(c) {
                    return Some(found);
                }
            }
        }
    }
    None
}

fn find_type_annotation_child<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> Option<BbnfBootstrapNodeView<'a>> {
    find_child_by_kind(node, BbnfBootstrapRuleKind::type_annotation)
        .or_else(|| {
            // Walk one level deeper for nested wrappers.
            for c in node.children() {
                if let Some(found) =
                    find_child_by_kind(c, BbnfBootstrapRuleKind::type_annotation)
                {
                    return Some(found);
                }
            }
            None
        })
}

/// Lower a `factor = big_comment? term ?w modifier? big_comment?` view.
///
/// Children are positionally `[big_comment?, term, modifier?,
/// big_comment?]`. Under structural mode, the optional comment
/// wrappers may push empty compounds that shift the positional
/// indices, so we dispatch by rule_kind: find the `term`-family
/// child (anything in the term sub-tree) and look for a `modifier`
/// child for trailing quantifiers.
fn lower_factor<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    // Locate the term child by rule_kind (it's the only child
    // belonging to the term sub-tree). Skip comments and modifier
    // markers; they're metadata, not the value.
    let term = node
        .children()
        .find(|c| {
            matches!(
                c.rule_kind(),
                BbnfBootstrapRuleKind::term
                    | BbnfBootstrapRuleKind::term_0
                    | BbnfBootstrapRuleKind::term_1
                    | BbnfBootstrapRuleKind::term_2
                    | BbnfBootstrapRuleKind::value_atom_0
                    | BbnfBootstrapRuleKind::literal
                    | BbnfBootstrapRuleKind::regex
                    | BbnfBootstrapRuleKind::identifier
            )
        })
        .expect("factor: missing term child");
    let base = lower_term_dispatch(term, ctx);

    // Modifier is optional. Match by rule_kind first (preserved
    // compound), then fall back to span_text inspection on any
    // sibling with a non-empty span that looks like a modifier
    // glyph.
    let modifier = find_child_by_kind(node, BbnfBootstrapRuleKind::modifier);
    if let Some(mod_node) = modifier {
        if mod_node.span().1 > mod_node.span().0 {
            return apply_modifier(base, mod_node.span_text());
        }
    }
    base
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
fn lower_leaf_by_span_text<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> Option<IrNode> {
    use ::bbnf::runtime::tape::TapeKind;
    // Only Rule / Span leaves carry semantic spans we can
    // classify; Repeat / Optional wrappers should be handled
    // by `iter_rep_children` further up.
    match node.kind() {
        TapeKind::Rule
        | TapeKind::Span
        | TapeKind::Literal
        | TapeKind::Regex => {}
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

fn lower_term_dispatch<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node.rule_kind() {
        // Transparent wrapper
        BbnfBootstrapRuleKind::term => {
            let inner = node.child(0).expect("term: missing inner child");
            lower_term_dispatch(inner, ctx)
        }

        // Epsilon: "ε" or "epsilon"
        BbnfBootstrapRuleKind::term_0 => IrNode::Epsilon,

        // Identifier with optional call: identifier ( "(" rhs ("," rhs)* ")" )?
        BbnfBootstrapRuleKind::term_1 => {
            let ident = node.child(0).expect("term_1: missing identifier");
            let call_args = node.child(1);
            let name = ident.span_text();
            if let Some(call) = call_args {
                if call.span().1 > call.span().0 {
                    // call_args = "(", first, (",", arg)*, ")"
                    let first_arg = call
                        .child(1)
                        .expect("term_1 call: missing first arg");
                    let rest_args = call.child(2);
                    return lower_grammar_call(name, first_arg, rest_args, ctx);
                }
            }
            resolve_name(name, ctx)
        }

        // Grouped: "(" rhs ")", "[" rhs "]", "{" rhs "}", "@{" rhs "}"
        // Note: the bootstrap parser may produce term_2 OR value_atom_0 for
        // parenthesized expressions (both have the same (Span, &Enum, Span) shape).
        BbnfBootstrapRuleKind::term_2 | BbnfBootstrapRuleKind::value_atom_0 => {
            let open = node.child(0).expect("term_2: missing open delimiter");
            let inner = node.child(1).expect("term_2: missing inner");
            let expr = lower_rhs(inner, ctx);
            match open.span_text() {
                "(" => expr,
                "[" => IrNode::Repeat {
                    inner: Box::new(expr),
                    lo: 0,
                    hi: 1,
                },
                "@{" => {
                    let fn_id = ctx.fns.push(bbnf_ir::FnDescriptor::SpanCapture);
                    IrNode::Map {
                        inner: Box::new(expr),
                        fn_id,
                    }
                }
                "{" => IrNode::Repeat {
                    inner: Box::new(expr),
                    lo: 0,
                    hi: u32::MAX,
                },
                _ => expr,
            }
        }

        // Terminals
        BbnfBootstrapRuleKind::literal => {
            let raw = node.span_text();
            let inner = &raw[1..raw.len() - 1]; // Strip quote delimiters.
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            IrNode::Literal(id)
        }
        BbnfBootstrapRuleKind::regex => {
            let raw = node.span_text();
            let inner = &raw[1..raw.len() - 1]; // Strip / delimiters.
            let id = ctx.strings.intern(inner);
            IrNode::Regex(id)
        }
        BbnfBootstrapRuleKind::identifier => resolve_name(node.span_text(), ctx),

        // Anything else routes back through dispatch_expression.
        _ => dispatch_expression(node, ctx),
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
    first_arg: BbnfBootstrapNodeView<'a>,
    rest_args: Option<BbnfBootstrapNodeView<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let Some(closure) = ctx.closures.get(name) else {
        return resolve_name(name, ctx);
    };
    // Snapshot params + body so we can take `&mut ctx` for env push/pop.
    let params: Vec<&'a str> = closure.params.clone();
    let body: BbnfBootstrapNodeView<'a> = closure.body;

    let mut args: Vec<BbnfBootstrapNodeView<'a>> = Vec::with_capacity(
        1 + rest_args.map(|r| r.children().count()).unwrap_or(0),
    );
    args.push(first_arg);
    if let Some(rest) = rest_args {
        for pair in rest.children() {
            // pair = (",", arg)
            if let Some(arg) = pair.child(1) {
                args.push(arg);
            }
        }
    }

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
            let sid = ctx.strings.intern(name);
            Some(TypeDesc::Named(sid))
        } else {
            None
        }
    });

    // Type-shorthand: bare type name like `-> f64`.
    // unwrap_value_ident_str recursively peels value expression wrappers.
    if let Some(name) = unwrap_value_ident_str(value_expr) {
        if is_type_name(name) && return_type.is_none() {
            let type_sid = ctx.strings.intern(name);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(TypeDesc::Named(type_sid)),
            });
        }
    }

    // Extract type suffix from integer/float literals when no explicit type annotation.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        let text = match leaf.rule_kind() {
            BbnfBootstrapRuleKind::int_lit | BbnfBootstrapRuleKind::float_lit => {
                Some(leaf.span_text())
            }
            _ => None,
        };
        text.and_then(|t| {
            let (_, suffix) = split_numeric_suffix(t);
            if suffix.is_empty() {
                None
            } else {
                let sid = ctx.strings.intern(suffix);
                Some(TypeDesc::Named(sid))
            }
        })
    });

    // Bool literal → bool type.
    let return_type = return_type.or_else(|| {
        let leaf = deep_unwrap_value(value_expr);
        if leaf.rule_kind() == BbnfBootstrapRuleKind::bool_lit {
            let sid = ctx.strings.intern("bool");
            Some(TypeDesc::Named(sid))
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
                .map(|type_name| {
                    let sid = ctx.strings.intern(type_name);
                    TypeDesc::Named(sid)
                })
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

    let (expr, type_sid) = match desc {
        FnDescriptor::Expr {
            expr,
            return_type: Some(TypeDesc::Named(sid)),
        } => (expr.clone(), *sid),
        _ => return fn_id,
    };

    let regex_sid = match inner {
        IrNode::Regex(sid) => *sid,
        _ => return fn_id,
    };

    let type_name = ctx.strings.resolve(type_sid).to_owned();
    let pattern = ctx.strings.resolve(regex_sid).to_owned();

    match type_name.as_str() {
        "f64" => {
            if matches!(expr, MapExpr::Input)
                && matches!(
                    classify_regex(&pattern),
                    RegexClass::Numeric { .. } | RegexClass::JsonNumber
                )
            {
                ctx.fns.push(FnDescriptor::NumberConvert)
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
