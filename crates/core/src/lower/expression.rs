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

/// Iterate the "content" view of each iteration pair under a
/// `(content, optional_separator) +` shape. The view passed in is
/// the alternation/concatenation rule compound (possibly wrapped
/// in a `Repeat` compound from the `+` quantifier).
///
/// Each iteration's content is `pair.child(0)` (the binary_factor
/// or concatenation), and the trailing optional `,` / `|` is
/// dropped. When the optimizer has elided wrapper compounds, the
/// iteration may have skipped levels — defensively try `child(0)`
/// first, fall back to the pair view itself when the pair is the
/// content.
fn iter_iteration_pairs<'a>(
    node: BbnfBootstrapNodeView<'a>,
) -> impl Iterator<Item = BbnfBootstrapNodeView<'a>> + 'a {
    iter_rep_children(node).filter_map(|pair| {
        // The pair is normally a Seq compound holding [content,
        // optional_sep]. Under post-AC.2 structural emission, Seq
        // doesn't push its own compound — children land directly
        // in the enclosing Repeat. So `pair` here may already be
        // the content view, with the optional separator following
        // as the next iteration entry.
        //
        // Disambiguation: if `pair` has children of its own AND
        // its first child is a known expression-level rule_kind,
        // treat the pair as a wrapping Seq and return `child(0)`.
        // Otherwise treat the pair itself as the content.
        let first_child = pair.child(0);
        match first_child.map(|c| c.rule_kind()) {
            Some(BbnfBootstrapRuleKind::binary_factor)
            | Some(BbnfBootstrapRuleKind::concatenation)
            | Some(BbnfBootstrapRuleKind::mapped_factor)
            | Some(BbnfBootstrapRuleKind::factor)
            | Some(BbnfBootstrapRuleKind::term)
            | Some(BbnfBootstrapRuleKind::term_0)
            | Some(BbnfBootstrapRuleKind::term_1)
            | Some(BbnfBootstrapRuleKind::term_2)
            | Some(BbnfBootstrapRuleKind::call_arg)
            | Some(BbnfBootstrapRuleKind::alternation) => first_child,
            _ => Some(pair),
        }
    })
}

/// Lower a `binary_factor = mapped_factor ( binary_op mapped_factor )*` view.
///
/// The first child is always the leftmost operand. Subsequent
/// `(op, operand)` pairs may be flattened directly into the
/// compound's children (post-AC.2 structural) or wrapped in a
/// rest-list compound (pre-AC.2 optimizer). Disambiguate by
/// inspecting the second child's rule_kind: if it's a
/// `binary_operators` leaf, treat the rest as flat alternating
/// pairs; otherwise treat the second child as a rest-list
/// compound containing the pairs.
fn lower_binary_factor<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let first = node
        .child(0)
        .expect("binary_factor: missing first operand");
    let mut result = dispatch_expression(first, ctx);

    // Walk the remaining children as alternating (op, operand)
    // pairs. Skip the first child since we already consumed it.
    let mut iter = node.children().skip(1).peekable();
    if let Some(&first_rest) = iter.peek() {
        if first_rest.rule_kind() == BbnfBootstrapRuleKind::binary_operators {
            // Flat shape: alternating (op, operand) at the top
            // level. Walk pairs from the iterator directly.
            while let Some(op_node) = iter.next() {
                let operand = match iter.next() {
                    Some(o) => o,
                    None => break,
                };
                result = apply_binary_op(result, op_node.span_text(), operand, ctx);
            }
        } else {
            // Wrapped shape: child(1) is a rest-list compound
            // containing (op, operand) pairs as its own children.
            for pair in first_rest.children() {
                let Some(op_node) = pair.child(0) else { continue };
                let Some(operand) = pair.child(1) else { continue };
                result = apply_binary_op(result, op_node.span_text(), operand, ctx);
            }
            // No further iterator advancement — child(1) was the
            // whole rest-list.
        }
    }
    result
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
        _ => lhs,
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
            BbnfBootstrapRuleKind::value_or
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
