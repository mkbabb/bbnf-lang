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
//! Tranche AC.2: every `BbnfBootstrapEnum` pattern-match is now a
//! `rule_kind()` dispatch + typed child accessors on the view.

use std::collections::HashMap;

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapExpr, TypeDesc};
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::grammar::generated::{BbnfBootstrapNodeView, BbnfBootstrapRuleKind};

use super::LowerCtx;
use super::value_expr::{
    deep_unwrap_value, extract_value_func_name, is_type_name, lower_value_expr,
    split_numeric_suffix, unwrap_value_ident_str,
};

// ─── Top-level entry ──────────────────────────────────────────────────────────

/// Lower the RHS of a rule: `rhs = closure | alternation`.
pub(crate) fn lower_rhs<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::closure => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            // closure = "|", first_param, rest, "|", body — body is child(4).
            let body = node
                .child(4)
                .expect("closure: missing body child");
            lower_rhs(body, ctx)
        }
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            lower_alternation(node, ctx)
        }
        _ => lower_node(node, ctx),
    }
}

// ─── Grammar expression hierarchy ─────────────────────────────────────────────

fn lower_alternation<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // alternation = branch (pipe branch)* — one direct child per
    // `(branch, pipe?)` pair.
    let branches: Vec<BbnfBootstrapNodeView<'a>> = node
        .children()
        .filter_map(|pair| pair.child(0))
        .collect();
    if branches.len() == 1 {
        return lower_concatenation_dispatch(branches[0], ctx);
    }
    let alts: Vec<AltBranch> = branches
        .into_iter()
        .map(|branch| AltBranch {
            node: lower_concatenation_dispatch(branch, ctx),
            first_set: None,
        })
        .collect();
    IrNode::Alt(alts, None)
}

fn lower_concatenation_dispatch<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if node.rule_kind() == BbnfBootstrapRuleKind::concatenation {
        return lower_concatenation(node, ctx);
    }
    lower_node(node, ctx)
}

fn lower_concatenation<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    // concatenation = part ("," part)* — one direct child per
    // `(part, comma?)` pair.
    let parts: Vec<BbnfBootstrapNodeView<'a>> = node
        .children()
        .filter_map(|pair| pair.child(0))
        .collect();
    if parts.len() == 1 {
        return lower_binary_factor_dispatch(parts[0], ctx);
    }
    let children: Vec<IrNode> = parts
        .into_iter()
        .map(|part| lower_binary_factor_dispatch(part, ctx))
        .collect();
    IrNode::Seq(children)
}

fn lower_binary_factor_dispatch<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if node.rule_kind() == BbnfBootstrapRuleKind::binary_factor {
        // binary_factor = (first, (op, operand)*)
        let first = node
            .child(0)
            .expect("binary_factor: missing first child");
        let rest_list = node.child(1);
        let mut result = lower_mapped_factor_dispatch(first, ctx);
        if let Some(rest) = rest_list {
            for pair in rest.children() {
                // pair = (op, operand)
                let op_node = pair.child(0);
                let operand = match pair.child(1) {
                    Some(o) => o,
                    None => continue,
                };
                let rhs = lower_mapped_factor_dispatch(operand, ctx);
                let op_str = op_node.map(|o| o.span_text()).unwrap_or("");
                result = match op_str {
                    "<<" => IrNode::Skip(Box::new(result), Box::new(rhs)),
                    ">>" => IrNode::Next(Box::new(result), Box::new(rhs)),
                    "-" => IrNode::Minus(Box::new(result), Box::new(rhs)),
                    _ => result,
                };
            }
        }
        return result;
    }
    lower_node(node, ctx)
}

fn lower_mapped_factor_dispatch<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if node.rule_kind() == BbnfBootstrapRuleKind::mapped_factor {
        // mapped_factor = (inner, mapping?)
        // mapping = (arrow, (value_expr, type_ann?))
        let inner = node
            .child(0)
            .expect("mapped_factor: missing inner child");
        let base = lower_factor_dispatch(inner, ctx);
        let mapping = node.child(1);
        if let Some(mapping_node) = mapping {
            // `mapping` is an optional pair whose span collapses to
            // empty when absent. A bare span-check distinguishes
            // Some(..) from None.
            if mapping_node.span().1 > mapping_node.span().0 {
                let value_pair = mapping_node
                    .child(1)
                    .unwrap_or(mapping_node);
                let value_expr = value_pair.child(0).unwrap_or(value_pair);
                let type_ann = value_pair.child(1);
                let fn_id = lower_map_arrow(value_expr, type_ann, ctx);
                let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
                return IrNode::Map {
                    inner: Box::new(base),
                    fn_id,
                };
            }
        }
        return base;
    }
    lower_node(node, ctx)
}

fn lower_factor_dispatch<'a>(
    node: BbnfBootstrapNodeView<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if node.rule_kind() == BbnfBootstrapRuleKind::factor {
        // factor = (comment_before?, term, modifier?, comment_after?)
        let term = node
            .child(1)
            .expect("factor: missing term child");
        let modifier = node.child(2);
        let base = lower_term_dispatch(term, ctx);
        if let Some(mod_node) = modifier {
            if mod_node.rule_kind() == BbnfBootstrapRuleKind::modifier
                || mod_node.span().1 > mod_node.span().0
            {
                return match mod_node.span_text() {
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
                };
            }
        }
        return base;
    }
    lower_node(node, ctx)
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

        // Fallback
        _ => lower_node(node, ctx),
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

// ─── Generic node dispatcher ──────────────────────────────────────────────────

/// Fallback dispatcher: handles any view by routing through the
/// appropriate layer.
fn lower_node<'a>(node: BbnfBootstrapNodeView<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node.rule_kind() {
        BbnfBootstrapRuleKind::alternation | BbnfBootstrapRuleKind::call_arg => {
            lower_alternation(node, ctx)
        }
        BbnfBootstrapRuleKind::concatenation => lower_concatenation(node, ctx),
        BbnfBootstrapRuleKind::binary_factor => lower_binary_factor_dispatch(node, ctx),
        BbnfBootstrapRuleKind::mapped_factor => lower_mapped_factor_dispatch(node, ctx),
        BbnfBootstrapRuleKind::factor => lower_factor_dispatch(node, ctx),
        BbnfBootstrapRuleKind::term
        | BbnfBootstrapRuleKind::term_0
        | BbnfBootstrapRuleKind::term_1
        | BbnfBootstrapRuleKind::term_2
        | BbnfBootstrapRuleKind::value_atom_0
        | BbnfBootstrapRuleKind::literal
        | BbnfBootstrapRuleKind::regex
        | BbnfBootstrapRuleKind::identifier => lower_term_dispatch(node, ctx),
        BbnfBootstrapRuleKind::closure => {
            // closure = "|", first_param, rest, "|", body — body is child(4).
            let body = node
                .child(4)
                .expect("closure: missing body child");
            lower_rhs(body, ctx)
        }
        BbnfBootstrapRuleKind::comment | BbnfBootstrapRuleKind::big_comment => IrNode::Epsilon,
        _ => IrNode::Epsilon,
    }
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
