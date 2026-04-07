//! Lowering: BbnfBootstrapEnum → IrNode.
//!
//! Layered descent through the grammar hierarchy:
//!   rhs → alternation → concatenation → binary_factor → mapped_factor →
//!   factor → term → leaf
//!
//! Produces IrNode directly from the bootstrap parse tree — no intermediate
//! Expression AST.

use std::collections::HashMap;

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapBinOp, MapExpr, MapUnaryOp, TypeDesc};
use parse_that::Span;
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::grammar::generated::BbnfBootstrapEnum;

use super::LowerCtx;

// ─── Top-level entry ──────────────────────────────────────────────────────────

/// Lower the RHS of a rule: `rhs = closure | alternation`.
pub(crate) fn lower_rhs<'a>(node: &'a BbnfBootstrapEnum<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node {
        BbnfBootstrapEnum::closure((_pipe1, _first_param, _params, _pipe2, body)) => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            lower_rhs(body, ctx)
        }
        BbnfBootstrapEnum::alternation(branches) => lower_alternation(branches, ctx),
        other => lower_node(other, ctx),
    }
}

// ─── Grammar expression hierarchy ─────────────────────────────────────────────

fn lower_alternation<'a>(
    branches: &'a [(&'a BbnfBootstrapEnum<'a>, Span<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if branches.len() == 1 {
        return lower_concatenation_dispatch(branches[0].0, ctx);
    }
    let alts: Vec<AltBranch> = branches
        .iter()
        .map(|(branch, _pipe)| AltBranch {
            node: lower_concatenation_dispatch(branch, ctx),
            first_set: None,
        })
        .collect();
    IrNode::Alt(alts, None)
}

fn lower_concatenation_dispatch<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node {
        BbnfBootstrapEnum::concatenation(parts) => lower_concatenation(parts, ctx),
        other => lower_node(other, ctx),
    }
}

fn lower_concatenation<'a>(
    parts: &'a [(&'a BbnfBootstrapEnum<'a>, Span<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    if parts.len() == 1 {
        return lower_binary_factor_dispatch(parts[0].0, ctx);
    }
    let children: Vec<IrNode> = parts
        .iter()
        .map(|(part, _comma)| lower_binary_factor_dispatch(part, ctx))
        .collect();
    IrNode::Seq(children)
}

fn lower_binary_factor_dispatch<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node {
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            let mut result = lower_mapped_factor_dispatch(first, ctx);
            for (op, operand) in *rest {
                let rhs = lower_mapped_factor_dispatch(operand, ctx);
                let op_str = match op {
                    BbnfBootstrapEnum::binary_operators(s) => s.as_str(),
                    _ => "",
                };
                result = match op_str {
                    "<<" => IrNode::Skip(Box::new(result), Box::new(rhs)),
                    ">>" => IrNode::Next(Box::new(result), Box::new(rhs)),
                    "-" => IrNode::Minus(Box::new(result), Box::new(rhs)),
                    _ => result,
                };
            }
            result
        }
        other => lower_node(other, ctx),
    }
}

fn lower_mapped_factor_dispatch<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node {
        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let base = lower_factor_dispatch(inner, ctx);
            if let Some((_arrow_span, (value_expr, type_ann))) = mapping {
                let fn_id = lower_map_arrow(value_expr, type_ann.as_deref(), ctx);
                let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
                IrNode::Map {
                    inner: Box::new(base),
                    fn_id,
                }
            } else {
                base
            }
        }
        other => lower_node(other, ctx),
    }
}

fn lower_factor_dispatch<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node {
        BbnfBootstrapEnum::factor((_comment_before, term, modifier, _comment_after)) => {
            let base = lower_term_dispatch(term, ctx);
            match modifier {
                Some(BbnfBootstrapEnum::modifier(s)) => match s.as_str() {
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
                },
                _ => base,
            }
        }
        other => lower_node(other, ctx),
    }
}

fn lower_term_dispatch<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match node {
        // Transparent wrapper
        BbnfBootstrapEnum::term(inner) => lower_term_dispatch(inner, ctx),

        // Epsilon: "ε" or "epsilon"
        BbnfBootstrapEnum::term_0(_) => IrNode::Epsilon,

        // Identifier with optional call: identifier ( "(" rhs ("," rhs)* ")" )?
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = crate::grammar::host::extract_span_text(ident);

            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                // Grammar function call — check for beta-reduction.
                if let Some(closure) = ctx.closures.get(name) {
                    let params = closure.params.clone();
                    let body = closure.body;

                    let mut args = vec![*first_arg];
                    for (_comma, arg) in *rest_args {
                        args.push(arg);
                    }

                    assert_eq!(
                        args.len(),
                        params.len(),
                        "arity mismatch: `{}` expects {} args, got {}",
                        name,
                        params.len(),
                        args.len(),
                    );

                    let subs: HashMap<&str, &'a BbnfBootstrapEnum<'a>> = params
                        .iter()
                        .zip(args.iter())
                        .map(|(&p, &a)| (p, a))
                        .collect();

                    return substitute_and_lower(body, &subs, ctx);
                }

                // Not a closure — fall through to nonterminal reference.
                lower_nonterminal(name, ctx)
            } else {
                lower_nonterminal(name, ctx)
            }
        }

        // Grouped: "(" rhs ")", "[" rhs "]", "{" rhs "}"
        // Note: the bootstrap parser may produce term_2 OR value_atom_0 for
        // parenthesized expressions (both have the same (Span, &Enum, Span) shape).
        BbnfBootstrapEnum::term_2((open, inner, _close))
        | BbnfBootstrapEnum::value_atom_0((open, inner, _close)) => {
            let expr = lower_rhs(inner, ctx);
            match open.as_str() {
                "(" => expr, // Group is transparent.
                "[" => IrNode::Repeat {
                    inner: Box::new(expr),
                    lo: 0,
                    hi: 1,
                },
                "{" => IrNode::Repeat {
                    inner: Box::new(expr),
                    lo: 0,
                    hi: u32::MAX,
                },
                _ => expr,
            }
        }

        // Terminals
        BbnfBootstrapEnum::literal(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1]; // Strip quote delimiters.
            let unescaped = crate::backend::unescape_literal(inner);
            let id = ctx.strings.intern(&unescaped);
            IrNode::Literal(id)
        }
        BbnfBootstrapEnum::regex(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1]; // Strip / delimiters.
            let id = ctx.strings.intern(inner);
            IrNode::Regex(id)
        }
        BbnfBootstrapEnum::identifier(s) => lower_nonterminal(s.as_str(), ctx),

        // Fallback
        other => lower_node(other, ctx),
    }
}

fn lower_nonterminal(name: &str, ctx: &LowerCtx<'_>) -> IrNode {
    match ctx.name_to_rule_id.get(name) {
        Some(&rule_id) => IrNode::Ref(rule_id),
        None if ctx.recovery_mode => IrNode::Epsilon,
        None => panic!(
            "unknown nonterminal `{}` — should have been caught by validate_ast()",
            name,
        ),
    }
}

// ─── Generic node dispatcher ──────────────────────────────────────────────────

/// Fallback dispatcher: handles any BbnfBootstrapEnum variant by routing
/// through the appropriate layer.
fn lower_node<'a>(node: &'a BbnfBootstrapEnum<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node {
        BbnfBootstrapEnum::alternation(branches) => lower_alternation(branches, ctx),
        BbnfBootstrapEnum::concatenation(parts) => lower_concatenation(parts, ctx),
        BbnfBootstrapEnum::binary_factor(_) => lower_binary_factor_dispatch(node, ctx),
        BbnfBootstrapEnum::mapped_factor(_) => lower_mapped_factor_dispatch(node, ctx),
        BbnfBootstrapEnum::factor(_) => lower_factor_dispatch(node, ctx),
        BbnfBootstrapEnum::term(_)
        | BbnfBootstrapEnum::term_0(_)
        | BbnfBootstrapEnum::term_1(_)
        | BbnfBootstrapEnum::term_2(_)
        | BbnfBootstrapEnum::value_atom_0(_)
        | BbnfBootstrapEnum::literal(_)
        | BbnfBootstrapEnum::regex(_)
        | BbnfBootstrapEnum::identifier(_) => lower_term_dispatch(node, ctx),
        BbnfBootstrapEnum::closure((_pipe1, _first_param, _params, _pipe2, body)) => lower_rhs(body, ctx),
        BbnfBootstrapEnum::comment(_) | BbnfBootstrapEnum::big_comment(_) => IrNode::Epsilon,
        _ => {
            if ctx.recovery_mode {
                IrNode::Epsilon
            } else {
                IrNode::Epsilon
            }
        }
    }
}

// ─── Beta-reduction for grammar function application ────────────────────────

fn substitute_and_lower<'a>(
    body: &'a BbnfBootstrapEnum<'a>,
    subs: &HashMap<&str, &'a BbnfBootstrapEnum<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match body {
        // Identifier matching a param → substitute with arg.
        BbnfBootstrapEnum::identifier(s) => {
            let name = s.as_str();
            if let Some(&arg) = subs.get(name) {
                lower_rhs(arg, ctx)
            } else {
                lower_nonterminal(name, ctx)
            }
        }
        BbnfBootstrapEnum::term_1((ident, call_args)) => {
            let name = crate::grammar::host::extract_span_text(ident);
            if call_args.is_none() {
                if let Some(&arg) = subs.get(name) {
                    return lower_rhs(arg, ctx);
                }
            }
            // Not a substitution — lower normally (may trigger nested beta-reduction).
            lower_term_dispatch(body, ctx)
        }

        // Recursive structural descent.
        BbnfBootstrapEnum::alternation(branches)
        | BbnfBootstrapEnum::call_arg(branches) => {
            let alts: Vec<AltBranch> = branches
                .iter()
                .map(|(b, _)| AltBranch {
                    node: substitute_and_lower(b, subs, ctx),
                    first_set: None,
                })
                .collect();
            if alts.len() == 1 {
                alts.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(alts, None)
            }
        }
        BbnfBootstrapEnum::concatenation(parts) => {
            let children: Vec<IrNode> = parts
                .iter()
                .map(|(p, _)| substitute_and_lower(p, subs, ctx))
                .collect();
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }
        BbnfBootstrapEnum::binary_factor((first, rest)) => {
            let mut result = substitute_and_lower(first, subs, ctx);
            for (op, operand) in *rest {
                let rhs = substitute_and_lower(operand, subs, ctx);
                let op_str = match op {
                    BbnfBootstrapEnum::binary_operators(s) => s.as_str(),
                    _ => "",
                };
                result = match op_str {
                    "<<" => IrNode::Skip(Box::new(result), Box::new(rhs)),
                    ">>" => IrNode::Next(Box::new(result), Box::new(rhs)),
                    "-" => IrNode::Minus(Box::new(result), Box::new(rhs)),
                    _ => result,
                };
            }
            result
        }
        BbnfBootstrapEnum::mapped_factor((inner, mapping)) => {
            let base = substitute_and_lower(inner, subs, ctx);
            if let Some((_arrow, (value_expr, type_ann))) = mapping {
                let fn_id = lower_map_arrow(value_expr, type_ann.as_deref(), ctx);
                let fn_id = try_specialize_map_fn(&base, fn_id, ctx);
                IrNode::Map {
                    inner: Box::new(base),
                    fn_id,
                }
            } else {
                base
            }
        }
        BbnfBootstrapEnum::factor((_comment, term, modifier, _comment2)) => {
            let base = substitute_and_lower(term, subs, ctx);
            match modifier {
                Some(BbnfBootstrapEnum::modifier(s)) => match s.as_str() {
                    "?" => IrNode::Repeat { inner: Box::new(base), lo: 0, hi: 1 },
                    "*" => IrNode::Repeat { inner: Box::new(base), lo: 0, hi: u32::MAX },
                    "+" => IrNode::Repeat { inner: Box::new(base), lo: 1, hi: u32::MAX },
                    "?w" => IrNode::OptionalWhitespace(Box::new(base)),
                    _ => base,
                },
                _ => base,
            }
        }
        BbnfBootstrapEnum::term(inner) => substitute_and_lower(inner, subs, ctx),
        BbnfBootstrapEnum::term_2((open, inner, _close))
        | BbnfBootstrapEnum::value_atom_0((open, inner, _close)) => {
            let expr = substitute_and_lower(inner, subs, ctx);
            match open.as_str() {
                "(" => expr,
                "[" => IrNode::Repeat { inner: Box::new(expr), lo: 0, hi: 1 },
                "{" => IrNode::Repeat { inner: Box::new(expr), lo: 0, hi: u32::MAX },
                _ => expr,
            }
        }

        // Leaf: lower directly (no substitution targets).
        _ => lower_node(body, ctx),
    }
}

// ─── MapArrow / ValueExpr lowering ─────────────────────────────────────────────

/// Lower a `->` mapping to a `FnId`.
///
/// `value_expr` is the value expression node, `type_ann` is the optional type annotation node.
fn lower_map_arrow<'a>(
    value_expr: &'a BbnfBootstrapEnum<'a>,
    type_ann: Option<&'a BbnfBootstrapEnum<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> FnId {
    let return_type = type_ann.and_then(|ann| {
        if let BbnfBootstrapEnum::type_annotation((_colon, type_node)) = ann {
            let name = match type_node {
                BbnfBootstrapEnum::type_name(s) => s.as_str(),
                BbnfBootstrapEnum::identifier(s) => s.as_str(),
                _ => return None,
            };
            let sid = ctx.strings.intern(name);
            Some(TypeDesc::Named(sid))
        } else {
            None
        }
    });

    // Type-shorthand: bare type name like `-> f64`.
    if let BbnfBootstrapEnum::value_ident(s) = value_expr {
        let name = s.as_str();
        if is_type_name(name) && return_type.is_none() {
            let type_sid = ctx.strings.intern(name);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(TypeDesc::Named(type_sid)),
            });
        }
    }
    // Unwrap transparent wrappers to detect type shorthand.
    if let Some(inner) = unwrap_value_atom(value_expr) {
        if let BbnfBootstrapEnum::value_ident(s) = inner {
            let name = s.as_str();
            if is_type_name(name) && return_type.is_none() {
                let type_sid = ctx.strings.intern(name);
                return ctx.fns.push(FnDescriptor::Expr {
                    expr: MapExpr::Input,
                    return_type: Some(TypeDesc::Named(type_sid)),
                });
            }
        }
    }

    // Extract type suffix from integer/float literals when no explicit type annotation.
    let return_type = return_type.or_else(|| {
        let text = match value_expr {
            BbnfBootstrapEnum::int_lit(s) | BbnfBootstrapEnum::float_lit(s) => Some(s.as_str()),
            _ => unwrap_value_atom(value_expr).and_then(|inner| match inner {
                BbnfBootstrapEnum::int_lit(s) | BbnfBootstrapEnum::float_lit(s) => {
                    Some(s.as_str())
                }
                _ => None,
            }),
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
        let is_bool = matches!(value_expr, BbnfBootstrapEnum::bool_lit(_))
            || unwrap_value_atom(value_expr)
                .is_some_and(|inner| matches!(inner, BbnfBootstrapEnum::bool_lit(_)));
        if is_bool {
            let sid = ctx.strings.intern("bool");
            Some(TypeDesc::Named(sid))
        } else {
            None
        }
    });

    // @host return type propagation.
    let return_type = return_type.or_else(|| {
        let func_name = extract_value_func_name(value_expr)
            .or_else(|| unwrap_value_atom(value_expr).and_then(extract_value_func_name));
        func_name.and_then(|name| {
            ctx.host_fns
                .and_then(|hosts| hosts.get(name))
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

/// Unwrap transparent value wrappers to reach the inner value node.
fn unwrap_value_atom<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<&'a BbnfBootstrapEnum<'a>> {
    match node {
        BbnfBootstrapEnum::value_atom(inner)
        | BbnfBootstrapEnum::value_unary(inner) => Some(inner),
        BbnfBootstrapEnum::value_or((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_and((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_cmp((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_add((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_mul((first, rest)) if rest.is_empty() => Some(first),
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => Some(inner),
        _ => None,
    }
}

fn extract_value_func_name<'a>(node: &'a BbnfBootstrapEnum<'a>) -> Option<&'a str> {
    match node {
        BbnfBootstrapEnum::value_ident(s) => Some(s.as_str()),
        BbnfBootstrapEnum::value_fn_call((name, _, _, _)) => {
            Some(crate::grammar::host::extract_span_text(name))
        }
        _ => None,
    }
}

fn is_type_name(name: &str) -> bool {
    matches!(
        name,
        "f64" | "f32" | "u32" | "u64" | "i32" | "i64" | "usize" | "u8" | "u16" | "i8" | "i16"
    )
}

// ─── ValueExpr lowering ────────────────────────────────────────────────────────

/// Lower a value expression bootstrap node to a `MapExpr`.
fn lower_value_expr<'a>(node: &'a BbnfBootstrapEnum<'a>, ctx: &mut LowerCtx<'a>) -> MapExpr {
    match node {
        // Precedence chain: or → and → cmp → add → mul → unary → atom
        BbnfBootstrapEnum::value_or((first, rest)) => {
            fold_binop_chain(first, rest, |s| match s.as_str() {
                "||" => MapBinOp::Or,
                _ => MapBinOp::Or,
            }, ctx)
        }
        BbnfBootstrapEnum::value_and((first, rest)) => {
            fold_binop_chain(first, rest, |s| match s.as_str() {
                "&&" => MapBinOp::And,
                _ => MapBinOp::And,
            }, ctx)
        }
        BbnfBootstrapEnum::value_cmp((first, rest)) => {
            fold_binop_chain_enum(first, rest, ctx)
        }
        BbnfBootstrapEnum::value_add((first, rest)) => {
            fold_binop_chain_enum(first, rest, ctx)
        }
        BbnfBootstrapEnum::value_mul((first, rest)) => {
            fold_binop_chain_enum(first, rest, ctx)
        }

        // Unary
        BbnfBootstrapEnum::value_unary(inner) => lower_value_expr(inner, ctx),
        BbnfBootstrapEnum::value_unary_0((op_span, inner)) => {
            let op = match op_span.as_str() {
                "-" => MapUnaryOp::Neg,
                "!" => MapUnaryOp::Not,
                _ => MapUnaryOp::Neg,
            };
            MapExpr::UnaryOp {
                op,
                inner: Box::new(lower_value_expr(inner, ctx)),
            }
        }

        // Atom
        BbnfBootstrapEnum::value_atom(inner) => lower_value_expr(inner, ctx),
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => lower_value_expr(inner, ctx),

        // Literals
        BbnfBootstrapEnum::int_lit(s) => parse_int_literal(s.as_str()),
        BbnfBootstrapEnum::float_lit(s) => parse_float_literal(s.as_str()),
        BbnfBootstrapEnum::bool_lit(s) => MapExpr::BoolLit(s.as_str() == "true"),
        BbnfBootstrapEnum::string_lit(s) => {
            let raw = s.as_str();
            let inner = &raw[1..raw.len() - 1]; // Strip quotes.
            let sid = ctx.strings.intern(inner);
            MapExpr::StringLit(sid)
        }

        // Input
        BbnfBootstrapEnum::value_input((_, props)) => {
            if props.is_empty() {
                MapExpr::Input
            } else {
                // Take the last property access.
                let (_dot, prop_node) = &props[props.len() - 1];
                let prop_name = match prop_node {
                    BbnfBootstrapEnum::value_ident(s) => s.as_str(),
                    BbnfBootstrapEnum::identifier(s) => s.as_str(),
                    _ => "unknown",
                };
                let sid = ctx.strings.intern(prop_name);
                MapExpr::InputProp { prop: sid }
            }
        }

        // Function call
        BbnfBootstrapEnum::value_fn_call((name_enum, _open, args_opt, _close)) => {
            let name_str = crate::grammar::host::extract_span_text(name_enum);
            let sid = ctx.strings.intern(name_str);
            let ir_args: Vec<MapExpr> = match args_opt {
                Some((first_arg, rest_args)) => {
                    let mut args = vec![lower_value_expr(first_arg, ctx)];
                    for (_comma, arg) in *rest_args {
                        args.push(lower_value_expr(arg, ctx));
                    }
                    args
                }
                None => vec![],
            };
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }

        // Identifier (bare name — treat as function call on input)
        BbnfBootstrapEnum::value_ident(s) => {
            let sid = ctx.strings.intern(s.as_str());
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }

        // Value closure: |params| body
        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            lower_value_expr_with_bindings(body, first_param, rest_params, ctx)
        }

        // Fallback
        _ => MapExpr::Input,
    }
}

/// Fold a left-associative binary operator chain with Span-keyed ops (value_or, value_and).
fn fold_binop_chain<'a>(
    first: &'a BbnfBootstrapEnum<'a>,
    rest: &'a [(Span<'a>, &'a BbnfBootstrapEnum<'a>)],
    op_map: impl Fn(Span<'a>) -> MapBinOp,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    if rest.is_empty() {
        return lower_value_expr(first, ctx);
    }
    let mut result = lower_value_expr(first, ctx);
    for (op_span, operand) in rest {
        result = MapExpr::BinOp {
            op: op_map(*op_span),
            lhs: Box::new(result),
            rhs: Box::new(lower_value_expr(operand, ctx)),
        };
    }
    result
}

/// Fold a left-associative binary operator chain with enum-keyed ops (cmp, add, mul).
fn fold_binop_chain_enum<'a>(
    first: &'a BbnfBootstrapEnum<'a>,
    rest: &'a [(&'a BbnfBootstrapEnum<'a>, &'a BbnfBootstrapEnum<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    if rest.is_empty() {
        return lower_value_expr(first, ctx);
    }
    let mut result = lower_value_expr(first, ctx);
    for (op_node, operand) in rest {
        let op_str = match op_node {
            BbnfBootstrapEnum::cmp_op(s)
            | BbnfBootstrapEnum::add_op(s)
            | BbnfBootstrapEnum::mul_op(s) => s.as_str(),
            _ => "+",
        };
        let op = match op_str {
            "+" => MapBinOp::Add,
            "-" => MapBinOp::Sub,
            "*" => MapBinOp::Mul,
            "/" => MapBinOp::Div,
            "%" => MapBinOp::Mod,
            "==" => MapBinOp::Eq,
            "!=" => MapBinOp::Ne,
            "<" => MapBinOp::Lt,
            ">" => MapBinOp::Gt,
            "<=" => MapBinOp::Le,
            ">=" => MapBinOp::Ge,
            _ => MapBinOp::Add,
        };
        result = MapExpr::BinOp {
            op,
            lhs: Box::new(result),
            rhs: Box::new(lower_value_expr(operand, ctx)),
        };
    }
    result
}

/// Lower a value closure body with param→Input bindings.
///
/// `first_param` is the first closure parameter (mapped to `MapExpr::Input`).
/// `rest_params` are the remaining `(comma, param)` pairs (mapped to `InputProp`).
fn lower_value_expr_with_bindings<'a>(
    body: &'a BbnfBootstrapEnum<'a>,
    first_param: &'a BbnfBootstrapEnum<'a>,
    rest_params: &'a [(Span<'a>, &'a BbnfBootstrapEnum<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let mut bindings: HashMap<&str, MapExpr> = HashMap::new();

    let first_name = match first_param {
        BbnfBootstrapEnum::value_ident(s) | BbnfBootstrapEnum::identifier(s) => s.as_str(),
        _ => "",
    };
    bindings.insert(first_name, MapExpr::Input);

    for (_comma, param_node) in rest_params {
        let name = match param_node {
            BbnfBootstrapEnum::value_ident(s) | BbnfBootstrapEnum::identifier(s) => s.as_str(),
            _ => "",
        };
        let sid = ctx.strings.intern(name);
        bindings.insert(name, MapExpr::InputProp { prop: sid });
    }

    lower_value_expr_substituted(body, &bindings, ctx)
}

fn lower_value_expr_substituted<'a>(
    node: &'a BbnfBootstrapEnum<'a>,
    bindings: &HashMap<&str, MapExpr>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match node {
        BbnfBootstrapEnum::value_ident(s) => {
            let name = s.as_str();
            if let Some(bound) = bindings.get(name) {
                return bound.clone();
            }
            let sid = ctx.strings.intern(name);
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }
        BbnfBootstrapEnum::value_fn_call((name_enum, _open, args_opt, _close)) => {
            let name_str = crate::grammar::host::extract_span_text(name_enum);
            let sid = ctx.strings.intern(name_str);
            let ir_args: Vec<MapExpr> = match args_opt {
                Some((first_arg, rest_args)) => {
                    let mut args = vec![lower_value_expr_substituted(first_arg, bindings, ctx)];
                    for (_comma, arg) in *rest_args {
                        args.push(lower_value_expr_substituted(arg, bindings, ctx));
                    }
                    args
                }
                None => vec![],
            };
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }
        BbnfBootstrapEnum::value_atom(inner) | BbnfBootstrapEnum::value_unary(inner) => {
            lower_value_expr_substituted(inner, bindings, ctx)
        }
        BbnfBootstrapEnum::value_atom_0((_open, inner, _close)) => {
            lower_value_expr_substituted(inner, bindings, ctx)
        }
        BbnfBootstrapEnum::value_closure((_pipe, first_param, rest_params, _pipe2, body)) => {
            lower_value_expr_with_bindings(body, first_param, rest_params, ctx)
        }
        // Literals and Input don't reference params — delegate to standard lowering.
        _ => lower_value_expr(node, ctx),
    }
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

// ─── Numeric parsing helpers ───────────────────────────────────────────────────

fn parse_int_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = if digits.starts_with("0x") || digits.starts_with("0X") {
        i64::from_str_radix(&digits[2..], 16).unwrap_or(0)
    } else {
        digits.parse::<i64>().unwrap_or(0)
    };
    MapExpr::IntLit(value)
}

fn parse_float_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = digits.parse::<f64>().unwrap_or(0.0);
    MapExpr::FloatLit(value)
}

fn split_numeric_suffix(text: &str) -> (&str, &str) {
    let bytes = text.as_bytes();
    let mut i = 0;
    if bytes.len() > 2 && bytes[0] == b'0' && (bytes[1] == b'x' || bytes[1] == b'X') {
        i = 2;
        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
    } else {
        while i < bytes.len()
            && (bytes[i].is_ascii_digit()
                || bytes[i] == b'.'
                || bytes[i] == b'e'
                || bytes[i] == b'E'
                || bytes[i] == b'+'
                || bytes[i] == b'-')
        {
            i += 1;
        }
    }
    (&text[..i], &text[i..])
}
