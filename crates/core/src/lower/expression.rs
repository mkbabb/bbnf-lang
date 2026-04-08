//! Lowering: BbnfBootstrapEnum → IrNode.
//!
//! Layered descent through the grammar hierarchy:
//!   rhs → alternation → concatenation → binary_factor → mapped_factor →
//!   factor → term → leaf
//!
//! Produces IrNode directly from the bootstrap parse tree — no intermediate
//! Expression AST.
//!
//! Beta reduction is environment-driven, not walker-driven: when a grammar
//! closure is applied, we push a frame on `LowerCtx.env` mapping each param
//! to its argument CST node, lower the body recursively, and pop. Identifier
//! resolution (`resolve_name`) checks the env stack first before the rule
//! table. This eliminates the parallel `substitute_and_lower` walker.

use std::collections::HashMap;

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapExpr, TypeDesc};
use parse_that::Span;
use parse_that::regex::classify::{RegexClass, classify_regex};

use crate::grammar::generated::BbnfBootstrapEnum;

use super::LowerCtx;
use super::value_expr::{
    deep_unwrap_value, extract_value_func_name, is_type_name, lower_value_expr,
    split_numeric_suffix, unwrap_value_ident_str,
};

// ─── Top-level entry ──────────────────────────────────────────────────────────

/// Lower the RHS of a rule: `rhs = closure | alternation`.
pub(crate) fn lower_rhs<'a>(node: &'a BbnfBootstrapEnum<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node {
        BbnfBootstrapEnum::closure((_pipe1, _first_param, _params, _pipe2, body)) => {
            // Grammar closure at rule level — lower the body directly.
            // (Closures are expanded at call sites via beta-reduction.)
            lower_rhs(body, ctx)
        }
        BbnfBootstrapEnum::alternation(branches)
        | BbnfBootstrapEnum::call_arg(branches) => lower_alternation(branches, ctx),
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
            let name = crate::grammar::generated::BbnfBootstrapEnum::span_text(ident);
            if let Some((_open, first_arg, rest_args, _close)) = call_args {
                lower_grammar_call(name, first_arg, rest_args, ctx)
            } else {
                resolve_name(name, ctx)
            }
        }

        // Grouped: "(" rhs ")", "[" rhs "]", "{" rhs "}"
        // Note: the bootstrap parser may produce term_2 OR value_atom_0 for
        // parenthesized expressions (both have the same (Span, &Enum, Span) shape).
        BbnfBootstrapEnum::term_2((open, inner, _close))
        | BbnfBootstrapEnum::value_atom_0((open, inner, _close)) => {
            let expr = lower_rhs(inner, ctx);
            match open.as_str() {
                "(" => expr,
                "[" => IrNode::Repeat { inner: Box::new(expr), lo: 0, hi: 1 },
                "@{" => {
                    let fn_id = ctx.fns.push(bbnf_ir::FnDescriptor::SpanCapture);
                    IrNode::Map { inner: Box::new(expr), fn_id }
                }
                "{" => IrNode::Repeat { inner: Box::new(expr), lo: 0, hi: u32::MAX },
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
        BbnfBootstrapEnum::identifier(s) => resolve_name(s.as_str(), ctx),

        // Fallback
        other => lower_node(other, ctx),
    }
}

/// Resolve a bare nonterminal name to an `IrNode`.
///
/// Lookup order:
/// 1. **Beta-reduction environment** — if the name is bound by an enclosing
///    grammar closure application, lower the bound CST node in the current
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
/// Pushes a fresh env frame mapping each parameter to its argument CST node,
/// lowers the closure body in the augmented context (so identifier sites
/// inside the body see the bindings via `resolve_name`), then pops the
/// frame.  If `name` doesn't refer to a closure, falls back to a normal
/// nonterminal reference.
fn lower_grammar_call<'a>(
    name: &'a str,
    first_arg: &'a BbnfBootstrapEnum<'a>,
    rest_args: &'a [(Span<'a>, &'a BbnfBootstrapEnum<'a>)],
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    let Some(closure) = ctx.closures.get(name) else {
        return resolve_name(name, ctx);
    };
    // Snapshot params + body so we can take `&mut ctx` for env push/pop.
    let params: Vec<&'a str> = closure.params.clone();
    let body: &'a BbnfBootstrapEnum<'a> = closure.body;

    let mut args: Vec<&'a BbnfBootstrapEnum<'a>> = Vec::with_capacity(1 + rest_args.len());
    args.push(first_arg);
    for (_comma, arg) in rest_args {
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

    let mut frame: HashMap<&'a str, &'a BbnfBootstrapEnum<'a>> = HashMap::with_capacity(args.len());
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
    env: &[HashMap<&'a str, &'a BbnfBootstrapEnum<'a>>],
) -> Option<&'a BbnfBootstrapEnum<'a>> {
    for frame in env.iter().rev() {
        if let Some(&bound) = frame.get(name) {
            return Some(bound);
        }
    }
    None
}

// ─── Generic node dispatcher ──────────────────────────────────────────────────

/// Fallback dispatcher: handles any BbnfBootstrapEnum variant by routing
/// through the appropriate layer.
fn lower_node<'a>(node: &'a BbnfBootstrapEnum<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match node {
        BbnfBootstrapEnum::alternation(branches)
        | BbnfBootstrapEnum::call_arg(branches) => lower_alternation(branches, ctx),
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
        let text = match leaf {
            BbnfBootstrapEnum::int_lit(s) | BbnfBootstrapEnum::float_lit(s) => Some(s.as_str()),
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
        if matches!(leaf, BbnfBootstrapEnum::bool_lit(_)) {
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
