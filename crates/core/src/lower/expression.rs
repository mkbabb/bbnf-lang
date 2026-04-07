//! Expression/node lowering logic — the recursive `lower_expression` function.

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapBinOp, MapExpr, MapUnaryOp, TypeDesc};

use parse_that::regex::classify::{RegexClass, classify_regex};
use crate::types::{BinOpKind, Expression, MapArrow, Token, UnaryOpKind, ValueExpr};

use std::collections::HashMap;

use super::LowerCtx;

/// Attempt to replace a `FnDescriptor::Expr` with a specialized descriptor
/// based on the combination of inner node type and MapExpr pattern.
///
/// Recognized patterns:
/// - `Regex(numeric_pattern) + Expr { Input, return_type: f64 }` → `FnDescriptor::NumberConvert`
/// - `Regex(hex_pattern) + Expr { FnCall(name, [Input]), return_type: u32 }` → `FnDescriptor::HexConvert`
fn try_specialize_map_fn(inner: &IrNode, fn_id: FnId, ctx: &mut LowerCtx<'_>) -> FnId {
    let desc = &ctx.fns.fns[fn_id as usize];

    // Only specialize Expr variants with a named return type.
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
            // Expr { Input, return_type: f64 } on a numeric regex → NumberConvert
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
            // Expr { FnCall(name, [Input]), return_type: u32 } on a hex regex → HexConvert
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

// ─── ValueExpr → MapExpr lowering ───────────���──────────────────────────────

fn lower_binop(kind: BinOpKind) -> MapBinOp {
    match kind {
        BinOpKind::Add => MapBinOp::Add,
        BinOpKind::Sub => MapBinOp::Sub,
        BinOpKind::Mul => MapBinOp::Mul,
        BinOpKind::Div => MapBinOp::Div,
        BinOpKind::Mod => MapBinOp::Mod,
        BinOpKind::Eq => MapBinOp::Eq,
        BinOpKind::Ne => MapBinOp::Ne,
        BinOpKind::Lt => MapBinOp::Lt,
        BinOpKind::Gt => MapBinOp::Gt,
        BinOpKind::Le => MapBinOp::Le,
        BinOpKind::Ge => MapBinOp::Ge,
        BinOpKind::And => MapBinOp::And,
        BinOpKind::Or => MapBinOp::Or,
    }
}

fn lower_unaryop(kind: UnaryOpKind) -> MapUnaryOp {
    match kind {
        UnaryOpKind::Neg => MapUnaryOp::Neg,
        UnaryOpKind::Not => MapUnaryOp::Not,
    }
}

/// Lower a value closure body with param→Input bindings.
///
/// `|s| parse_hex(s)` → when `s` appears as an Ident or FnCall arg, substitute `Input`.
fn lower_value_expr_with_bindings<'a>(
    expr: &ValueExpr<'a>,
    params: &[Token<'a, std::borrow::Cow<'a, str>>],
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    let bindings: HashMap<&str, MapExpr> = params
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let value = if i == 0 {
                MapExpr::Input // First param binds to the parse result.
            } else {
                // Additional params: bind to InputProp for named access.
                let sid = ctx.strings.intern(p.value.as_ref());
                MapExpr::InputProp { prop: sid }
            };
            (p.value.as_ref(), value)
        })
        .collect();

    lower_value_expr_substituted(expr, &bindings, ctx)
}

/// Lower a value expression with active param→MapExpr bindings.
fn lower_value_expr_substituted<'a>(
    expr: &ValueExpr<'a>,
    bindings: &HashMap<&str, MapExpr>,
    ctx: &mut LowerCtx<'a>,
) -> MapExpr {
    match expr {
        ValueExpr::Ident(token) => {
            let name = token.value.as_ref();
            if let Some(bound) = bindings.get(name) {
                return bound.clone();
            }
            // Not a bound param — treat as function call on input.
            let sid = ctx.strings.intern(name);
            MapExpr::FnCall { name: sid, args: vec![MapExpr::Input] }
        }
        ValueExpr::FnCall(name_token, args) => {
            let sid = ctx.strings.intern(name_token.value.as_ref());
            let ir_args: Vec<MapExpr> = args.iter()
                .map(|a| lower_value_expr_substituted(a, bindings, ctx))
                .collect();
            MapExpr::FnCall { name: sid, args: ir_args }
        }
        ValueExpr::BinOp(kind, lhs, rhs) => MapExpr::BinOp {
            op: lower_binop(*kind),
            lhs: Box::new(lower_value_expr_substituted(lhs, bindings, ctx)),
            rhs: Box::new(lower_value_expr_substituted(rhs, bindings, ctx)),
        },
        ValueExpr::UnaryOp(kind, inner) => MapExpr::UnaryOp {
            op: lower_unaryop(*kind),
            inner: Box::new(lower_value_expr_substituted(inner, bindings, ctx)),
        },
        ValueExpr::Paren(inner) => lower_value_expr_substituted(inner, bindings, ctx),
        ValueExpr::Closure(inner_params, body) => {
            lower_value_expr_with_bindings(body, inner_params, ctx)
        }
        // Literals and Input don't reference params — delegate to standard lowering.
        _ => lower_value_expr(expr, ctx),
    }
}

/// Lower a `ValueExpr` AST node to a `MapExpr` IR node.
fn lower_value_expr<'a>(expr: &ValueExpr<'a>, ctx: &mut LowerCtx<'a>) -> MapExpr {
    match expr {
        ValueExpr::IntLit(token) => {
            let text = token.value.as_ref();
            parse_int_literal(text)
        }
        ValueExpr::FloatLit(token) => {
            let text = token.value.as_ref();
            parse_float_literal(text)
        }
        ValueExpr::BoolLit(token) => MapExpr::BoolLit(token.value),
        ValueExpr::StringLit(token) => {
            let sid = ctx.strings.intern(token.value.as_ref());
            MapExpr::StringLit(sid)
        }
        ValueExpr::Input(_) => MapExpr::Input,
        ValueExpr::InputProp(_, prop) => {
            let sid = ctx.strings.intern(prop.value.as_ref());
            MapExpr::InputProp { prop: sid }
        }
        ValueExpr::Ident(token) => {
            // Bare identifier: could be a type shorthand (handled at MapArrow level)
            // or a variable reference. At the MapExpr level, treat as a function call
            // on input (for backward compat with path expressions like `crate::func`).
            let sid = ctx.strings.intern(token.value.as_ref());
            MapExpr::FnCall {
                name: sid,
                args: vec![MapExpr::Input],
            }
        }
        ValueExpr::FnCall(name_token, args) => {
            let sid = ctx.strings.intern(name_token.value.as_ref());
            let ir_args: Vec<MapExpr> = args.iter().map(|a| lower_value_expr(a, ctx)).collect();
            MapExpr::FnCall {
                name: sid,
                args: ir_args,
            }
        }
        ValueExpr::BinOp(kind, lhs, rhs) => MapExpr::BinOp {
            op: lower_binop(*kind),
            lhs: Box::new(lower_value_expr(lhs, ctx)),
            rhs: Box::new(lower_value_expr(rhs, ctx)),
        },
        ValueExpr::UnaryOp(kind, inner) => MapExpr::UnaryOp {
            op: lower_unaryop(*kind),
            inner: Box::new(lower_value_expr(inner, ctx)),
        },
        ValueExpr::Paren(inner) => lower_value_expr(inner, ctx),
        ValueExpr::Closure(params, body) => {
            // Value closure: |s| expr — first param binds to the parse result (Input).
            // Lower the body with param→Input substitution.
            lower_value_expr_with_bindings(body, params, ctx)
        }
    }
}

/// Parse an integer literal string to `MapExpr::IntLit`.
/// Handles decimal, hex (`0x`), and optional type suffix.
fn parse_int_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = if digits.starts_with("0x") || digits.starts_with("0X") {
        i64::from_str_radix(&digits[2..], 16).unwrap_or(0)
    } else {
        digits.parse::<i64>().unwrap_or(0)
    };
    MapExpr::IntLit(value)
}

/// Parse a float literal string to `MapExpr::FloatLit`.
fn parse_float_literal(text: &str) -> MapExpr {
    let (digits, _suffix) = split_numeric_suffix(text);
    let value = digits.parse::<f64>().unwrap_or(0.0);
    MapExpr::FloatLit(value)
}

/// Split a numeric literal into (digits, suffix). E.g. `"42u8"` → `("42", "u8")`.
fn split_numeric_suffix(text: &str) -> (&str, &str) {
    // Find where the suffix starts: first alphabetic/underscore after digits.
    let bytes = text.as_bytes();
    let mut i = 0;
    // Skip hex prefix.
    if bytes.len() > 2 && bytes[0] == b'0' && (bytes[1] == b'x' || bytes[1] == b'X') {
        i = 2;
        while i < bytes.len() && bytes[i].is_ascii_hexdigit() {
            i += 1;
        }
    } else {
        // Skip decimal digits, dots, exponents.
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

/// Lower a `MapArrow` to a `FnId`.
///
/// Detects type-shorthand patterns (bare type name like `f64`) and constant
/// expressions. Delegates to `lower_value_expr` for the general case.
fn lower_map_arrow<'a>(arrow: &MapArrow<'a>, ctx: &mut LowerCtx<'a>) -> FnId {
    let return_type = arrow
        .return_type
        .as_ref()
        .map(|tok| {
            let sid = ctx.strings.intern(tok.value.as_ref());
            TypeDesc::Named(sid)
        });

    // Type-shorthand: bare type name like `-> f64`, `-> u32`.
    if let ValueExpr::Ident(token) = &arrow.expr {
        let name = token.value.as_ref();
        if matches!(
            name,
            "f64" | "f32" | "u32" | "u64" | "i32" | "i64" | "usize" | "u8" | "u16" | "i8"
                | "i16"
        ) && arrow.return_type.is_none()
        {
            let type_sid = ctx.strings.intern(name);
            return ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::Input,
                return_type: Some(TypeDesc::Named(type_sid)),
            });
        }
    }

    // Extract type suffix from integer/float literals when no explicit type annotation.
    let return_type = return_type.or_else(|| {
        match &arrow.expr {
            ValueExpr::IntLit(token) | ValueExpr::FloatLit(token) => {
                let (_, suffix) = split_numeric_suffix(token.value.as_ref());
                if !suffix.is_empty() {
                    let sid = ctx.strings.intern(suffix);
                    Some(TypeDesc::Named(sid))
                } else {
                    None
                }
            }
            ValueExpr::BoolLit(_) => {
                let sid = ctx.strings.intern("bool");
                Some(TypeDesc::Named(sid))
            }
            _ => None,
        }
    });

    // @host return type propagation: if no explicit annotation and the expression
    // is a bare identifier or function call matching a typed @host declaration,
    // inherit the @host's return type. The @host declaration is the single source
    // of truth for the function's abstract return type.
    let return_type = return_type.or_else(|| {
        let func_name = match &arrow.expr {
            ValueExpr::Ident(tok) => Some(tok.value.as_ref()),
            ValueExpr::FnCall(tok, _) => Some(tok.value.as_ref()),
            _ => None,
        };
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

    let map_expr = lower_value_expr(&arrow.expr, ctx);

    ctx.fns.push(FnDescriptor::Expr {
        expr: map_expr,
        return_type,
    })
}

// ─── Beta-reduction for grammar function application ────────────────────────

/// Apply a grammar function: substitute params with args in the body, then lower.
///
/// This is beta-reduction: `(|x| body)(arg)` → `body[x := arg]`.
/// When the body references a param name as a Nonterminal, we lower the
/// corresponding arg expression instead. All other nodes lower normally.
/// Handles nested calls: `parens(csv(expr))` — expanding `parens` encounters
/// `csv(expr)` which triggers another beta-reduction recursively.
fn substitute_and_lower<'a>(
    body: &'a Expression<'a>,
    subs: &HashMap<&str, &'a Expression<'a>>,
    ctx: &mut LowerCtx<'a>,
) -> IrNode {
    match body {
        // The key case: a Nonterminal that matches a parameter name is replaced
        // by the corresponding argument expression, which is then lowered.
        Expression::Nonterminal(token) => {
            let name = token.value.as_ref();
            if let Some(&arg_expr) = subs.get(name) {
                // Substitute: lower the arg expression (which may itself contain
                // GrammarCalls that trigger further beta-reduction).
                lower_expression(arg_expr, ctx)
            } else {
                // Not a parameter — lower as a regular nonterminal reference.
                lower_expression(body, ctx)
            }
        }

        // GrammarCall inside the body — lower normally (triggers beta-reduction
        // if the call targets a closure, via lower_expression's GrammarCall arm).
        Expression::GrammarCall(_, _) => lower_expression(body, ctx),

        // Recursive structural descent — substitute through all sub-expressions.
        Expression::Concatenation(token) => {
            let children: Vec<IrNode> = token.value.iter()
                .map(|child| substitute_and_lower(child, subs, ctx))
                .collect();
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }
        Expression::Alternation(token) => {
            let branches: Vec<bbnf_ir::AltBranch> = token.value.iter()
                .map(|child| bbnf_ir::AltBranch {
                    node: substitute_and_lower(child, subs, ctx),
                    first_set: None,
                })
                .collect();
            if branches.len() == 1 {
                branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(branches, None)
            }
        }
        Expression::Group(inner) => substitute_and_lower(&inner.value, subs, ctx),
        Expression::Optional(inner) => {
            let node = substitute_and_lower(&inner.value, subs, ctx);
            IrNode::Repeat { inner: Box::new(node), lo: 0, hi: 1 }
        }
        Expression::Many(inner) => {
            let node = substitute_and_lower(&inner.value, subs, ctx);
            IrNode::Repeat { inner: Box::new(node), lo: 0, hi: u32::MAX }
        }
        Expression::Many1(inner) => {
            let node = substitute_and_lower(&inner.value, subs, ctx);
            IrNode::Repeat { inner: Box::new(node), lo: 1, hi: u32::MAX }
        }
        Expression::OptionalWhitespace(inner) => {
            let node = substitute_and_lower(&inner.value, subs, ctx);
            IrNode::OptionalWhitespace(Box::new(node))
        }
        Expression::Skip(left, right) => {
            let l = substitute_and_lower(&left.value, subs, ctx);
            let r = substitute_and_lower(&right.value, subs, ctx);
            IrNode::Skip(Box::new(l), Box::new(r))
        }
        Expression::Next(left, right) => {
            let l = substitute_and_lower(&left.value, subs, ctx);
            let r = substitute_and_lower(&right.value, subs, ctx);
            IrNode::Next(Box::new(l), Box::new(r))
        }
        Expression::Minus(left, right) => {
            let l = substitute_and_lower(&left.value, subs, ctx);
            let r = substitute_and_lower(&right.value, subs, ctx);
            IrNode::Minus(Box::new(l), Box::new(r))
        }
        Expression::MappedExpression(inner, arrow) => {
            let inner_node = substitute_and_lower(&inner.value, subs, ctx);
            let fn_id = lower_map_arrow(arrow, ctx);
            let fn_id = try_specialize_map_fn(&inner_node, fn_id, ctx);
            IrNode::Map { inner: Box::new(inner_node), fn_id }
        }

        // Leaf expressions that don't contain nonterminals — lower directly.
        _ => lower_expression(body, ctx),
    }
}

// ─── Expression lowering ────────────────────────────────────────────────────

/// Lower a single `Expression` to an `IrNode`.
pub(crate) fn lower_expression<'a>(expr: &'a Expression<'a>, ctx: &mut LowerCtx<'a>) -> IrNode {
    match expr {
        Expression::Literal(token) => {
            let id = ctx.strings.intern(token.value.as_ref());
            IrNode::Literal(id)
        }

        Expression::Regex(token) => {
            let id = ctx.strings.intern(token.value.as_ref());
            IrNode::Regex(id)
        }

        Expression::Epsilon(_) => IrNode::Epsilon,

        Expression::Nonterminal(token) => {
            let name: &str = token.value.as_ref();
            match ctx.name_to_rule_id.get(name) {
                Some(&rule_id) => IrNode::Ref(rule_id),
                None if ctx.recovery_mode => IrNode::Epsilon,
                None => {
                    panic!(
                        "unknown nonterminal `{}` — should have been caught by validate_ast()",
                        name,
                    );
                }
            }
        }

        Expression::Group(inner) => {
            lower_expression(&inner.value, ctx)
        }

        Expression::SpanCapture(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            let fn_id = ctx.fns.push(bbnf_ir::FnDescriptor::SpanCapture);
            IrNode::Map {
                inner: Box::new(inner_node),
                fn_id,
            }
        }

        Expression::Optional(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 0,
                hi: 1,
            }
        }

        Expression::Many(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 0,
                hi: u32::MAX,
            }
        }

        Expression::Many1(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::Repeat {
                inner: Box::new(inner_node),
                lo: 1,
                hi: u32::MAX,
            }
        }

        Expression::OptionalWhitespace(inner) => {
            let inner_node = lower_expression(&inner.value, ctx);
            IrNode::OptionalWhitespace(Box::new(inner_node))
        }

        Expression::Skip(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Skip(Box::new(left_node), Box::new(right_node))
        }

        Expression::Next(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Next(Box::new(left_node), Box::new(right_node))
        }

        Expression::Minus(left, right) => {
            let left_node = lower_expression(&left.value, ctx);
            let right_node = lower_expression(&right.value, ctx);
            IrNode::Minus(Box::new(left_node), Box::new(right_node))
        }

        Expression::Concatenation(token) => {
            let children: Vec<IrNode> = token
                .value
                .iter()
                .map(|child| lower_expression(child, ctx))
                .collect();
            if children.len() == 1 {
                children.into_iter().next().unwrap()
            } else {
                IrNode::Seq(children)
            }
        }

        Expression::Alternation(token) => {
            let branches: Vec<AltBranch> = token
                .value
                .iter()
                .enumerate()
                .map(|(i, child)| {
                    let node = lower_expression(child, ctx);

                    // Branch FIRST sets are populated by the IR CSP pass post-lowering.
                    AltBranch { node, first_set: None }
                })
                .collect();

            if branches.len() == 1 {
                branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(branches, None)
            }
        }

        Expression::MappedExpression(inner, arrow) => {
            let inner_node = lower_expression(&inner.value, ctx);
            let fn_id = lower_map_arrow(arrow, ctx);
            let fn_id = try_specialize_map_fn(&inner_node, fn_id, ctx);

            IrNode::Map {
                inner: Box::new(inner_node),
                fn_id,
            }
        }

        Expression::DebugExpression((inner, _label)) => {
            lower_expression(&inner.value, ctx)
        }

        Expression::Rule(inner, mapping) => {
            let inner_node = lower_expression(inner, ctx);

            if let Some(arrow) = mapping {
                let fn_id = lower_map_arrow(arrow, ctx);
                let fn_id = try_specialize_map_fn(&inner_node, fn_id, ctx);

                IrNode::Map {
                    inner: Box::new(inner_node),
                    fn_id,
                }
            } else {
                inner_node
            }
        }

        Expression::Closure(_params, body) => {
            // Grammar closure: lower the body expression directly.
            lower_expression(&body.value, ctx)
        }

        Expression::GrammarCall(name_tok, args) => {
            let name: &str = name_tok.value.as_ref();

            // Check if this is a grammar function call (closure application).
            if let Some(closure) = ctx.closures.get(name) {
                let params = closure.params;
                let body = closure.body;
                assert_eq!(
                    args.len(),
                    params.len(),
                    "arity mismatch: `{}` expects {} args, got {}",
                    name,
                    params.len(),
                    args.len(),
                );

                // Beta-reduction: substitute params with args, then lower.
                let subs: HashMap<&str, &Expression<'a>> = params
                    .iter()
                    .zip(args.iter())
                    .map(|(p, a)| (p.value.as_ref(), a))
                    .collect();

                return substitute_and_lower(body, &subs, ctx);
            }

            // Not a closure — regular nonterminal reference.
            match ctx.name_to_rule_id.get(name) {
                Some(&rule_id) => IrNode::Ref(rule_id),
                None if ctx.recovery_mode => IrNode::Epsilon,
                None => panic!("unknown rule or grammar function `{}`", name),
            }
        }

        Expression::ProductionRule(_lhs, rhs) => {
            lower_expression(rhs, ctx)
        }
    }
}
