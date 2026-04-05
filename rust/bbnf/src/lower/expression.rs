//! Expression/node lowering logic — the recursive `lower_expression` function.

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, MapExpr, TypeDesc};

use crate::generate::regex::classify::{RegexClass, classify_regex};
use crate::types::{Expression, Token};

use super::{LowerCtx, charset_to_128};

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

/// Extract the outermost function path from a closure body expression.
///
/// Given a closure like `|s: Span| -> u32 { crate::foo::bar(s.as_str()) }`,
/// parses it with `syn` and extracts the function path `crate::foo::bar`.
fn extract_closure_fn_path(source: &str) -> Option<String> {
    let closure: syn::ExprClosure = syn::parse_str(source).ok()?;
    let body_expr = match closure.body.as_ref() {
        syn::Expr::Block(block) => {
            let stmt = block.block.stmts.last()?;
            match stmt {
                syn::Stmt::Expr(expr, _) => expr,
                _ => return None,
            }
        }
        other => other,
    };

    let call_expr = match body_expr {
        syn::Expr::MethodCall(mc) => mc.receiver.as_ref(),
        syn::Expr::Call(_) => body_expr,
        _ => return None,
    };

    if let syn::Expr::Call(call) = call_expr {
        if let syn::Expr::Path(path_expr) = call.func.as_ref() {
            let path_str = quote::ToTokens::to_token_stream(&path_expr.path).to_string();
            return Some(path_str.replace(" :: ", "::"));
        }
    }

    None
}

/// Lower a mapping function expression to a `FnId`.
///
/// Produces `FnDescriptor::Expr` with structured `MapExpr` for all user-facing maps.
/// Uses `syn` to parse legacy raw-text mapper syntax (to be replaced by structured
/// value expression parser in Phase B).
///
/// Recognized forms:
/// - Type shorthand: `-> f64` → `Expr { Input, return_type: Named("f64") }`
/// - Constant literal: `-> 0u8` → `Expr { IntLit(0), return_type: Named("u8") }`
/// - Boolean constant: `-> true` → `Expr { BoolLit(true), return_type: Named("bool") }`
/// - Rust closure: `-> |s| fn(s)` → `Expr { FnCall(fn, [Input]), return_type }`
/// - Path expression: `-> crate::func` → `Expr { FnCall(path, [Input]), return_type: None }`
fn lower_mapping_fn<'a>(expr: &Expression<'a>, ctx: &mut LowerCtx<'a>) -> FnId {
    match expr {
        Expression::MappingFn(token) => {
            let mapper_str = token.value.as_ref().trim();

            // Type-shorthand: bare type name like `f64` or `u32`.
            if matches!(
                mapper_str,
                "f64" | "f32" | "u32" | "u64" | "i32" | "i64" | "usize"
            ) {
                let type_sid = ctx.strings.intern(mapper_str);
                return ctx.fns.push(FnDescriptor::Expr {
                    expr: MapExpr::Input,
                    return_type: Some(TypeDesc::Named(type_sid)),
                });
            }

            // Try as constant literal.
            if let Some((map_expr, return_type)) = try_parse_constant(mapper_str, ctx) {
                return ctx.fns.push(FnDescriptor::Expr {
                    expr: map_expr,
                    return_type,
                });
            }

            // Try as Rust closure — extract function path and return type.
            if let Ok(closure) = syn::parse_str::<syn::ExprClosure>(mapper_str) {
                let return_type = if let syn::ReturnType::Type(_, ty) = &closure.output {
                    let ty_str = quote::ToTokens::to_token_stream(ty).to_string();
                    let sid = ctx.strings.intern(&ty_str);
                    Some(TypeDesc::Named(sid))
                } else {
                    None
                };

                // Extract function path from closure body.
                if let Some(fn_path) = extract_closure_fn_path(mapper_str) {
                    let name_sid = ctx.strings.intern(&fn_path);
                    return ctx.fns.push(FnDescriptor::Expr {
                        expr: MapExpr::FnCall {
                            name: name_sid,
                            args: vec![MapExpr::Input],
                        },
                        return_type,
                    });
                }

                // Closure without extractable fn path — store source as FnCall name
                // (Rust backend emits it verbatim as a closure).
                let source_sid = ctx.strings.intern(mapper_str);
                return ctx.fns.push(FnDescriptor::Expr {
                    expr: MapExpr::FnCall {
                        name: source_sid,
                        args: vec![],
                    },
                    return_type,
                });
            }

            // Try as path expression (e.g., `crate::parse_hex_color`).
            if syn::parse_str::<syn::ExprPath>(mapper_str).is_ok() {
                let name_sid = ctx.strings.intern(mapper_str);
                return ctx.fns.push(FnDescriptor::Expr {
                    expr: MapExpr::FnCall {
                        name: name_sid,
                        args: vec![MapExpr::Input],
                    },
                    return_type: None,
                });
            }

            // Fallback: treat as opaque function call on input.
            let source_sid = ctx.strings.intern(mapper_str);
            ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::FnCall {
                    name: source_sid,
                    args: vec![MapExpr::Input],
                },
                return_type: None,
            })
        }
        _ => {
            let text = format!("{:?}", expr);
            let string_id = ctx.strings.intern(&text);
            ctx.fns.push(FnDescriptor::Expr {
                expr: MapExpr::FnCall {
                    name: string_id,
                    args: vec![],
                },
                return_type: None,
            })
        }
    }
}

/// Try to parse a mapper string as a constant literal, returning the MapExpr and type.
fn try_parse_constant(source: &str, ctx: &mut LowerCtx<'_>) -> Option<(MapExpr, Option<TypeDesc>)> {
    // Boolean constants.
    if source == "true" {
        let sid = ctx.strings.intern("bool");
        return Some((MapExpr::BoolLit(true), Some(TypeDesc::Named(sid))));
    }
    if source == "false" {
        let sid = ctx.strings.intern("bool");
        return Some((MapExpr::BoolLit(false), Some(TypeDesc::Named(sid))));
    }

    // Try integer literal with suffix.
    if let Ok(lit) = syn::parse_str::<syn::ExprLit>(source) {
        match &lit.lit {
            syn::Lit::Int(int_lit) => {
                let value: i64 = int_lit.base10_parse().ok()?;
                let suffix = int_lit.suffix();
                let return_type = if !suffix.is_empty() {
                    let sid = ctx.strings.intern(suffix);
                    Some(TypeDesc::Named(sid))
                } else {
                    None
                };
                return Some((MapExpr::IntLit(value), return_type));
            }
            syn::Lit::Float(float_lit) => {
                let value: f64 = float_lit.base10_parse().ok()?;
                let suffix = float_lit.suffix();
                let return_type = if !suffix.is_empty() {
                    let sid = ctx.strings.intern(suffix);
                    Some(TypeDesc::Named(sid))
                } else {
                    None
                };
                return Some((MapExpr::FloatLit(value), return_type));
            }
            syn::Lit::Bool(b) => {
                let sid = ctx.strings.intern("bool");
                return Some((MapExpr::BoolLit(b.value), Some(TypeDesc::Named(sid))));
            }
            syn::Lit::Str(s) => {
                let str_sid = ctx.strings.intern(&s.value());
                let type_sid = ctx.strings.intern("& str");
                return Some((MapExpr::StringLit(str_sid), Some(TypeDesc::Named(type_sid))));
            }
            _ => {}
        }
    }

    None
}

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

                    let first_set = ctx
                        .current_lhs
                        .and_then(|lhs| ctx.first_sets.branch_firsts.get(lhs))
                        .and_then(|branch_firsts| {
                            if i < branch_firsts.len() {
                                let (ref cs, nullable) = branch_firsts[i];
                                if !nullable && !cs.is_empty() {
                                    Some(charset_to_128(cs))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        });

                    AltBranch { node, first_set }
                })
                .collect();

            if branches.len() == 1 {
                branches.into_iter().next().unwrap().node
            } else {
                IrNode::Alt(branches, None)
            }
        }

        Expression::MappedExpression((inner, mapping_fn)) => {
            let inner_node = lower_expression(&inner.value, ctx);
            let fn_id = lower_mapping_fn(&mapping_fn.value, ctx);

            // Check for specialized conversion patterns (numeric, hex).
            let fn_id = try_specialize_map_fn(&inner_node, fn_id, ctx);

            IrNode::Map {
                inner: Box::new(inner_node),
                fn_id,
            }
        }

        Expression::DebugExpression((inner, label)) => {
            if !label.is_empty() {
                if let Some(rule_id) = ctx.current_lhs.and_then(|lhs| {
                    if let Expression::Nonterminal(Token { value, .. }) = lhs {
                        ctx.name_to_rule_id.get(value.as_ref()).copied()
                    } else {
                        None
                    }
                }) {
                    let label_id = ctx.strings.intern(label);
                    let _ = (rule_id, label_id);
                }
            }
            lower_expression(&inner.value, ctx)
        }

        Expression::MappingFn(_) => {
            let fn_id = lower_mapping_fn(expr, ctx);
            IrNode::Map {
                inner: Box::new(IrNode::Epsilon),
                fn_id,
            }
        }

        Expression::Rule(inner, mapping) => {
            let inner_node = lower_expression(inner, ctx);

            if let Some(mapping_expr) = mapping {
                let fn_id = lower_mapping_fn(mapping_expr.as_ref(), ctx);
                let fn_id = try_specialize_map_fn(&inner_node, fn_id, ctx);

                IrNode::Map {
                    inner: Box::new(inner_node),
                    fn_id,
                }
            } else {
                inner_node
            }
        }

        Expression::ProductionRule(_lhs, rhs) => {
            lower_expression(rhs, ctx)
        }
    }
}
