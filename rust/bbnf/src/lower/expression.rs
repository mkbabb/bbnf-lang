//! Expression/node lowering logic — the recursive `lower_expression` function.

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode, TypeDesc};

use crate::generate::regex_classify::{classify_regex, RegexClass};
use crate::types::{Expression, Token};

use super::LowerCtx;

/// Convert a `CharSet` ([u32; 4]) to a `CharSet128` ([u64; 2]).
fn charset_to_128(cs: &crate::analysis::CharSet) -> bbnf_ir::CharSet128 {
    bbnf_ir::CharSet128::from_u32x4(&cs.bits)
}

/// Unwrap a `Rule(inner, mapping)` to get the inner expression.
pub(crate) fn unwrap_rule<'a>(expr: &'a Expression<'a>) -> &'a Expression<'a> {
    match expr {
        Expression::Rule(inner, _) => inner,
        other => other,
    }
}

/// Attempt to replace a `FnDescriptor::Custom` with a specialized descriptor
/// based on the combination of inner node type and closure return type annotation.
///
/// Recognized patterns:
/// - `Regex(numeric_pattern) -> f64` → `FnDescriptor::NumberConvert`
/// - `Regex(hex_pattern) -> u32` → `FnDescriptor::HexConvert { fn_path }`
fn try_specialize_map_fn(inner: &IrNode, fn_id: FnId, ctx: &mut LowerCtx<'_>) -> FnId {
    let desc = &ctx.fns.fns[fn_id as usize];

    let (source_sid, type_sid) = match desc {
        FnDescriptor::Custom {
            source,
            return_type: Some(TypeDesc::Named(sid)),
        } => (*source, *sid),
        _ => return fn_id,
    };

    let regex_sid = match inner {
        IrNode::Regex(sid) => *sid,
        _ => return fn_id,
    };

    // Copy strings upfront to avoid holding immutable borrows across the
    // mutable `ctx.strings.intern()` / `ctx.fns.push()` calls below.
    let type_name = ctx.strings.resolve(type_sid).to_owned();
    let pattern = ctx.strings.resolve(regex_sid).to_owned();

    match type_name.as_str() {
        "f64" => {
            if matches!(classify_regex(&pattern), RegexClass::Numeric { .. }) {
                ctx.fns.push(FnDescriptor::NumberConvert)
            } else {
                fn_id
            }
        }
        "u32" => {
            if matches!(classify_regex(&pattern), RegexClass::HexDigits) {
                // Extract the function path from the closure body.
                let source_str = ctx.strings.resolve(source_sid).to_owned();
                match extract_closure_fn_path(&source_str) {
                    Some(fn_path) => {
                        let path_sid = ctx.strings.intern(&fn_path);
                        ctx.fns.push(FnDescriptor::HexConvert { fn_path: path_sid })
                    }
                    None => fn_id,
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
    // The body should be a block containing a single expression (the call).
    let body_expr = match closure.body.as_ref() {
        syn::Expr::Block(block) => {
            let stmt = block.block.stmts.last()?;
            match stmt {
                syn::Stmt::Expr(expr, _) => expr,
                _ => return None,
            }
        }
        // Could also be a bare expression (no block braces).
        other => other,
    };

    // Unwrap `.unwrap_or(...)` or other method chains to find the inner call.
    let call_expr = match body_expr {
        syn::Expr::MethodCall(mc) => {
            // e.g., `foo(x).unwrap_or(0)` — the receiver is the actual call.
            mc.receiver.as_ref()
        }
        syn::Expr::Call(_) => body_expr,
        _ => return None,
    };

    if let syn::Expr::Call(call) = call_expr {
        // The function position should be a path expression.
        if let syn::Expr::Path(path_expr) = call.func.as_ref() {
            // Convert the path back to a string, normalizing `::` spacing.
            let path_str =
                quote::ToTokens::to_token_stream(&path_expr.path).to_string();
            return Some(path_str.replace(" :: ", "::"));
        }
    }

    None
}

/// Lower a mapping function expression to a `FnId`.
///
/// B.3: Parses the closure's `-> ReturnType` annotation (if present) and stores
/// it as a `TypeDesc::Named` in the `FnDescriptor::Custom` variant. This allows
/// IR type inference to use the actual return type instead of the closure source text.
///
/// Shorthand forms:
/// - Constant literal: `"px" -> 0u8` — parsed as `FnDescriptor::Constant`
/// - Path expression: `hexDigits -> crate::parse_hex_color` — parsed as `FnDescriptor::Constant`
fn lower_mapping_fn<'a>(expr: &Expression<'a>, ctx: &mut LowerCtx<'a>) -> FnId {
    match expr {
        Expression::MappingFn(token) => {
            let mapper_str = token.value.as_ref().trim();
            let string_id = ctx.strings.intern(mapper_str);

            // Type-shorthand: bare type name like `f64` or `u32`.
            // Lowers as a Custom with the named return type so that
            // `try_specialize_map_fn` can detect `Regex(numeric) -> f64`
            // and emit `NumberConvert` / `HexConvert`.
            if syn::parse_str::<syn::Type>(mapper_str).is_ok()
                && syn::parse_str::<syn::ExprLit>(mapper_str).is_err()
                && matches!(mapper_str, "f64" | "f32" | "u32" | "u64" | "i32" | "i64" | "usize")
            {
                let type_sid = ctx.strings.intern(mapper_str);
                return ctx.fns.push(FnDescriptor::Custom {
                    source: string_id,
                    return_type: Some(bbnf_ir::TypeDesc::Named(type_sid)),
                });
            }

            // Try as closure first (existing path).
            if syn::parse_str::<syn::ExprClosure>(mapper_str).is_ok() {
                let return_type = parse_closure_return_type(mapper_str, ctx);
                return ctx.fns.push(FnDescriptor::Custom {
                    source: string_id,
                    return_type,
                });
            }

            // Try as constant/literal/path expression (shorthand syntax).
            if syn::parse_str::<syn::Expr>(mapper_str).is_ok() {
                let return_type = infer_expr_type(mapper_str, ctx);
                return ctx.fns.push(FnDescriptor::Constant {
                    value: string_id,
                    return_type,
                });
            }

            // Fallback: treat as opaque custom.
            ctx.fns.push(FnDescriptor::Custom {
                source: string_id,
                return_type: None,
            })
        }
        _ => {
            let text = format!("{:?}", expr);
            let string_id = ctx.strings.intern(&text);
            ctx.fns.push(FnDescriptor::Custom {
                source: string_id,
                return_type: None,
            })
        }
    }
}

/// Infer the return type of a non-closure expression for `FnDescriptor::Constant`.
///
/// Recognizes:
/// - Suffixed integer literals (`0u8`, `42i32`) → `TypeDesc::Named("u8")` etc.
/// - `true`/`false` → `TypeDesc::Named("bool")`
/// - Float literals (`3.14f64`) → `TypeDesc::Named("f64")`
/// - String literals → `TypeDesc::Named("&str")`
/// - Path expressions → `None` (type must come from context)
fn infer_expr_type(source: &str, ctx: &mut LowerCtx<'_>) -> Option<bbnf_ir::TypeDesc> {
    if let Ok(lit) = syn::parse_str::<syn::ExprLit>(source) {
        match &lit.lit {
            syn::Lit::Int(int_lit) => {
                let suffix = int_lit.suffix();
                if !suffix.is_empty() {
                    let sid = ctx.strings.intern(suffix);
                    return Some(bbnf_ir::TypeDesc::Named(sid));
                }
            }
            syn::Lit::Float(float_lit) => {
                let suffix = float_lit.suffix();
                if !suffix.is_empty() {
                    let sid = ctx.strings.intern(suffix);
                    return Some(bbnf_ir::TypeDesc::Named(sid));
                }
            }
            syn::Lit::Bool(_) => {
                let sid = ctx.strings.intern("bool");
                return Some(bbnf_ir::TypeDesc::Named(sid));
            }
            syn::Lit::Str(_) => {
                let sid = ctx.strings.intern("& str");
                return Some(bbnf_ir::TypeDesc::Named(sid));
            }
            _ => {}
        }
    }
    None
}

/// Parse a Rust closure source string to extract the return type annotation.
/// Returns `Some(TypeDesc::Named(sid))` if the closure has `-> ReturnType`, None otherwise.
fn parse_closure_return_type(source: &str, ctx: &mut LowerCtx<'_>) -> Option<bbnf_ir::TypeDesc> {
    let closure = syn::parse_str::<syn::ExprClosure>(source).ok()?;
    if let syn::ReturnType::Type(_, ty) = &closure.output {
        // Intern the return type as a string for TypeDesc::Named.
        let ty_str = quote::ToTokens::to_token_stream(ty).to_string();
        let sid = ctx.strings.intern(&ty_str);
        Some(bbnf_ir::TypeDesc::Named(sid))
    } else {
        None
    }
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
                    // Unknown nonterminal — emit as literal for robustness.
                    // Backends should validate and report this as an error.
                    let id = ctx.strings.intern(name);
                    IrNode::Literal(id)
                }
            }
        }

        Expression::Group(inner) => {
            // Group is purely syntactic — lower the inner expression directly.
            lower_expression(&inner.value, ctx)
        }

        Expression::SpanCapture(inner) => {
            // @{expr}: parse inner for validation, return Span of consumed input.
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

                    // Attach per-branch FIRST set if available.
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

                    AltBranch {
                        node,
                        first_set,
                    }
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
            // Debug expressions are transparent in the IR — the label is preserved
            // in GrammarIR::debug_labels for display in debug adapters.
            if !label.is_empty() {
                if let Some(rule_id) = ctx.current_lhs.and_then(|lhs| {
                    if let Expression::Nonterminal(Token { value, .. }) = lhs {
                        ctx.name_to_rule_id.get(value.as_ref()).copied()
                    } else {
                        None
                    }
                }) {
                    let label_id = ctx.strings.intern(label);
                    // Note: debug_labels is collected post-lowering via GrammarIR.
                    // We store the pair for later attachment.
                    let _ = (rule_id, label_id);
                }
            }
            lower_expression(&inner.value, ctx)
        }

        Expression::MappingFn(_) => {
            // A standalone mapping function reference shouldn't appear as a body,
            // but handle gracefully by treating it as a custom function with Epsilon input.
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

                // Check for specialized conversion patterns (numeric, hex).
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
            // Production rules shouldn't appear in the body, but handle gracefully.
            // Lower the RHS as the body.
            lower_expression(rhs, ctx)
        }
    }
}
