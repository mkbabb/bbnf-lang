//! Expression/node lowering logic — the recursive `lower_expression` function.

use bbnf_ir::{AltBranch, FnDescriptor, FnId, IrNode};

use crate::types::Expression;

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

/// Lower a mapping function expression to a `FnId`.
///
/// B.3: Parses the closure's `-> ReturnType` annotation (if present) and stores
/// it as a `TypeDesc::Named` in the `FnDescriptor::Custom` variant. This allows
/// IR type inference to use the actual return type instead of the closure source text.
fn lower_mapping_fn<'a>(expr: &Expression<'a>, ctx: &mut LowerCtx<'a>) -> FnId {
    match expr {
        Expression::MappingFn(token) => {
            let string_id = ctx.strings.intern(token.value.as_ref());
            // Try to parse the closure and extract its return type annotation.
            let return_type = parse_closure_return_type(token.value.as_ref(), ctx);
            ctx.fns.push(FnDescriptor::Custom {
                source: string_id,
                return_type,
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
            IrNode::Map {
                inner: Box::new(inner_node),
                fn_id,
            }
        }

        Expression::DebugExpression((inner, _label)) => {
            // Debug expressions are transparent in the IR — they're a development tool.
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
