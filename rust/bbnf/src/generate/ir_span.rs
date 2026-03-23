//! SpanParser code generation from IR.
//!
//! Generates `_sp()` methods for span-eligible rules by walking `IrNode` trees
//! and emitting `SpanParser` combinator code. Replaces AST-based `span_codegen.rs`.

use std::collections::HashSet;

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, RuleId};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::fast_paths;
use super::ir_codegen::unescape_literal;
use super::ir_types::IrCodegenCtx;

/// Try to generate a SpanParser expression for a rule's body.
/// Returns `None` if the body contains constructs that can't be expressed as SpanParser
/// (recursion, Map nodes with custom functions, etc.).
pub fn try_ir_span_parser(rule_id: RuleId, ctx: &IrCodegenCtx<'_>) -> Option<TokenStream> {
    let rule = &ctx.ir.rules[rule_id as usize];
    // Unwrap Map wrapper — SpanParser doesn't do enum wrapping.
    let body = unwrap_map(&rule.body);
    try_ir_span_expr(body, ctx)
}

/// Unwrap Map nodes that are just enum/box wrappers (transparent for span purposes).
fn unwrap_map(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } => unwrap_map(inner),
        other => other,
    }
}

/// Strip `Map` wrappers (EnumWrap, BoxWrap) from a node whose result will be discarded.
fn strip_discarded_maps_span<'a>(node: &'a IrNode, ctx: &IrCodegenCtx<'_>) -> &'a IrNode {
    match node {
        IrNode::Map { inner, fn_id } => {
            let fd = &ctx.ir.fns[*fn_id as usize];
            match fd {
                FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
                    strip_discarded_maps_span(inner, ctx)
                }
                _ => node,
            }
        }
        _ => node,
    }
}

/// Strip `OptionalWhitespace` from separator in sep_by_ws_span context.
fn strip_separator_ow_span(node: &IrNode) -> &IrNode {
    match node {
        IrNode::OptionalWhitespace(inner) => inner.as_ref(),
        _ => node,
    }
}

/// Recursively generate a SpanParser expression for an IrNode.
/// `pub(super)` so `ir_codegen` can call it for inlined dispatch branches.
pub(super) fn try_ir_span_expr(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> Option<TokenStream> {
    match node {
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let lit = proc_macro2::Literal::string(&unescaped);
            Some(quote! { ::parse_that::sp_string(#lit) })
        }

        IrNode::Regex(sid) => {
            let pattern = ctx.ir.get_string(*sid);
            Some(fast_paths::emit_regex_span(pattern))
        }

        IrNode::Epsilon => Some(quote! { ::parse_that::sp_epsilon() }),

        IrNode::Ref(rule_id) => {
            let rule = &ctx.ir.rules[*rule_id as usize];
            let name = ctx.ir.get_string(rule.name);
            if ctx.has_sp_method(name) {
                let ident = ctx.ident;
                let sp_ident = format_ident!("{}_sp", name);
                Some(quote! { #ident::#sp_ident() })
            } else {
                None // Can't express as SpanParser without _sp method.
            }
        }

        IrNode::Seq(children) => {
            if children.is_empty() {
                return Some(quote! { ::parse_that::sp_epsilon() });
            }
            let mut iter = children.iter();
            let mut acc = try_ir_span_expr(iter.next().unwrap(), ctx)?;
            for child in iter {
                let next = try_ir_span_expr(child, ctx)?;
                acc = quote! { #acc.then_span(#next) };
            }
            Some(acc)
        }

        IrNode::Alt(branches, _) => {
            if branches.is_empty() {
                return Some(quote! { ::parse_that::sp_epsilon() });
            }

            // All-literal → sp_any() fast path.
            let all_lit = branches
                .iter()
                .all(|b| matches!(&b.node, IrNode::Literal(_)));
            if all_lit {
                let lits: Vec<proc_macro2::Literal> = branches
                    .iter()
                    .map(|b| {
                        let IrNode::Literal(sid) = &b.node else {
                            unreachable!()
                        };
                        let raw = ctx.ir.get_string(*sid);
                        let unescaped = unescape_literal(raw);
                        proc_macro2::Literal::string(&unescaped)
                    })
                    .collect();
                return Some(quote! { ::parse_that::sp_any(&[#(#lits),*]) });
            }

            let mut iter = branches.iter();
            let mut acc = try_ir_span_expr(&iter.next()?.node, ctx)?;
            for branch in iter {
                let next = try_ir_span_expr(&branch.node, ctx)?;
                acc = quote! { #acc.or(#next) };
            }
            Some(acc)
        }

        IrNode::Repeat { inner, lo, hi } => {
            // sep_by_span detection: Repeat { inner: Skip(elem, Repeat { sep, 0, 1 }) }
            if !(*lo == 0 && *hi == 1) {
                if let IrNode::Skip(element, opt_sep) = inner.as_ref() {
                    if let IrNode::Repeat {
                        inner: separator,
                        lo: 0,
                        hi: 1,
                    } = opt_sep.as_ref()
                    {
                        // Fix 5: strip discarded Map wrappers from separator.
                        let separator = strip_discarded_maps_span(separator, ctx);
                        let elem_ts = try_ir_span_expr(element, ctx)?;
                        let sep_ts = try_ir_span_expr(separator, ctx)?;
                        let lo_usize = *lo as usize;
                        return Some(quote! { #elem_ts.sep_by_span(#sep_ts, #lo_usize..) });
                    }
                }
            }

            let inner_ts = try_ir_span_expr(inner, ctx)?;
            if *lo == 0 && *hi == 1 {
                Some(quote! { #inner_ts.opt_span() })
            } else if *lo == 0 {
                Some(quote! { #inner_ts.many_span(..) })
            } else if *lo == 1 {
                Some(quote! { #inner_ts.many_span(1..) })
            } else {
                let lo = *lo as usize;
                Some(quote! { #inner_ts.many_span(#lo..) })
            }
        }

        IrNode::Skip(left, right) => {
            // wrap_span detection: Skip(Next(open, middle), close)
            if let IrNode::Next(open, middle) = left.as_ref() {
                let o = try_ir_span_expr(open, ctx)?;
                let m = try_ir_span_expr(middle, ctx)?;
                let c = try_ir_span_expr(right, ctx)?;
                return Some(quote! { #m.wrap_span(#o, #c) });
            }
            let l = try_ir_span_expr(left, ctx)?;
            let r = try_ir_span_expr(right, ctx)?;
            Some(quote! { #l.skip_span(#r) })
        }

        IrNode::Next(left, right) => {
            // wrap_span detection: Next(open, Skip(middle, close))
            if let IrNode::Skip(middle, close) = right.as_ref() {
                let o = try_ir_span_expr(left, ctx)?;
                let m = try_ir_span_expr(middle, ctx)?;
                let c = try_ir_span_expr(close, ctx)?;
                return Some(quote! { #m.wrap_span(#o, #c) });
            }
            let l = try_ir_span_expr(left, ctx)?;
            let r = try_ir_span_expr(right, ctx)?;
            Some(quote! { #l.next_after(#r) })
        }

        IrNode::Minus(main, excluded) => {
            let m = try_ir_span_expr(main, ctx)?;
            let e = try_ir_span_expr(excluded, ctx)?;
            Some(quote! { #m.minus_span(#e) })
        }

        IrNode::Negate(inner) => {
            let inner_ts = try_ir_span_expr(inner, ctx)?;
            Some(quote! { #inner_ts.negate() })
        }

        IrNode::OptionalWhitespace(inner) => {
            // sep_by_ws_span detection: OptionalWhitespace(Repeat { Skip(elem, Repeat { sep, 0, 1 }) })
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let IrNode::Skip(element, opt_sep) = rep_inner.as_ref() {
                        if let IrNode::Repeat {
                            inner: separator,
                            lo: 0,
                            hi: 1,
                        } = opt_sep.as_ref()
                        {
                            // Fix 5+6: strip discarded Map wrappers and redundant OW.
                            let separator = strip_discarded_maps_span(separator, ctx);
                            let separator = strip_separator_ow_span(separator);
                            let elem_ts = try_ir_span_expr(element, ctx)?;
                            let sep_ts = try_ir_span_expr(separator, ctx)?;
                            let lo_usize = *lo as usize;
                            return Some(quote! { #elem_ts.sep_by_ws_span(#sep_ts, #lo_usize..) });
                        }
                    }
                }
            }

            let inner_ts = try_ir_span_expr(inner, ctx)?;
            Some(quote! { #inner_ts.trim_whitespace() })
        }

        // Map nodes can't be represented as SpanParser (they change output types).
        IrNode::Map { .. } => None,
    }
}

/// Compute the sp_method_rules set via iterative fixed-point.
///
/// Starting from all span-eligible rules, iteratively test which ones produce
/// valid SpanParser expressions (via `try_ir_span_parser`). Rules that reference
/// other span-eligible rules via `Self::rule_sp()` need those rules to also be
/// in the set, creating a dependency. Iterate until the set stabilizes.
pub fn compute_sp_method_rules(ir: &GrammarIR, ctx: &mut IrCodegenCtx<'_>) {
    loop {
        let next: HashSet<String> = ir
            .rules
            .iter()
            .filter(|r| r.meta.span_eligible)
            .filter(|r| try_ir_span_parser(r.id, ctx).is_some())
            .map(|r| ir.get_string(r.name).to_string())
            .collect();
        if next == ctx.sp_method_rules {
            break;
        }
        ctx.sp_method_rules = next;
    }
}
