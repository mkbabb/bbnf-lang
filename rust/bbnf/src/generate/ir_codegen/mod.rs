//! Core IR → TokenStream code generation.
//!
//! Walks `IrNode` trees and emits Rust parser combinator code as `TokenStream`.
//!
//! Sub-modules:
//! - `alt`: Alternation, dispatch tables, sub-variant coercion
//! - `seq`: Concatenation, Span compression, flattening
//! - `repeat`: Repeat quantifiers, sep_by pattern detection, separator stripping
//! - `wrap`: Wrap pattern emission, regex coalescing
//! - `infer`: Quick type inference for Span detection

mod alt;
pub mod infer;
mod repeat;
mod seq;
mod wrap;

use bbnf_ir::{FnDescriptor, IrNode, RuleId};

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::fast_paths;
use super::ir_types::IrCodegenCtx;

// Re-exports used by other generate modules.
pub use infer::infer_node_type;

/// Unescape a BBNF literal string (e.g. `\n` → newline, `\t` → tab).
/// BBNF literals store escape sequences as raw characters (backslash + letter)
/// since they come from source text between quotes. We need to unescape them
/// before embedding into Rust string literals via `quote!`.
pub fn unescape_literal(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some('f') => result.push('\x0C'),
                Some('b') => result.push('\x08'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

/// Generate a parser TokenStream from an IrNode.
pub fn ir_node_to_tokens(node: &IrNode, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let raw = ctx.ir.get_string(*sid);
            let unescaped = unescape_literal(raw);
            let lit = proc_macro2::Literal::string(&unescaped);
            quote! { ::parse_that::string_span(#lit) }
        }

        IrNode::Regex(sid) => {
            let pattern = ctx.ir.get_string(*sid);
            fast_paths::emit_regex_parser(pattern)
        }

        IrNode::Epsilon => quote! { ::parse_that::epsilon() },

        IrNode::Ref(rule_id) => emit_ref(*rule_id, ctx),

        IrNode::Seq(children) => seq::emit_seq(children, ctx),

        IrNode::Alt(branches, dispatch) => {
            alt::emit_alt(branches, dispatch.as_ref(), ctx)
        }

        IrNode::Repeat { inner, lo, hi } => repeat::emit_repeat(inner, *lo, *hi, ctx),

        IrNode::Skip(left, right) => {
            // wrap detection: Skip(Next(open, middle), close) → middle.wrap(open, close)
            if let IrNode::Next(open, middle) = left.as_ref() {
                return wrap::emit_wrap(open, middle, right, ctx);
            }
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.skip(#right_ts) }
        }

        IrNode::Next(left, right) => {
            // wrap detection: Next(open, Skip(middle, close)) → middle.wrap(open, close)
            if let IrNode::Skip(middle, close) = right.as_ref() {
                return wrap::emit_wrap(left, middle, close, ctx);
            }
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.next(#right_ts) }
        }

        IrNode::Minus(left, right) => {
            let left_ts = ir_node_to_tokens(left, ctx);
            let right_ts = ir_node_to_tokens(right, ctx);
            quote! { #left_ts.minus(#right_ts) }
        }

        IrNode::Negate(inner) => {
            let inner_ts = ir_node_to_tokens(inner, ctx);
            quote! { #inner_ts.negate() }
        }

        IrNode::Map { inner, fn_id } => {
            let inner_ts = ir_node_to_tokens(inner, ctx);
            emit_map(inner_ts, *fn_id, ctx)
        }

        IrNode::OptionalWhitespace(inner) => {
            // sep_by_ws detection: OptionalWhitespace(Repeat { inner: Skip(elem, Repeat { sep, 0, 1 }) })
            if let IrNode::Repeat {
                inner: rep_inner,
                lo,
                hi,
            } = inner.as_ref()
            {
                if !(*lo == 0 && *hi == 1) {
                    if let Some((element, separator)) = repeat::try_sep_by(rep_inner) {
                        return repeat::emit_sep_by_ws(element, separator, *lo, ctx);
                    }
                }
            }

            let inner_ts = ir_node_to_tokens(inner, ctx);
            quote! { #inner_ts.trim_whitespace() }
        }
    }
}

/// Emit a nonterminal reference.
fn emit_ref(rule_id: RuleId, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let rule = &ctx.ir.rules[rule_id as usize];
    let resolved_name = ctx.resolve_rule_name(rule_id);
    let ident = format_ident!("{}", resolved_name);

    if rule.meta.is_transparent {
        // Transparent rules return Box<Enum> directly — no extra boxing.
        quote! { Self::#ident() }
    } else {
        // Non-transparent: wrap result in Box for recursive types.
        quote! { Self::#ident().map(|x| Box::new(x)) }
    }
}

/// Emit a Map expression.
fn emit_map(inner_ts: TokenStream, fn_id: u32, ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let fd = &ctx.ir.fns[fn_id as usize];
    match fd {
        FnDescriptor::EnumWrap { variant } => {
            let vname = ctx.ir.get_string(*variant);
            let vident = format_ident!("{}", vname);
            let enum_ident = &ctx.enum_ident;
            quote! { #inner_ts.map(|x| #enum_ident::#vident(x)) }
        }
        FnDescriptor::BoxWrap => {
            quote! { #inner_ts.map(Box::new) }
        }
        FnDescriptor::Custom { source, .. } => {
            let closure_src = ctx.ir.get_string(*source);
            let closure: syn::ExprClosure = syn::parse_str(closure_src).unwrap_or_else(|e| {
                panic!(
                    "Invalid mapping closure `{}` in IR codegen: {}",
                    closure_src, e
                )
            });
            quote! { #inner_ts.map(#closure) }
        }
    }
}
