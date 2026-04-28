//! AW-V.W4-fix — visitor-path Wrap emitter.
//!
//! Mirrors the tape-path's transparent dispatcher: skips leading ws,
//! byte-dispatches to the chosen branch's visitor-path shape fn. No
//! visitor event fires on the Wrap rule itself; the chosen branch's
//! visitor fn owns the event emission.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use bbnf_ir::registry::EmitStrategy;
use super::super::dispatcher::{visitor_dispatcher_fn_ident, visitor_shape_fn_ident};
use super::super::root_rule_name;
use super::{shape_tag_name, unwrap_outer};

/// Emit `pub fn parse_wrap_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
///
/// AZ-I.W2.RD — `strategy` accepted for signature uniformity with the
/// tape-path entry. The visitor-path emission is strategy-independent;
/// `has_w4_classified` upstream gates whether the visitor path emits
/// at all. Threading the strategy here keeps the per-shape dispatch
/// surface uniform across all shape emitters.
pub fn emit_parse_wrap_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    _strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let body = unwrap_outer(&rule.body);
    let dispatch = match body {
        IrNode::Alt(branches, _) => emit_alt_visitor_dispatch(
            branches,
            grammar_suffix,
            &dispatcher_ident,
            ir,
        ),
        _ => quote! {
            #dispatcher_ident(input, p, state, visitor)
        },
    };

    quote! {
        /// AW-V.W4-fix — visitor-path Wrap-shape parse function.
        ///
        /// Transparent dispatcher — skip leading ws, byte-dispatch to
        /// the chosen branch's visitor-path shape fn. No visitor event
        /// fires here; the chosen branch's visitor fn owns the event
        /// emission.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            #dispatch
        }
    }
}

/// Emit the visitor-path Alt-dispatch body.
fn emit_alt_visitor_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    dispatcher_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;

    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let mut ref_arms: Vec<TokenStream> = Vec::new();
    for branch in branches {
        let inner = unwrap_outer(&branch.node);
        let IrNode::Ref(rid) = inner else { continue };
        let Some(target) = ir.rules.iter().find(|r| r.id == *rid) else {
            continue;
        };
        let tag = ir.shape_assignments.get(*rid);
        let shape_name = shape_tag_name(tag);
        let Some(shape_name) = shape_name else { continue };
        let target_fn =
            visitor_shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
        let first_bytes: Vec<u8> =
            target.meta.first_set.iter().collect();
        if first_bytes.is_empty() || first_bytes.len() > 16 {
            continue;
        }
        let byte_pats: Vec<TokenStream> =
            first_bytes.iter().map(|b| quote! { #b }).collect();
        // AX.W0a.2.g — visitor-path Keyword signature extended with
        // `state` (see tape-path call).
        let call = match tag {
            ShapeTag::Number => quote! {
                #target_fn(input, p, first, visitor)
            },
            ShapeTag::Keyword => quote! {
                #target_fn(input, p, first, state, visitor)
            },
            ShapeTag::String => quote! {
                #target_fn(input, p, state, visitor, /*is_key=*/ false)
            },
            _ => quote! {
                #target_fn(input, p, state, visitor)
            },
        };
        ref_arms.push(quote! {
            #(#byte_pats)|* => #call,
        });
    }

    if ref_arms.is_empty() {
        return quote! {
            let _ = #support_mod::skip_space(input, p, state);
            #dispatcher_ident(input, p, state, visitor)
        };
    }

    let fallback = quote! {
        _ => #dispatcher_ident(input, p, state, visitor),
    };

    quote! {
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(crate::runtime::ParseErr::Syntax {
                offset: *p as u32, rule: None,
            })?;
        match first {
            #(#ref_arms)*
            #fallback
        }
    }
}
