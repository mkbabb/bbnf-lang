//! Trivial EmitPlan → TokenStream walk.
//!
//! Every decision is pre-resolved in the plan. This module does zero TypeMap
//! queries and zero type inference. It's pure structure → syntax conversion.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Index;

use crate::backend::decisions::{ConstantKind, ConstantReverseArm, MapReverse};
use crate::generate::ir_types::IrCodegenCtx;

use super::plan::*;

pub fn emit_from_plan(plan: &EmitPlan, val: &TokenStream, ctx: &IrCodegenCtx) -> TokenStream {
    match plan {
        EmitPlan::Structural(frags) => emit_frags(frags),

        EmitPlan::Leaf(leaf) => match leaf {
            Leaf::SpanText => quote! { __sink.text(#val.as_str()); },
            Leaf::F64 => quote! { __sink.f64(*#val); },
            Leaf::Hex => quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{:x}", #val); __sink.text(&__b); }
            },
            Leaf::Display => quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{}", #val); __sink.text(&__b); }
            },
        },

        EmitPlan::Seq(seq) => {
            let parts: Vec<_> = seq.children.iter().map(|child| match child {
                SeqChild::Structural(frags) => emit_frags(frags),
                SeqChild::TupleSpan { index } => {
                    let idx = Index::from(*index);
                    quote! { __sink.text(#val.#idx.as_str()); }
                }
                SeqChild::TupleValue { index, plan } => {
                    let idx = Index::from(*index);
                    let child_val = quote! { #val.#idx };
                    emit_from_plan(plan, &child_val, ctx)
                }
                SeqChild::Direct { plan } => emit_from_plan(plan, val, ctx),
            }).collect();
            quote! { #(#parts)* }
        }

        EmitPlan::Alt(alt) => match alt {
            AltPlan::ConstantReverse(arms) => {
                let match_arms: Vec<_> = arms.iter().map(|arm| {
                    let lit = &arm.literal;
                    let pat = match &arm.expr {
                        ConstantKind::Bool(true) => quote! { true },
                        ConstantKind::Bool(false) => quote! { false },
                        ConstantKind::Int(n) => {
                            let l = proc_macro2::Literal::i64_unsuffixed(*n);
                            quote! { #l }
                        }
                        ConstantKind::Float(f) => {
                            let l = proc_macro2::Literal::f64_unsuffixed(*f);
                            quote! { #l }
                        }
                    };
                    quote! { #pat => { __sink.text(#lit); } }
                }).collect();
                quote! { match *#val { #(#match_arms)* _ => {} } }
            }
            AltPlan::Dispatch { branches } => {
                let enum_ident = &ctx.enum_ident;
                let arms: Vec<_> = branches.iter().map(|branch| {
                    let variant = format_ident!("{}", branch.variant_name);
                    let inner = quote! { __inner };
                    let body = emit_from_plan(&branch.plan, &inner, ctx);
                    quote! { #enum_ident::#variant(#inner) => { #body } }
                }).collect();
                quote! { match #val { #(#arms)* _ => {} } }
            }
        },

        EmitPlan::Repeat(rep) => match rep {
            RepeatPlan::Optional { inner } => {
                let inner_val = quote! { __opt_v };
                let inner_emit = emit_from_plan(inner, &inner_val, ctx);
                quote! { if let Some(#inner_val) = #val { #inner_emit } }
            }
            RepeatPlan::SepBy { element, separator } => {
                let sep = emit_frags(separator);
                let item_val = quote! { __item };
                let elem_emit = emit_from_plan(element, &item_val, ctx);
                quote! {
                    let mut __first = true;
                    for #item_val in #val.iter() {
                        if !__first { #sep }
                        __first = false;
                        #elem_emit
                    }
                }
            }
            RepeatPlan::Plain { element } => {
                let item_val = quote! { __item };
                let elem_emit = emit_from_plan(element, &item_val, ctx);
                quote! { for #item_val in #val.iter() { #elem_emit } }
            }
        },

        EmitPlan::Ref(r) => match &r.strategy {
            RefStrategy::Call { rule_type } => {
                let emit_fn = format_ident!("{}_emit", r.rule_name);
                quote! {
                    {
                        let __ref: &#rule_type = &#val;
                        Self::#emit_fn(__ref, __sink);
                    }
                }
            }
            RefStrategy::Inline { body } => emit_from_plan(body, val, ctx),
            RefStrategy::VecUnwrap { variant_name, rule_type } => {
                let variant = format_ident!("{}", variant_name);
                let enum_ident = &ctx.enum_ident;
                let emit_fn = format_ident!("{}_emit", variant_name);
                quote! {
                    if let #enum_ident::#variant(__inner) = #val {
                        let __ref: &#rule_type = __inner;
                        Self::#emit_fn(__ref, __sink);
                    }
                }
            }
        },

        EmitPlan::Map(m) => match &m.strategy {
            MapReverse::F64 => quote! { __sink.f64(*#val); },
            MapReverse::Hex => quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{:x}", #val); __sink.text(&__b); }
            },
            MapReverse::SpanText => quote! { __sink.text(#val.as_str()); },
            MapReverse::Passthrough => emit_from_plan(&m.inner, val, ctx),
            MapReverse::Constant => emit_from_plan(&m.inner, val, ctx),
            MapReverse::Display => quote! {
                { use ::std::fmt::Write as _; let mut __b = String::new();
                  let _ = write!(__b, "{}", #val); __sink.text(&__b); }
            },
        },

        EmitPlan::FlatVec(fv) => {
            let sep = emit_frags(&fv.separator);
            let item_val = quote! { __item };
            let item_emit = emit_from_plan(&fv.item, &item_val, ctx);
            quote! {
                let mut __first = true;
                for #item_val in #val.iter() {
                    if !__first { #sep }
                    __first = false;
                    #item_emit
                }
            }
        }
    }
}

fn emit_frags(frags: &[Frag]) -> TokenStream {
    let parts: Vec<_> = frags.iter().map(|f| match f {
        Frag::Char(b) => quote! { __sink.char(#b); },
        Frag::Text(s) => quote! { __sink.text(#s); },
    }).collect();
    quote! { #(#parts)* }
}
