//! Type-only emit: recurse on collapsed TypeDesc. No IR walking.
//!
//! The type IS the value. Every collapse is invisible because we emit
//! exactly what the type says.

use bbnf_ir::{GrammarIR, IrNode, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Index;

use crate::generate::ir_types::IrCodegenCtx;

/// Emit code for a value of the given TypeDesc.
pub fn emit_type(
    ty: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match ty {
        TypeDesc::Span => quote! { __sink.text(#val.as_str()); },
        TypeDesc::F64  => quote! { __sink.f64(*#val); },
        TypeDesc::U32  => quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{}", #val); __sink.text(&__b); }
        },

        TypeDesc::Option(inner) => {
            let inner_emit = emit_type(inner, &quote! { __opt_v }, ir, ctx);
            quote! { if let Some(__opt_v) = #val { #inner_emit } }
        }

        TypeDesc::Vec(inner) => {
            let item_emit = emit_type(inner, &quote! { __item }, ir, ctx);
            quote! { for __item in #val.iter() { #item_emit } }
        }

        TypeDesc::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
                let idx = Index::from(i);
                let child_val = quote! { #val.#idx };
                emit_type(elem_ty, &child_val, ir, ctx)
            }).collect();
            quote! { #(#parts)* }
        }

        TypeDesc::Enum | TypeDesc::BoxedEnum => {
            emit_variant_dispatch(val, ir, ctx)
        }

        TypeDesc::Named(_) => quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{}", #val); __sink.text(&__b); }
        },
    }
}

/// Emit a leaf type — same as emit_type but Enum/BoxedEnum call the
/// generated dispatch function instead of recursing into emit_variant_dispatch.
/// This prevents infinite recursion during codegen.
fn emit_leaf_type(
    ty: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match ty {
        TypeDesc::Enum | TypeDesc::BoxedEnum => {
            // For Enum/BoxedEnum inside sub-variants: call the entry rule's emit.
            // This delegates to the GENERATED function, not the codegen function.
            let entry_rule = &ir.rules[ir.entry as usize];
            let entry_name = ir.get_string(entry_rule.name);
            let emit_fn = format_ident!("{}_emit", entry_name);
            quote! { Self::#emit_fn(#val, __sink); }
        }
        TypeDesc::Tuple(elems) => {
            let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
                let idx = Index::from(i);
                let child_val = quote! { #val.#idx };
                emit_leaf_type(elem_ty, &child_val, ir, ctx)
            }).collect();
            quote! { #(#parts)* }
        }
        TypeDesc::Vec(inner) => {
            let item_emit = emit_leaf_type(inner, &quote! { __item }, ir, ctx);
            quote! { for __item in #val.iter() { #item_emit } }
        }
        TypeDesc::Option(inner) => {
            let inner_emit = emit_leaf_type(inner, &quote! { __opt_v }, ir, ctx);
            quote! { if let Some(__opt_v) = #val { #inner_emit } }
        }
        _ => emit_type(ty, val, ir, ctx),
    }
}

/// Emit enum variant dispatch: match on all known variants, call rule_emit.
fn emit_variant_dispatch(
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    // One arm per non-transparent rule.
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let name = ir.get_string(rule.name);
        let variant = format_ident!("{}", name);
        let emit_fn = format_ident!("{}_emit", name);
        arms.push(quote! {
            #enum_ident::#variant(__inner) => {
                Self::#emit_fn(__inner, __sink);
            }
        });
    }

    // Sub-variants (heterogeneous Alt branches).
    // These are non-recursive leaf types (Span, Tuple of Spans, etc.).
    // Emit their type directly — no recursion through rule_emit.
    for (ty_desc, variant_name) in &ctx.global_sub_variants {
        // Skip if already covered by a rule variant above.
        let already_covered = ir.rules.iter().any(|r| {
            !r.meta.is_transparent && ir.get_string(r.name) == variant_name.as_str()
        });
        if already_covered {
            continue;
        }
        // Skip Enum/BoxedEnum — these are covered by rule arms.
        if matches!(ty_desc, TypeDesc::Enum | TypeDesc::BoxedEnum) {
            continue;
        }
        let variant = format_ident!("{}", variant_name);
        // For sub-variants, emit the LEAF type (no recursion into Enum).
        let inner_emit = emit_leaf_type(ty_desc, &quote! { __inner }, ir, ctx);
        arms.push(quote! {
            #enum_ident::#variant(__inner) => { #inner_emit }
        });
    }

    if arms.is_empty() {
        quote! { __sink.text(#val.as_str()); } // fallback: treat as Span
    } else {
        quote! { match #val { #(#arms)* _ => {} } }
    }
}
