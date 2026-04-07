//! Concrete-type-driven emit: recurse on syn::Type.
//!
//! The concrete Rust type IS the specification. No abstraction gap.
//! Convert TypeDesc → syn::Type via type_desc_to_syn, then pattern-match
//! on the syn::Type structure to generate emit code.

use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{self, Type, parse_quote};

use crate::generate::ir_types::{IrCodegenCtx, type_desc_to_syn};

/// Emit code for a value whose TypeDesc is known.
/// Converts to syn::Type and dispatches on the concrete Rust type.
pub fn emit_for_type(
    td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let syn_ty = type_desc_to_syn(td, ctx);
    emit_for_syn_type(&syn_ty, val, ir, ctx)
}

/// Emit code driven by a concrete syn::Type.
fn emit_for_syn_type(
    ty: &Type,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Span<'a> → emit text
    if is_span_type(ty) {
        return quote! { __sink.text(#val.as_str()); };
    }

    // Primitive: f64, u32, u8, bool
    if is_type_name(ty, "f64") {
        return quote! { __sink.f64(*#val); };
    }
    if is_type_name(ty, "u32") || is_type_name(ty, "u8") || is_type_name(ty, "i64") {
        return quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{}", #val); __sink.text(&__b); }
        };
    }
    if is_type_name(ty, "bool") {
        return quote! {
            if *#val { __sink.text("true"); } else { __sink.text("false"); }
        };
    }

    // Option<T> → if let Some
    if let Some(inner) = extract_option_inner(ty) {
        let inner_emit = emit_for_syn_type(&inner, &quote! { __opt_v }, ir, ctx);
        return quote! { if let Some(__opt_v) = #val { #inner_emit } };
    }

    // &'a [T] (slice) → iterate
    if let Some(inner) = extract_slice_inner(ty) {
        let item_emit = emit_for_syn_type(&inner, &quote! { __item }, ir, ctx);
        return quote! { for __item in #val.iter() { #item_emit } };
    }

    // (T1, T2, ...) → index
    if let Some(elems) = extract_tuple_elems(ty) {
        let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
            let idx = syn::Index::from(i);
            let child_val = quote! { #val.#idx };
            emit_for_syn_type(elem_ty, &child_val, ir, ctx)
        }).collect();
        return quote! { #(#parts)* };
    }

    // &'a Enum<'a> (reference to enum) → deref and dispatch
    if let Some(inner) = extract_reference_inner(ty) {
        if is_enum_type(&inner, ctx) {
            return emit_variant_dispatch(val, ir, ctx);
        }
        // Reference to something else: deref and recurse
        return emit_for_syn_type(&inner, val, ir, ctx);
    }

    // Enum<'a> (direct enum) → dispatch
    if is_enum_type(ty, ctx) {
        return emit_variant_dispatch(val, ir, ctx);
    }

    // Fallback: Display
    quote! {
        { use ::std::fmt::Write as _; let mut __b = String::new();
          let _ = write!(__b, "{}", #val); __sink.text(&__b); }
    }
}

/// Emit for a leaf sub-variant type. Same as emit_for_syn_type except
/// Enum/BoxedEnum types call the entry rule's generated emit function
/// instead of recursing through emit_variant_dispatch (prevents infinite
/// recursion during codegen).
fn emit_leaf_syn_type(
    ty: &Type,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // &Enum or Enum → call the entry rule's GENERATED emit function.
    // This delegates to runtime dispatch (not codegen recursion).
    if let Some(inner) = extract_reference_inner(ty) {
        if is_enum_type(&inner, ctx) {
            let entry_rule = &ir.rules[ir.entry as usize];
            let entry_name = ir.get_string(entry_rule.name);
            let emit_fn = format_ident!("{}_emit", entry_name);
            return quote! { Self::#emit_fn(#val, __sink); };
        }
        return emit_leaf_syn_type(&inner, val, ir, ctx);
    }
    if is_enum_type(ty, ctx) {
        let entry_rule = &ir.rules[ir.entry as usize];
        let entry_name = ir.get_string(entry_rule.name);
        let emit_fn = format_ident!("{}_emit", entry_name);
        return quote! { Self::#emit_fn(&#val, __sink); };
    }
    // For Tuples/Options/Slices inside sub-variants: recurse with leaf guard.
    if let Some(elems) = extract_tuple_elems(ty) {
        let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
            let idx = syn::Index::from(i);
            emit_leaf_syn_type(elem_ty, &quote! { #val.#idx }, ir, ctx)
        }).collect();
        return quote! { #(#parts)* };
    }
    if let Some(inner) = extract_option_inner(ty) {
        let inner_emit = emit_leaf_syn_type(&inner, &quote! { __opt_v }, ir, ctx);
        return quote! { if let Some(__opt_v) = #val { #inner_emit } };
    }
    if let Some(inner) = extract_slice_inner(ty) {
        let item_emit = emit_leaf_syn_type(&inner, &quote! { __item }, ir, ctx);
        return quote! { for __item in #val.iter() { #item_emit } };
    }
    // Leaf primitives: delegate to main emit.
    emit_for_syn_type(ty, val, ir, ctx)
}

/// Emit variant dispatch: match on all known enum variants.
fn emit_variant_dispatch(
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    // One arm per non-transparent rule → call its _emit function.
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let name = ir.get_string(rule.name);
        let variant = format_ident!("{}", name);
        let emit_fn = format_ident!("{}_emit", name);
        // __inner from match is already a reference. If the rule's emit fn
        // takes the type directly (BoxedEnum), pass __inner. Otherwise &__inner.
        let rule_td = ir.types.iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td);
        let is_boxed = matches!(rule_td, Some(TypeDesc::BoxedEnum));
        let call = if is_boxed {
            quote! { Self::#emit_fn(__inner, __sink); }
        } else {
            quote! { Self::#emit_fn(&__inner, __sink); }
        };
        arms.push(quote! {
            #enum_ident::#variant(__inner) => { #call }
        });
    }

    // Sub-variant arms → emit inline based on type.
    for (ty_desc, variant_name) in &ctx.global_sub_variants {
        if matches!(ty_desc, TypeDesc::Enum | TypeDesc::BoxedEnum) {
            continue;
        }
        let already_covered = ir.rules.iter().any(|r| {
            !r.meta.is_transparent && ir.get_string(r.name) == variant_name.as_str()
        });
        if already_covered {
            continue;
        }
        let variant = format_ident!("{}", variant_name);
        let inner_ty = type_desc_to_syn(ty_desc, ctx);
        let inner_emit = emit_leaf_syn_type(&inner_ty, &quote! { __inner }, ir, ctx);
        arms.push(quote! {
            #enum_ident::#variant(__inner) => { #inner_emit }
        });
    }

    if arms.is_empty() {
        quote! {}
    } else {
        quote! { match #val { #(#arms)* _ => {} } }
    }
}

// ── syn::Type classification helpers ─────────────────────────────────────────

fn is_span_type(ty: &Type) -> bool {
    type_ends_with(ty, "Span")
}

fn is_enum_type(ty: &Type, ctx: &IrCodegenCtx) -> bool {
    let enum_name = ctx.enum_ident.to_string();
    type_ends_with(ty, &enum_name)
}

fn is_type_name(ty: &Type, name: &str) -> bool {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            return seg.ident == name;
        }
    }
    false
}

fn type_ends_with(ty: &Type, suffix: &str) -> bool {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            return seg.ident == suffix;
        }
    }
    false
}

/// Extract T from Option<T>.
fn extract_option_inner(ty: &Type) -> Option<Type> {
    if let Type::Path(tp) = ty {
        if let Some(seg) = tp.path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                    if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                        return Some(inner.clone());
                    }
                }
            }
        }
    }
    None
}

/// Extract T from &'a [T].
fn extract_slice_inner(ty: &Type) -> Option<Type> {
    if let Type::Reference(r) = ty {
        if let Type::Slice(s) = r.elem.as_ref() {
            return Some(*s.elem.clone());
        }
    }
    None
}

/// Extract elements from (T1, T2, ...).
fn extract_tuple_elems(ty: &Type) -> Option<Vec<Type>> {
    if let Type::Tuple(t) = ty {
        if t.elems.len() >= 2 {
            return Some(t.elems.iter().cloned().collect());
        }
    }
    None
}

/// Extract T from &T or &'a T.
fn extract_reference_inner(ty: &Type) -> Option<Type> {
    if let Type::Reference(r) = ty {
        // Don't extract if it's a slice (&[T]) — handled separately.
        if matches!(r.elem.as_ref(), Type::Slice(_)) {
            return None;
        }
        return Some(*r.elem.clone());
    }
    None
}
