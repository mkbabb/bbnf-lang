//! Concrete-type-driven emit: recurse on syn::Type.
//!
//! The concrete Rust type IS the specification. No abstraction gap.
//! Convert TypeDesc → syn::Type via type_desc_to_syn, then pattern-match
//! on the syn::Type structure to generate emit code.

use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{self, Type};

use crate::generate::ir_types::{IrCodegenCtx, type_desc_to_syn};

// ─── Core predicate ──────────────────────────────────────────────────────────

/// Is this TypeDesc already a reference type after codegen conversion?
/// BoxedEnum → &'a Enum<'a>, Vec(T) → &'a [T]. Both are references.
pub fn type_desc_is_ref(td: &TypeDesc) -> bool {
    matches!(td, TypeDesc::BoxedEnum | TypeDesc::Vec(_))
}

// ─── Public entry ────────────────────────────────────────────────────────────

/// Emit code for a value whose TypeDesc is known.
pub fn emit_for_type(
    td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let syn_ty = type_desc_to_syn(td, ctx);
    emit_for_syn_type(&syn_ty, val, ir, ctx)
}

// ─── Recursive syn::Type dispatch ────────────────────────────────────────────

fn emit_for_syn_type(
    ty: &Type,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    if is_span_type(ty) {
        return quote! { __sink.text(#val.as_str()); };
    }
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
    if let Some(inner) = extract_option_inner(ty) {
        let inner_emit = emit_for_syn_type(&inner, &quote! { __opt_v }, ir, ctx);
        return quote! { if let Some(__opt_v) = #val { #inner_emit } };
    }
    if let Some(inner) = extract_slice_inner(ty) {
        // .iter() yields &ElemType. If ElemType is Enum, __item is &Enum.
        // Wrap inner type in a reference so dispatch doesn't add another &.
        let ref_inner: Type = syn::parse_quote! { &#inner };
        let item_emit = emit_for_syn_type(&ref_inner, &quote! { __item }, ir, ctx);
        return quote! { for __item in #val.iter() { #item_emit } };
    }
    if let Some(elems) = extract_tuple_elems(ty) {
        let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
            let idx = syn::Index::from(i);
            let child_val = quote! { #val.#idx };
            emit_for_syn_type(elem_ty, &child_val, ir, ctx)
        }).collect();
        return quote! { #(#parts)* };
    }
    if let Some(inner) = extract_reference_inner(ty) {
        if is_enum_type(&inner, ctx) {
            return emit_dispatch_call(val, ctx);
        }
        return emit_for_syn_type(&inner, val, ir, ctx);
    }
    if is_enum_type(ty, ctx) {
        return emit_dispatch_call(&quote! { &#val }, ctx);
    }

    // Fallback: Display
    quote! {
        { use ::std::fmt::Write as _; let mut __b = String::new();
          let _ = write!(__b, "{}", #val); __sink.text(&__b); }
    }
}

/// Call the generated __dispatch_emit function.
fn emit_dispatch_call(val: &TokenStream, ctx: &IrCodegenCtx) -> TokenStream {
    quote! { Self::__dispatch_emit(#val, __sink); }
}

// ─── Leaf sub-variant emit (guards against codegen recursion) ────────────────

/// Same as emit_for_syn_type but Enum/&Enum calls __dispatch_emit
/// instead of inlining the match (prevents infinite codegen recursion).
fn emit_leaf_syn_type(
    ty: &Type,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    if let Some(inner) = extract_reference_inner(ty) {
        if is_enum_type(&inner, ctx) {
            return emit_dispatch_call(val, ctx);
        }
        return emit_leaf_syn_type(&inner, val, ir, ctx);
    }
    if is_enum_type(ty, ctx) {
        return emit_dispatch_call(&quote! { &#val }, ctx);
    }
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
        let ref_inner: Type = syn::parse_quote! { &#inner };
        let item_emit = emit_leaf_syn_type(&ref_inner, &quote! { __item }, ir, ctx);
        return quote! { for __item in #val.iter() { #item_emit } };
    }
    emit_for_syn_type(ty, val, ir, ctx)
}

// ─── Dispatch match body (used by __dispatch_emit and inline) ────────────────

/// Generate the match arms for variant dispatch.
/// `val` is `&'a Enum<'a>` (single reference to the enum).
pub fn generate_dispatch_arms(
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> Vec<TokenStream> {
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    // Rule arms: match ergonomics gives __inner: &FieldType.
    // Ref-type rules take FieldType directly (it's already a ref) → pass *__inner.
    // Value-type rules take &FieldType → pass __inner directly.
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        let name = ir.get_string(rule.name);
        let variant = format_ident!("{}", name);
        let emit_fn = format_ident!("{}_emit", name);

        let rule_td = ir.types.iter()
            .find(|(id, _)| *id == rule.id)
            .map(|(_, td)| td);
        let needs_deref = rule_td.map_or(false, |td| type_desc_is_ref(td));

        let call = if needs_deref {
            quote! { Self::#emit_fn(*__inner, __sink); }
        } else {
            quote! { Self::#emit_fn(__inner, __sink); }
        };

        arms.push(quote! {
            #enum_ident::#variant(__inner) => { #call }
        });
    }

    // Sub-variant arms: inline emit based on type.
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

    arms
}

// ─── syn::Type classification helpers ────────────────────────────────────────

fn is_span_type(ty: &Type) -> bool { type_ends_with(ty, "Span") }

fn is_enum_type(ty: &Type, ctx: &IrCodegenCtx) -> bool {
    type_ends_with(ty, &ctx.enum_ident.to_string())
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

fn extract_slice_inner(ty: &Type) -> Option<Type> {
    if let Type::Reference(r) = ty {
        if let Type::Slice(s) = r.elem.as_ref() {
            return Some(*s.elem.clone());
        }
    }
    None
}

fn extract_tuple_elems(ty: &Type) -> Option<Vec<Type>> {
    if let Type::Tuple(t) = ty {
        if t.elems.len() >= 2 {
            return Some(t.elems.iter().cloned().collect());
        }
    }
    None
}

fn extract_reference_inner(ty: &Type) -> Option<Type> {
    if let Type::Reference(r) = ty {
        if matches!(r.elem.as_ref(), Type::Slice(_)) {
            return None;
        }
        return Some(*r.elem.clone());
    }
    None
}
