//! Concrete-type-driven emit: recurse on syn::Type.
//!
//! The concrete Rust type IS the specification. No abstraction gap.
//! Convert TypeDesc → syn::Type via type_desc_to_syn, then pattern-match
//! on the syn::Type structure to generate emit code.

use bbnf_ir::{FnDescriptor, GrammarIR, IrNode, MapExpr, TypeDesc};
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

/// Emit code for a value whose TypeDesc is known (no IR context).
pub fn emit_for_type(
    td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    let syn_ty = type_desc_to_syn(td, ctx);
    emit_for_syn_type(&syn_ty, val, ir, ctx)
}

/// Emit code for a rule body: type-driven destructuring + IR structural content.
pub fn emit_for_node(
    node: &IrNode,
    td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    // Map reversal: if the node is a Map, emit the original literal (for constants)
    // or use the FnDescriptor to reverse the transformation.
    if let IrNode::Map { inner, fn_id } = node {
        return emit_map_reverse(inner, *fn_id, td, val, ir, ctx);
    }

    // Skip/Next: emit structural content + value.
    if let IrNode::Skip(kept, structural) = node {
        let kept_emit = emit_for_node(kept, td, val, ir, ctx);
        let structural_emit = emit_structural(structural, ir);
        return quote! { #kept_emit #structural_emit };
    }
    if let IrNode::Next(structural, kept) = node {
        let structural_emit = emit_structural(structural, ir);
        let kept_emit = emit_for_node(kept, td, val, ir, ctx);
        return quote! { #structural_emit #kept_emit };
    }

    // OptionalWhitespace: delegate to inner.
    if let IrNode::OptionalWhitespace(inner) = node {
        return emit_for_node(inner, td, val, ir, ctx);
    }

    // Seq: type says Tuple, IR says children. Use both.
    if let (TypeDesc::Tuple(elems), IrNode::Seq(children)) = (td, node) {
        return emit_tuple_with_ir(children, elems, val, ir, ctx);
    }

    // Repeat: type says Vec or Option, IR might have sep_by.
    if let IrNode::Repeat { inner, lo, hi } = node {
        if *lo == 0 && *hi == 1 {
            if let TypeDesc::Option(inner_td) = td {
                let inner_emit = emit_for_node(inner, inner_td, &quote! { __opt_v }, ir, ctx);
                return quote! { if let Some(__opt_v) = #val { #inner_emit } };
            }
        }
        // Vec with sep_by detection.
        if let TypeDesc::Vec(elem_td) = td {
            return emit_vec_with_ir(inner, elem_td, val, ir, ctx);
        }
    }

    // Alt with constant reverse.
    if let IrNode::Alt(branches, _) = node {
        if let Some(cr) = try_constant_reverse_from_ir(branches, val, ir) {
            return cr;
        }
    }

    // Ref: delegate to rule's emit fn.
    if let IrNode::Ref(rule_id) = node {
        let ref_rule = &ir.rules[*rule_id as usize];
        if ref_rule.meta.is_transparent {
            return emit_for_node(&ref_rule.body, td, val, ir, ctx);
        }
    }

    // Fallback: type-only emit.
    emit_for_type(td, val, ir, ctx)
}

/// Emit map reversal (constant literals, number→text, etc.)
fn emit_map_reverse(
    inner: &IrNode,
    fn_id: u32,
    td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    match &ir.fns[fn_id as usize] {
        FnDescriptor::NumberConvert => quote! { __sink.f64(*#val); },
        FnDescriptor::HexConvert { .. } => quote! {
            { use ::std::fmt::Write as _; let mut __b = String::new();
              let _ = write!(__b, "{:x}", #val); __sink.text_owned(&__b); }
        },
        FnDescriptor::SpanCapture => quote! { __sink.text(#val.as_str()); },
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap => {
            emit_for_node(inner, td, val, ir, ctx)
        }
        FnDescriptor::Expr { expr, .. } => {
            match expr {
                MapExpr::IntLit(_) | MapExpr::FloatLit(_) | MapExpr::StringLit(_)
                | MapExpr::BoolLit(_) => {
                    // Constant: emit the original literal from the inner IR node.
                    emit_structural(inner, ir)
                }
                MapExpr::Input => emit_for_node(inner, td, val, ir, ctx),
                _ => quote! {
                    { use ::std::fmt::Write as _; let mut __b = String::new();
                      let _ = write!(__b, "{}", #val); __sink.text_owned(&__b); }
                },
            }
        }
    }
}

/// Emit a Tuple using IR Seq children for structural content.
fn emit_tuple_with_ir(
    children: &[IrNode],
    elems: &[TypeDesc],
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    if elems.len() != children.len() {
        // Mismatch: fall back to type-only Tuple emit.
        let parts: Vec<_> = elems.iter().enumerate().map(|(i, elem_ty)| {
            let idx = syn::Index::from(i);
            let binding = format_ident!("__t{}", i);
            let syn_ty = type_desc_to_syn(elem_ty, ctx);
            let child_emit = emit_for_syn_type(&syn_ty, &quote! { #binding }, ir, ctx);
            quote! { let #binding = #val.#idx; #child_emit }
        }).collect();
        return quote! { #(#parts)* };
    }

    let parts: Vec<_> = children.iter().zip(elems.iter()).enumerate().map(|(i, (child, elem_td))| {
        let idx = syn::Index::from(i);
        let binding = format_ident!("__t{}", i);
        let child_emit = emit_for_node(child, elem_td, &quote! { #binding }, ir, ctx);
        quote! { let #binding = #val.#idx; #child_emit }
    }).collect();
    quote! { #(#parts)* }
}

/// Emit Vec iteration with IR-derived separator.
fn emit_vec_with_ir(
    inner: &IrNode,
    elem_td: &TypeDesc,
    val: &TokenStream,
    ir: &GrammarIR,
    ctx: &IrCodegenCtx,
) -> TokenStream {
    use crate::backend::patterns::decisions;

    // Detect sep_by pattern.
    if let Some((elem_node, sep_node)) = decisions::detect_sep_by(inner) {
        let sep_emit = emit_structural(sep_node, ir);
        let ref_elem_td: TypeDesc = TypeDesc::BoxedEnum; // placeholder — item is &Enum from iter
        let syn_ty = type_desc_to_syn(elem_td, ctx);
        let ref_syn_ty: Type = syn::parse_quote! { &#syn_ty };
        let item_emit = emit_for_syn_type(&ref_syn_ty, &quote! { __item }, ir, ctx);
        return quote! {
            let mut __first = true;
            for __item in #val.iter() {
                if !__first { #sep_emit }
                __first = false;
                #item_emit
            }
        };
    }

    // Plain iteration.
    let syn_ty = type_desc_to_syn(elem_td, ctx);
    let ref_syn_ty: Type = syn::parse_quote! { &#syn_ty };
    let item_emit = emit_for_syn_type(&ref_syn_ty, &quote! { __item }, ir, ctx);
    quote! { for __item in #val.iter() { #item_emit } }
}

/// Try to emit constant-reverse from IR Alt branches.
fn try_constant_reverse_from_ir(
    branches: &[bbnf_ir::AltBranch],
    val: &TokenStream,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    let mut arms = Vec::new();
    for branch in branches {
        let IrNode::Map { inner, fn_id } = &branch.node else { return None };
        let IrNode::Literal(sid) = inner.as_ref() else { return None };
        let FnDescriptor::Expr { expr, .. } = &ir.fns[*fn_id as usize] else { return None };
        let lit = ir.get_string(*sid);
        let pat = match expr {
            MapExpr::BoolLit(true) => quote! { true },
            MapExpr::BoolLit(false) => quote! { false },
            MapExpr::IntLit(n) => { let l = proc_macro2::Literal::i64_unsuffixed(*n); quote! { #l } }
            MapExpr::FloatLit(f) => { let l = proc_macro2::Literal::f64_unsuffixed(*f); quote! { #l } }
            _ => return None,
        };
        arms.push(quote! { #pat => { __sink.text(#lit); } });
    }
    Some(quote! { match *#val { #(#arms)* _ => {} } })
}

/// Emit structural content from an IR node.
fn emit_structural(node: &IrNode, ir: &GrammarIR) -> TokenStream {
    match node {
        IrNode::Literal(sid) => {
            let s = ir.get_string(*sid);
            if s.len() == 1 {
                let b = s.as_bytes()[0];
                quote! { __sink.char(#b); }
            } else {
                quote! { __sink.text(#s); }
            }
        }
        IrNode::Ref(rule_id) => emit_structural(&ir.rules[*rule_id as usize].body, ir),
        IrNode::Seq(children) => {
            let parts: Vec<_> = children.iter().map(|c| emit_structural(c, ir)).collect();
            quote! { #(#parts)* }
        }
        IrNode::OptionalWhitespace(inner) => emit_structural(inner, ir),
        IrNode::Skip(l, _) | IrNode::Next(_, l) => emit_structural(l, ir),
        IrNode::Repeat { inner, .. } => emit_structural(inner, ir),
        _ => quote! {},
    }
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
              let _ = write!(__b, "{}", #val); __sink.text_owned(&__b); }
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
            let binding = format_ident!("__t{}", i);
            let child_emit = emit_for_syn_type(elem_ty, &quote! { #binding }, ir, ctx);
            quote! { let #binding = #val.#idx; #child_emit }
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
          let _ = write!(__b, "{}", #val); __sink.text_owned(&__b); }
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
            let binding = format_ident!("__t{}", i);
            let child_emit = emit_leaf_syn_type(elem_ty, &quote! { #binding }, ir, ctx);
            quote! { let #binding = #val.#idx; #child_emit }
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

        // Explicitly destructure through &: match &Enum → &Enum::V(inner).
        // This avoids edition 2024 match ergonomics issues.
        // inner: FieldType (by-value access to the field inside &Enum).
        let call = if needs_deref {
            // Ref-type field: inner is &'a T already. Pass directly.
            quote! { Self::#emit_fn(__inner, __sink); }
        } else {
            // Value-type field: inner is T. Pass &inner.
            quote! { Self::#emit_fn(&__inner, __sink); }
        };

        arms.push(quote! {
            &#enum_ident::#variant(ref __inner) => { #call }
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
            &#enum_ident::#variant(ref __inner) => { #inner_emit }
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
