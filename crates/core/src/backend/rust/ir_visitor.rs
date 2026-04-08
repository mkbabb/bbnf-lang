//! Auto-generated visitor infrastructure for parser enums.
//!
//! Generates `children_of()`, `span_text()`, and `BbnfVisitor` trait
//! from IR metadata. The generated code mirrors the enum variant shapes
//! so walkers can override specific methods without structural boilerplate.

use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::ir_types::IrCodegenCtx;

/// Generate visitor infrastructure: `children_of()` + `span_text()` + trait.
pub fn generate_visitor(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let children_fn = generate_children_of(ctx);
    let span_text_fn = generate_span_text(ctx);
    let visitor_trait = generate_visitor_trait(ctx);

    quote! {
        /// Extract child enum references from a node for recursive traversal.
        pub fn children_of<'a>(node: &'a #enum_ident<'a>) -> Vec<&'a #enum_ident<'a>> {
            #children_fn
        }

        /// Extract text from a terminal node by finding its leaf Span.
        pub fn span_text<'a>(node: &'a #enum_ident<'a>) -> &'a str {
            #span_text_fn
        }

        #visitor_trait
    }
}

/// Generate the `children_of()` match body.
fn generate_children_of(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    for rule in &ctx.ir.rules {
        if rule.meta.is_transparent {
            continue; // Transparent rules don't generate enum variants.
        }

        let name = ctx.ir.get_string(rule.name);
        let ident = format_ident!("{}", name);

        // Get the TypeDesc for this rule.
        let type_desc = ctx
            .ir
            .types
            .iter()
            .find(|(rid, _)| *rid == rule.id)
            .map(|(_, td)| td);

        if ctx.fused_number_rules.contains(&rule.id) {
            // Fused number: (Span, f64) → no enum children
            arms.push(quote! { #enum_ident::#ident(_) => Vec::new() });
            continue;
        }

        match type_desc {
            Some(td) if !has_enum_children(td) => {
                // No enum children (Span, f64, etc.)
                arms.push(quote! { #enum_ident::#ident(_) => Vec::new() });
            }
            Some(td) => {
                // Has enum children — generate extraction code.
                let extraction = generate_extraction(td, enum_ident);
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extraction }
                });
            }
            None => {
                // No type info — default to no children.
                arms.push(quote! { #enum_ident::#ident(_) => Vec::new() });
            }
        }
    }

    // Sub-variants: extract children from their TypeDesc.
    let mut seen_names = std::collections::HashSet::new();
    for rule in &ctx.ir.rules {
        for sv in &rule.meta.sub_variants {
            let name = ctx.ir.get_string(sv.variant_name);
            if !seen_names.insert(name.to_string()) {
                continue;
            }
            let ident = format_ident!("{}", name);
            if has_enum_children(&sv.ty) {
                let extraction = generate_extraction(&sv.ty, enum_ident);
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extraction }
                });
            } else {
                arms.push(quote! { #enum_ident::#ident(_) => Vec::new() });
            }
        }
    }

    // Recovered + Phantom
    arms.push(quote! { #enum_ident::__Phantom(_) => Vec::new() });
    if ctx
        .ir
        .rules
        .iter()
        .any(|r| r.meta.directives.recover.is_some())
        && !ctx.parser_attrs.skip_recover
    {
        arms.push(quote! { #enum_ident::Recovered => Vec::new() });
    }

    quote! {
        match node {
            #(#arms),*
        }
    }
}

/// Check if a TypeDesc contains any enum references (BoxedEnum, Enum).
fn has_enum_children(td: &TypeDesc) -> bool {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => true,
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 => false,
        TypeDesc::Option(inner) => has_enum_children(inner),
        TypeDesc::Vec(inner) => has_enum_children(inner),
        TypeDesc::Tuple(elems) => elems.iter().any(has_enum_children),
        TypeDesc::Named(_) => false, // Named types are opaque — not enum refs
    }
}

/// Generate code to extract all enum children from a value of the given TypeDesc.
fn generate_extraction(td: &TypeDesc, enum_ident: &syn::Ident) -> TokenStream {
    let mut collectors = Vec::new();
    extract_from_type(
        td,
        &quote! { value },
        &mut collectors,
        enum_ident,
        0,
    );
    if collectors.is_empty() {
        quote! { Vec::new() }
    } else {
        quote! {
            let mut __children: Vec<&'a #enum_ident<'a>> = Vec::new();
            #(#collectors)*
            __children
        }
    }
}

/// Recursively generate extraction code for enum children at each position.
fn extract_from_type(
    td: &TypeDesc,
    accessor: &TokenStream,
    collectors: &mut Vec<TokenStream>,
    _enum_ident: &syn::Ident,
    depth: usize,
) {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            collectors.push(quote! { __children.push(#accessor); });
        }
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 | TypeDesc::Named(_) => {
            // Leaf — nothing to extract.
        }
        TypeDesc::Option(inner) if has_enum_children(inner) => {
            let inner_var = format_ident!("__opt_{}", depth);
            match inner.as_ref() {
                // Option<Enum> → unwrap and push directly.
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        if let Some(#inner_var) = #accessor {
                            __children.push(#inner_var);
                        }
                    });
                }
                // Option<Tuple/Vec/...> → unwrap, then extract children from inner.
                _ => {
                    let mut inner_collectors = Vec::new();
                    extract_from_type(inner, &quote! { #inner_var }, &mut inner_collectors, _enum_ident, depth + 1);
                    if !inner_collectors.is_empty() {
                        collectors.push(quote! {
                            if let Some(#inner_var) = #accessor {
                                #(#inner_collectors)*
                            }
                        });
                    }
                }
            }
        }
        TypeDesc::Option(_) => {}
        TypeDesc::Vec(inner) if has_enum_children(inner) => {
            let iter_var = format_ident!("__item_{}", depth);
            match inner.as_ref() {
                // Vec<Enum> → iterate and push each item.
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __children.push(#iter_var);
                        }
                    });
                }
                // Vec<Tuple> → iterate, extract enum children from each element.
                TypeDesc::Tuple(inner_elems) => {
                    let mut inner_collectors = Vec::new();
                    for (i, elem) in inner_elems.iter().enumerate() {
                        if has_enum_children(elem) {
                            let idx = syn::Index::from(i);
                            let field_acc = quote! { #iter_var.#idx };
                            extract_from_type(elem, &field_acc, &mut inner_collectors, _enum_ident, depth + 1);
                        }
                    }
                    if !inner_collectors.is_empty() {
                        collectors.push(quote! {
                            for #iter_var in (#accessor).iter() {
                                #(#inner_collectors)*
                            }
                        });
                    }
                }
                _ => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __children.push(#iter_var);
                        }
                    });
                }
            }
        }
        TypeDesc::Vec(_) => {}
        TypeDesc::Tuple(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                if has_enum_children(elem) {
                    let idx = syn::Index::from(i);
                    let child_accessor = quote! { (#accessor).#idx };
                    extract_from_type(elem, &child_accessor, collectors, _enum_ident, depth + 1);
                }
            }
        }
    }
}

/// Generate the `span_text()` match body.
fn generate_span_text(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let mut arms = Vec::new();

    for rule in &ctx.ir.rules {
        if rule.meta.is_transparent {
            continue; // Transparent rules don't generate enum variants.
        }

        let name = ctx.ir.get_string(rule.name);
        let ident = format_ident!("{}", name);

        let type_desc = ctx
            .ir
            .types
            .iter()
            .find(|(rid, _)| *rid == rule.id)
            .map(|(_, td)| td);

        match type_desc {
            Some(TypeDesc::Span) => {
                arms.push(quote! {
                    #enum_ident::#ident(s) => s.as_str()
                });
            }
            // Single-child enum ref (alias): recurse.
            Some(TypeDesc::BoxedEnum | TypeDesc::Enum) => {
                arms.push(quote! {
                    #enum_ident::#ident(inner) => span_text(inner)
                });
            }
            _ => {}
        }
    }

    quote! {
        match node {
            #(#arms,)*
            _ => ""
        }
    }
}

/// Generate a basic visitor trait.
fn generate_visitor_trait(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;

    quote! {
        /// Auto-generated visitor trait for the parser enum.
        ///
        /// Default `visit()` calls `walk()` which extracts children via
        /// `children_of()` and recurses. Override `visit()` to inject behavior
        /// at specific nodes.
        pub trait Visitor<'a> {
            type Output: Default;

            fn combine(&mut self, outputs: Vec<Self::Output>) -> Self::Output {
                let _ = outputs;
                Self::Output::default()
            }

            fn visit(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                self.walk(node)
            }

            fn walk(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                let ch = children_of(node);
                if ch.is_empty() {
                    Self::Output::default()
                } else {
                    let outputs: Vec<_> = ch.into_iter().map(|c| self.visit(c)).collect();
                    self.combine(outputs)
                }
            }
        }
    }
}
