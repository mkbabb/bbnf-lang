//! Rust schema emitter — full parity with the v1 ir_visitor codegen.
//!
//! Takes a `CstSchema` (frontend-owned, target-agnostic) and emits the
//! Rust CST helper code that replaces hand-written walkers across the
//! repo. Generates:
//!
//! - `impl<'a> {Enum}<'a>::children(node) -> Vec<&'a {Enum}<'a>>` — debug helper
//! - `impl<'a> {Enum}<'a>::span_text(node) -> &'a str` — terminal text accessor
//! - `impl<'a> {Enum}<'a>::identifier_text(node) -> &'a str` — identifier extractor
//! - `impl<'a> {Enum}<'a>::walk_children<V>(self, v) -> Vec<V::Output>` —
//!   direct per-variant dispatch (allocation per call, but no intermediate
//!   `Vec<&Enum>` like the legacy `children()` path)
//! - `pub trait {Enum}Visitor<'a>` — namespaced visitor trait
//!
//! Reads `fused_number_rules` from the caller — it's a Rust-backend-specific
//! override (those rules carry `(Span, f64)` instead of plain `Span`, so
//! they have no enum children).

use std::collections::HashSet;

use bbnf_ir::{RuleId, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::model::{CstSchema, FieldRole, VariantCategory, VariantDescriptor};

/// Generate the full Rust CST helper bundle from a `CstSchema`.
///
/// `fused_number_rules` is the Rust-backend-specific set of rules whose
/// payload is `(Span, f64)` instead of plain `Span`. Those variants emit
/// no children regardless of their schema-level type.
pub fn generate(schema: &CstSchema, fused_number_rules: &HashSet<RuleId>) -> TokenStream {
    let enum_ident = format_ident!("{}", schema.enum_name);
    let visitor_ident = format_ident!("{}Visitor", schema.enum_name);

    let children_fn = generate_children_fn(schema, &enum_ident, fused_number_rules);
    let walk_children_fn = generate_walk_children_fn(schema, &enum_ident, fused_number_rules);
    let span_text_fn = generate_span_text_fn(schema, &enum_ident);
    let identifier_text_fn = generate_identifier_text_fn(schema, &enum_ident);
    let visitor_trait = generate_visitor_trait(&enum_ident, &visitor_ident);

    quote! {
        impl<'a> #enum_ident<'a> {
            /// Debug helper: collect references to all enum-typed children.
            ///
            /// Allocates a `Vec`. Walkers should prefer `walk_children`, which
            /// dispatches per variant directly with the visitor in scope.
            pub fn children(node: &'a #enum_ident<'a>) -> ::std::vec::Vec<&'a #enum_ident<'a>> {
                #children_fn
            }

            /// Extract terminal text by recursively unwrapping wrapper variants.
            pub fn span_text(node: &'a #enum_ident<'a>) -> &'a str {
                #span_text_fn
            }

            /// Recursively extract an identifier carrier's text. Returns the
            /// empty string if no identifier is reachable.
            pub fn identifier_text(node: &'a #enum_ident<'a>) -> &'a str {
                #identifier_text_fn
            }

            /// Direct per-variant dispatch: visit each enum-typed child via
            /// the supplied visitor and collect their `Output`s. No intermediate
            /// allocation of a `Vec<&Enum>`.
            pub fn walk_children<__V: #visitor_ident<'a> + ?Sized>(
                node: &'a #enum_ident<'a>,
                v: &mut __V,
            ) -> ::std::vec::Vec<__V::Output> {
                #walk_children_fn
            }
        }

        #visitor_trait
    }
}

// ─── children() — debug helper ───────────────────────────────────────────────

fn generate_children_fn(
    schema: &CstSchema,
    enum_ident: &syn::Ident,
    fused_number_rules: &HashSet<RuleId>,
) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        let ident = format_ident!("{}", variant.name);
        match variant.category {
            VariantCategory::Phantom => {
                arms.push(quote! { #enum_ident::__Phantom(_) => ::std::vec::Vec::new() });
                continue;
            }
            VariantCategory::Recovered => {
                arms.push(quote! { #enum_ident::Recovered => ::std::vec::Vec::new() });
                continue;
            }
            _ => {}
        }

        // Rust override: fused number rules have `(Span, f64)` payload, no children.
        if let Some(rid) = variant.rule_id {
            if fused_number_rules.contains(&rid) {
                arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
                continue;
            }
        }

        let Some(td) = &variant.type_desc else {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        };

        if !type_has_enum_children(td) {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        }

        let extraction = generate_children_extraction(td);
        arms.push(quote! {
            #enum_ident::#ident(value) => { #extraction }
        });
    }

    quote! {
        match node {
            #(#arms),*
        }
    }
}

/// Build the body of a `children()` match arm: collect references into
/// a local `__children: Vec<&Enum>`.
fn generate_children_extraction(td: &TypeDesc) -> TokenStream {
    let mut collectors = Vec::new();
    extract_children_from_type(td, &quote! { value }, &mut collectors, 0);
    if collectors.is_empty() {
        quote! { ::std::vec::Vec::new() }
    } else {
        quote! {
            let mut __children: ::std::vec::Vec<&'a Self> = ::std::vec::Vec::new();
            #(#collectors)*
            __children
        }
    }
}

/// Recursively walk a `TypeDesc` and emit code that pushes enum references
/// to a local `__children` accumulator.
fn extract_children_from_type(
    td: &TypeDesc,
    accessor: &TokenStream,
    collectors: &mut Vec<TokenStream>,
    depth: usize,
) {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            collectors.push(quote! { __children.push(#accessor); });
        }
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 | TypeDesc::Named(_) => {
            // Leaf — no enum content.
        }
        TypeDesc::Option(inner) if type_has_enum_children(inner) => {
            let inner_var = format_ident!("__opt_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        if let Some(#inner_var) = #accessor {
                            __children.push(#inner_var);
                        }
                    });
                }
                _ => {
                    let mut inner_collectors = Vec::new();
                    extract_children_from_type(
                        inner,
                        &quote! { #inner_var },
                        &mut inner_collectors,
                        depth + 1,
                    );
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
        TypeDesc::Vec(inner) if type_has_enum_children(inner) => {
            let iter_var = format_ident!("__item_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __children.push(#iter_var);
                        }
                    });
                }
                TypeDesc::Tuple(inner_elems) => {
                    let mut inner_collectors = Vec::new();
                    for (i, elem) in inner_elems.iter().enumerate() {
                        if type_has_enum_children(elem) {
                            let idx = syn::Index::from(i);
                            let field_acc = quote! { #iter_var.#idx };
                            extract_children_from_type(
                                elem,
                                &field_acc,
                                &mut inner_collectors,
                                depth + 1,
                            );
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
                if type_has_enum_children(elem) {
                    let idx = syn::Index::from(i);
                    let child_accessor = quote! { (#accessor).#idx };
                    extract_children_from_type(elem, &child_accessor, collectors, depth + 1);
                }
            }
        }
    }
}

// ─── walk_children() — direct dispatch ───────────────────────────────────────

fn generate_walk_children_fn(
    schema: &CstSchema,
    enum_ident: &syn::Ident,
    fused_number_rules: &HashSet<RuleId>,
) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        let ident = format_ident!("{}", variant.name);
        match variant.category {
            VariantCategory::Phantom => {
                arms.push(quote! { #enum_ident::__Phantom(_) => ::std::vec::Vec::new() });
                continue;
            }
            VariantCategory::Recovered => {
                arms.push(quote! { #enum_ident::Recovered => ::std::vec::Vec::new() });
                continue;
            }
            _ => {}
        }

        if let Some(rid) = variant.rule_id {
            if fused_number_rules.contains(&rid) {
                arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
                continue;
            }
        }

        let Some(td) = &variant.type_desc else {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        };

        if !type_has_enum_children(td) {
            arms.push(quote! { #enum_ident::#ident(_) => ::std::vec::Vec::new() });
            continue;
        }

        let body = generate_walk_extraction(td);
        arms.push(quote! {
            #enum_ident::#ident(value) => { #body }
        });
    }

    quote! {
        match node {
            #(#arms),*
        }
    }
}

/// Build the body of a `walk_children()` match arm: visit each enum-typed
/// child and collect outputs into `__outputs`.
fn generate_walk_extraction(td: &TypeDesc) -> TokenStream {
    let mut collectors = Vec::new();
    extract_walk_from_type(td, &quote! { value }, &mut collectors, 0);
    if collectors.is_empty() {
        quote! { ::std::vec::Vec::new() }
    } else {
        quote! {
            let mut __outputs: ::std::vec::Vec<__V::Output> = ::std::vec::Vec::new();
            #(#collectors)*
            __outputs
        }
    }
}

fn extract_walk_from_type(
    td: &TypeDesc,
    accessor: &TokenStream,
    collectors: &mut Vec<TokenStream>,
    depth: usize,
) {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            collectors.push(quote! { __outputs.push(v.visit(#accessor)); });
        }
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 | TypeDesc::Named(_) => {}
        TypeDesc::Option(inner) if type_has_enum_children(inner) => {
            let inner_var = format_ident!("__opt_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        if let Some(#inner_var) = #accessor {
                            __outputs.push(v.visit(#inner_var));
                        }
                    });
                }
                _ => {
                    let mut inner_collectors = Vec::new();
                    extract_walk_from_type(
                        inner,
                        &quote! { #inner_var },
                        &mut inner_collectors,
                        depth + 1,
                    );
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
        TypeDesc::Vec(inner) if type_has_enum_children(inner) => {
            let iter_var = format_ident!("__item_{}", depth);
            match inner.as_ref() {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    collectors.push(quote! {
                        for #iter_var in (#accessor).iter() {
                            __outputs.push(v.visit(#iter_var));
                        }
                    });
                }
                TypeDesc::Tuple(inner_elems) => {
                    let mut inner_collectors = Vec::new();
                    for (i, elem) in inner_elems.iter().enumerate() {
                        if type_has_enum_children(elem) {
                            let idx = syn::Index::from(i);
                            let field_acc = quote! { #iter_var.#idx };
                            extract_walk_from_type(
                                elem,
                                &field_acc,
                                &mut inner_collectors,
                                depth + 1,
                            );
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
                            __outputs.push(v.visit(#iter_var));
                        }
                    });
                }
            }
        }
        TypeDesc::Vec(_) => {}
        TypeDesc::Tuple(elems) => {
            for (i, elem) in elems.iter().enumerate() {
                if type_has_enum_children(elem) {
                    let idx = syn::Index::from(i);
                    let child_accessor = quote! { (#accessor).#idx };
                    extract_walk_from_type(elem, &child_accessor, collectors, depth + 1);
                }
            }
        }
    }
}

// ─── span_text() ────────────────────────────────────────────────────────────

fn generate_span_text_fn(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        if matches!(
            variant.category,
            VariantCategory::Phantom | VariantCategory::Recovered
        ) {
            continue;
        }
        let ident = format_ident!("{}", variant.name);
        let Some(td) = &variant.type_desc else {
            continue;
        };

        match td {
            TypeDesc::Span => {
                arms.push(quote! {
                    #enum_ident::#ident(s) => s.as_str()
                });
            }
            TypeDesc::BoxedEnum | TypeDesc::Enum => {
                arms.push(quote! {
                    #enum_ident::#ident(inner) => Self::span_text(inner)
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

// ─── identifier_text() ──────────────────────────────────────────────────────

fn generate_identifier_text_fn(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    // Find all variants with an `IdentifierCarrier` field — those return
    // the carried identifier directly. Other variants recurse via children().
    let mut arms = Vec::new();

    for variant in &schema.variants {
        if matches!(
            variant.category,
            VariantCategory::Phantom | VariantCategory::Recovered
        ) {
            continue;
        }
        let ident = format_ident!("{}", variant.name);
        let Some(td) = &variant.type_desc else {
            continue;
        };

        // Identifier-carrier rule (`identifier`) — return the Span text directly.
        if matches!(td, TypeDesc::Span) && variant.name == "identifier" {
            arms.push(quote! {
                #enum_ident::#ident(s) => s.as_str()
            });
            continue;
        }

        // Variants whose first field is an IdentifierCarrier (e.g. `term_1((ident, _))`).
        if let Some(idx) = variant
            .fields
            .iter()
            .position(|f| f.role == FieldRole::IdentifierCarrier)
        {
            let extraction = identifier_extraction_from_field(td, idx, variant);
            if let Some(extr) = extraction {
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extr }
                });
                continue;
            }
        }

        // Otherwise, fall through to the recursive default (`children().first()`).
    }

    quote! {
        match node {
            #(#arms,)*
            _ => {
                // Fall back to descending into the first enum child.
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_text(first)
                } else {
                    ""
                }
            }
        }
    }
}

/// Generate the body of an identifier-extraction match arm for a variant.
///
/// Returns `None` if the field's type is not directly addressable (we fall
/// back to `Self::identifier_text(...)` recursion via children).
fn identifier_extraction_from_field(
    td: &TypeDesc,
    field_idx: usize,
    _variant: &VariantDescriptor,
) -> Option<TokenStream> {
    match td {
        TypeDesc::Tuple(elems) => {
            let elem = elems.get(field_idx)?;
            let idx = syn::Index::from(field_idx);
            match elem {
                TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
                    Self::identifier_text((value).#idx)
                }),
                TypeDesc::Span => Some(quote! {
                    (value).#idx.as_str()
                }),
                _ => None,
            }
        }
        TypeDesc::Span => Some(quote! { value.as_str() }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            Some(quote! { Self::identifier_text(value) })
        }
        _ => None,
    }
}

// ─── Visitor trait ───────────────────────────────────────────────────────────

fn generate_visitor_trait(enum_ident: &syn::Ident, visitor_ident: &syn::Ident) -> TokenStream {
    quote! {
        /// Auto-generated visitor trait for the parser enum.
        ///
        /// Default `visit()` calls `walk()` which dispatches via
        /// `walk_children` (per-variant direct dispatch). Override
        /// `visit()` for short-circuiting; override `combine()` for
        /// non-default fold semantics.
        pub trait #visitor_ident<'a> {
            type Output: Default;

            fn combine(&mut self, outputs: ::std::vec::Vec<Self::Output>) -> Self::Output {
                let _ = outputs;
                Self::Output::default()
            }

            fn visit(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                self.walk(node)
            }

            fn walk(&mut self, node: &'a #enum_ident<'a>) -> Self::Output {
                let outputs = #enum_ident::walk_children(node, self);
                if outputs.is_empty() {
                    Self::Output::default()
                } else {
                    self.combine(outputs)
                }
            }
        }
    }
}

// ─── helpers ─────────────────────────────────────────────────────────────────

fn type_has_enum_children(td: &TypeDesc) -> bool {
    match td {
        TypeDesc::BoxedEnum | TypeDesc::Enum => true,
        TypeDesc::Span | TypeDesc::F64 | TypeDesc::U32 => false,
        TypeDesc::Option(inner) => type_has_enum_children(inner),
        TypeDesc::Vec(inner) => type_has_enum_children(inner),
        TypeDesc::Tuple(elems) => elems.iter().any(type_has_enum_children),
        TypeDesc::Named(_) => false,
    }
}
