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

use super::super::model::{
    CstSchema, DirectiveKind, FieldRole, VariantCategory, VariantDescriptor,
};

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
    let identifier_span_fn = generate_identifier_span_fn(schema, &enum_ident);
    let visitor_trait = generate_visitor_trait(&enum_ident, &visitor_ident);
    let directive_module = generate_directive_module(schema, &enum_ident);
    let directive_accessors = generate_directive_accessors(schema, &enum_ident);

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

            /// Recursively extract an identifier carrier's `Span`. Returns
            /// `Span::default()` if no identifier is reachable.
            pub fn identifier_span(node: &'a #enum_ident<'a>) -> ::parse_that::Span<'a> {
                #identifier_span_fn
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

            #directive_accessors
        }

        #visitor_trait

        #directive_module
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
            TypeDesc::Tuple(elems) => {
                // Tuple variant: recurse on the first "meaningful" child.
                // Priority: PrimaryChild → IdentifierCarrier → first BoxedEnum/Enum
                // → first Span. Mirrors the v1 hand-coded leaf-text descent.
                if let Some(arm) = generate_tuple_span_arm(variant, elems, enum_ident, &ident) {
                    arms.push(arm);
                }
            }
            TypeDesc::Vec(inner) => {
                // Vec variant (alternation/concatenation): recurse on first element.
                if let Some(arm) = generate_vec_span_arm(inner, enum_ident, &ident) {
                    arms.push(arm);
                }
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

/// For a Vec-payload variant (alternation/concatenation), recurse on the
/// first element. Handles `Vec<BoxedEnum>` and `Vec<Tuple([BoxedEnum, ...])>`.
fn generate_vec_span_arm(
    inner: &TypeDesc,
    enum_ident: &syn::Ident,
    variant_ident: &syn::Ident,
) -> Option<TokenStream> {
    match inner {
        TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
            #enum_ident::#variant_ident(items) if !items.is_empty() => Self::span_text(&items[0])
        }),
        TypeDesc::Tuple(elems) => {
            // Recurse on the first BoxedEnum field of the first element.
            let first_enum_idx = elems
                .iter()
                .position(|e| matches!(e, TypeDesc::BoxedEnum | TypeDesc::Enum))?;
            let idx = syn::Index::from(first_enum_idx);
            Some(quote! {
                #enum_ident::#variant_ident(items) if !items.is_empty() => Self::span_text(items[0].#idx)
            })
        }
        _ => None,
    }
}

/// For a tuple-payload variant, find the first field that should carry the
/// span and emit a match arm that recurses (or returns directly for Span).
fn generate_tuple_span_arm(
    variant: &VariantDescriptor,
    elems: &[TypeDesc],
    enum_ident: &syn::Ident,
    variant_ident: &syn::Ident,
) -> Option<TokenStream> {
    // Pick the highest-priority field to recurse on.
    let pick = |role_filter: fn(&FieldRole) -> bool| -> Option<usize> {
        variant
            .fields
            .iter()
            .position(|f| role_filter(&f.role))
    };

    let idx = pick(|r| matches!(r, FieldRole::PrimaryChild))
        .or_else(|| pick(|r| matches!(r, FieldRole::IdentifierCarrier)))
        .or_else(|| {
            elems
                .iter()
                .position(|e| matches!(e, TypeDesc::BoxedEnum | TypeDesc::Enum))
        })
        .or_else(|| elems.iter().position(|e| matches!(e, TypeDesc::Span)))?;

    let elem = elems.get(idx)?;
    let tuple_idx = syn::Index::from(idx);

    match elem {
        TypeDesc::Span => Some(quote! {
            #enum_ident::#variant_ident(value) => (value).#tuple_idx.as_str()
        }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => Some(quote! {
            #enum_ident::#variant_ident(value) => Self::span_text((value).#tuple_idx)
        }),
        _ => None,
    }
}

// ─── identifier_span() ──────────────────────────────────────────────────────

fn generate_identifier_span_fn(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut arms = Vec::new();

    for variant in &schema.variants {
        if matches!(
            variant.category,
            VariantCategory::Phantom | VariantCategory::Recovered
        ) {
            continue;
        }
        let ident = format_ident!("{}", variant.name);
        let Some(td) = &variant.type_desc else { continue };

        // The `identifier` rule itself is a Span carrier — return it directly.
        if matches!(td, TypeDesc::Span) && variant.name == "identifier" {
            arms.push(quote! {
                #enum_ident::#ident(s) => *s
            });
            continue;
        }

        // Variants whose first field is an IdentifierCarrier — recurse into it.
        if let Some(idx) = variant
            .fields
            .iter()
            .position(|f| f.role == FieldRole::IdentifierCarrier)
        {
            if let Some(extr) = identifier_span_extraction_from_field(td, idx) {
                arms.push(quote! {
                    #enum_ident::#ident(value) => { #extr }
                });
                continue;
            }
        }
    }

    quote! {
        match node {
            #(#arms,)*
            _ => {
                let ch = Self::children(node);
                if let Some(first) = ch.first() {
                    Self::identifier_span(first)
                } else {
                    ::parse_that::Span::default()
                }
            }
        }
    }
}

fn identifier_span_extraction_from_field(td: &TypeDesc, field_idx: usize) -> Option<TokenStream> {
    match td {
        TypeDesc::Tuple(elems) => {
            let elem = elems.get(field_idx)?;
            let idx = syn::Index::from(field_idx);
            match elem {
                TypeDesc::BoxedEnum | TypeDesc::Enum => {
                    Some(quote! { Self::identifier_span((value).#idx) })
                }
                // Span is Copy; tuple field access returns it by value.
                TypeDesc::Span => Some(quote! { (value).#idx }),
                _ => None,
            }
        }
        // Top-level Span variant: `value` is `&Span<'a>`, so deref.
        TypeDesc::Span => Some(quote! { *value }),
        TypeDesc::BoxedEnum | TypeDesc::Enum => {
            Some(quote! { Self::identifier_span(value) })
        }
        _ => None,
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

// ─── Directive accessors ─────────────────────────────────────────────────────

/// Generate the `cst_directives` sub-module containing one struct per directive
/// variant. Field names are conventional per `DirectiveKind`.
fn generate_directive_module(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut structs = Vec::new();

    for variant in &schema.variants {
        let VariantCategory::Directive(ref kind) = variant.category else {
            continue;
        };
        let Some(layout) = directive_field_layout(kind) else {
            continue;
        };
        let struct_ident = format_ident!("{}", layout.struct_name);
        let mut fields = Vec::new();
        for slot in layout.slots {
            let name = format_ident!("{}", slot.name);
            let ty: TokenStream = match slot.kind {
                DirectiveFieldKind::Identifier => quote! { &'a str },
                DirectiveFieldKind::TextSpan => quote! { &'a str },
                DirectiveFieldKind::SpanRaw => quote! { ::parse_that::Span<'a> },
                DirectiveFieldKind::Inner => quote! { &'a #enum_ident<'a> },
                DirectiveFieldKind::OptionInner => quote! { ::std::option::Option<&'a #enum_ident<'a>> },
                DirectiveFieldKind::Slice => quote! { &'a [#enum_ident<'a>] },
            };
            fields.push(quote! { pub #name: #ty });
        }
        // Always include the directive's source span as `.span`.
        fields.push(quote! { pub span: ::parse_that::Span<'a> });
        // No `#[derive(Debug)]`: these structs are produced/consumed in
        // hot paths (host extraction, lowering) and Debug expands to the
        // nightly `fmt_helpers_for_derive` API which the bootstrap pipeline
        // (`cargo expand`) cannot embed in stable output.
        structs.push(quote! {
            #[derive(Clone, Copy)]
            pub struct #struct_ident<'a> {
                #(#fields),*
            }
        });
    }

    if structs.is_empty() {
        return quote! {};
    }

    quote! {
        /// Schema-emitted directive value structs. Returned by the
        /// `as_*_directive` accessors on the parser enum.
        #[allow(dead_code, non_snake_case)]
        pub mod cst_directives {
            use super::#enum_ident;

            #(#structs)*
        }
    }
}

/// Generate `as_*_directive()` accessor methods that destructure the
/// matching enum variant and return the corresponding struct.
fn generate_directive_accessors(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
    let mut methods = Vec::new();

    for variant in &schema.variants {
        let VariantCategory::Directive(ref kind) = variant.category else {
            continue;
        };
        let Some(layout) = directive_field_layout(kind) else {
            continue;
        };
        let Some(td) = &variant.type_desc else { continue };
        let TypeDesc::Tuple(elems) = td else { continue };

        let variant_ident = format_ident!("{}", variant.name);
        let struct_ident = format_ident!("{}", layout.struct_name);
        let method_ident = format_ident!("as_{}", variant.name);

        // Build a tuple destructuring pattern + field assignments.
        // Convention: field 0 = leading keyword span (skip), last field =
        // trailing terminator span (captured as `.span`). Middle fields map
        // to the slots in `layout`.
        let arity = elems.len();
        if arity < 2 {
            continue;
        }
        let mut pat_parts: Vec<TokenStream> = Vec::with_capacity(arity);
        // Skip leading keyword.
        pat_parts.push(quote! { _ });
        // Bind middle fields by index.
        let mut middle_idents: Vec<syn::Ident> = Vec::new();
        for (i, _) in elems.iter().enumerate().skip(1).take(arity - 2) {
            let id = format_ident!("__f{}", i);
            pat_parts.push(quote! { #id });
            middle_idents.push(id);
        }
        // Bind trailing terminator span.
        let term_ident = format_ident!("__term");
        pat_parts.push(quote! { #term_ident });

        // Build the slot → value assignments.
        // Each slot consumes the corresponding middle ident.
        if middle_idents.len() != layout.slots.len() {
            // Layout mismatch — skip this directive (defensive).
            continue;
        }
        let mut slot_assignments = Vec::new();
        for (slot, ident) in layout.slots.iter().zip(middle_idents.iter()) {
            let name = format_ident!("{}", slot.name);
            let value: TokenStream = match slot.kind {
                DirectiveFieldKind::Identifier => quote! {
                    #enum_ident::identifier_text(#ident)
                },
                DirectiveFieldKind::TextSpan => quote! {
                    #enum_ident::span_text(#ident)
                },
                DirectiveFieldKind::SpanRaw => quote! { *#ident },
                DirectiveFieldKind::Inner => quote! { #ident },
                DirectiveFieldKind::OptionInner => quote! { #ident.as_ref().map(|t| t.1) },
                DirectiveFieldKind::Slice => quote! { #ident },
            };
            slot_assignments.push(quote! { #name: #value });
        }

        let pat = quote! { (#(#pat_parts),*) };
        methods.push(quote! {
            /// Schema-generated directive accessor.
            pub fn #method_ident(&'a self) -> ::std::option::Option<cst_directives::#struct_ident<'a>> {
                if let #enum_ident::#variant_ident(#pat) = self {
                    ::std::option::Option::Some(cst_directives::#struct_ident {
                        #(#slot_assignments,)*
                        span: *#term_ident,
                    })
                } else {
                    ::std::option::Option::None
                }
            }
        });
    }

    quote! { #(#methods)* }
}

/// Layout for a directive variant's semantic fields. Driven by `DirectiveKind`.
struct DirectiveLayout {
    struct_name: &'static str,
    slots: &'static [DirectiveSlot],
}

struct DirectiveSlot {
    name: &'static str,
    kind: DirectiveFieldKind,
}

#[derive(Clone, Copy)]
enum DirectiveFieldKind {
    /// `&'a Enum<'a>` — extract via `identifier_text(...)`.
    Identifier,
    /// `&'a Enum<'a>` — extract via `span_text(...)` (returns the leaf span).
    TextSpan,
    /// `Span<'a>` — pass through (`*field`).
    SpanRaw,
    /// `&'a Enum<'a>` — pass through reference.
    Inner,
    /// `Option<(Span, &'a Enum)>` — extract `.1` (the inner enum) into Option.
    OptionInner,
    /// `&'a [Enum<'a>]` — pass through slice.
    Slice,
}

fn directive_field_layout(kind: &DirectiveKind) -> Option<DirectiveLayout> {
    use DirectiveFieldKind as F;
    Some(match kind {
        DirectiveKind::Recover => DirectiveLayout {
            struct_name: "RecoverDirective",
            slots: &[
                DirectiveSlot { name: "rule_name", kind: F::Identifier },
                DirectiveSlot { name: "sync_expr", kind: F::Inner },
            ],
        },
        DirectiveKind::Pretty => DirectiveLayout {
            struct_name: "PrettyDirective",
            slots: &[
                DirectiveSlot { name: "target", kind: F::TextSpan },
                DirectiveSlot { name: "hints", kind: F::Slice },
            ],
        },
        DirectiveKind::Import => DirectiveLayout {
            struct_name: "ImportDirective",
            slots: &[DirectiveSlot { name: "inner", kind: F::Inner }],
        },
        DirectiveKind::Ws => DirectiveLayout {
            struct_name: "WsDirective",
            slots: &[DirectiveSlot { name: "value", kind: F::Inner }],
        },
        DirectiveKind::Token => DirectiveLayout {
            struct_name: "TokenDirective",
            slots: &[DirectiveSlot { name: "name", kind: F::Identifier }],
        },
        DirectiveKind::Debug => DirectiveLayout {
            struct_name: "DebugDirective",
            slots: &[DirectiveSlot { name: "target", kind: F::TextSpan }],
        },
        DirectiveKind::Host => DirectiveLayout {
            struct_name: "HostDirective",
            slots: &[
                DirectiveSlot { name: "name", kind: F::Identifier },
                DirectiveSlot { name: "type_annotation", kind: F::OptionInner },
            ],
        },
        DirectiveKind::Other(_) => return None,
    })
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
