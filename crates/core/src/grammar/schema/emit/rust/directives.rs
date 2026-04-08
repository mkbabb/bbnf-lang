//! Directive value structs + schema-driven accessor methods.
//!
//! Generates:
//!
//! - `pub mod cst_directives { ... }` — one typed struct per directive variant
//! - `impl<'a> {Enum}<'a>::as_*_directive(&'a self) -> Option<cst_directives::*>`
//!
//! Field layouts are driven by a static table per `DirectiveKind`. Destructuring
//! binds the leading keyword span as `__kw` and the trailing terminator as
//! `__term`; the full directive span is constructed as `Span::new(__kw.start,
//! __term.end, __kw.src)`.

use bbnf_ir::TypeDesc;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::super::model::{CstSchema, DirectiveKind, VariantCategory};

pub(super) fn generate_module(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
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
pub(super) fn generate_accessors(schema: &CstSchema, enum_ident: &syn::Ident) -> TokenStream {
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

        // Tuple convention: field 0 = leading keyword span (`__kw`),
        // last field = trailing terminator span (`__term`), middle fields
        // map to slots in `layout`.
        let arity = elems.len();
        if arity < 2 {
            continue;
        }
        let mut pat_parts: Vec<TokenStream> = Vec::with_capacity(arity);
        // Bind leading keyword span.
        let kw_ident = format_ident!("__kw");
        pat_parts.push(quote! { #kw_ident });
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
                        span: ::parse_that::Span::new(
                            #kw_ident.start,
                            #term_ident.end,
                            #kw_ident.src,
                        ),
                    })
                } else {
                    ::std::option::Option::None
                }
            }
        });
    }

    quote! { #(#methods)* }
}

// ─── Directive layout table ──────────────────────────────────────────────────

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
