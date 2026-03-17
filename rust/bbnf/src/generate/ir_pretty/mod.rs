//! Pretty-printer code generation from IR.
//!
//! Generates `to_doc()` and `source_range()` impl blocks for the parser enum
//! by walking IR rule bodies and `PrettyHints` metadata.
//!
//! This module replaces the AST-based `prettify/` directory orchestrator.
//! It reuses the existing doc generation functions (`to_doc.rs`, `source_range.rs`,
//! `hints.rs`, `prettify_utils.rs`) which are AST-independent -- they operate on
//! `syn::Type` + hint strings, not AST nodes.

mod codegen;
mod heuristics;
mod patterns;
mod utils;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use codegen::{generate_compound_doc_ir, generate_sub_variant_arms, generate_vec_doc_ir};
use heuristics::resolve_ir_hints;
use patterns::{detect_wrapped_pattern_ir, resolve_and_detect_wrapped_ir};
use utils::{pretty_hints_to_strings, unwrap_ir_map};

use super::ir_types::{type_is_span, IrCodegenCtx};
use super::prettify::hints::{self, is_valid_hint};
use super::prettify::prettify_utils::*;
use super::prettify::source_range::generate_compound_range;
use super::prettify::to_doc::*;

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Generate `to_doc()` and `source_range()` impl blocks from IR.
pub fn generate_prettify_ir(ctx: &IrCodegenCtx<'_>) -> TokenStream {
    let enum_ident = &ctx.enum_ident;
    let has_recovers = ctx.ir.rules.iter().any(|r| r.meta.recover.is_some())
        && !ctx.parser_attrs.skip_recover;

    let mut to_doc_arms = Vec::new();
    let mut source_range_arms = Vec::new();

    for rule in &ctx.ir.rules {
        // Skip transparent rules -- they don't have enum variants.
        if rule.meta.is_transparent {
            continue;
        }

        let name = ctx.ir.get_string(rule.name);
        let variant = format_ident!("{}", name);

        // Get the inferred type for this rule.
        let ty = match ctx.rule_types.get(&rule.id) {
            Some(t) => t.clone(),
            None => ctx.boxed_enum_type.clone(),
        };

        // Unwrap Map wrapper to find the inner expression for pattern detection.
        let inner = unwrap_ir_map(&rule.body);

        // Get @pretty hints for this rule.
        let hints_vec = resolve_ir_hints(rule, &ty, ctx);

        // Validate explicit hints.
        if let Some(ref ph) = rule.meta.pretty {
            let explicit = pretty_hints_to_strings(ph);
            for hint in &explicit {
                if !is_valid_hint(hint) {
                    let valid = hints::valid_hint_names();
                    panic!(
                        "@pretty directive for rule `{}` contains unknown hint `{}`. \
                         Valid hints are: {}",
                        name,
                        hint,
                        valid.join(", ")
                    );
                }
            }
        }

        // Determine type shape.
        let is_span = type_is_span(&ty);
        let is_vec = is_vec_type(&ty);

        // Check for wrapped pattern before type dispatch.
        let wrapped = detect_wrapped_pattern_ir(inner, ctx.ir)
            .or_else(|| resolve_and_detect_wrapped_ir(inner, ctx.ir));

        // Generate the to_doc match arm.
        let doc_body = if let Some((ref left, ref right)) = wrapped {
            if is_span {
                generate_wrapped_span_doc(&variant, left, right, &hints_vec)
            } else {
                generate_wrapped_doc(&variant, left, right, &ty, &hints_vec)
            }
        } else if is_span {
            generate_span_doc(&variant, &hints_vec)
        } else if is_vec {
            generate_vec_doc_ir(&variant, &ty, &hints_vec)
        } else {
            generate_compound_doc_ir(&variant, inner, &ty, &hints_vec, ctx)
        };
        to_doc_arms.push(doc_body);

        // Generate source_range arm.
        let range_body = if is_span {
            quote! {
                Self::#variant(s) => Some((s.start, s.end)),
            }
        } else if is_vec {
            let item_source_range = generate_item_source_range(&ty);
            quote! {
                Self::#variant(items) => {
                    let mut _min_s = usize::MAX;
                    let mut _max_e = 0usize;
                    let mut _found = false;
                    for i in items.iter() {
                        if let Some((s, e)) = #item_source_range {
                            if s < _min_s { _min_s = s; }
                            if e > _max_e { _max_e = e; }
                            _found = true;
                        }
                    }
                    if _found { Some((_min_s, _max_e)) } else { None }
                }
            }
        } else {
            generate_compound_range(&variant, &ty)
        };
        source_range_arms.push(range_body);
    }

    // Add sub-variant arms for heterogeneous alternation branches.
    generate_sub_variant_arms(ctx, &mut to_doc_arms, &mut source_range_arms);

    // Recovered variant handling.
    if has_recovers {
        to_doc_arms.push(quote! {
            Self::Recovered => ::pprint::Doc::Null,
        });
        source_range_arms.push(quote! {
            Self::Recovered => None,
        });
    }

    quote! {
        impl<'a> #enum_ident<'a> {
            pub fn to_doc(&self) -> ::pprint::Doc<'a> {
                match self {
                    #(#to_doc_arms)*
                }
            }

            pub fn source_range(&self) -> Option<(usize, usize)> {
                match self {
                    #(#source_range_arms)*
                }
            }
        }
    }
}
