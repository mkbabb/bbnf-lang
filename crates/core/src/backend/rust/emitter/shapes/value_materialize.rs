//! AY.W3b.2 — json-prototype per-shape inline fn pattern. The
//! BEAT-sonic lever per AY.md prop 3: emit five `#[inline(always)]`
//! per-shape fns per grammar, plus the root materialiser, each
//! monomorphised at the `parsed.to_value()` call site so LLVM
//! inlines the entire tree-build into a single flat function.
//! Matches `json-prototype`'s 0.89-0.94× sonic ceiling.
//!
//! # The five shapes
//!
//! Each grammar emits:
//!
//! - `materialize_object_<Grammar>` — compound whose children are
//!   `(key, value)` alternating pairs. Returns `Vec<<Grammar>Value>`
//!   of length `2 * entry_count`, ready for the root dispatcher to
//!   wrap in the grammar's Compound variant.
//! - `materialize_array_<Grammar>` — compound whose children are
//!   sequenced values. Returns `Vec<<Grammar>Value>` in declaration
//!   order.
//! - `materialize_string_<Grammar>` — Span-shaped leaves. Resolves
//!   through `payload_Span` with a span-text fallback; returns a
//!   borrowed `&'p str`.
//! - `materialize_number_<Grammar>` — f64-shaped leaves. Reads
//!   `payload_f64` with a span-parse fallback.
//! - `materialize_literal_<Grammar>` — Bool leaves. Reads
//!   `payload_bool` with a span-text fallback.
//!
//! Plus the root dispatcher:
//!
//! - `materialize_value_<Grammar>` — inspects `rule_kind()` and
//!   tail-calls the appropriate per-shape fn before wrapping the
//!   result in the grammar's `<Grammar>Value` variant.
//!
//! # Dispatch shape
//!
//! The root dispatcher is a single `match view.rule_kind() { … }`
//! with one arm per variant entry. The per-shape fns do the walk;
//! the root constructs the variant. Under LLVM with
//! `#[inline(always)]` on every per-shape fn, the root body
//! collapses into one flat function at the caller's site — the
//! same layout `json-prototype::parse_value` produces.

use bbnf_ir::{GrammarIR, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::backend::rust::view::{VariantInfo, VariantInfoShape, variant_entries_for};

/// Emit the per-shape + root materialise fns for `ir`.
///
/// Returns an empty [`TokenStream`] when the grammar has no
/// non-transparent rules.
pub fn emit_materialize_fns(ir: &GrammarIR, grammar_name: &str) -> TokenStream {
    let variants = variant_entries_for(ir);
    if variants.is_empty() {
        return quote! {};
    }

    let value_ident = format_ident!("{}Value", grammar_name);
    let node_view_ident = format_ident!("{}NodeView", grammar_name);
    let rule_kind_ident = format_ident!("{}RuleKind", grammar_name);

    let object_fn = emit_object_fn(&value_ident, &node_view_ident, grammar_name);
    let array_fn = emit_array_fn(&value_ident, &node_view_ident, grammar_name);
    let string_fn = emit_string_fn(&node_view_ident, grammar_name);
    let number_fn = emit_number_fn(&node_view_ident, grammar_name);
    let literal_fn = emit_literal_fn(&node_view_ident, grammar_name);
    let root_fn = emit_root_fn(
        &value_ident,
        &node_view_ident,
        &rule_kind_ident,
        &variants,
        grammar_name,
    );

    quote! {
        #object_fn
        #array_fn
        #string_fn
        #number_fn
        #literal_fn
        #root_fn
    }
}

/// Object shape — interleaved (key, value) children. Walks the
/// compound's direct children and drives each through the root
/// materialiser, so keys and values are both recursively
/// materialised in declaration order.
fn emit_object_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_object_{}", grammar_name);
    let root_fn = format_ident!("materialize_value_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — object shape walker. Returns the compound's
        /// children recursively materialised, ready for the root
        /// dispatcher to wrap in its grammar-specific Compound variant.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> ::std::vec::Vec<#value_ident<'p>> {
            let mut out: ::std::vec::Vec<#value_ident<'p>> =
                ::std::vec::Vec::with_capacity(view.cursor().child_count().max(1) * 2);
            for child in view.children() {
                out.push(#root_fn(child));
            }
            out
        }
    }
}

/// Array shape — sequenced child materialisation.
fn emit_array_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_array_{}", grammar_name);
    let root_fn = format_ident!("materialize_value_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — array shape walker. Produces a
        /// `Vec<<Grammar>Value>` of the compound's children in
        /// declaration order.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> ::std::vec::Vec<#value_ident<'p>> {
            let mut out: ::std::vec::Vec<#value_ident<'p>> =
                ::std::vec::Vec::with_capacity(view.cursor().child_count());
            for child in view.children() {
                out.push(#root_fn(child));
            }
            out
        }
    }
}

/// String shape (Span leaf). Resolves through `payload_Span`
/// then falls back to the cursor's own span text. Zero-copy
/// borrow from the input slice.
fn emit_string_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_string_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — string (Span) materialiser. Zero-copy borrow
        /// from the input slice.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> &'p str {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some((lo, hi)) = tape.payload_Span(rec) {
                return &view.input()[lo as usize..hi as usize];
            }
            view.span_text()
        }
    }
}

/// Number shape — f64 specialisation. The root dispatcher reads
/// per-TypeDesc payloads inline at its arm; this fn is the shared
/// f64 fallback for rules whose type classification is Number at
/// emit time.
fn emit_number_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_number_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — number (f64) materialiser. Payload-first
        /// read with a span-parse fallback.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> f64 {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some(v) = tape.payload_f64(rec) {
                return v;
            }
            view.span_text().parse::<f64>().unwrap_or(0.0)
        }
    }
}

/// Literal shape (Bool). Payload-first read with a span-text
/// comparison fallback.
fn emit_literal_fn(node_view_ident: &syn::Ident, grammar_name: &str) -> TokenStream {
    let fn_name = format_ident!("materialize_literal_{}", grammar_name);
    quote! {
        /// AY.W3b.2 — literal / keyword materialiser.
        #[inline(always)]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> bool {
            let tape = view.cursor().tape();
            let rec = view.cursor().record();
            if let Some(v) = tape.payload_bool(rec) {
                return v;
            }
            view.span_text() == "true"
        }
    }
}

/// Root dispatcher — `materialize_value_<Grammar>`. Single `match
/// view.rule_kind() { … }` with one arm per variant:
///
/// - Span variants → read the span text via the string materialiser.
/// - Scalar variants → read the typed payload inline.
/// - Compound variants → drive children through the object walker
///   and wrap in the Compound variant.
/// - Cursor variants → wrap the `NodeView` directly.
/// - Unknown → fallback.
fn emit_root_fn(
    value_ident: &syn::Ident,
    node_view_ident: &syn::Ident,
    rule_kind_ident: &syn::Ident,
    variants: &[VariantInfo],
    grammar_name: &str,
) -> TokenStream {
    let fn_name = format_ident!("materialize_value_{}", grammar_name);
    let string_fn = format_ident!("materialize_string_{}", grammar_name);
    let number_fn = format_ident!("materialize_number_{}", grammar_name);
    let literal_fn = format_ident!("materialize_literal_{}", grammar_name);
    let object_fn = format_ident!("materialize_object_{}", grammar_name);

    let arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let kind_variant = format_ident!("{}", v.name);
            let value_variant = format_ident!("{}", v.name);

            match &v.shape {
                VariantInfoShape::Span => {
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            #value_ident::#value_variant(#string_fn(view))
                        }
                    }
                }
                VariantInfoShape::Scalar(td) => emit_scalar_arm(
                    rule_kind_ident,
                    &kind_variant,
                    value_ident,
                    &value_variant,
                    td,
                    &number_fn,
                    &literal_fn,
                ),
                VariantInfoShape::Compound => {
                    // Route through the object walker — its capacity
                    // hint handles interleaved key-value compounds
                    // and degrades gracefully to `child_count()` on
                    // array-shaped rules. The Array walker is still
                    // emitted as a specialised alternative the
                    // future compound-classification pass can call.
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            let children = #object_fn(view);
                            #value_ident::#value_variant(children)
                        }
                    }
                }
                VariantInfoShape::Cursor => {
                    quote! {
                        #rule_kind_ident::#kind_variant => {
                            #value_ident::#value_variant(view)
                        }
                    }
                }
            }
        })
        .collect();

    quote! {
        /// AY.W3b.2 — root value materialiser. Dispatches on
        /// `rule_kind()` and constructs the grammar's
        /// `<Grammar>Value` variant directly. Every per-shape fn
        /// it calls is `#[inline(always)]`; this root is `#[inline]`
        /// so the whole tree collapses into a single flat function
        /// at the `parsed.to_value()` call site.
        #[inline]
        fn #fn_name<'p>(view: #node_view_ident<'p>) -> #value_ident<'p> {
            match view.rule_kind() {
                #(#arms)*
                _ => #value_ident::Unknown(view),
            }
        }
    }
}

/// Emit a Scalar variant arm — reads the typed payload inline.
/// F64 and Bool variants route through the dedicated per-shape
/// fns; other scalar types read directly via the grammar's typed
/// payload accessor.
fn emit_scalar_arm(
    rule_kind_ident: &syn::Ident,
    kind_variant: &syn::Ident,
    value_ident: &syn::Ident,
    value_variant: &syn::Ident,
    td: &TypeDesc,
    number_fn: &syn::Ident,
    literal_fn: &syn::Ident,
) -> TokenStream {
    match td {
        TypeDesc::F64 => {
            quote! {
                #rule_kind_ident::#kind_variant => {
                    #value_ident::#value_variant(#number_fn(view))
                }
            }
        }
        TypeDesc::Bool => {
            quote! {
                #rule_kind_ident::#kind_variant => {
                    #value_ident::#value_variant(#literal_fn(view))
                }
            }
        }
        _ => {
            let ident = td
                .rust_ident()
                .expect("scalar TypeDesc has rust_ident");
            let payload_fn = format_ident!("payload_{}", ident);
            let ty_ident = format_ident!("{}", ident);
            let fallback = scalar_fallback(td);
            quote! {
                #rule_kind_ident::#kind_variant => {
                    let tape = view.cursor().tape();
                    let rec = view.cursor().record();
                    let v: #ty_ident = if let Some(v) = tape.#payload_fn(rec) {
                        v
                    } else {
                        #fallback
                    };
                    #value_ident::#value_variant(v)
                }
            }
        }
    }
}

/// Span-text parse fallback for scalars without a payload slot.
/// Mirrors `view/leaves.rs::scalar_value_fallback` — identical
/// logic, inlined here so this module stays self-contained.
fn scalar_fallback(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::Bool => quote! { view.span_text() == "true" },
        TypeDesc::U32 => quote! {
            view.span_text().parse::<u32>().unwrap_or(0u32)
        },
        TypeDesc::F64 => quote! {
            view.span_text().parse::<f64>().unwrap_or(0.0)
        },
        TypeDesc::I8 => quote! { view.span_text().parse::<i8>().unwrap_or(0) },
        TypeDesc::U8 => quote! { view.span_text().parse::<u8>().unwrap_or(0) },
        TypeDesc::I16 => quote! {
            view.span_text().parse::<i16>().unwrap_or(0)
        },
        TypeDesc::U16 => quote! {
            view.span_text().parse::<u16>().unwrap_or(0)
        },
        TypeDesc::I32 => quote! {
            view.span_text().parse::<i32>().unwrap_or(0)
        },
        TypeDesc::I64 => quote! {
            view.span_text().parse::<i64>().unwrap_or(0)
        },
        TypeDesc::U64 => quote! {
            view.span_text().parse::<u64>().unwrap_or(0)
        },
        _ => quote! { ::core::default::Default::default() },
    }
}
