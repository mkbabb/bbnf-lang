//! View emission for `TapeSpanOnly` (single-leaf) rules.
//!
//! A `TapeSpanOnly` rule's view exposes `.text()` — the source text
//! of the matched span — and `.span()` (a byte-range pair). These
//! are the typed leaf accessors that complement the universal
//! accessor set in `mod.rs`.
//!
//! For scalar payload types, `.value()` and an `.as_<T>()` alias
//! resolve to the typed `tape.payload_<T>(rec)` reader with a
//! span-parse fallback for tapes produced by older codegen.
//! For `U32` leaves (fused hex scan), `.as_u32()` is the fallback
//! convenience used by hex-conversion grammars.

use bbnf_ir::{IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed leaf accessors for a rule whose projected type is a
/// leaf (`Span`, any scalar `TypeDesc`, or a `Named` scalar).
///
/// Returns an `impl` block that adds `.text()` and optional scalar
/// conversion methods to the rule's view struct.
pub fn emit_leaf_accessors(rule: &IrRule, rule_name: &str, type_desc: &TypeDesc) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);

    let mut methods = Vec::new();

    // `.text()` — always available on leaf views: returns the source
    // substring for the matched span.
    methods.push(quote! {
        /// The source text matched by this leaf rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    });

    // AQ.6.A: scalar-typed views uniformly read through the
    // generalized `tape.payload_<rust_ident>(rec)` accessor with an
    // O(1) fast path and a span-parse fallback. Bool keeps its
    // historical `"true"` literal comparison fallback (it cannot be
    // `.parse()`-d); U32 keeps its hex-digit fallback because the
    // typical U32-typed rule is a fused hex scan whose underlying
    // span is a hex string, not a base-10 number.
    if type_desc.is_scalar_payload() {
        let rust_ident = type_desc
            .rust_ident()
            .expect("scalar TypeDesc has rust_ident");
        let ty_ident = format_ident!("{}", rust_ident);
        let payload_fn = format_ident!("payload_{}", rust_ident);
        let as_fn = format_ident!("as_{}", rust_ident);
        let fallback = scalar_value_fallback(type_desc);
        methods.push(quote! {
            /// Get the parsed scalar value.
            ///
            /// Payload-first: reads the pre-computed value from the
            /// tape payload buffer in O(1). Falls back to span text
            /// parsing if no payload is present.
            #[inline]
            pub fn value(&self) -> #ty_ident {
                let tape = self.cursor.tape();
                let rec = self.cursor.record();
                if let Some(v) = tape.#payload_fn(rec) {
                    return v;
                }
                #fallback
            }

            /// Convert the matched span to the scalar type.
            ///
            /// Alias for backward compatibility. Prefer `.value()`.
            #[inline]
            pub fn #as_fn(&self) -> #ty_ident {
                self.value()
            }
        });
    }

    // Span-eligible rules get a convenience `.byte_span()` that
    // returns a `Range<usize>` for slicing.
    if rule.meta.span_eligible {
        methods.push(quote! {
            /// The matched byte range as a `Range<usize>`, suitable
            /// for slicing the input string directly.
            #[inline]
            pub fn byte_range(&self) -> ::core::ops::Range<usize> {
                let (lo, hi) = self.span();
                lo as usize..hi as usize
            }
        });
    }

    if methods.is_empty() {
        return quote! {};
    }

    quote! {
        #[allow(dead_code)]
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// AQ.6.A: emit the span-parse fallback expression used when the
/// tape carries no payload (e.g. `payload_idx == 0`).
///
/// - Bool falls back to `span_text() == "true"` because `bool` lacks
///   a `FromStr` instance compatible with the tape grammar.
/// - U32 falls back to base-16 parsing because the typical U32 rule
///   is a fused hex scan over `[0-9a-fA-F]+`.
/// - Every other scalar uses `str::parse::<T>()`, with `unwrap_or`
///   the type's natural zero.
fn scalar_value_fallback(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::Bool => quote! { self.span_text() == "true" },
        TypeDesc::U32 => quote! {
            u32::from_str_radix(self.span_text(), 16).unwrap_or(0)
        },
        TypeDesc::F64 => quote! { self.span_text().parse::<f64>().unwrap_or(0.0) },
        TypeDesc::I8 => quote! { self.span_text().parse::<i8>().unwrap_or(0) },
        TypeDesc::U8 => quote! { self.span_text().parse::<u8>().unwrap_or(0) },
        TypeDesc::I16 => quote! { self.span_text().parse::<i16>().unwrap_or(0) },
        TypeDesc::U16 => quote! { self.span_text().parse::<u16>().unwrap_or(0) },
        TypeDesc::I32 => quote! { self.span_text().parse::<i32>().unwrap_or(0) },
        TypeDesc::I64 => quote! { self.span_text().parse::<i64>().unwrap_or(0) },
        TypeDesc::U64 => quote! { self.span_text().parse::<u64>().unwrap_or(0) },
        _ => quote! { ::core::default::Default::default() },
    }
}
