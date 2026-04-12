//! View emission for `TapeSpanOnly` (single-leaf) rules.
//!
//! A `TapeSpanOnly` rule's view exposes `.text()` — the source text
//! of the matched span — and `.span()` (a byte-range pair). These
//! are the typed leaf accessors that complement the universal
//! accessor set in `mod.rs`.
//!
//! For `F64` leaves (fused number scan), an additional `.as_f64()`
//! accessor parses the span text lazily at view-read time.
//! For `U32` leaves (fused hex scan), `.as_u32()` does likewise.

use bbnf_ir::{IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

/// Emit typed leaf accessors for a rule whose projected type is a
/// leaf (`Span`, `F64`, `U32`, or `Named` scalar).
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

    // Type-specific scalar projections.
    match type_desc {
        TypeDesc::F64 => {
            methods.push(quote! {
                /// Parse the matched span as an `f64`.
                ///
                /// Lazy projection: the numeric conversion runs at
                /// view-read time, not at parse time.
                #[inline]
                pub fn as_f64(&self) -> f64 {
                    self.span_text().parse::<f64>().unwrap_or(0.0)
                }
            });
        }
        TypeDesc::U32 => {
            methods.push(quote! {
                /// Parse the matched span as a `u32` (hex digits).
                ///
                /// Lazy projection: the hex conversion runs at
                /// view-read time, not at parse time.
                #[inline]
                pub fn as_u32(&self) -> u32 {
                    u32::from_str_radix(self.span_text(), 16).unwrap_or(0)
                }
            });
        }
        _ => {}
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
