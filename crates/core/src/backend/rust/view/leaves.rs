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
//!
//! AQ.6.B aggregate views: when a rule carries a `PayloadLayout`,
//! [`emit_aggregate_accessors`] generates a `.value()` that returns
//! a typed Rust tuple constructed from the per-field bytes via
//! `<T>::from_le_bytes` reads at the layout-recorded offsets.

use bbnf_ir::passes::PayloadLayout;
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

    // AS.2: Span-typed views decode (lo, hi) from the tape payload
    // and return `&'p str` by indexing into the input. Falls back to
    // the record's own span when no payload is present.
    if matches!(type_desc, TypeDesc::Span) {
        methods.push(quote! {
            /// Get the sub-span value as a string slice.
            ///
            /// Payload-first: reads the packed (lo, hi) u32 pair from
            /// the tape payload buffer in O(1). Falls back to the
            /// record's own span text if no payload is present.
            #[inline]
            pub fn value(&self) -> &'p str {
                let tape = self.cursor.tape();
                let rec = self.cursor.record();
                if let Some((lo, hi)) = tape.payload_Span(rec) {
                    return &self.input[lo as usize..hi as usize];
                }
                self.span_text()
            }

            /// Alias for backward compatibility. Prefer `.value()`.
            #[inline]
            pub fn as_span(&self) -> &'p str {
                self.value()
            }
        });
    } else if type_desc.is_scalar_payload() {
        // AQ.6.A: scalar-typed views uniformly read through the
        // generalized `tape.payload_<rust_ident>(rec)` accessor with an
        // O(1) fast path and a span-parse fallback. Bool keeps its
        // historical `"true"` literal comparison fallback (it cannot be
        // `.parse()`-d); U32 keeps its hex-digit fallback because the
        // typical U32-typed rule is a fused hex scan whose underlying
        // span is a hex string, not a base-10 number.
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
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// AQ.6.B: emit the typed aggregate `.value()` accessor for a rule
/// whose tape record carries a packed multi-field payload.
///
/// The accessor reads `layout.total_bytes` bytes via
/// [`bbnf::runtime::tape::Tape::payload_bytes`] and constructs a
/// tuple of typed scalars via per-field `<T>::from_le_bytes`
/// conversions at the layout-recorded offsets. When no payload was
/// written (the rule completed without producing scalars), the
/// accessor returns the tuple of zero-initialized scalars.
pub fn emit_aggregate_accessors(
    rule: &IrRule,
    rule_name: &str,
    layout: &PayloadLayout,
    _type_desc: Option<&TypeDesc>,
) -> TokenStream {
    let view_ident = format_ident!("{}View", rule_name);
    let total_bytes = layout.total_bytes as usize;

    // Per-field types (for the tuple return type) and per-field
    // reads (for the construction body).
    let field_tys: Vec<TokenStream> = layout
        .fields
        .iter()
        .map(|f| aggregate_field_type(&f.ty))
        .collect();

    let field_reads: Vec<TokenStream> = layout
        .fields
        .iter()
        .map(aggregate_field_read)
        .collect();

    let field_zero: Vec<TokenStream> = layout
        .fields
        .iter()
        .map(|f| scalar_zero_init(&f.ty))
        .collect();

    let mut methods = Vec::new();

    // `.text()` is still useful — the matched source span.
    methods.push(quote! {
        /// The source text matched by this rule.
        #[inline]
        pub fn text(&self) -> &'p str {
            self.span_text()
        }
    });

    methods.push(quote! {
        /// The packed scalar fields decoded from the tape's
        /// aggregate payload buffer.
        ///
        /// Returns the layout-zeroed tuple if no payload was
        /// written for this record (e.g. an alternative branch
        /// path that never set any fields).
        #[inline]
        pub fn value(&self) -> ( #(#field_tys),* ) {
            let tape = self.cursor.tape();
            let rec = self.cursor.record();
            match tape.payload_bytes(rec, #total_bytes) {
                Some(__bytes) => ( #(#field_reads),* ),
                None => ( #(#field_zero),* ),
            }
        }
    });

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

    quote! {
        impl<'p> #view_ident<'p> {
            #(#methods)*
        }
    }
}

/// Emit the Rust type token for an aggregate field.
///
/// `TypeDesc::Span` fields are stored as packed `(u32, u32)` pairs
/// in the aggregate payload buffer — NOT as `parse_that::Span<'_>`.
/// Every other scalar uses its natural Rust primitive name.
fn aggregate_field_type(td: &TypeDesc) -> TokenStream {
    if matches!(td, TypeDesc::Span) {
        return quote! { (u32, u32) };
    }
    let ident = td
        .rust_ident()
        .expect("aggregate field has rust_ident");
    let ty = format_ident!("{}", ident);
    quote! { #ty }
}

/// AQ.6.B: emit the per-field decoder for an aggregate read. Each
/// scalar slices its own offset out of the byte buffer and
/// reconstructs via `<T>::from_le_bytes`.
fn aggregate_field_read(field: &bbnf_ir::passes::PayloadField) -> TokenStream {
    let offset = field.offset as usize;
    let size = field
        .ty
        .payload_size_bytes()
        .expect("aggregate field has payload_size") as usize;
    let end = offset + size;
    match field.ty {
        TypeDesc::Bool => quote! { __bytes[#offset] != 0 },
        TypeDesc::I8 => quote! { __bytes[#offset] as i8 },
        TypeDesc::U8 => quote! { __bytes[#offset] },
        TypeDesc::F64 => quote! {
            f64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 8 bytes"),
            )
        },
        TypeDesc::I16 => quote! {
            i16::from_le_bytes(
                <[u8; 2]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 2 bytes"),
            )
        },
        TypeDesc::U16 => quote! {
            u16::from_le_bytes(
                <[u8; 2]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 2 bytes"),
            )
        },
        TypeDesc::I32 => quote! {
            i32::from_le_bytes(
                <[u8; 4]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 4 bytes"),
            )
        },
        TypeDesc::U32 => quote! {
            u32::from_le_bytes(
                <[u8; 4]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 4 bytes"),
            )
        },
        TypeDesc::I64 => quote! {
            i64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 8 bytes"),
            )
        },
        TypeDesc::U64 => quote! {
            u64::from_le_bytes(
                <[u8; 8]>::try_from(&__bytes[#offset..#end])
                    .expect("aggregate slice is 8 bytes"),
            )
        },
        TypeDesc::Span => quote! {
            {
                let __raw = u64::from_le_bytes(
                    <[u8; 8]>::try_from(&__bytes[#offset..#end])
                        .expect("aggregate slice is 8 bytes"),
                );
                (__raw as u32, (__raw >> 32) as u32)
            }
        },
        _ => unreachable!("aggregate layout planner only admits scalar TypeDescs"),
    }
}

/// AQ.6.B: emit the zero value for a scalar `TypeDesc`. Used by
/// the aggregate view's no-payload fallback.
fn scalar_zero_init(td: &TypeDesc) -> TokenStream {
    match td {
        TypeDesc::Span => quote! { (0_u32, 0_u32) },
        TypeDesc::F64 => quote! { 0.0_f64 },
        TypeDesc::Bool => quote! { false },
        TypeDesc::I8 => quote! { 0_i8 },
        TypeDesc::U8 => quote! { 0_u8 },
        TypeDesc::I16 => quote! { 0_i16 },
        TypeDesc::U16 => quote! { 0_u16 },
        TypeDesc::I32 => quote! { 0_i32 },
        TypeDesc::U32 => quote! { 0_u32 },
        TypeDesc::I64 => quote! { 0_i64 },
        TypeDesc::U64 => quote! { 0_u64 },
        _ => quote! { ::core::default::Default::default() },
    }
}

/// AQ.6.A: emit the span-parse fallback expression used when the
/// tape carries no payload (`rec.child_off.is_none()`).
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
