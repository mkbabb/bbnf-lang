//! CSS unit-tail kernel emission (Tranche X.10c).
//!
//! Emits a fused number scanner + unit literal memcmp for CSS
//! numeric-with-unit values (`12px`, `1.5em`, `100%`, `200vh`, etc.).
//! The numeric portion reuses the Eisel-Lemire-fast-path
//! `css_number_scan_f64` helper in `parse_that`; the unit suffix is a
//! direct byte-range memcmp.
//!
//! Two output shapes are exposed:
//!
//! 1. **Span form** — `emit_call_span` — returns `Option<Span>`
//!    covering the full match. Used by rules whose result type is
//!    already a `Span` (the common CSS value lane).
//! 2. **Fused form** — `emit_call_fused` — returns `Option<(Span, f64)>`
//!    so callers that need the parsed float value (e.g. CSS calc()
//!    arithmetic) avoid a second pass over the numeric bytes.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the Span-only form for a known CSS unit suffix.
pub fn emit_call_span(unit: &[u8]) -> TokenStream {
    let unit_lit = proc_macro2::Literal::byte_string(unit);
    let unit_len = unit.len();
    let unit_len_lit = proc_macro2::Literal::usize_unsuffixed(unit_len);

    quote! {
        {
            let __start = state.offset;
            // Numeric portion — fast path via the CSS number scanner.
            // Returns `Some((span, _))` on success; the span's end
            // becomes our unit check anchor.
            if ::parse_that::css_number_scan_f64(state).is_some() {
                let __mid = state.offset;
                if state.src_bytes.get(__mid..__mid + #unit_len_lit)
                    == Some(#unit_lit as &[u8])
                {
                    state.offset = __mid + #unit_len_lit;
                    Some(::parse_that::Span::new(__start, state.offset, state.src))
                } else {
                    state.offset = __start;
                    None
                }
            } else {
                None
            }
        }
    }
}

/// Emit the fused numeric+unit form that preserves the parsed `f64`.
pub fn emit_call_fused(unit: &[u8]) -> TokenStream {
    let unit_lit = proc_macro2::Literal::byte_string(unit);
    let unit_len = unit.len();
    let unit_len_lit = proc_macro2::Literal::usize_unsuffixed(unit_len);

    quote! {
        {
            let __start = state.offset;
            match ::parse_that::css_number_scan_f64(state) {
                Some((__num_span, __value)) => {
                    let __mid = state.offset;
                    if state.src_bytes.get(__mid..__mid + #unit_len_lit)
                        == Some(#unit_lit as &[u8])
                    {
                        state.offset = __mid + #unit_len_lit;
                        Some((
                            ::parse_that::Span::new(__start, state.offset, state.src),
                            __value,
                        ))
                    } else {
                        state.offset = __start;
                        None
                    }
                }
                None => None,
            }
        }
    }
}
