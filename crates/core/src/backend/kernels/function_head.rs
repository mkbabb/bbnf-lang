//! CSS function-head kernel emission (Tranche X.10a).
//!
//! Single combined memcmp + paren-byte check for the eight canonical
//! CSS function names (`rgb(`, `rgba(`, `hsl(`, `hsla(`, `calc(`,
//! `var(`, `url(`, `attr(`). Replaces the inline `Literal(name) >>
//! Literal("(")` shape with a fused byte-range read that advances
//! `state.offset` by `name.len() + 1` on success.
//!
//! The returned expression is typed `Option<Span>` and preserves the
//! ordinary "rewind on mismatch" contract the driver expects.

use proc_macro2::TokenStream;
use quote::quote;

/// Emit the fused function-head scanner for a specific CSS function
/// name + paren byte.
///
/// ```ignore
/// {
///     let __start = state.offset;
///     let __total = #name_len + 1usize;
///     if state.src_bytes.get(__start..__start + __total)
///         == Some(b"<name>(" as &[u8])
///     {
///         state.offset = __start + __total;
///         Some(::parse_that::Span::new(__start, state.offset, state.src))
///     } else {
///         None
///     }
/// }
/// ```
pub fn emit_call(name: &[u8], paren_byte: u8) -> TokenStream {
    // Fused literal: the entire `<name>(` byte string as one memcmp.
    let mut fused: Vec<u8> = Vec::with_capacity(name.len() + 1);
    fused.extend_from_slice(name);
    fused.push(paren_byte);
    let fused_lit = proc_macro2::Literal::byte_string(&fused);
    let total = fused.len();
    let total_lit = proc_macro2::Literal::usize_unsuffixed(total);

    quote! {
        {
            let __start = state.offset;
            if state.src_bytes.get(__start..__start + #total_lit)
                == Some(#fused_lit as &[u8])
            {
                state.offset = __start + #total_lit;
                Some(::parse_that::Span::new(__start, state.offset, state.src))
            } else {
                None
            }
        }
    }
}
