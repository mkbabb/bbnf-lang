//! Body source for the quoted-string scan kernel.
//!
//! # Paired runtime
//!
//! Prototype reference in `crates/bbnf-json-prototype/src/string.rs:34`
//! (`parse_string_body`). This body is the escape-free fast path that
//! sonic-rs owns as its tight inner loop on string-heavy workloads
//! (twitter 64% strings, data_xl 42%). W2.1 verified the inlining with
//! `nm` + `cargo expand`; lifting it here allows the per-shape emitter
//! (W3.2 String-shape module) to splice it inline without depending on
//! cross-crate `#[inline(always)]` discipline.
//!
//! # Splice-target type
//!
//! [`syn::Block`]. The block is architecture-neutral — all SIMD work is
//! delegated to [`crate::emit::first_quote_or_backslash`] (spliced
//! alongside in the generated module). The shape of the inner loop is
//! shared across arches; only the `first_quote_or_backslash` scan
//! varies arch-to-arch.
//!
//! # Prebound names
//!
//! - `input: &[u8]` — full parse buffer.
//! - `p: &mut usize` — cursor; enters pointing AT the opening `b'"'`,
//!   exits pointing one past the closing `b'"'`.
//! - `visitor: &mut V` where `V: JsonVisitor` (or equivalent
//!   shape-specific trait; the emitter substitutes the concrete trait
//!   bound at splice time).
//! - `is_key: bool` — dispatches visitor.key() vs visitor.string() on
//!   the fast path.
//! - `open: usize` — the byte offset of the opening quote (for error
//!   reporting).
//! - `ParseError: type` — the parse-error enum in the enclosing crate,
//!   with `UnterminatedString(usize)` / `VisitorReject(usize)` /
//!   `Eof(usize)` variants.
//! - `first_quote_or_backslash: fn(&[u8]) -> Option<(usize, u8)>` —
//!   available as a local helper (the splice site emits this as a
//!   sibling fn via the [`crate::emit::first_quote_or_backslash`]
//!   fragment, or qualifies it as `crate::emit::first_quote_or_backslash::...`).
//! - `parse_string_escaped: fn(...)` — the cold-path decoder; the
//!   enclosing crate supplies it (the prototype's equivalent is
//!   `src/string.rs:parse_string_escaped`).
//!
//! # Shape
//!
//! ```text
//! {
//!     let body_start = open + 1;
//!     let tail = input.get(body_start..).ok_or(ParseError::Eof(open))?;
//!     match first_quote_or_backslash(tail) {
//!         Some((off, b'"')) => {
//!             let end = body_start + off;
//!             let body = &input[body_start..end];
//!             *p = end + 1;
//!             if is_key {
//!                 visitor.key(body).map_err(|_| ParseError::VisitorReject(open))?;
//!             } else {
//!                 visitor.string(body).map_err(|_| ParseError::VisitorReject(open))?;
//!             }
//!         }
//!         Some((off, b'\\')) => {
//!             let esc_start = body_start + off;
//!             parse_string_escaped(input, p, body_start, esc_start, visitor, is_key, open)?;
//!         }
//!         Some(_) => unreachable!(
//!             "first_quote_or_backslash returned non-quote/non-escape byte"
//!         ),
//!         None => return Err(ParseError::UnterminatedString(open)),
//!     }
//! }
//! ```
//!
//! The block is `?`-exiting and propagates errors up through the
//! enclosing fn's `Result<(), ParseError>` return type. The enclosing
//! emitter-produced fn supplies that signature; the fragment assumes
//! `Result<(), ParseError>` is the caller's return type.

use proc_macro2::TokenStream;
use quote::ToTokens;

/// Verbatim body of the escape-free fast path + cold-path routing for
/// JSON quoted strings. See module docs for the prebound-name contract.
pub const SOURCE: &str = r#"{
    let body_start = open + 1;
    let tail = input.get(body_start..).ok_or(ParseError::Eof(open))?;
    match first_quote_or_backslash(tail) {
        Some((off, b'"')) => {
            let end = body_start + off;
            let body = &input[body_start..end];
            *p = end + 1;
            if is_key {
                visitor.key(body).map_err(|_| ParseError::VisitorReject(open))?;
            } else {
                visitor.string(body).map_err(|_| ParseError::VisitorReject(open))?;
            }
        }
        Some((off, b'\\')) => {
            let esc_start = body_start + off;
            parse_string_escaped(input, p, body_start, esc_start, visitor, is_key, open)?;
        }
        Some(_) => unreachable!(
            "first_quote_or_backslash returned non-quote/non-escape byte"
        ),
        None => return Err(ParseError::UnterminatedString(open)),
    }
}"#;

/// Parse [`SOURCE`] into a [`TokenStream`].
pub fn fragment() -> TokenStream {
    syn::parse_str::<syn::Block>(SOURCE)
        .expect("bbnf-simd-scan::emit::quoted_string_simd_body: SOURCE must parse as syn::Block")
        .to_token_stream()
}
