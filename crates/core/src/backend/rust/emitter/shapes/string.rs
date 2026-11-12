//! String-shape emitter — `parse_string_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar String-shape parse function mirroring
//! `json_prototype::parse_string` (crates/json-prototype/
//! src/lib.rs:351) + `json_prototype::string::parse_string_body`.
//!
//! The emitted function:
//!
//! 1. Expects `input[*p] == b'"'` (caller's dispatcher gate).
//! 2. Fast path: scan for first `"` or `\\` via the per-grammar
//!    `first_quote_or_backslash` SIMD primitive (support module).
//! 3. On a `"` hit: borrowed-string path — push a borrowed string
//!    slice into the active StructDirect builder.
//! 4. On a `\\` hit: cold path — decode through
//!    `parse_that::parsers::scan::decode_json_string_to_arena`
//!    (AY.W4.1; the kernel's SIMD `u8x16` scan + stripe-copy escape
//!    decoder), then emit the decoded string through the same builder
//!    surface.
//!
//! The `-> decode_json_string_to_arena(input) : String` annotation
//! on JSON's `string` rule means every string leaf must either
//! borrow (no escapes) or be arena-decoded. We check for presence of
//! backslashes via the SIMD scan; the borrow path is the fast case
//! (~90% of real-world inputs).

use bbnf_ir::{GrammarIR, IrNode, IrRule, TypeDesc};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::cursor_param::{cursor_param, cursor_where_clause};
use super::dispatcher::shape_fn_ident;
use bbnf_ir::registry::EmitStrategy;

/// True when the rule's projected type is `TypeDesc::Span` — meaning
/// the grammar declares `-> input : Span` (or omits the projection
/// and inherits the Span default). Span-typed string rules must
/// emit the FULL quoted span `[open..end+1]` so the source bytes
/// (including the surrounding quotes) round-trip through the leaf;
/// String-typed rules with a host-fn decoder (JSON's
/// `decode_json_string_to_arena`) emit only the inner body
/// `[body_start..end]` because the decoder produced the unquoted
/// payload.
fn rule_is_span_typed(rule: &IrRule, ir: &GrammarIR) -> bool {
    ir.types.iter().any(|(rid, t)| {
        if *rid == rule.id {
            matches!(t, TypeDesc::Span)
        } else {
            false
        }
    })
}

/// The bytes the rule's plain-content class refuses besides the quote and
/// the backslash (sk-v27 LW3.ORC1-STR). The scanner finds the closing quote
/// by the quote and the backslash alone, so without this a class such as
/// `[^"\\\x00-\x1F]` (RFC 8259: no unescaped U+0000 to U+001F) was read as
/// `[^"\\]`. The class is the one negated class among the alternatives of
/// the body's repetition (`"(?:[^…]|\\…)*"`); every byte it refuses must be
/// one no escape can hold, so the scanner can test the raw body for it.
fn refused_plain_bytes(rule: &IrRule, ir: &GrammarIR, quote: u8) -> Vec<u8> {
    use parse_that::regex::hir::{CharClass, Hir};
    fn regex(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Regex(sid) => Some(*sid),
            IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => regex(inner),
            _ => None,
        }
    }
    fn ungroup(h: &Hir) -> &Hir {
        match h {
            Hir::Group(sub) => ungroup(sub),
            _ => h,
        }
    }
    let Some(sid) = regex(&rule.body) else {
        return Vec::new();
    };
    let pattern = ir.get_string(sid);
    let Ok(hir) =
        parse_that::regex::parse_with(pattern, &parse_that::regex::ParseOptions::byte_mode())
    else {
        return Vec::new();
    };
    let Hir::Concat(parts) = ungroup(&hir) else {
        return Vec::new();
    };
    let mut refused = Vec::new();
    for part in &parts[1..parts.len().saturating_sub(1)] {
        let Hir::Repetition(rep) = ungroup(part) else {
            continue;
        };
        let branches: Vec<&Hir> = match ungroup(&rep.sub) {
            Hir::Alternation(alts) => alts.iter().map(ungroup).collect(),
            one => vec![one],
        };
        for b in branches {
            if let Hir::Class(CharClass::Bytes { ranges, negated: true }) = b {
                for r in ranges {
                    for byte in r.start..=r.end {
                        if byte != quote && byte != b'\\' && !refused.contains(&byte) {
                            refused.push(byte);
                        }
                    }
                }
            }
        }
    }
    refused.sort_unstable();
    // An escape's bytes (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`,
    // `\uXXXX`) are read by the decoder, not by the class: a refused byte the
    // decoder can consume would make the raw-body test wrong.
    let escape_bytes = b"\"\\/bfnrtu0123456789abcdefABCDEF";
    assert!(
        refused.iter().all(|b| !escape_bytes.contains(b) && *b < 0x80),
        "String shape on rule `{}`: its class refuses a byte an escape or a UTF-8 sequence holds ({:?}); the raw-body test cannot read it",
        ir.get_string(rule.name),
        refused
    );
    refused
}

/// Emit `pub fn parse_string_<grammar>_<rule>(input, p, state, builder)
/// -> Result<(), DtaError>`.
///
/// AZ-I.W2.RC / AZ-II.cutover.O4 — emits the struct-builder body.
/// The borrow path slices `input[body_start..end]` (or `[open..end+1]`
/// for `: Span`-typed rules) with the parse-fn lifetime; the cold
/// escape path decodes and emits through the same
/// `push_leaf_with_str` surface.
pub fn emit_parse_string(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("string", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let EmitStrategy::StructDirect { rust, .. } = strategy;
    let builder_ty: syn::Path = syn::parse_str(rust.builder_path)
        .expect("EmitStrategy::StructDirect.rust.builder_path must parse as a Rust path");
    // AZ-IV.W1-CLOSE.A — Span-typed string rules (Sheets's
    // `string -> input : Span`) capture the full quoted span; the
    // borrow-path slice and the cold-escape Borrowed slice both
    // include the surrounding quotes. Decoder-typed string rules
    // (JSON's `string -> decode_json_string_to_arena(input) :
    // String`) keep the inner-body slicing unchanged.
    let is_span_typed = rule_is_span_typed(rule, ir);
    let (borrow_start_expr, borrow_end_expr) = if is_span_typed {
        (quote! { open }, quote! { end + 1 })
    } else {
        (quote! { body_start }, quote! { end })
    };
    let cold_borrowed_slice = if is_span_typed {
        // Cold-escape path's Borrowed payload reports `[start, end)`
        // for the body; widen to include the surrounding quotes.
        // `start` is `open + 1` and `end` is the closing quote
        // offset, so `[start - 1, end + 1)` is the full quoted span.
        quote! { &input[(start as usize).saturating_sub(1)..(end as usize) + 1] }
    } else {
        quote! { &input[start as usize..end as usize] }
    };
    let cursor_p = cursor_param();
    let cursor_where = cursor_where_clause();
    // The class's refused bytes, tested over the raw body once its end is
    // known (the borrow path's `[body_start..end]`, the escape path's
    // `[body_start..end_pos - 1]`); empty for a class that refuses only the
    // quote and the backslash.
    let refused = refused_plain_bytes(rule, ir, b'"');
    let (refused_check_borrow, refused_check_escape) = if refused.is_empty() {
        (quote! {}, quote! {})
    } else {
        let mut table = [false; 256];
        for b in &refused {
            table[*b as usize] = true;
        }
        let table = table.iter();
        let decl = quote! {
            const __REFUSED: [bool; 256] = [#(#table),*];
        };
        (
            quote! {
                #decl
                if let Some(__at) = input[body_start..end]
                    .iter()
                    .position(|&__b| __REFUSED[__b as usize])
                {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: (body_start + __at) as u32,
                    });
                }
            },
            quote! {
                #decl
                if let Some(__at) = input[body_start..end_pos - 1]
                    .iter()
                    .position(|&__b| __REFUSED[__b as usize])
                {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: (body_start + __at) as u32,
                    });
                }
            },
        )
    };
    quote! {
        /// AZ-I.W2.RC — per-grammar String-shape parse function
        /// (struct-direct substrate).
        ///
        /// `"` must NOT be consumed by the caller — this
        /// function reads it, scans for the closing quote, and
        /// pushes a `&'p str` leaf via the builder. The borrow
        /// path slices the input directly; the cold escape
        /// path decodes into the builder's arena and emits
        /// the decoded bytes via the same `push_leaf_with_str`
        /// surface.
        ///
        /// AZ-IV.W3.6 — Cursor parameter is threaded for signature
        /// uniformity; string is a leaf (no recursion), so the
        /// cursor is not consulted in the body.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            _state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty<'p>,
            #cursor_p,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            #cursor_where,
        {
            use crate::runtime::builder::StructBuilder as _;
            let open = *p;
            if input.get(open).copied() != Some(b'"') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: open as u32,
                });
            }
            let body_start = open + 1;
            let tail = match input.get(body_start..) {
                Some(t) => t,
                None => {
                    return Err(crate::runtime::DtaError::UnexpectedEnd {
                        offset: open as u32,
                    });
                }
            };
            match #support_mod::first_quote_or_backslash(tail) {
                Some((off, b'"')) => {
                    let end = body_start + off;
                    #refused_check_borrow
                    *p = end + 1;
                    // Borrow path: slice directly from the
                    // input. The `'p` lifetime threads through
                    // the parse-fn signature so the slice's
                    // lifetime matches the builder's arena.
                    // For `: Span`-typed rules the slice spans
                    // the full quoted region (`open..end+1`);
                    // decoder-typed rules slice the inner body.
                    let body: &'p str = unsafe {
                        ::core::str::from_utf8_unchecked(
                            &input[#borrow_start_expr..#borrow_end_expr],
                        )
                    };
                    builder.push_leaf_with_str(body);
                    Ok(())
                }
                Some((_off, b'\\')) => {
                    // AZ-I.W2.RC — cold escape path. Routes
                    // through `decode_json_string_to_arena`
                    // against a per-call `Vec<u8>`; the decoded
                    // bytes leak to `&'static str` to satisfy
                    // the `push_leaf_with_str` lifetime
                    // contract pending W2.A byte-arena
                    // substrate extension. Borrow-path fast
                    // case (escape-free strings, ≥95% of
                    // real-world JSON inputs) is sound and
                    // allocation-free.
                    let mut buf: Vec<u8> = Vec::with_capacity(
                        input.len().saturating_sub(open + 1),
                    );
                    match ::parse_that::parsers::scan::decode_json_string_to_arena(
                        input, open, &mut buf,
                    ) {
                        Some((
                            ::parse_that::parsers::scan::StringPayload::Owned { .. },
                            end_pos,
                        )) => {
                            #refused_check_escape
                            *p = end_pos;
                            let bytes: Box<[u8]> = buf.into_boxed_slice();
                            let leaked: &'static [u8] = Box::leak(bytes);
                            let leaked_str: &'static str = unsafe {
                                ::core::str::from_utf8_unchecked(leaked)
                            };
                            builder.push_leaf_with_str(leaked_str);
                            Ok(())
                        }
                        Some((
                            ::parse_that::parsers::scan::StringPayload::Borrowed {
                                start, end,
                            },
                            end_pos,
                        )) => {
                            #refused_check_escape
                            *p = end_pos;
                            let body: &'p str = unsafe {
                                ::core::str::from_utf8_unchecked(
                                    #cold_borrowed_slice,
                                )
                            };
                            builder.push_leaf_with_str(body);
                            Ok(())
                        }
                        None => Err(crate::runtime::DtaError::Syntax {
                            offset: open as u32,
                        }),
                    }
                }
                Some(_) => unreachable!(),
                None => Err(crate::runtime::DtaError::UnexpectedEnd {
                    offset: open as u32,
                }),
            }
        }
    }
}
