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

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};
use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_string_<grammar>_<rule>(input, p, state, builder)
/// -> Result<(), DtaError>`.
///
/// AZ-I.W2.RC / AZ-II.cutover.O4 — emits the struct-builder body.
/// The borrow path slices `input[body_start..end]` with the parse-fn
/// lifetime; the cold escape path decodes and emits through the same
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
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            _state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty<'p>,
        ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
            use crate::runtime::builder::StructBuilder as _;
            let open = *p;
            if input.get(open).copied() != Some(b'"') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: open as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let body_start = open + 1;
            let tail = match input.get(body_start..) {
                Some(t) => t,
                None => {
                    return Err(crate::runtime::tape::DtaError::UnexpectedEnd {
                        offset: open as u32,
                    });
                }
            };
            match #support_mod::first_quote_or_backslash(tail) {
                Some((off, b'"')) => {
                    let end = body_start + off;
                    *p = end + 1;
                    // Borrow path: slice directly from the
                    // input. The `'p` lifetime threads through
                    // the parse-fn signature so the slice's
                    // lifetime matches the builder's arena.
                    let body: &'p str = unsafe {
                        ::core::str::from_utf8_unchecked(
                            &input[body_start..end],
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
                            *p = end_pos;
                            let body: &'p str = unsafe {
                                ::core::str::from_utf8_unchecked(
                                    &input[start as usize..end as usize],
                                )
                            };
                            builder.push_leaf_with_str(body);
                            Ok(())
                        }
                        None => Err(crate::runtime::tape::DtaError::Syntax {
                            offset: open as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        }),
                    }
                }
                Some(_) => unreachable!(),
                None => Err(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: open as u32,
                }),
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W3-bench-fix — visitor-path String emitter.
//
// Mirrors the prototype's
// `json_prototype::string::parse_string_body::<V>`. Borrow-path
// fast: `visitor.string(&input[body_start..end])` / `visitor.key(...)`
// without tape / arena copy. Escape-path cold: decodes into a local
// buffer and emits `visitor.string(&buf)` / `visitor.key(&buf)`.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_string_visitor_<grammar>_<rule><V: JsonVisitor>(
/// input, p, state, visitor, is_key) -> Result<(), ParseErr>`.
///
/// `is_key: bool` selects between `visitor.string(bytes)` and
/// `visitor.key(bytes)` — sonic-rs's `visit_borrowed_str` /
/// `visit_borrowed_key` split done at the call site.
pub fn emit_parse_string_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("string", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let escaped_fn = format_ident!("parse_string_visitor_escaped_{}", grammar_suffix);

    quote! {
        /// AW-V.W3-bench-fix — visitor-path String-shape parse function.
        ///
        /// Mirrors `json_prototype::string::parse_string_body::<V>`.
        /// `"` must NOT be consumed by the caller. Borrow-path reads
        /// the full span from input; escape-path decodes into a local
        /// buffer.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            _state: &mut #support_mod::ScanState,
            visitor: &mut V,
            is_key: bool,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::StringVisitor
                + crate::runtime::tape::ObjectVisitor,
        {
            let open = *p;
            if input.get(open).copied() != Some(b'"') {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32, rule: None,
                });
            }
            let body_start = open + 1;
            let tail = match input.get(body_start..) {
                Some(t) => t,
                None => {
                    return Err(crate::runtime::ParseErr::Syntax {
                        offset: open as u32, rule: None,
                    });
                }
            };
            match #support_mod::first_quote_or_backslash(tail) {
                Some((off, b'"')) => {
                    let end = body_start + off;
                    let body = &input[body_start..end];
                    *p = end + 1;
                    if is_key {
                        visitor.key(body)
                            .map_err(|_| crate::runtime::ParseErr::Syntax {
                                offset: open as u32, rule: None,
                            })
                    } else {
                        visitor.string(body)
                            .map_err(|_| crate::runtime::ParseErr::Syntax {
                                offset: open as u32, rule: None,
                            })
                    }
                }
                Some((off, b'\\')) => {
                    let esc_start = body_start + off;
                    #escaped_fn(input, p, body_start, esc_start, visitor, is_key, open)
                }
                Some(_) => unreachable!(),
                None => Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32, rule: None,
                }),
            }
        }
    }
}

/// Pre-emitted visitor-path escape helper fn — one per grammar.
///
/// AY.W4.1 — routes through
/// [`parse_that::parsers::scan::decode_json_string_to_arena`] using a
/// stack-local `Vec<u8>` buffer as the arena, then dispatches to
/// `visitor.string` / `visitor.key` with the decoded bytes. The
/// kernel performs SIMD `u8x16` scan + stripe-copy escape decoding;
/// the previous body walked input scalar-byte-by-byte.
pub fn emit_visitor_escape_helper(grammar_suffix: &str) -> TokenStream {
    let helper_ident = format_ident!("parse_string_visitor_escaped_{}", grammar_suffix);
    quote! {
        /// AY.W4.1 — visitor-path escape decoder. Cold path.
        ///
        /// Reuses the SIMD-fused `decode_json_string_to_arena` kernel
        /// against a stack-local `Vec<u8>` buffer; the borrow / owned
        /// dispatch is unified at the visitor call site.
        #[cold]
        #[inline(never)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        fn #helper_ident<V>(
            input: &[u8],
            p: &mut usize,
            body_start: usize,
            _esc_start: usize,
            visitor: &mut V,
            is_key: bool,
            open: usize,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::StringVisitor
                + crate::runtime::tape::ObjectVisitor,
        {
            let mut buf: Vec<u8> = Vec::with_capacity(
                input.len().saturating_sub(body_start),
            );
            match ::parse_that::parsers::scan::decode_json_string_to_arena(
                input, open, &mut buf,
            ) {
                Some((
                    ::parse_that::parsers::scan::StringPayload::Owned { .. },
                    end_pos,
                )) => {
                    *p = end_pos;
                    if is_key {
                        visitor.key(&buf).map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    } else {
                        visitor.string(&buf).map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    }
                }
                Some((
                    ::parse_that::parsers::scan::StringPayload::Borrowed {
                        start,
                        end,
                    },
                    end_pos,
                )) => {
                    // Defensive: kernel saw no real backslashes; emit
                    // the borrowed body slice directly.
                    *p = end_pos;
                    let body = &input[start as usize..end as usize];
                    if is_key {
                        visitor.key(body).map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    } else {
                        visitor.string(body).map_err(|_| {
                            crate::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    }
                }
                None => Err(crate::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                }),
            }
        }
    }
}
