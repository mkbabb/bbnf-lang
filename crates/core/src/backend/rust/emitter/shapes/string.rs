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
//! 3. On a `"` hit: borrowed-string path — push a Span leaf with
//!    `STRING_BORROW_BIT` set; the tape reader slices bytes directly
//!    from the input.
//! 4. On a `\\` hit: cold path — `parse_string_escaped` routes
//!    through `parse_that::parsers::scan::decode_json_string_to_arena`
//!    (AY.W4.1; the kernel's SIMD `u8x16` scan + stripe-copy escape
//!    decoder). The tape's `payload_string` reader resolves via the
//!    arena's `(len: u32 LE, bytes: [u8; len])` frame.
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

/// Emit `pub fn parse_string_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_string(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("string", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    quote! {
        /// AW-V.W3.2 — per-grammar String-shape parse function.
        ///
        /// Mirrors `json_prototype::string::parse_string_body`.
        /// `"` must NOT be consumed by the caller — this function
        /// reads it, scans for the closing quote, and pushes a Span
        /// leaf with appropriate borrow / arena-decode metadata.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            _state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let open = *p;
            if input.get(open).copied() != Some(b'"') {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: open as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            let body_start = open + 1;
            let tail = match input.get(body_start..) {
                Some(t) => t,
                None => {
                    return Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                        offset: open as u32,
                    });
                }
            };
            match #support_mod::first_quote_or_backslash(tail) {
                Some((off, b'"')) => {
                    let end = body_start + off;
                    *p = end + 1;
                    let lo = open as u32;
                    let hi = *p as u32;
                    // Borrow path: Span leaf with STRING_BORROW_BIT set;
                    // the reader slices input[lo+1..hi-1] directly.
                    let leaf = builder.push_leaf_borrowed_string(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        lo,
                        hi,
                        #variant_idx,
                        0,
                    );
                    Ok(leaf)
                }
                Some((_off, b'\\')) => {
                    // Escape path — scan forward to the closing quote,
                    // accounting for escape sequences. The decode
                    // into the arena matches the AW-IV walker's
                    // JSON-string decode kernel contract so
                    // `tape.payload_string(rec)` resolves on reads.
                    parse_string_escaped(input, p, open, builder, #variant_idx)
                }
                Some(_) => unreachable!(),
                None => Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                    offset: open as u32,
                }),
            }
        }
    }
}

/// Pre-emitted escape-path helper fn — landed at module scope in the
/// support module so every String-shape fn can share it (the decode
/// kernel's body is identical per-grammar; only the `variant_idx`
/// tag varies per rule).
///
/// AY.W4.1 — slow-path decode now routes through
/// [`parse_that::parsers::scan::decode_json_string_to_arena`], which
/// is a fully-fused SIMD `u8x16` scan + `vst1q_u8`/`_mm_storeu_si128`
/// stripe-copy escape-decoder. The previous body walked input
/// scalar-byte-by-byte and dominated `decode_json_string_to_arena`
/// self-time on twitter at 4-6%. The replacement reuses the kernel
/// landed in AU.3.1 + extended in AV.4.3.
///
/// This function is referenced by the emitted shape fns; it lives
/// outside the dispatcher's `__shape_support_<grammar>` module to
/// keep the emitter token stream composition simple. Emit once per
/// compilation; no-op if already emitted.
pub fn emit_escape_helper(grammar_suffix: &str) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    quote! {
        /// AY.W4.1 — string escape-path decoder. Cold path; routes
        /// through `parse_that::parsers::scan::decode_json_string_to_arena`,
        /// a fully-SIMD scan + escape-decode kernel. The kernel
        /// internally does its own SIMD `u8x16` re-scan from `open`
        /// (a few dozen bytes of redundant work vs. the fast-path
        /// SIMD scan that already located the first `\\`), then
        /// performs SIMD-stride copies of escape-free runs between
        /// each escape sequence.
        ///
        /// The arena layout is `(len: u32 LE, bytes: [u8; len])` per
        /// `Tape::payload_string_bytes`'s contract. We reserve 4
        /// bytes for the length prefix before invoking the kernel,
        /// then back-stamp the decoded length once the kernel
        /// returns.
        ///
        /// In the rare case where the SIMD fast-path scan flagged a
        /// backslash that was actually escaped (impossible given
        /// odd-parity tracking but defensive), the kernel may return
        /// `StringPayload::Borrowed` — we rewind the reserved prefix
        /// bytes and push a borrow-safe leaf instead.
        #[cold]
        #[inline(never)]
        #[allow(non_snake_case)]
        fn parse_string_escaped(
            input: &[u8],
            p: &mut usize,
            open: usize,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
            variant_idx: u8,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let arena = builder.arena_mut();
            let frame_offset = arena.len() as u32;
            // Reserve the 4-byte length prefix; the kernel writes
            // body bytes directly after it.
            arena.extend_from_slice(&[0u8; 4]);
            let body_start_in_arena = arena.len();

            match ::parse_that::parsers::scan::decode_json_string_to_arena(
                input, open, arena,
            ) {
                Some((
                    ::parse_that::parsers::scan::StringPayload::Owned { .. },
                    end_pos,
                )) => {
                    *p = end_pos;
                    let arena_final = builder.arena_mut();
                    let decoded_len =
                        (arena_final.len() - body_start_in_arena) as u32;
                    let prefix = frame_offset as usize;
                    arena_final[prefix..prefix + 4]
                        .copy_from_slice(&decoded_len.to_le_bytes());
                    let lo = open as u32;
                    let hi = *p as u32;
                    let leaf = builder.push_leaf_with_arena_frame(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        lo,
                        hi,
                        variant_idx,
                        0,
                        frame_offset,
                    );
                    Ok(leaf)
                }
                Some((
                    ::parse_that::parsers::scan::StringPayload::Borrowed {
                        ..
                    },
                    end_pos,
                )) => {
                    // Defensive: kernel saw no real backslashes (the
                    // fast-path scan's parity tracker is permissive).
                    // Rewind the reserved prefix and emit a borrowed
                    // leaf; semantically equivalent to the fast path.
                    let arena_final = builder.arena_mut();
                    arena_final.truncate(frame_offset as usize);
                    *p = end_pos;
                    let leaf = builder.push_leaf_borrowed_string(
                        ::bbnf::runtime::tape::TapeKind::Span,
                        open as u32,
                        *p as u32,
                        variant_idx,
                        0,
                    );
                    Ok(leaf)
                }
                None => {
                    // Kernel rejected the input (unterminated, invalid
                    // escape, bad surrogate). Roll back the reserved
                    // prefix bytes the kernel may not have touched.
                    let arena_final = builder.arena_mut();
                    arena_final.truncate(frame_offset as usize);
                    Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: open as u32,
                        failing_state:
                            ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule:
                            ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    })
                }
            }
        }

        // Suppress unused-warnings for the support module reference —
        // the support module's helpers are consumed by the sibling
        // shape fns.
        #[allow(dead_code)]
        const __SHAPE_ESCAPE_HELPER_MARKER: () = {
            use #support_mod as _;
        };
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
    let escaped_fn =
        format_ident!("parse_string_visitor_escaped_{}", grammar_suffix);

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
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::ObjectVisitor,
        {
            let open = *p;
            if input.get(open).copied() != Some(b'"') {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: open as u32, rule: None,
                });
            }
            let body_start = open + 1;
            let tail = match input.get(body_start..) {
                Some(t) => t,
                None => {
                    return Err(::bbnf::runtime::ParseErr::Syntax {
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
                            .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32, rule: None,
                            })
                    } else {
                        visitor.string(body)
                            .map_err(|_| ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32, rule: None,
                            })
                    }
                }
                Some((off, b'\\')) => {
                    let esc_start = body_start + off;
                    #escaped_fn(input, p, body_start, esc_start, visitor, is_key, open)
                }
                Some(_) => unreachable!(),
                None => Err(::bbnf::runtime::ParseErr::Syntax {
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
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let helper_ident =
        format_ident!("parse_string_visitor_escaped_{}", grammar_suffix);
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
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::StringVisitor
                + ::bbnf::runtime::tape::ObjectVisitor,
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
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    } else {
                        visitor.string(&buf).map_err(|_| {
                            ::bbnf::runtime::ParseErr::Syntax {
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
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    } else {
                        visitor.string(body).map_err(|_| {
                            ::bbnf::runtime::ParseErr::Syntax {
                                offset: open as u32,
                                rule: None,
                            }
                        })
                    }
                }
                None => Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: open as u32,
                    rule: None,
                }),
            }
        }

        // Suppress unused-warning for the support module reference —
        // sibling visitor-path fns already consume it.
        #[allow(dead_code)]
        const __VISITOR_ESCAPE_HELPER_MARKER: () = {
            use #support_mod as _;
        };
    }
}
