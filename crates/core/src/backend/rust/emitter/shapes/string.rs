//! String-shape emitter — `parse_string_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar String-shape parse function mirroring
//! `bbnf_json_prototype::parse_string` (crates/bbnf-json-prototype/
//! src/lib.rs:351) + `bbnf_json_prototype::string::parse_string_body`.
//!
//! The emitted function:
//!
//! 1. Expects `input[*p] == b'"'` (caller's dispatcher gate).
//! 2. Fast path: scan for first `"` or `\\` via the per-grammar
//!    `first_quote_or_backslash` SIMD primitive (support module).
//! 3. On a `"` hit: borrowed-string path — push a Span leaf with
//!    `STRING_BORROW_BIT` set; the tape reader slices bytes directly
//!    from the input.
//! 4. On a `\\` hit: cold path — emit a borrowed-string push with the
//!    full span (including trailing quote). The tape's
//!    `payload_string_with_source` reads via arena, but the prototype
//!    defers the decode to a visitor side call; for the shape emitter
//!    we match the AW-IV walker's decode-into-arena path so the
//!    bench's `payload_string` accessor resolves.
//!
//! The `-> decode_json_string_to_arena(input) : String` annotation
//! on JSON's `string` rule means every string leaf must either
//! borrow (no escapes) or be arena-decoded. We check for presence of
//! backslashes via the SIMD scan; the borrow path is the fast case
//! (~90% of real-world inputs).

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::shape_fn_ident;

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
        /// Mirrors `bbnf_json_prototype::string::parse_string_body`.
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
/// This function is referenced by the emitted shape fns; it lives
/// outside the dispatcher's `__shape_support_<grammar>` module to
/// keep the emitter token stream composition simple. Emit once per
/// compilation; no-op if already emitted.
pub fn emit_escape_helper(grammar_suffix: &str) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    quote! {
        /// AW-V.W3.2 — string escape-path decoder. Cold path; decodes
        /// RFC 8259 escape sequences into the tape's arena and pushes
        /// a Span leaf with `PAYLOAD_IN_ARENA_BIT` set so the reader
        /// resolves via `tape.payload_string`.
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
            let body_start = open + 1;
            // Reserve arena space: 4-byte length prefix + worst-case
            // body size (every escape shrinks output).
            let max_body = input.len().saturating_sub(body_start);
            let arena = builder.arena_mut();
            let arena_offset = arena.len() as u32;
            arena.extend_from_slice(&(0u32).to_le_bytes());
            let decoded_start = arena.len();
            arena.reserve(max_body);

            let mut cur = body_start;
            while cur < input.len() {
                let b = input[cur];
                match b {
                    b'"' => {
                        *p = cur + 1;
                        let arena_final = builder.arena_mut();
                        let decoded_len = (arena_final.len() - decoded_start) as u32;
                        // Stamp the length prefix.
                        let start = arena_offset as usize;
                        arena_final[start..start + 4]
                            .copy_from_slice(&decoded_len.to_le_bytes());
                        let lo = open as u32;
                        let hi = *p as u32;
                        // Push a Span leaf with PAYLOAD_IN_ARENA_BIT
                        // via `push_leaf_with_arena_frame`.
                        let leaf = builder.push_leaf_with_arena_frame(
                            ::bbnf::runtime::tape::TapeKind::Span,
                            lo,
                            hi,
                            variant_idx,
                            0,
                            arena_offset,
                        );
                        return Ok(leaf);
                    }
                    b'\\' => {
                        if cur + 1 >= input.len() {
                            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: cur as u32,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        }
                        let esc = input[cur + 1];
                        let arena = builder.arena_mut();
                        match esc {
                            b'"' => arena.push(b'"'),
                            b'\\' => arena.push(b'\\'),
                            b'/' => arena.push(b'/'),
                            b'b' => arena.push(0x08),
                            b'f' => arena.push(0x0C),
                            b'n' => arena.push(b'\n'),
                            b'r' => arena.push(b'\r'),
                            b't' => arena.push(b'\t'),
                            b'u' => {
                                if cur + 6 > input.len() {
                                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                        offset: cur as u32,
                                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                }
                                let cp = parse_u4(&input[cur + 2..cur + 6]).ok_or(
                                    ::bbnf::runtime::tape::DtaError::Syntax {
                                        offset: cur as u32,
                                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                    },
                                )?;
                                let (code, consumed) = if (0xD800..=0xDBFF).contains(&cp) {
                                    if cur + 12 > input.len()
                                        || input[cur + 6] != b'\\'
                                        || input[cur + 7] != b'u'
                                    {
                                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                            offset: cur as u32,
                                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                        });
                                    }
                                    let lo = parse_u4(&input[cur + 8..cur + 12]).ok_or(
                                        ::bbnf::runtime::tape::DtaError::Syntax {
                                            offset: cur as u32,
                                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                        },
                                    )?;
                                    if !(0xDC00..=0xDFFF).contains(&lo) {
                                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                            offset: cur as u32,
                                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                        });
                                    }
                                    (
                                        0x10000
                                            + (((cp - 0xD800) as u32) << 10)
                                            + (lo - 0xDC00) as u32,
                                        12,
                                    )
                                } else if (0xDC00..=0xDFFF).contains(&cp) {
                                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                        offset: cur as u32,
                                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                    });
                                } else {
                                    (cp as u32, 6)
                                };
                                encode_utf8_to(arena, code);
                                cur += consumed;
                                continue;
                            }
                            _ => {
                                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                    offset: cur as u32,
                                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                                });
                            }
                        }
                        cur += 2;
                    }
                    _ => {
                        let arena = builder.arena_mut();
                        arena.push(b);
                        cur += 1;
                    }
                }
            }
            Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                offset: open as u32,
            })
        }

        /// Parse a 4-hex-digit sequence into a `u16`.
        #[inline]
        #[allow(non_snake_case)]
        fn parse_u4(bytes: &[u8]) -> ::core::option::Option<u16> {
            if bytes.len() != 4 { return None; }
            let mut out: u16 = 0;
            for &b in bytes {
                let v = match b {
                    b'0'..=b'9' => b - b'0',
                    b'a'..=b'f' => b - b'a' + 10,
                    b'A'..=b'F' => b - b'A' + 10,
                    _ => return None,
                };
                out = out.wrapping_shl(4) | (v as u16);
            }
            Some(out)
        }

        /// Encode a Unicode code point as UTF-8.
        #[inline]
        #[allow(non_snake_case)]
        fn encode_utf8_to(buf: &mut Vec<u8>, code: u32) {
            if code < 0x80 { buf.push(code as u8); }
            else if code < 0x800 {
                buf.push(0xC0 | (code >> 6) as u8);
                buf.push(0x80 | (code & 0x3F) as u8);
            } else if code < 0x10000 {
                buf.push(0xE0 | (code >> 12) as u8);
                buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
                buf.push(0x80 | (code & 0x3F) as u8);
            } else {
                buf.push(0xF0 | (code >> 18) as u8);
                buf.push(0x80 | ((code >> 12) & 0x3F) as u8);
                buf.push(0x80 | ((code >> 6) & 0x3F) as u8);
                buf.push(0x80 | (code & 0x3F) as u8);
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
