//! HRegex-shape emitter — `parse_hregex_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar HRegex-shape parse functions for regex leaves
//! whose classification is NOT QuotedString / Numeric (those are
//! String / Number shapes). Canonical:
//!
//! - CSS `ident = /[a-zA-Z_][\w-]*/` — a bare Identifier-class regex
//!   leaf.
//! - Sheets `cell_ref = /\$?[A-Za-z]{1,3}\$?\d+/ -> input : Span`.
//! - Sheets `identifier = /[A-Za-z_][A-Za-z0-9_.]*/ -> input : Span`.
//! - BBNF `identifier = /[_a-zA-Z][_a-zA-Z0-9-]*/ -> Span`.
//!
//! # Emission shape
//!
//! The emitted function:
//!
//! 1. Captures `span_lo` at `*p`.
//! 2. Calls the per-grammar regex-scan adapter
//!    (`__regex_scan_<grammar>`) with the rule's pattern string; the
//!    adapter returns `Some(match_len)` on success, `None` on no-match.
//! 3. Advances `*p` by `match_len`.
//! 4. Pushes a `TapeKind::Regex` leaf carrying a `PayloadData::Span`
//!    (default `-> input : Span`) or the rule's decoded scalar payload
//!    when the rule declares `-> host_fn(input) : type`.
//!
//! # Host decode
//!
//! Rules with `-> <fn>(input) : <type>` annotations (e.g. CSS `hex ->
//! parse_hex_color(input) : u32`) emit a call to the declared host
//! function post-scan and package the return as a `PayloadData`
//! variant matching the type. The AW-V emitter honours the same
//! contract as the walker's `DtaState::Regex` arm: when a decoder
//! selector is attached, the arena slot is pre-allocated up front.
//!
//! W4.1 emitted scaffolding; W4-fix ships the walker-parity body
//! that invokes the regex scanner and pushes a Span leaf.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dta_walker::regex_scan_adapter_ident;
use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};
use super::sanitise_grammar;

/// AX.W0a.2.q — resolve the typed-payload emission shape for an
/// HRegex rule whose body is `Map { Regex, <FnDescriptor> }`.
///
/// Mirrors the flat.rs `emit_map_regex_host_fn` pattern, but adapted
/// for HRegex's shape-fn signature (no leading `first_byte`) and the
/// rule-level payload policy: owner-rule-type decides KvPair vs Span;
/// rules typed `F64` write 8 LE bytes; `U32` rules (host-fn decode)
/// write 4 LE bytes; `U8` discriminant rules write 1 byte.
///
/// Returns `Some(body_tokens)` describing the typed-payload emission
/// (scan → decode → arena push), or `None` when the rule is a plain
/// regex scan with no typed host decode.
fn hregex_typed_payload_body(
    rule: &IrRule,
    variant_idx: u8,
    pattern_lit: &str,
    regex_scan_ident: &proc_macro2::Ident,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};
    // Walk the body: we want a `Map { fn_id }` wrapping a `Regex`.
    fn extract_map_fn(node: &IrNode) -> Option<u32> {
        match node {
            IrNode::Map { fn_id, inner } => match inner.as_ref() {
                IrNode::Regex(_) => Some(*fn_id),
                _ => None,
            },
            IrNode::OptionalWhitespace(inner) => extract_map_fn(inner),
            _ => None,
        }
    }
    let fn_id = extract_map_fn(&rule.body)?;
    let fd = ir.fns.get(fn_id as usize)?;

    // Rule type drives leaf kind (KvPair for Tuple-shaped; Span
    // otherwise) — mirrors flat.rs `emit_map_regex_host_fn` policy so
    // walker-parity readers see a consistent kind shape.
    let kind_is_kv = matches!(
        ir.types.iter().find_map(|(rid, t)| {
            if *rid == rule.id {
                Some(t)
            } else {
                None
            }
        }),
        Some(TypeDesc::Tuple(fields)) if matches!(
            fields.as_slice(),
            [TypeDesc::Span, value] if value.is_scalar_payload()
        )
    );
    let leaf_kind = if kind_is_kv {
        quote! { ::bbnf::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { ::bbnf::runtime::tape::TapeKind::Span }
    };

    match fd {
        FnDescriptor::NumberConvert { allow_leading_dot: _ } => Some(quote! {
            {
                let span_lo = *p as u32;
                let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: span_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                };
                *p += match_len as usize;
                let span_hi = *p as u32;
                let __f64: f64 = core::str::from_utf8(
                    &input[span_lo as usize..span_hi as usize]
                )
                .ok()
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or(0.0);
                let __arena_off: u32 = builder.arena_mut().len() as u32;
                builder.arena_mut().extend_from_slice(&__f64.to_le_bytes());
                let leaf_off = builder.push_leaf_with_arena_payload(
                    #leaf_kind,
                    span_lo,
                    span_hi,
                    #variant_idx,
                    0u8,
                    __arena_off,
                    8u32,
                );
                Ok(leaf_off)
            }
        }),
        FnDescriptor::HexConvert { fn_path } => {
            let path_str = ir.get_string(*fn_path);
            let path: syn::Path = syn::parse_str(path_str).ok()?;
            Some(quote! {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: span_lo,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    let __decoded_u32: u32 = #path(
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                    let __arena_off: u32 = builder.arena_mut().len() as u32;
                    builder.arena_mut().extend_from_slice(&__decoded_u32.to_le_bytes());
                    let leaf_off = builder.push_leaf_with_arena_payload(
                        #leaf_kind,
                        span_lo,
                        span_hi,
                        #variant_idx,
                        0u8,
                        __arena_off,
                        4u32,
                    );
                    Ok(leaf_off)
                }
            })
        }
        FnDescriptor::Expr { expr, return_type } => {
            use bbnf_ir::{MapExpr, TypeDesc};
            // AX.W0a.2.q — `-> f64` / `-> u32` shorthand syntax lowers
            // to `Expr { expr: Input, return_type: Some(F64|U32) }`
            // (not the internal NumberConvert / HexConvert
            // specialisations, which only fire for specific inner IR
            // shapes). When the expression is the transparent `input`
            // identity AND the return_type is a known scalar payload,
            // emit the scan + `str::parse`-backed decode into an arena
            // payload of the matching width.
            //
            // Sheets `number = /regex/ -> f64` canonically lands here —
            // the lifter binds `Map { Regex, Expr { Input, return_type:
            // F64 } }`, and `typed_f64_payloads` asserts the 8-byte
            // arena payload reaches the tape.
            if !matches!(expr, MapExpr::Input) {
                return None;
            }
            match return_type {
                Some(TypeDesc::F64) => Some(quote! {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let __f64: f64 = core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        )
                        .ok()
                        .and_then(|s| s.parse::<f64>().ok())
                        .unwrap_or(0.0);
                        let __arena_off: u32 = builder.arena_mut().len() as u32;
                        builder.arena_mut().extend_from_slice(&__f64.to_le_bytes());
                        let leaf_off = builder.push_leaf_with_arena_payload(
                            #leaf_kind,
                            span_lo,
                            span_hi,
                            #variant_idx,
                            0u8,
                            __arena_off,
                            8u32,
                        );
                        Ok(leaf_off)
                    }
                }),
                Some(TypeDesc::U32) => Some(quote! {
                    {
                        let span_lo = *p as u32;
                        let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                            return Err(::bbnf::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                                failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                            });
                        };
                        *p += match_len as usize;
                        let span_hi = *p as u32;
                        let __u32: u32 = core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        )
                        .ok()
                        .and_then(|s| s.parse::<u32>().ok())
                        .unwrap_or(0);
                        let __arena_off: u32 = builder.arena_mut().len() as u32;
                        builder.arena_mut().extend_from_slice(&__u32.to_le_bytes());
                        let leaf_off = builder.push_leaf_with_arena_payload(
                            #leaf_kind,
                            span_lo,
                            span_hi,
                            #variant_idx,
                            0u8,
                            __arena_off,
                            4u32,
                        );
                        Ok(leaf_off)
                    }
                }),
                _ => None,
            }
        }
        FnDescriptor::EnumWrap { .. }
        | FnDescriptor::BoxWrap
        | FnDescriptor::SpanCapture => None,
    }
}

/// Emit `pub fn parse_hregex_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_hregex(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("hregex", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    // Extract the regex pattern from the rule body. HRegex admits a
    // single Regex leaf (after Map / OW stripping); when the body
    // is a different shape, emit a minimal scaffold that returns an
    // error — the detector gate is upstream.
    let pattern_sid = extract_regex_pattern(&rule.body);
    let Some(sid) = pattern_sid else {
        return emit_unsupported_stub(&fn_ident, &support_mod, variant_idx);
    };
    let pattern = ir.get_string(sid);
    let pattern_lit = pattern.to_string();

    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    // AX.W0a.2.q — typed-payload body. When the HRegex rule body is
    // `Map { Regex, NumberConvert }` / `Map { Regex, HexConvert }`,
    // emit the scan + decode + arena-payload push so downstream
    // typed-payload readers (`typed_f64_payloads`,
    // `payload_scalar::<u32>`) observe the decoded value. Rules with
    // `Map { Regex, Expr(Input) }` annotations (`-> input : Span`)
    // fall through to the raw Span emission — no typed payload
    // expected.
    if let Some(body) =
        hregex_typed_payload_body(rule, variant_idx, &pattern_lit, &regex_scan_ident, ir)
    {
        return quote! {
            /// AX.W0a.2.q — HRegex-shape parse function with typed
            /// host-fn decode (`NumberConvert` → f64, `HexConvert` → u32).
            ///
            /// Runs the per-grammar regex scan, invokes the decoder,
            /// writes the decoded bytes into the tape arena, pushes a
            /// payload-carrying leaf (KvPair when the rule projects as
            /// `Tuple([Span, scalar])`; Span otherwise) so the walker-
            /// parity reader (`payload_bytes(rec, N)`) finds the value.
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut ::bbnf::runtime::tape::TapeBuilder,
            ) -> ::core::result::Result<
                ::bbnf::runtime::tape::TapeOffset,
                ::bbnf::runtime::tape::DtaError,
            > {
                #body
            }
        };
    }

    quote! {
        /// AW-V.W4-fix — per-grammar HRegex-shape parse function.
        ///
        /// Regex scan via the per-grammar adapter; emits a
        /// `TapeKind::Regex` leaf carrying the matched span. Decoder
        /// hooks (host_fn payloads) are wired at the dispatcher level
        /// post-scan; the raw Span-leaf path is the default.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: span_lo,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            let leaf_off = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );
            Ok(leaf_off)
        }
    }
}

/// Emit a defensive stub when the rule body is not a single Regex
/// leaf. The HRegex detector should gate admission upstream; this
/// branch exists only to keep the emitter output compilable when the
/// emitter is invoked over a misclassified rule.
fn emit_unsupported_stub(
    fn_ident: &proc_macro2::Ident,
    support_mod: &proc_macro2::Ident,
    variant_idx: u8,
) -> TokenStream {
    quote! {
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            let _ = state;
            let span_lo = *p as u32;
            let span_hi = *p as u32;
            let leaf_off = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );
            Ok(leaf_off)
        }
    }
}

/// Extract the regex pattern's `StringId` from the rule body, walking
/// through Map / OptionalWhitespace trivia wrappers.
fn extract_regex_pattern(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Regex(sid) => Some(*sid),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            extract_regex_pattern(inner)
        }
        _ => None,
    }
}

// ─────────────────────────────────────────────────────────────────────
// AW-V.W4-fix — visitor-path HRegex emitter.
// ─────────────────────────────────────────────────────────────────────

/// Emit `pub fn parse_hregex_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_hregex_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("hregex", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let pattern_sid = extract_regex_pattern(&rule.body);
    let Some(sid) = pattern_sid else {
        return quote! {
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident<V>(
                input: &[u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                visitor: &mut V,
            ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
            where
                V: ::bbnf::runtime::tape::StringVisitor,
            {
                let _ = (input, p, state, visitor);
                Ok(())
            }
        };
    };
    let pattern = ir.get_string(sid);
    let pattern_lit = pattern.to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    quote! {
        /// AW-V.W4-fix — visitor-path HRegex-shape parse function.
        ///
        /// Regex scan via the per-grammar adapter; fires the
        /// visitor's `string()` event with the matched span when
        /// visitor is a StringVisitor. Non-string decoders (host_fn
        /// payloads) dispatch at the per-grammar consumer wave.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), ::bbnf::runtime::ParseErr>
        where
            V: ::bbnf::runtime::tape::StringVisitor,
        {
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(::bbnf::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                });
            };
            let span_hi = *p + match_len as usize;
            *p = span_hi;
            visitor.string(&input[span_lo..span_hi]).map_err(|_| {
                ::bbnf::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                }
            })
        }
    }
}
