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

use super::super::dfa_codegen::regex_scan_adapter_ident;
use super::dispatcher::{shape_fn_ident, visitor_shape_fn_ident};
use super::sanitise_grammar;
use bbnf_ir::registry::EmitStrategy;

/// AX.W0a.2.q — resolve the typed-payload emission shape for an
/// HRegex rule whose body is `Map { Regex, <FnDescriptor> }`.
///
/// Applies the whole-rule typed-payload pattern to HRegex's shape-fn
/// signature (no leading `first_byte`) and the
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
    // otherwise) — mirrors the HRegex typed-payload policy so
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
        quote! { crate::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { crate::runtime::tape::TapeKind::Span }
    };

    match fd {
        FnDescriptor::NumberConvert {
            allow_leading_dot: _,
        } => Some(quote! {
            {
                let span_lo = *p as u32;
                let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: span_lo,
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
                        return Err(crate::runtime::DtaError::Syntax {
                            offset: span_lo,
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
                            return Err(crate::runtime::DtaError::Syntax {
                                offset: span_lo,
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
                            return Err(crate::runtime::DtaError::Syntax {
                                offset: span_lo,
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
        FnDescriptor::EnumWrap { .. } | FnDescriptor::BoxWrap | FnDescriptor::SpanCapture => None,
    }
}

/// Emit `pub fn parse_hregex_<grammar>_<rule>(input, p, state,
/// builder) -> Result<(), DtaError>`.
///
/// # AZ-I.W2.RE — strategy gate
///
/// `strategy` is the codegen-time substrate selector resolved by
/// [`EmitStrategy::for_grammar`] in `shapes/mod.rs`. HRegex-shape
/// rules are tape-only in W2; JSON does not exercise this shape. On
/// [`EmitStrategy::StructDirect`] this emitter panics at codegen time
/// (unreachable assertion preventing silent codegen drift; W3 / W2.B
/// extend the per-shape struct-direct path before activating
/// StructDirect for any grammar that exercises this shape).
pub fn emit_parse_hregex(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    if matches!(strategy, EmitStrategy::StructDirect { .. }) {
        // AZ-I.W2-act.B3 — HRegex struct-direct body. The W2.RE panic
        // retires by surfacing a real body that scans, decodes per
        // host fn, and pushes via the StructBuilder trait. The body
        // is grammar-general — the SubstrateBinding's builder type
        // splices via `super::substrate::builder_ty_with_lifetime`.
        return emit_parse_hregex_struct_direct(strategy, grammar_suffix, rule, ir);
    }
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
                builder: &mut crate::runtime::tape::Tape<()>,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
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
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: span_lo,
                });
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            let leaf_off = builder.push_leaf_with(
                crate::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                #variant_idx,
                0,
                crate::runtime::tape::PayloadData::None,
            );
            Ok(leaf_off)
        }
    }
}

/// AX.W0a.2.q — true iff the Number-classified rule's pattern carries
/// `allow_leading_dot` in its `RegexClass::Numeric` classification.
/// CSS's `number` canonically lands here (admits `.5`); JSON's does
/// not. Controls the shape-dispatch routing in `mod.rs`: lenient
/// numbers emit via [`emit_parse_number_via_hregex`] below so the raw
/// regex-scan path admits leading-dot literals, where the default
/// Number emitter's inline scanner requires at least one integer
/// digit before the dot.
pub fn number_rule_allows_leading_dot(rule: &IrRule, ir: &GrammarIR) -> bool {
    use parse_that::regex::classify::RegexClass;
    let Some(sid) = extract_regex_pattern(&rule.body) else {
        return false;
    };
    let Some(info) = ir.regex_info.get(&sid) else {
        return false;
    };
    matches!(
        info.classification,
        RegexClass::Numeric {
            allow_leading_dot: true,
            ..
        }
    )
}

/// AX.W0a.2.q — emit a `parse_number_<grammar>_<rule>` fn using the
/// per-grammar regex-scan adapter + typed-f64 arena payload, keeping
/// the Number-shape caller signature (`input, p, first_byte,
/// builder`). Used when the rule's regex classifier admits leading-
/// dot literals (CSS `.5`-compatible number grammar) — the default
/// Number emitter's inline scanner rejects leading-dot, so CSS's
/// `opacity: .5` / `width: .5px` parses must route through the
/// pattern-aware regex-scan path instead.
///
/// The emitted body:
///   1. Scans via `__regex_scan_<grammar>` with the rule's pattern.
///   2. On success, decodes the matched bytes to `f64` via
///      `str::parse::<f64>()` (consistent with the HRegex NumberConvert
///      arm above).
///   3. Writes 8 LE bytes into the arena and pushes a payload-carrying
///      leaf (Span leaf + `PAYLOAD_IN_ARENA_BIT` so
///      `payload_wide::<f64>` / `payload_bytes(rec, 8)` reads the
///      decoded value).
///
/// The emitted fn name is `parse_number_<grammar>_<rule>` — the
/// Number-shape naming convention — so every existing call site
/// (`emit_ref_call_shape`, Wrap dispatch, AltDispatch dispatch)
/// resolves unchanged. `first_byte` is ignored (regex-scan doesn't
/// need the pre-read byte); retained so the caller's emission stays
/// identical.
pub fn emit_parse_number_via_hregex(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    if matches!(strategy, EmitStrategy::StructDirect { .. }) {
        // AZ-I.W2-act.B3 — Number-via-HRegex struct-direct body. CSS
        // L4's `number` rule lands here (admits leading-dot literals
        // per its regex classification).
        return emit_parse_number_via_hregex_struct_direct(strategy, grammar_suffix, rule, ir);
    }
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("number", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let _ = support_mod; // `state` parameter is absent on Number sigs.

    let Some(sid) = extract_regex_pattern(&rule.body) else {
        // Shouldn't reach here — the routing gate above checks the
        // regex classification, which implies the body resolves to a
        // Regex. Emit a minimal scaffold for safety.
        return quote! {
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident(
                input: &[u8],
                p: &mut usize,
                first_byte: u8,
                builder: &mut crate::runtime::tape::Tape<()>,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let _ = (input, p, first_byte, builder);
                Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                })
            }
        };
    };
    let pattern_lit = ir.get_string(sid).to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    // Leaf-kind policy: owner-rule `F64` ⇒ Span (walker-parity for
    // JSON-style numbers); `Tuple([Span, scalar])` ⇒ KvPair; every
    // other typed form ⇒ Span.
    use bbnf_ir::TypeDesc;
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
        quote! { crate::runtime::tape::TapeKind::KvPair }
    } else {
        quote! { crate::runtime::tape::TapeKind::Span }
    };

    quote! {
        /// AX.W0a.2.q — Number-shape parse function routed through the
        /// per-grammar regex-scan adapter so leading-dot literals
        /// (`.5`) admitted by the rule's regex classification parse
        /// without the JSON-strict "digit before dot" rejection the
        /// default Number emitter enforces.
        ///
        /// Writes 8 LE f64 bytes into the tape arena; the leaf kind
        /// mirrors the HRegex typed-payload policy (KvPair when the
        /// rule projects as `Tuple([Span, scalar])`; Span otherwise).
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            first_byte: u8,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let _ = first_byte;
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: span_lo,
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
    }
}

/// AX.W0a.2.q — visitor-path mirror of
/// [`emit_parse_number_via_hregex`] with the Number visitor
/// signature (`input, p, first_byte, visitor`). Emits
/// `parse_number_visitor_<grammar>_<rule>` so the visitor dispatcher
/// continues to route via the Number-shape naming convention. Fires
/// `visitor.number_f64(value)` with the decoded f64.
pub fn emit_parse_number_visitor_via_hregex(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let _ = strategy;
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("number", grammar_suffix, rule_name);

    let Some(sid) = extract_regex_pattern(&rule.body) else {
        return quote! {
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident<V>(
                input: &[u8],
                p: &mut usize,
                first_byte: u8,
                visitor: &mut V,
            ) -> ::core::result::Result<(), crate::runtime::ParseErr>
            where
                V: crate::runtime::tape::NumberVisitor,
            {
                let _ = (input, p, first_byte, visitor);
                Err(crate::runtime::ParseErr::Syntax {
                    offset: *p as u32, rule: None,
                })
            }
        };
    };
    let pattern_lit = ir.get_string(sid).to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    quote! {
        /// AX.W0a.2.q — visitor-path Number-shape via regex-scan for
        /// leading-dot-admitting rules; see
        /// `emit_parse_number_via_hregex` for tape-path rationale.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            first_byte: u8,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::NumberVisitor,
        {
            let _ = first_byte;
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                });
            };
            let span_hi = span_lo + match_len as usize;
            *p = span_hi;
            let __f64: f64 = core::str::from_utf8(&input[span_lo..span_hi])
                .ok()
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or(0.0);
            visitor.number_f64(__f64).map_err(|_| {
                crate::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                }
            })
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
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let _ = state;
            let span_lo = *p as u32;
            let span_hi = *p as u32;
            let leaf_off = builder.push_leaf_with(
                crate::runtime::tape::TapeKind::Regex,
                span_lo,
                span_hi,
                #variant_idx,
                0,
                crate::runtime::tape::PayloadData::None,
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
///
/// # AZ-I.W2.RE — strategy gate
///
/// Mirrors [`emit_parse_hregex`]. HRegex-shape rules are tape-only in
/// W2; codegen-time panic on [`EmitStrategy::StructDirect`].
pub fn emit_parse_hregex_visitor(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let _ = strategy;
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
            ) -> ::core::result::Result<(), crate::runtime::ParseErr>
            where
                V: crate::runtime::tape::StringVisitor,
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
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::StringVisitor,
        {
            let span_lo = *p;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                });
            };
            let span_hi = *p + match_len as usize;
            *p = span_hi;
            visitor.string(&input[span_lo..span_hi]).map_err(|_| {
                crate::runtime::ParseErr::Syntax {
                    offset: span_lo as u32, rule: None,
                }
            })
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// AZ-I.W2-act.B3 — HRegex struct-direct bodies.
//
// Replace the W2.RE codegen-time panics with real bodies that scan
// the input via the per-grammar regex adapter, decode any host-fn
// payload, and route the decoded value through the StructBuilder
// trait surface. Per `feedback_no-orthogonal-codepaths` the body is
// grammar-general — the SubstrateBinding's builder type splices in
// via `super::substrate::builder_ty_with_lifetime`.
// ─────────────────────────────────────────────────────────────────────

use super::substrate::builder_ty_with_lifetime;

/// Emit the struct-direct HRegex body. Routes per FnDescriptor:
///
/// - `HexConvert { fn_path }` → call `<fn_path>(matched_str)` and
///   `builder.push_leaf_with_u64(decoded as u64)` (u32 widens to u64
///   on the trait surface; the builder downcasts to the typed payload
///   per the grammar's enum). The CSS L4 `hex` rule canonically lands
///   here.
/// - `NumberConvert { .. }` → `str::parse::<f64>()` and
///   `builder.push_leaf_with_f64(value)`.
/// - `Expr { Input, return_type: F64 }` → same as NumberConvert.
/// - `Expr { Input, return_type: U32 }` → `str::parse::<u32>()` and
///   `builder.push_leaf_with_u64(value as u64)`.
/// - `SpanCapture` / no host fn → `builder.push_leaf_with_str(slice)`
///   from the matched bytes.
fn emit_parse_hregex_struct_direct(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    use bbnf_ir::{FnDescriptor, MapExpr, TypeDesc};

    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("hregex", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let pattern_sid = extract_regex_pattern(&rule.body);
    let Some(sid) = pattern_sid else {
        return emit_struct_direct_unsupported_stub(&fn_ident, &support_mod, &builder_ty);
    };
    let pattern_lit = ir.get_string(sid).to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    // Walk the body for the FnDescriptor (Map { fn_id, Regex }).
    let map_fn_id = extract_map_fn_id(&rule.body);
    let descriptor = map_fn_id.and_then(|fn_id| ir.fns.get(fn_id as usize));

    let push_call: TokenStream = match descriptor {
        Some(FnDescriptor::HexConvert { fn_path }) => {
            let path_str = ir.get_string(*fn_path);
            match syn::parse_str::<syn::Path>(path_str) {
                Ok(host_path) => quote! {
                    let __decoded: u32 = #host_path(
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_u64(
                        builder, __decoded as u64,
                    );
                },
                Err(_) => quote! {
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_str(
                        builder,
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                },
            }
        }
        Some(FnDescriptor::NumberConvert { .. }) => quote! {
            let __f64: f64 = core::str::from_utf8(
                &input[span_lo as usize..span_hi as usize]
            )
            .ok()
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
            <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_f64(
                builder, __f64,
            );
        },
        Some(FnDescriptor::Expr { expr, return_type }) if matches!(expr, MapExpr::Input) => {
            match return_type {
                Some(TypeDesc::F64) => quote! {
                    let __f64: f64 = core::str::from_utf8(
                        &input[span_lo as usize..span_hi as usize]
                    )
                    .ok()
                    .and_then(|s| s.parse::<f64>().ok())
                    .unwrap_or(0.0);
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_f64(
                        builder, __f64,
                    );
                },
                Some(TypeDesc::U32) => quote! {
                    let __u32: u32 = core::str::from_utf8(
                        &input[span_lo as usize..span_hi as usize]
                    )
                    .ok()
                    .and_then(|s| s.parse::<u32>().ok())
                    .unwrap_or(0);
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_u64(
                        builder, __u32 as u64,
                    );
                },
                Some(TypeDesc::I64) => quote! {
                    let __i64: i64 = core::str::from_utf8(
                        &input[span_lo as usize..span_hi as usize]
                    )
                    .ok()
                    .and_then(|s| s.parse::<i64>().ok())
                    .unwrap_or(0);
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_i64(
                        builder, __i64,
                    );
                },
                _ => quote! {
                    <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_str(
                        builder,
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                },
            }
        }
        // SpanCapture / EnumWrap / BoxWrap / no descriptor — emit a
        // borrowed-span push so the typed value path captures the
        // matched bytes for round-trip.
        _ => quote! {
            <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_str(
                builder,
                core::str::from_utf8(
                    &input[span_lo as usize..span_hi as usize]
                ).unwrap_or(""),
            );
        },
    };

    quote! {
        /// AZ-I.W2-act.B3 — per-grammar HRegex-shape parse function,
        /// **struct-direct body**.
        ///
        /// Runs the per-grammar regex scan, decodes the matched bytes
        /// per the rule's host-fn descriptor (HexConvert, NumberConvert,
        /// or Expr { Input, return_type }), and routes the decoded
        /// value through the StructBuilder trait. Returns
        /// unit for StructDirect composition with sibling
        /// shape fns under struct-direct mode.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: span_lo,
                });
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            #push_call
            Ok(())
        }
    }
}

/// Mirror of [`emit_parse_hregex_struct_direct`] with the Number-shape
/// signature (`first_byte` instead of `state`). Routes lenient-number
/// dialects (CSS `.5` literals) through the same regex-scan path.
fn emit_parse_number_via_hregex_struct_direct(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("number", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let _ = support_mod;
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let Some(sid) = extract_regex_pattern(&rule.body) else {
        return quote! {
            #[inline(always)]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident<'p>(
                input: &'p [u8],
                p: &mut usize,
                first_byte: u8,
                builder: &mut #builder_ty,
            ) -> ::core::result::Result<(), crate::runtime::DtaError> {
                let _ = (input, p, first_byte, builder);
                Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                })
            }
        };
    };
    let pattern_lit = ir.get_string(sid).to_string();
    let regex_scan_ident = regex_scan_adapter_ident(&sanitise_grammar(grammar_suffix));

    quote! {
        /// AZ-I.W2-act.B3 — Number-shape parse function via HRegex,
        /// struct-direct body. Routes lenient-number dialects (CSS
        /// `.5` / `.25e3` literals) through the per-grammar regex-scan
        /// adapter and pushes the decoded f64 via the StructBuilder
        /// trait.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            first_byte: u8,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let _ = first_byte;
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: span_lo,
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
            <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_f64(
                builder, __f64,
            );
            Ok(())
        }
    }
}

/// Extract the `Map { fn_id, Regex }` fn_id when the rule body fits
/// the host-fn-decode pattern. Returns None for plain regex rules
/// without a host descriptor.
fn extract_map_fn_id(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Map { fn_id, inner } => match inner.as_ref() {
            IrNode::Regex(_) => Some(*fn_id),
            _ => None,
        },
        IrNode::OptionalWhitespace(inner) => extract_map_fn_id(inner),
        _ => None,
    }
}

/// Emit a struct-direct stub for HRegex rules whose body is not a
/// recognisable regex pattern. The body returns a syntax error
/// without reading; the routing gate upstream prevents this from
/// firing on well-formed input.
fn emit_struct_direct_unsupported_stub(
    fn_ident: &proc_macro2::Ident,
    support_mod: &proc_macro2::Ident,
    builder_ty: &TokenStream,
) -> TokenStream {
    quote! {
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            let _ = (input, state, builder);
            Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            })
        }
    }
}
