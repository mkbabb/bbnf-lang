//! AX.W0a.2.p — `Map { Regex, host-fn }` position emission (Class 2).
//!
//! Detects `Map { Regex(s), FnDescriptor that returns u32 or f64 }` —
//! the host-function-backed typed-leaf pattern (CSS `hex = "#" ,
//! /regex/ -> parse_hex_color(input) : u32` and analogous) — and
//! emits a regex-scan + host fn call + arena-payload push so the u32
//! / f64 value reaches the tape via
//! `push_leaf_with_arena_payload`.
//!
//! Returns `None` for non-recognized shapes so the caller can fall
//! back to a transparent Map unwrap.

use bbnf_ir::GrammarIR;
use proc_macro2::TokenStream;
use quote::quote;

/// Emit a position whose structure is `Map { Regex(s), FnDescriptor
/// that returns u32 or f64 }` — the host-function-backed typed-leaf
/// pattern (CSS `hex = "#" , /regex/ -> parse_hex_color(input) : u32`
/// and analogous).
///
/// Returns `None` when the Map doesn't match one of the recognized
/// host-fn shapes; the caller falls back to the transparent Map
/// unwrap.
///
/// Supported FnDescriptor arms:
///
/// - `HexConvert { fn_path }` — scan the regex, call `fn_path(input)`,
///   push a Span leaf carrying the u32 return value as a 4-byte arena
///   aggregate (little-endian). `TapeKind::KvPair` when the owning
///   rule's type is `Tuple([Span, U32])` (hex's inferred type post-
///   layout-planning); Span otherwise.
/// - `NumberConvert { allow_leading_dot }` — scan number, convert to
///   f64, push Span/KvPair with 8-byte arena payload.
pub(super) fn emit_map_regex_host_fn(
    inner: &bbnf_ir::IrNode,
    fn_id: u32,
    variant_idx: u8,
    rule_id: bbnf_ir::RuleId,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> Option<TokenStream> {
    use bbnf_ir::{FnDescriptor, IrNode, TypeDesc};
    let IrNode::Regex(sid) = inner else {
        return None;
    };
    let pattern = ir.get_string(*sid).to_string();
    let fd = ir.fns.get(fn_id as usize)?;
    let regex_scan_ident =
        super::super::super::dfa_codegen::regex_scan_adapter_ident(&super::super::sanitise_grammar(grammar_suffix));
    // Owner-rule type decides KvPair-vs-Span on the pushed leaf. A
    // rule whose inferred type is `Tuple([Span, scalar])` is KvPair-
    // shaped per `is_kv_pair_shape`; the walker rewrites such Seq
    // compounds to KvPair at frame-pop time. Matching that on the
    // leaf side at emit time lets `css_l4_parity::hex_color_*` find
    // a KvPair record with the 4-byte hex payload without a runtime
    // compound-rewrite.
    let kind_is_kv = matches!(
        ir.types.iter().find_map(|(rid, t)| {
            if *rid == rule_id {
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
        FnDescriptor::HexConvert { fn_path } => {
            let path_str = ir.get_string(*fn_path);
            let path: syn::Path = syn::parse_str(path_str).ok()?;
            Some(quote! {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                        return ::core::result::Result::Err(
                            crate::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state:
                                    crate::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    // Host fn sees the matched substring as &str; the
                    // Map's `Expr { FnCall(path, [Input]) }` / the
                    // HexConvert specialisation declares the return
                    // type is u32, which the walker-parity emitter
                    // packs as 4-byte LE into the arena.
                    let __decoded_u32: u32 = #path(
                        core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ).unwrap_or(""),
                    );
                    let __arena_off: u32 =
                        builder.arena_mut().len() as u32;
                    builder
                        .arena_mut()
                        .extend_from_slice(&__decoded_u32.to_le_bytes());
                    let _ = builder.push_leaf_with_arena_payload(
                        #leaf_kind,
                        span_lo,
                        span_hi,
                        #variant_idx,
                        0u8,
                        __arena_off,
                        4u32,
                    );
                }
            })
        }
        FnDescriptor::NumberConvert { allow_leading_dot } => {
            let _ = allow_leading_dot;
            Some(quote! {
                {
                    let span_lo = *p as u32;
                    let Some(match_len) = #regex_scan_ident(#pattern, input, *p) else {
                        return ::core::result::Result::Err(
                            crate::runtime::tape::DtaError::Syntax {
                                offset: span_lo,
                                failing_state:
                                    crate::runtime::tape::DtaStateId::NONE,
                                failing_rule:
                                    crate::runtime::tape::DtaRuleId(u32::MAX),
                            },
                        );
                    };
                    *p += match_len as usize;
                    let span_hi = *p as u32;
                    let __f64: f64 = core::str::from_utf8(
                        &input[span_lo as usize..span_hi as usize]
                    )
                    .ok()
                    .and_then(|s| s.parse::<f64>().ok())
                    .unwrap_or(0.0);
                    let __arena_off: u32 =
                        builder.arena_mut().len() as u32;
                    builder
                        .arena_mut()
                        .extend_from_slice(&__f64.to_le_bytes());
                    let _ = builder.push_leaf_with_arena_payload(
                        #leaf_kind,
                        span_lo,
                        span_hi,
                        #variant_idx,
                        0u8,
                        __arena_off,
                        8u32,
                    );
                }
            })
        }
        _ => None,
    }
}
