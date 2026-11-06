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
//! 4. Pushes the matched or decoded value through the grammar's
//!    concrete StructBuilder.
//!
//! # Host decode
//!
//! Rules with `-> <fn>(input) : <type>` annotations (e.g. CSS `hex ->
//! parse_hex_color(input) : u32`) emit a call to the declared host
//! function post-scan and route the decoded value through the
//! StructBuilder leaf-payload API.
//!
//! W4.1 emitted scaffolding; W4-fix ships the walker-parity body
//! that invokes the regex scanner and pushes a Span leaf.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::regex_scan_adapter::regex_scan_adapter_ident;
use super::alt_dispatch::branches::payload_push_from_fn;
use super::dispatcher::shape_fn_ident;
use super::sanitise_grammar;
use bbnf_ir::registry::EmitStrategy;

/// Emit `pub fn parse_hregex_<grammar>_<rule>(input, p, state,
/// builder) -> Result<(), DtaError>`.
///
/// # AZ-I.W2.RE — strategy gate
///
/// `strategy` is the codegen-time substrate selector resolved by
/// [`EmitStrategy::for_grammar`] in `shapes/mod.rs`. AZ-II O5 leaves
/// only the StructDirect substrate.
pub fn emit_parse_hregex(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    emit_parse_hregex_struct_direct(strategy, grammar_suffix, rule, ir)
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
    emit_parse_number_via_hregex_struct_direct(strategy, grammar_suffix, rule, ir)
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

fn regex_path_has_optional_whitespace(node: &IrNode) -> bool {
    match node {
        IrNode::OptionalWhitespace(_) => true,
        IrNode::Map { inner, .. } => regex_path_has_optional_whitespace(inner),
        _ => false,
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

fn emit_f64_input_push(builder_ty: &TokenStream) -> TokenStream {
    quote! {
        let __text = match core::str::from_utf8(
            &input[span_lo as usize..span_hi as usize]
        ) {
            ::core::result::Result::Ok(value) => value,
            ::core::result::Result::Err(_) => {
                *p = span_lo as usize;
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax { offset: span_lo },
                );
            }
        };
        let __decoded: f64 = match __text.parse::<f64>() {
            ::core::result::Result::Ok(value) => value,
            ::core::result::Result::Err(_) => {
                *p = span_lo as usize;
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax { offset: span_lo },
                );
            }
        };
        <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_f64(
            builder, __decoded,
        );
    }
}

/// Emit a suffix-aware, fallible integer decoder for an HRegex `input` map.
/// The regex owns recognition; this continuation owns only exact scalar
/// conversion and restores the leaf offset when conversion rejects.
fn emit_integer_input_push(ty: &bbnf_ir::TypeDesc, builder_ty: &TokenStream) -> TokenStream {
    use bbnf_ir::TypeDesc;

    let (rust_ty, suffix, signed, push) = match ty {
        TypeDesc::I8 => (
            quote! { i8 },
            "i8",
            true,
            format_ident!("push_leaf_with_i64"),
        ),
        TypeDesc::U8 => (
            quote! { u8 },
            "u8",
            false,
            format_ident!("push_leaf_with_u8"),
        ),
        TypeDesc::I16 => (
            quote! { i16 },
            "i16",
            true,
            format_ident!("push_leaf_with_i64"),
        ),
        TypeDesc::U16 => (
            quote! { u16 },
            "u16",
            false,
            format_ident!("push_leaf_with_u64"),
        ),
        TypeDesc::I32 => (
            quote! { i32 },
            "i32",
            true,
            format_ident!("push_leaf_with_i64"),
        ),
        TypeDesc::U32 => (
            quote! { u32 },
            "u32",
            false,
            format_ident!("push_leaf_with_u64"),
        ),
        TypeDesc::I64 => (
            quote! { i64 },
            "i64",
            true,
            format_ident!("push_leaf_with_i64"),
        ),
        TypeDesc::U64 => (
            quote! { u64 },
            "u64",
            false,
            format_ident!("push_leaf_with_u64"),
        ),
        _ => return quote! { compile_error!("non-integer HRegex decoder type"); },
    };

    let parse = if signed {
        quote! {
            let __parsed: ::core::result::Result<#rust_ty, ()> =
                if let Some(hex) = __digits
                    .strip_prefix("-0x")
                    .or_else(|| __digits.strip_prefix("-0X"))
                {
                    u128::from_str_radix(hex, 16)
                        .ok()
                        .and_then(|magnitude| i128::try_from(magnitude).ok())
                        .and_then(|magnitude| magnitude.checked_neg())
                        .and_then(|value| <#rust_ty>::try_from(value).ok())
                        .ok_or(())
                } else if let Some(hex) = __digits
                    .strip_prefix("0x")
                    .or_else(|| __digits.strip_prefix("0X"))
                {
                    u128::from_str_radix(hex, 16)
                        .ok()
                        .and_then(|value| <#rust_ty>::try_from(value).ok())
                        .ok_or(())
                } else {
                    __digits.parse::<#rust_ty>().map_err(|_| ())
                };
        }
    } else {
        quote! {
            let __parsed: ::core::result::Result<#rust_ty, ()> =
                if let Some(hex) = __digits
                    .strip_prefix("0x")
                    .or_else(|| __digits.strip_prefix("0X"))
                {
                    <#rust_ty>::from_str_radix(hex, 16).map_err(|_| ())
                } else {
                    __digits.parse::<#rust_ty>().map_err(|_| ())
                };
        }
    };

    let widened = if matches!(ty, TypeDesc::U8) {
        quote! { __decoded }
    } else if signed {
        quote! { __decoded as i64 }
    } else {
        quote! { __decoded as u64 }
    };

    quote! {
        let __text = match core::str::from_utf8(
            &input[span_lo as usize..span_hi as usize]
        ) {
            ::core::result::Result::Ok(value) => value,
            ::core::result::Result::Err(_) => {
                *p = span_lo as usize;
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax { offset: span_lo },
                );
            }
        };
        let __digits = __text.strip_suffix(#suffix).unwrap_or(__text);
        #parse
        let __decoded = match __parsed {
            ::core::result::Result::Ok(value) => value,
            ::core::result::Result::Err(()) => {
                *p = span_lo as usize;
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax { offset: span_lo },
                );
            }
        };
        <#builder_ty as crate::runtime::StructBuilder>::#push(builder, #widened);
    }
}

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
/// - `Expr { Input, return_type: U8 }` → `str::parse::<u8>()` and
///   repeatable `builder.push_leaf_with_u8(value)`.
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
    let has_optional_whitespace = regex_path_has_optional_whitespace(&rule.body);
    let leading_whitespace = has_optional_whitespace.then(|| {
        quote! {
            let _ = #support_mod::skip_space(input, p, state);
        }
    });
    let trailing_whitespace = has_optional_whitespace.then(|| {
        quote! {
            let _ = #support_mod::skip_space(input, p, state);
        }
    });

    // Walk the body for the FnDescriptor (Map { fn_id, Regex }).
    let map_fn_id = extract_map_fn_id(&rule.body);
    let descriptor = map_fn_id.and_then(|fn_id| ir.fns.get(fn_id as usize));
    let descriptor_missing = map_fn_id.is_some() && descriptor.is_none();
    let nested_map = map_fn_id.is_some() && !map_directly_wraps_regex(&rule.body);
    let nested_map_is_proven_replacement = descriptor
        .is_some_and(|descriptor| matches!(payload_push_from_fn(descriptor, ir), Ok(Some(_))));

    let push_call: TokenStream = if descriptor_missing {
        quote! {
            compile_error!("HRegex Map references a missing function descriptor");
        }
    } else if nested_map && !nested_map_is_proven_replacement {
        quote! {
            compile_error!(
                "nested HRegex Map is not a proven constant replacement"
            );
        }
    } else {
        match descriptor {
            Some(FnDescriptor::HexConvert { fn_path }) => {
                let path_str = ir.get_string(*fn_path);
                match syn::parse_str::<syn::Path>(path_str) {
                    Ok(host_path) => quote! {
                        let __text = match core::str::from_utf8(
                            &input[span_lo as usize..span_hi as usize]
                        ) {
                            ::core::result::Result::Ok(value) => value,
                            ::core::result::Result::Err(_) => {
                                *p = span_lo as usize;
                                return ::core::result::Result::Err(
                                    crate::runtime::DtaError::Syntax { offset: span_lo },
                                );
                            }
                        };
                        let __decoded: u32 = #host_path(__text);
                        <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_u64(
                            builder, __decoded as u64,
                        );
                    },
                    Err(_) => quote! { compile_error!("invalid HRegex host function path"); },
                }
            }
            Some(FnDescriptor::NumberConvert { .. }) => emit_f64_input_push(&builder_ty),
            Some(FnDescriptor::Expr { expr, return_type }) if matches!(expr, MapExpr::Input) => {
                match return_type {
                    Some(TypeDesc::F64) => emit_f64_input_push(&builder_ty),
                    Some(
                        integer @ (TypeDesc::I8
                        | TypeDesc::U8
                        | TypeDesc::I16
                        | TypeDesc::U16
                        | TypeDesc::I32
                        | TypeDesc::U32
                        | TypeDesc::I64
                        | TypeDesc::U64),
                    ) => emit_integer_input_push(integer, &builder_ty),
                    _ => quote! {
                        compile_error!("HRegex input map requires a supported scalar return type");
                    },
                }
            }
            Some(descriptor @ FnDescriptor::Expr { .. }) => {
                match payload_push_from_fn(descriptor, ir) {
                    Ok(Some(payload)) => payload,
                    Ok(None) => quote! {
                        compile_error!("unsupported non-scalar HRegex expression map");
                    },
                    Err(message) => quote! { compile_error!(#message); },
                }
            }
            // SpanCapture / EnumWrap / BoxWrap / no descriptor — emit a
            // borrowed-span push so the typed value path captures the
            // matched bytes for round-trip.
            _ => quote! {
                let __text = match core::str::from_utf8(
                    &input[span_lo as usize..span_hi as usize]
                ) {
                    ::core::result::Result::Ok(value) => value,
                    ::core::result::Result::Err(_) => {
                        *p = span_lo as usize;
                        return ::core::result::Result::Err(
                            crate::runtime::DtaError::Syntax { offset: span_lo },
                        );
                    }
                };
                <#builder_ty as crate::runtime::StructBuilder>::push_leaf_with_str(
                    builder, __text,
                );
            },
        }
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
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            __P: for<'__c> crate::path::schema::PathSchema<'__c>,
        {
            let _ = cursor;
            #leading_whitespace
            let span_lo = *p as u32;
            let Some(match_len) = #regex_scan_ident(#pattern_lit, input, *p) else {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: span_lo,
                });
            };
            *p += match_len as usize;
            let span_hi = *p as u32;
            #push_call
            #trailing_whitespace
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
    let decoded_push = emit_f64_input_push(&builder_ty);

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
            #decoded_push
            Ok(())
        }
    }
}

/// Extract the `Map { fn_id, Regex }` fn_id when the rule body fits
/// the host-fn-decode pattern. Returns None for plain regex rules
/// without a host descriptor.
fn extract_map_fn_id(node: &IrNode) -> Option<u32> {
    match node {
        IrNode::Map { fn_id, inner } if extract_regex_pattern(inner).is_some() => Some(*fn_id),
        IrNode::OptionalWhitespace(inner) => extract_map_fn_id(inner),
        _ => None,
    }
}

fn map_directly_wraps_regex(node: &IrNode) -> bool {
    match node {
        IrNode::Map { inner, .. } => {
            let mut inner = inner.as_ref();
            while let IrNode::OptionalWhitespace(next) = inner {
                inner = next;
            }
            matches!(inner, IrNode::Regex(_))
        }
        IrNode::OptionalWhitespace(inner) => map_directly_wraps_regex(inner),
        _ => false,
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
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            cursor: &mut crate::path::cursor::PathCursor<'_, __P>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            __P: for<'__c> crate::path::schema::PathSchema<'__c>,
        {
            let _ = (input, state, builder, cursor);
            Err(crate::runtime::DtaError::Syntax {
                offset: *p as u32,
            })
        }
    }
}
