//! Object-shape emitter — `parse_object_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits the per-grammar Object-shape parse function mirroring
//! `bbnf_json_prototype::parse_object` (crates/bbnf-json-prototype/
//! src/lib.rs:258). The emitted function:
//!
//! 1. Marks children via [`TapeBuilder::mark_children`].
//! 2. Fast-empty check: `}` immediately after `{` closes the compound
//!    without visiting keys.
//! 3. Loop: key (`"`) → `:` → value (recurse into dispatcher) → `,`
//!    or `}`.
//! 4. Pushes a compound record with `TapeKind::Rule` on close.
//!
//! The emitter assumes the Object-shape detector has already admitted
//! the rule (W3.1 contract). For JSON's `object =
//! "{" >> ((pair << comma?)*)?w << "}"` the emitted shape is
//! identical to the prototype; other grammars' Object-shape rules
//! emit the same mechanism with per-grammar `open` / `close` / `sep`
//! bytes (W4 generalisation).
//!
//! # Wire contract
//!
//! The emitted function's signature is
//! `parse_object_<grammar>_<rule>(input, p, state, builder) ->
//! Result<TapeOffset, DtaError>`. The returned `TapeOffset` is the
//! compound record's index — the dispatcher threads this up the
//! recursion as the parent compound's root / a child of an enclosing
//! compound.

use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{dispatcher_fn_ident, shape_fn_ident};
use super::root_rule_name;

/// Emit `pub fn parse_object_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_object(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("object", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let variant_idx = (rule.id & 0xFF) as u8;

    // Resolve the dispatcher — the fn the value position recurses into.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => dispatcher_fn_ident(grammar_suffix, &root),
        None => {
            // No root — emit an empty stub (shouldn't reach here in
            // the wire-up path).
            return quote! {};
        }
    };

    // The key parse path: when the grammar has a String-shape rule,
    // call its parse fn for keys; otherwise inline a minimal quoted-
    // string key reader. JSON's `pair = string, colon >> value` uses
    // the grammar's `string` rule for keys.
    let key_call = resolve_key_parse(grammar_suffix, ir);

    quote! {
        /// AW-V.W3.2 — per-grammar Object-shape parse function.
        ///
        /// Mirrors `bbnf_json_prototype::parse_object`. `{` must NOT
        /// be consumed by the caller — this function reads and
        /// verifies it, advances past it, and parses the object body.
        #[inline(always)]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut ::bbnf::runtime::tape::TapeBuilder,
        ) -> ::core::result::Result<
            ::bbnf::runtime::tape::TapeOffset,
            ::bbnf::runtime::tape::DtaError,
        > {
            // The dispatcher has already verified `input[*p] == b'{'`
            // via its byte-dispatch arm; we consume it here.
            let span_lo = *p as u32;
            if input.get(*p).copied() != Some(b'{') {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }
            *p += 1;
            let child_off = builder.mark_children();
            let mut cur = #support_mod::skip_space(input, p, state);

            if cur == Some(b'}') {
                *p += 1;
                let span_hi = *p as u32;
                let off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    child_off,
                    span_lo,
                    span_hi,
                    #variant_idx,
                    0,
                );
                return Ok(off);
            }

            loop {
                if cur != Some(b'"') {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                // Key parse — inline string read, pushes a Span leaf.
                #key_call

                if #support_mod::skip_space(input, p, state) != Some(b':') {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                *p += 1;

                // Value — recurse into the dispatcher.
                let _value_off = #dispatcher_ident(input, p, state, builder)?;

                match #support_mod::skip_space(input, p, state) {
                    Some(b'}') => {
                        *p += 1;
                        let span_hi = *p as u32;
                        let off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Rule,
                            child_off,
                            span_lo,
                            span_hi,
                            #variant_idx,
                            0,
                        );
                        return Ok(off);
                    }
                    Some(b',') => {
                        *p += 1;
                        cur = #support_mod::skip_space(input, p, state);
                    }
                    _ => {
                        return Err(::bbnf::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                            failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                        });
                    }
                }
            }
        }
    }
}

/// Resolve the per-grammar key-parse call. When the grammar declares
/// a String-shape rule (e.g. JSON's `string`), dispatch to its
/// shape fn. Otherwise emit an inline quoted-string reader.
fn resolve_key_parse(grammar_suffix: &str, ir: &GrammarIR) -> TokenStream {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    for rule in &ir.rules {
        if rule.meta.is_transparent {
            continue;
        }
        if !matches!(ir.shape_assignments.get(rule.id), ShapeTag::String) {
            continue;
        }
        let name = ir.get_string(rule.name);
        let string_fn = shape_fn_ident("string", grammar_suffix, name);
        return quote! {
            {
                let _key_off = #string_fn(input, p, state, builder)?;
            }
        };
    }
    // No String-shape rule — emit an inline quoted-string span push.
    // This is the fallback for grammars without a typed string rule.
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    quote! {
        {
            let key_lo = *p as u32;
            let body_start = *p + 1;
            let tail = &input[body_start..];
            match #support_mod::first_quote_or_backslash(tail) {
                Some((off, b'"')) => {
                    *p = body_start + off + 1;
                }
                _ => {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: key_lo,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
            }
            let _key_off = builder.push_leaf_borrowed_string(
                ::bbnf::runtime::tape::TapeKind::Span,
                key_lo,
                *p as u32,
                0,
                0,
            );
        }
    }
}
