//! Object-shape emitter — `parse_object_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2
//!
//! Emits walker-tape-identical code for the canonical JSON object rule:
//!
//! ```text
//! object = "{" >> ((pair << comma?)*)?w << "}"
//! pair   = string, colon >> value
//! ```
//!
//! The emitted tape mirrors the walker's structural compound tree
//! exactly — the only difference is that dispatch is inlined (no
//! `dispatch_one`, no `try_branch`) while the record stream matches
//! byte-for-byte.
//!
//! # Emitted tape shape (for `{"k":v}`)
//!
//! ```text
//! [ 0] Seq     variant=<object_id>  span=0..N                                <- object outer Seq
//! [ 1] Seq     variant=0            span=0..N-1 has_children                 <- Next("{" , rest)
//! [ 2] Literal variant=0            span=0..1                                <- "{"
//! [ 3] Seq     variant=0            has_children                             <- OptionalWhitespace
//! [ 4] Rule    variant=0            has_children                             <- Repeat
//! [ 5] Seq     variant=0            has_children                             <- per-iter Skip(pair, Repeat(,?))
//! [ 6] Seq     variant=<pair_id>    has_children                             <- pair (Seq)
//! [ 7] ... string records ...                                                <- Ref(string)
//! [ .] Seq     variant=0            has_children                             <- Next(OptionalWhitespace(":"), Ref(value))
//! [ .] Seq     variant=0            has_children                             <- OptionalWhitespace(":")
//! [ .] Literal variant=0                                                     <- ":"
//! [ .] ... value records ...                                                 <- Ref(value)
//! [ .] Rule    variant=0            has_children                             <- Repeat(,?)
//! [ .] ... optional "," records ...
//! [ .] Literal variant=0                                                     <- "}"
//! ```

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

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Locate the string rule and its variant_idx so we can call its
    // shape fn + stamp the pair's `Ref(string)` correctly.
    let (string_fn, string_variant, pair_variant) = resolve_pair_context(grammar_suffix, ir);

    quote! {
        /// AW-V.W3.2 — per-grammar Object-shape parse function,
        /// **walker-tape-identical**.
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
            let span_lo = *p as u32;
            if input.get(*p).copied() != Some(b'{') {
                return Err(::bbnf::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                    failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                });
            }

            // Outer object Seq compound.
            let outer_child = builder.mark_children();

            // Next("{" , rest) Seq compound.
            let lbrace_open = *p as u32;
            let next_child = builder.mark_children();

            // Leaf: "{" Literal — walker stamps variant_idx with the
            // enclosing rule's id (object, here) from the Ref's pending
            // stamp inherited into the Literal arm.
            *p += 1;
            let brace_close = *p as u32;
            let _ = builder.push_leaf_with(
                ::bbnf::runtime::tape::TapeKind::Literal,
                lbrace_open,
                brace_close,
                #variant_idx,
                0,
                ::bbnf::runtime::tape::PayloadData::None,
            );

            // OptionalWhitespace Seq compound.
            let opt_ws_open = *p as u32;
            let opt_ws_child = builder.mark_children();

            let _ = #support_mod::skip_space(input, p, state);
            let repeat_open = *p as u32;
            let repeat_child = builder.mark_children();

            if input.get(*p).copied() == Some(b'}') {
                // Empty object — close the Repeat (0 iters), OptionalWhitespace, Next, outer.
                let repeat_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    repeat_child,
                    repeat_open,
                    repeat_close,
                    0,
                    0,
                );
                let opt_ws_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    opt_ws_child,
                    opt_ws_open,
                    opt_ws_close,
                    0,
                    0,
                );
                let next_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    next_child,
                    lbrace_open,
                    next_close,
                    0,
                    0,
                );
                *p += 1;
                let rbrace_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    next_close,
                    rbrace_hi,
                    #variant_idx,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
                let outer_close = *p as u32;
                let outer_off = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    outer_child,
                    span_lo,
                    outer_close,
                    #variant_idx,
                    0,
                );
                return Ok(outer_off);
            }

            // Non-empty: loop per iter (Skip(pair, Repeat(,?))).
            loop {
                let iter_open = *p as u32;
                let iter_child = builder.mark_children();

                // pair Seq compound.
                let pair_open = *p as u32;
                let pair_child = builder.mark_children();

                // Ref(string) — emit string shape fn (its own Span leaf).
                if input.get(*p).copied() != Some(b'"') {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                let _key_off = #string_fn(input, p, state, builder)?;

                // Next(OptionalWhitespace(":"), Ref(value)) Seq compound.
                let _ = #support_mod::skip_space(input, p, state);
                let colon_open = *p as u32;
                let colon_next_child = builder.mark_children();

                // OptionalWhitespace(":") Seq compound.
                let opt_colon_child = builder.mark_children();
                if input.get(*p).copied() != Some(b':') {
                    return Err(::bbnf::runtime::tape::DtaError::Syntax {
                        offset: *p as u32,
                        failing_state: ::bbnf::runtime::tape::DtaStateId::NONE,
                        failing_rule: ::bbnf::runtime::tape::DtaRuleId(u32::MAX),
                    });
                }
                let colon_lo = *p as u32;
                *p += 1;
                let colon_hi = *p as u32;
                let _ = builder.push_leaf_with(
                    ::bbnf::runtime::tape::TapeKind::Literal,
                    colon_lo,
                    colon_hi,
                    #pair_variant,
                    0,
                    ::bbnf::runtime::tape::PayloadData::None,
                );
                let opt_colon_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    opt_colon_child,
                    colon_open,
                    opt_colon_close,
                    0,
                    0,
                );

                // Ref(value) — recurse through dispatcher.
                let _ = #support_mod::skip_space(input, p, state);
                let _value_off = #dispatcher_ident(input, p, state, builder)?;

                // Close Next(colon_ws, value) Seq compound.
                let value_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    colon_next_child,
                    colon_open,
                    value_close,
                    0,
                    0,
                );

                // Close pair Seq compound.
                let pair_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    pair_child,
                    pair_open,
                    pair_close,
                    #pair_variant,
                    0,
                );

                // Comma-optional Repeat compound.
                let _ = #support_mod::skip_space(input, p, state);
                let comma_repeat_open = *p as u32;
                let comma_repeat_child = builder.mark_children();
                if input.get(*p).copied() == Some(b',') {
                    let opt_comma_open = *p as u32;
                    let opt_comma_child = builder.mark_children();
                    let comma_lo = *p as u32;
                    *p += 1;
                    let comma_hi = *p as u32;
                    let _ = builder.push_leaf_with(
                        ::bbnf::runtime::tape::TapeKind::Literal,
                        comma_lo,
                        comma_hi,
                        #pair_variant,
                        0,
                        ::bbnf::runtime::tape::PayloadData::None,
                    );
                    let opt_comma_close = *p as u32;
                    let _ = builder.push_compound(
                        ::bbnf::runtime::tape::TapeKind::Seq,
                        opt_comma_child,
                        opt_comma_open,
                        opt_comma_close,
                        0,
                        0,
                    );
                }
                let comma_repeat_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Rule,
                    comma_repeat_child,
                    comma_repeat_open,
                    comma_repeat_close,
                    0,
                    0,
                );

                let iter_close = *p as u32;
                let _ = builder.push_compound(
                    ::bbnf::runtime::tape::TapeKind::Seq,
                    iter_child,
                    iter_open,
                    iter_close,
                    0,
                    0,
                );

                // Peek: loop or close.
                let _ = #support_mod::skip_space(input, p, state);
                match input.get(*p).copied() {
                    Some(b'}') => {
                        let repeat_close = *p as u32;
                        let _ = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Rule,
                            repeat_child,
                            repeat_open,
                            repeat_close,
                            0,
                            0,
                        );
                        let opt_ws_close = *p as u32;
                        let _ = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            opt_ws_child,
                            opt_ws_open,
                            opt_ws_close,
                            0,
                            0,
                        );
                        let next_close = *p as u32;
                        let _ = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            next_child,
                            lbrace_open,
                            next_close,
                            0,
                            0,
                        );
                        *p += 1;
                        let rbrace_hi = *p as u32;
                        let _ = builder.push_leaf_with(
                            ::bbnf::runtime::tape::TapeKind::Literal,
                            next_close,
                            rbrace_hi,
                            0,
                            0,
                            ::bbnf::runtime::tape::PayloadData::None,
                        );
                        let outer_close = *p as u32;
                        let outer_off = builder.push_compound(
                            ::bbnf::runtime::tape::TapeKind::Seq,
                            outer_child,
                            span_lo,
                            outer_close,
                            #variant_idx,
                            0,
                        );
                        let _ = #string_variant;
                        return Ok(outer_off);
                    }
                    Some(_) => {
                        // next iteration
                    }
                    None => {
                        return Err(::bbnf::runtime::tape::DtaError::UnexpectedEnd {
                            offset: *p as u32,
                        });
                    }
                }
            }
        }
    }
}

/// Resolve the grammar's String-shape rule (for key parsing) and the
/// pair rule's variant_idx. Returns `(string_fn_ident, string_variant_idx, pair_variant_idx)`.
///
/// The pair rule is the one whose body is `Seq(Ref(string), ...)`; per
/// JSON's canonical shape `pair = string, colon >> value`, walking the
/// rule list for a Seq-bodied rule with a Ref-to-string head yields it.
fn resolve_pair_context(
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> (proc_macro2::Ident, u8, u8) {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    use bbnf_ir::IrNode;

    // Find the String-shape rule.
    let string_rule = ir
        .rules
        .iter()
        .find(|r| matches!(ir.shape_assignments.get(r.id), ShapeTag::String))
        .expect("object-shape admission requires a String-shape sibling rule");
    let string_name = ir.get_string(string_rule.name);
    let string_fn = shape_fn_ident("string", grammar_suffix, string_name);
    let string_variant = (string_rule.id & 0xFF) as u8;

    // Find the pair rule — a Seq whose first child is Ref(string_rule_id).
    let pair_rule = ir.rules.iter().find(|r| match &r.body {
        IrNode::Seq(children) => children
            .first()
            .map(|c| matches!(c, IrNode::Ref(rid) if *rid == string_rule.id))
            .unwrap_or(false),
        _ => false,
    });
    let pair_variant = pair_rule.map(|r| (r.id & 0xFF) as u8).unwrap_or(0);

    (string_fn, string_variant, pair_variant)
}
