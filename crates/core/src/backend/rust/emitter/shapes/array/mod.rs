//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2 / AX.W0a.2.a
//!
//! Emits the per-grammar Array-shape parse function against the
//! StructBuilder substrate. The Array detector admits two structural
//! shapes:
//!
//! 1. **Shape 1 — wrapped homogeneous repeat** (JSON `array`):
//!
//!    ```text
//!    array = "[" >> ((value << comma?)*)?w << "]"
//!    ```
//!
//!    The body unwraps to `Wrap(open_byte, Repeat, close_byte)` where
//!    `open` and `close` are concrete single-byte literals.
//!    The struct-direct body consumes the delimiters and routes each
//!    value position through the grammar-derived value dispatch.
//!
//! 2. **Shape 2 — entry-rule list** (CSS `stylesheet`, BBNF `grammar`):
//!
//!    ```text
//!    stylesheet = ruleList ?w          // OW(Repeat(...)) after inline
//!    grammar    = ( grammar_item ?w )* // direct Repeat
//!    ```
//!
//!    The body has no bracket wrap — the rule body is either a direct
//!    `Repeat` or an `OptionalWhitespace(Repeat(...))`. No close-
//!    delimiter sentinel exists; iteration terminates when the inner
//!    value's first-byte dispatch rejects (end-of-input or a byte not
//!    in the element's first set). The struct-direct body opens the
//!    rule frame once and iterates until the entry dispatch rejects.
//!
//! Each value position is emitted by grammar-derived dispatch only; no
//! legacy record stream fallback is selected by this module.

use bbnf_ir::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::cursor_param::{cursor_param, cursor_where_clause};
use super::dispatcher::{dispatcher_fn_ident, emit_ref_call_shape, shape_fn_ident};
use super::root_rule_name;
use bbnf_ir::registry::EmitStrategy;

mod element;

/// Emit the `parse_array_<grammar>_<rule>` body for the resolved
/// struct-builder substrate.
///
/// AZ-I.W2.RB — strategy match is at codegen time, not at runtime.
/// Per `feedback_no-orthogonal-codepaths` ONE function body is emitted
/// per `(grammar, rule)`; the strategy selects which.
///
/// Dispatches on rule body structure:
///
/// - **Shape 1** — body unwraps to `Wrap(open, middle, close)` with
///   concrete single-byte open/close literals.
/// - **Shape 2** — body is a `Repeat` (direct) or `OptionalWhitespace(Repeat)`
///   with no delimiter wrap.
///
/// Struct-direct path emits a uniform body — `begin_compound(&__layout)`
/// + element loop + `end_compound(handle)` — agnostic to the wrapped /
/// list distinction (the JsonStructBuilder's Array frame collects items
/// the same way regardless of which delimiter shape produced them).
pub fn emit_parse_array(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    emit_parse_array_struct_direct(grammar_suffix, rule, ir, strategy)
}

/// AZ-I.W2.RB — struct-direct body for the Array shape.
///
/// Emits a `parse_array_<grammar>_<rule>` whose body drives the
/// per-grammar `StructBuilder` directly. It dispatches on the rule
/// body shape (Wrap-vs-Repeat) and emits the matching per-shape body.
/// The StructDirect path branches here so emission tracks the body
/// shape rather than
/// claiming a single hard-coded shape.
///
/// AZ-II.cutover.F — pre-fix the body unconditionally emitted the
/// Shape-1 wrapped form (hard-coded `[`/`,`/`]`), which rejected
/// every Shape-2 entry-rule list (BBNF `grammar`, future CSS-like
/// list rules). Post-fix the dispatch is `unwrap_wrap` -> Shape 1
/// (wrapped homogeneous repeat); else -> Shape 2 (entry-rule list).
///
/// The `__layout` lookup is asserted (not fall-back). Per the W2-EMITTER-
/// REWIRE plan §1, `for_grammar` GUARANTEES the layout exists for every
/// rule when the strategy is `StructDirect`.
fn emit_parse_array_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let body = unwrap_map_ow(&rule.body);
    if let Some((open, _middle, close)) = unwrap_wrap(body) {
        if single_byte_literal(open, ir).is_some() && single_byte_literal(close, ir).is_some() {
            return emit_parse_array_struct_direct_wrapped(grammar_suffix, rule, ir, strategy);
        }
    }
    emit_parse_array_struct_direct_list(grammar_suffix, rule, ir, strategy)
}

/// Shape-1 struct-direct body — wrapped homogeneous repeat
/// (canonical JSON `array`). Hard-codes the open/comma/close
/// punctuation derived from `unwrap_wrap`.
fn emit_parse_array_struct_direct_wrapped(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name.to_string();
    let p_lt = format_ident!("p");
    let builder_ty = super::substrate::builder_ty_with_lifetime(strategy, &p_lt);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Per-Ref direct value call when classified — same routing as the
    // tape path; targets are the per-shape struct-direct fns.
    let value_ref = element::extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_shape(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, builder, cursor)?;
            }
        });

    let cursor_p = cursor_param();
    let cursor_where = cursor_where_clause();

    quote! {
        /// AZ-I.W2.RB — per-grammar Array-shape parse function,
        /// **struct-direct body** (Shape 1 — wrapped homogeneous repeat).
        ///
        /// AZ-IV.W3.6 — Cursor-threaded. The element loop consults
        /// `cursor.decide(rule_id) -> Decision` so the lazy bail-out
        /// parse can break after `ParseUntil(idx)` reaches the targeted
        /// element index. Eager parses pass an always-`ParseFully`
        /// cursor; the consult is a no-op against the pre-W3.6 body.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            #cursor_p,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            #cursor_where,
        {
            use crate::runtime::builder::StructBuilder;
            use crate::path::cursor::Decision as __Decision;

            if input.get(*p).copied() != Some(b'[') {
                return Err(crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                });
            }

            // AZ-I.W2.RB — open the array compound. Inline layout
            // literal mirrors W2.RD/RF's pattern; JsonStructBuilder's
            // `begin_compound` consults `(kind, rule_name)` to route
            // OpenFrame::Array.
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::Struct,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __array_checkpoint = builder.checkpoint();
            let __handle = builder.begin_compound(&__layout);

            // AZ-IV.W3.6 — consult the cursor's decision once; the
            // result is invariant over the loop body for a fixed
            // (rule_id, segment_kind).
            let __decision: __Decision = cursor.decide(#rule_id_lit as u32);
            let mut __elem_idx: u32 = 0;

            let __array_result: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                *p += 1;
                let _ = #support_mod::skip_space(input, p, state);

                if input.get(*p).copied() == Some(b']') {
                    *p += 1;
                    return Ok(());
                }

                loop {
                    // Element dispatch — per-Ref routing matches tape path.
                    #value_call

                    // AZ-IV.W3.6 — ParseUntil cut: after the element at
                    // index `cut` is consumed, break. The remaining
                    // bytes through `]` need a brace-balanced skip
                    // scanner (W3.7 wires); the loop break here drops
                    // out so the bytes-past-cut path never reaches
                    // record emission.
                    if let __Decision::ParseUntil(__cut) = __decision
                        && __elem_idx as u32 >= __cut as u32
                    {
                        return Ok(());
                    }
                    __elem_idx = __elem_idx.saturating_add(1);

                    let _ = #support_mod::skip_space(input, p, state);
                    match input.get(*p).copied() {
                        Some(b',') => {
                            *p += 1;
                            let _ = #support_mod::skip_space(input, p, state);
                        }
                        Some(b']') => {
                            *p += 1;
                            return Ok(());
                        }
                        _ => return Err(crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        }),
                    }
                }
            })();

            match __array_result {
                Ok(()) => {
                    builder.end_compound(__handle);
                    Ok(())
                }
                Err(__err) => {
                    builder.rollback(__array_checkpoint);
                    Err(__err)
                }
            }
        }
    }
}

/// AZ-II.cutover.F — Shape-2 struct-direct body — entry-rule list
/// with NO bracket wrap. Opens the rule's compound frame against the
/// StructBuilder, iterates the inner Repeat with per-iter savepoint +
/// rollback, dispatches each element via the per-Ref shape fn, and
/// closes the compound frame on EOF / first-set rejection.
///
/// The body uses NO hard-coded delimiter literals: termination is
/// driven by the dispatcher's first-byte rejection (which surfaces
/// as `Err`) and the savepoint protocol on `*p`.
///
/// Body-shape recognition: `unwrap_map_ow` is applied first to
/// peel transparent wrappers, then a structural match selects
/// between direct `Repeat` (BBNF `grammar = (item ?w)*`) and
/// outer-OW-wrapped `Repeat` (CSS `stylesheet = ruleList ?w`);
/// `Map { inner, .. }` is also peeled. Anything else returns an
/// empty TokenStream — the rule's classification was inconsistent
/// with the body shape, and per `feedback_no-silent-epsilon` the
/// downstream consumer will surface the missing fn at link time.
fn emit_parse_array_struct_direct_list(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    use bbnf_ir::IrNode;

    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("array", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name.to_string();
    let p_lt = format_ident!("p");
    let builder_ty = super::substrate::builder_ty_with_lifetime(strategy, &p_lt);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // Pattern-match the body using the same direct-list shape admitted
    // by the array detector.
    let (has_outer_ow, repeat_inner) = match &rule.body {
        IrNode::OptionalWhitespace(inner) => match inner.as_ref() {
            IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
            _ => return quote! {},
        },
        IrNode::Repeat { inner, .. } => (false, inner.as_ref()),
        IrNode::Map { inner, .. } => match inner.as_ref() {
            IrNode::OptionalWhitespace(ow_inner) => match ow_inner.as_ref() {
                IrNode::Repeat { inner: r_inner, .. } => (true, r_inner.as_ref()),
                _ => return quote! {},
            },
            IrNode::Repeat { inner: r_inner, .. } => (false, r_inner.as_ref()),
            _ => return quote! {},
        },
        _ => return quote! {},
    };

    // Per-Ref direct value call when classified. Every per-shape
    // struct-direct fn carries the `(input, p, state, builder, cursor)`
    // signature.
    let value_ref = element::extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_shape(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, builder, cursor)?;
            }
        });

    // Skip the leading whitespace in OW-wrapped variants so the
    // first-set check below sees the iter's actual leading byte.
    // For the bare-Repeat shape the inner iterator's own dispatcher
    // handles whitespace via the per-Ref pre-skip.
    let leading_ow_skip = if has_outer_ow {
        quote! { let _ = #support_mod::skip_space(input, p, state); }
    } else {
        quote! {}
    };
    let trailing_ow_skip = leading_ow_skip.clone();

    // Whether the Repeat's inner carries an OW wrapper that admits
    // intra-iteration whitespace. Mirrors `list::has_iter_ow`.
    let has_iter_ow = matches!(
        repeat_inner,
        IrNode::OptionalWhitespace(_) | IrNode::Seq(_) | IrNode::Next(_, _) | IrNode::Skip(_, _),
    );
    let intra_iter_ws = if has_iter_ow {
        quote! { let _ = #support_mod::skip_space(input, p, state); }
    } else {
        quote! {}
    };

    let cursor_p = cursor_param();
    let cursor_where = cursor_where_clause();

    quote! {
        /// AZ-II.cutover.F — per-grammar Array-shape parse function
        /// (Shape 2 — entry-rule list, **struct-direct body**).
        ///
        /// Opens the rule's compound frame on the StructBuilder,
        /// iterates the inner Repeat with savepoint rollback, and
        /// closes the frame on first-byte rejection or EOF. NO
        /// bracket-delimiter literals — termination is driven by
        /// the inner dispatcher's first-set check.
        ///
        /// AZ-IV.W3.6 — Cursor-threaded. Each iteration consults
        /// `cursor.decide(rule_id) -> Decision` to honour the lazy
        /// bail-out parse's `ParseUntil(idx)` cut.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            #cursor_p,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            #cursor_where,
        {
            use crate::runtime::builder::StructBuilder;
            use crate::path::cursor::Decision as __Decision;

            // Open the rule's compound frame (the outer Repeat).
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::Struct,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __handle = builder.begin_compound(&__layout);

            // AZ-IV.W3.6 — consult cursor decision once.
            let __decision: __Decision = cursor.decide(#rule_id_lit as u32);
            let mut __elem_idx: u32 = 0;

            // Outer-OW leading skip. No-op when the body is a bare
            // Repeat without an outer OW wrapper.
            #leading_ow_skip

            loop {
                let __iter_save_p = *p;
                // EOF terminates iteration.
                if input.get(*p).is_none() {
                    break;
                }
                // AZ-IV.W3.6 — ParseUntil cut.
                if let __Decision::ParseUntil(__cut) = __decision
                    && __elem_idx as u32 > __cut as u32
                {
                    break;
                }
                let __iter_builder_checkpoint = builder.checkpoint();
                // Attempt one iteration via a closure so failures
                // surface as `Err` and unwind to `*p` rollback.
                let __iter_result: ::core::result::Result<
                    (),
                    crate::runtime::DtaError,
                > = (|| {
                    #intra_iter_ws
                    #value_call
                    #intra_iter_ws
                    Ok(())
                })();
                match __iter_result {
                    Ok(()) => {
                        // Zero-width iteration guard — terminate
                        // rather than spin.
                        if *p == __iter_save_p {
                            builder.rollback(__iter_builder_checkpoint);
                            break;
                        }
                        builder.commit(__iter_builder_checkpoint);
                        __elem_idx = __elem_idx.saturating_add(1);
                    }
                    Err(_) => {
                        *p = __iter_save_p;
                        builder.rollback(__iter_builder_checkpoint);
                        break;
                    }
                }
            }

            // Outer-OW trailing skip. No-op when the body is bare
            // Repeat.
            #trailing_ow_skip

            builder.end_compound(__handle);
            Ok(())
        }
    }
}
