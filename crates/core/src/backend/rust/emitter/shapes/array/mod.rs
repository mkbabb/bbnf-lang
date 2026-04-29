//! Array-shape emitter — `parse_array_<grammar>_<rule>`.
//!
//! # Role — AW-V.W3.2 / AX.W0a.2.a
//!
//! Emits the per-grammar Array-shape parse function with **walker-
//! identical tape emission**. The Array detector admits two structural
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
//!    [`wrapped::emit_parse_array_wrapped`] emits the nested
//!    Seq/Seq/Repeat/Seq compound tree with the bracket literals as
//!    Literal leaves.
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
//!    in the element's first set). [`list::emit_parse_array_list`]
//!    emits the matching Seq/Rule compound tree — outer Seq when an OW
//!    wrapper is present, otherwise the Repeat's Rule compound
//!    directly.
//!
//! Each structural IR production becomes a `push_compound` record.
//! Downstream view derives (`arrayView`, `valueView`, typed-field
//! projections) and the `tape_parity` golden fixtures navigate that
//! exact record sequence, so the shape emitter must reproduce it byte-
//! for-byte — only the **dispatch** is inlined (no `dispatch_one` /
//! `try_branch` / cross-crate helper chain), not the **records**.
//!
//! # Emitted tape shape — Shape 1 (for `[v1, v2]`)
//!
//! ```text
//! [ 0] Seq     variant=<array_id>  span=0..N   child=1  has_children=true
//! [ 1] Seq     variant=0           span=0..N-1 child=2  has_children=true   <- Next("[", rest)
//! [ 2] Literal variant=0           span=0..1                                 <- "["
//! [ 3] Seq     variant=0           span=1..N-1 child=4  has_children=true   <- OptionalWhitespace
//! [ 4] Rule    variant=0           span=1..N-1 child=5  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- Skip(value, Repeat(,?))
//!         ...value records...
//!         Rule  variant=0          has_children=true                        <- Repeat(,?)
//!           Seq variant=0                                                    <- OptionalWhitespace(",")
//!             Literal variant=0                                              <- ","
//! [ N] Literal variant=0           span=N-1..N                               <- "]"
//! ```
//!
//! # Emitted tape shape — Shape 2 (CSS stylesheet `OW(Repeat(OW(Ref)))`)
//!
//! ```text
//! [ 0] Seq     variant=<rule_id>  span=0..N   child=1  has_children=true   <- OW(Repeat)
//! [ 1] Rule    variant=0          span=L..R   child=2  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```
//!
//! # Emitted tape shape — Shape 2 (BBNF grammar, direct Repeat)
//!
//! ```text
//! [ 0] Rule    variant=<rule_id>  span=0..N   child=1  has_children=true   <- Repeat
//!     per-iteration:
//!       Seq     variant=0          child=... has_children=true              <- OW(Ref)
//!         ...value records via Ref dispatch...
//! ```

use bbnf_ir::passes::inspect::{single_byte_literal, unwrap_map_ow, unwrap_wrap};
use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use bbnf_ir::registry::EmitStrategy;
use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, shape_fn_ident,
};
use super::root_rule_name;

mod element;
mod list;
mod visitor;
mod wrapped;

pub use visitor::emit_parse_array_visitor;

/// Emit `pub fn parse_array_<grammar>_<rule>(input, p, state, builder)
/// -> Result<TapeOffset, DtaError>` for [`EmitStrategy::TapeDirect`], or
/// the matching `JsonStructBuilder`-targeted body for
/// [`EmitStrategy::StructDirect`].
///
/// AZ-I.W2.RB — strategy match is at codegen time, not at runtime.
/// Per `feedback_no-orthogonal-codepaths` ONE function body is emitted
/// per `(grammar, rule)`; the strategy selects which.
///
/// Dispatches on rule body structure (TapeDirect path only):
///
/// - **Shape 1** — body unwraps to `Wrap(open, middle, close)` with
///   concrete single-byte open/close literals → [`wrapped::emit_parse_array_wrapped`].
/// - **Shape 2** — body is a `Repeat` (direct) or `OptionalWhitespace(Repeat)`
///   with no delimiter wrap → [`list::emit_parse_array_list`].
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
    match strategy {
        EmitStrategy::StructDirect { .. } => {
            emit_parse_array_struct_direct(grammar_suffix, rule, ir, strategy)
        }
        EmitStrategy::TapeDirect => {
            let body = unwrap_map_ow(&rule.body);
            if let Some((open, _middle, close)) = unwrap_wrap(body) {
                if single_byte_literal(open, ir).is_some()
                    && single_byte_literal(close, ir).is_some()
                {
                    return wrapped::emit_parse_array_wrapped(grammar_suffix, rule, ir);
                }
            }
            list::emit_parse_array_list(grammar_suffix, rule, ir)
        }
    }
}

/// AZ-I.W2.RB — struct-direct body for the Array shape.
///
/// Emits a `parse_array_<grammar>_<rule>` whose body drives the
/// per-grammar `StructBuilder` directly. Structurally mirrors the
/// TapeDirect dispatch: dispatch on the rule body shape (Wrap-vs-
/// Repeat) and emit the matching per-shape body. The TapeDirect
/// path branches at [`emit_parse_array`]; the StructDirect path
/// branches here so emission tracks the body shape rather than
/// claiming a single hard-coded shape.
///
/// AZ-II.cutover.F — pre-fix the body unconditionally emitted the
/// Shape-1 wrapped form (hard-coded `[`/`,`/`]`), which rejected
/// every Shape-2 entry-rule list (BBNF `grammar`, future CSS-like
/// list rules). Post-fix the dispatch is the same as the
/// TapeDirect path: `unwrap_wrap` → Shape 1 (wrapped homogeneous
/// repeat); else → Shape 2 (entry-rule list).
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
        if single_byte_literal(open, ir).is_some()
            && single_byte_literal(close, ir).is_some()
        {
            return emit_parse_array_struct_direct_wrapped(
                grammar_suffix,
                rule,
                ir,
                strategy,
            );
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
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, builder)?;
            }
        });

    quote! {
        /// AZ-I.W2.RB — per-grammar Array-shape parse function,
        /// **struct-direct body** (Shape 1 — wrapped homogeneous repeat).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            use crate::runtime::builder::StructBuilder;

            if input.get(*p).copied() != Some(b'[') {
                return Err(crate::runtime::tape::DtaError::Syntax {
                    offset: *p as u32,
                    failing_state: crate::runtime::tape::DtaStateId::NONE,
                    failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
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
            let __array_result: ::core::result::Result<
                (),
                crate::runtime::tape::DtaError,
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
                        _ => return Err(crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        }),
                    }
                }
            })();

            match __array_result {
                Ok(()) => {
                    builder.end_compound(__handle);
                    Ok(crate::runtime::tape::TapeOffset::NONE)
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
/// with NO bracket wrap. Mirrors the TapeDirect Shape-2 dispatch
/// at [`list::emit_parse_array_list`]: open the rule's compound
/// frame against the StructBuilder, iterate the inner Repeat with
/// per-iter savepoint + rollback, dispatch each element via the
/// per-Ref shape fn, close the compound frame on EOF / first-set
/// rejection.
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

    // Pattern-match the body. Mirrors `list::emit_parse_array_list`.
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

    // Per-Ref direct value call when classified. The struct-direct
    // path uses the same dispatcher contract as TapeDirect — every
    // per-shape struct-direct fn carries the
    // `(input, p, state, builder)` signature.
    let value_ref = element::extract_array_value_ref(&rule.body, ir);
    let value_call = value_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                #dispatcher_ident(input, p, state, builder)?;
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
        IrNode::OptionalWhitespace(_)
            | IrNode::Seq(_)
            | IrNode::Next(_, _)
            | IrNode::Skip(_, _),
    );
    let intra_iter_ws = if has_iter_ow {
        quote! { let _ = #support_mod::skip_space(input, p, state); }
    } else {
        quote! {}
    };

    quote! {
        /// AZ-II.cutover.F — per-grammar Array-shape parse function
        /// (Shape 2 — entry-rule list, **struct-direct body**).
        ///
        /// Opens the rule's compound frame on the StructBuilder,
        /// iterates the inner Repeat with savepoint rollback, and
        /// closes the frame on first-byte rejection or EOF. NO
        /// bracket-delimiter literals — termination is driven by
        /// the inner dispatcher's first-set check.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            use crate::runtime::builder::StructBuilder;

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

            // Outer-OW leading skip. No-op when the body is a bare
            // Repeat without an outer OW wrapper.
            #leading_ow_skip

            loop {
                let __iter_save_p = *p;
                // EOF terminates iteration.
                if input.get(*p).is_none() {
                    break;
                }
                let __iter_builder_checkpoint = builder.checkpoint();
                // Attempt one iteration via a closure so failures
                // surface as `Err` and unwind to `*p` rollback.
                let __iter_result: ::core::result::Result<
                    (),
                    crate::runtime::tape::DtaError,
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
            Ok(crate::runtime::tape::TapeOffset::NONE)
        }
    }
}
