//! AZ-I.W2.RD — struct-direct Wrap-shape body.
//!
//! Per the W2-EMITTER-REWIRE plan §6 Risk 2, the Wrap emitter resolves
//! its own layout from `ir.struct_registry.layout(rule.id)` and emits
//! the JSON `value`-shape pattern documented at
//! `crates/core/src/runtime/json/builder.rs:80`:
//!
//! ```text
//! let __wrap_handle = builder.begin_compound(&__wrap_layout);
//! builder.push_branch_tag(__branch_idx);
//! // dispatched Ref per-shape body emits its own builder calls
//! builder.end_compound(__wrap_handle);
//! ```
//!
//! The dispatched per-Ref call resolves the Ref-target's layout in its
//! own per-shape body — Object / Array / leaves carry their own
//! layout reads. The two layout reads are the existing `mod.rs:196`
//! substrate; no new lookup mechanism is needed.
//!
//! # Layout literal construction
//!
//! `JsonStructBuilder::begin_compound` consults `(layout.kind,
//! layout.rule_name)` and routes any rule whose name is not one of the
//! named JSON cases (`array` / `object` / `pair`) to
//! `OpenFrame::Wrap`. The Wrap emitter therefore constructs a minimal
//! `bbnf_ir::StructLayout` literal inline at runtime carrying the
//! grammar-registered rule name, the `LayoutKind` derived from the
//! registry entry (or `LayoutKind::TaggedEnum` as the structural
//! default for `Alt`-of-Refs), and the rule's id. The `fields` and
//! `rule_type` slots are unused by the Wrap dispatch; defaults
//! suffice.

use bbnf_ir::registry::{EmitStrategy, LayoutKind};
use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::shape_fn_ident;
use super::super::substrate::{builder_ty_elided, builder_ty_with_lifetime};
use super::shape_tag_name;
use super::unwrap_outer;

/// Codegen-time inspection of the rule's registered layout. Returns
/// the registry entry when present; otherwise the structural fallback
/// `(LayoutKind::TaggedEnum, rule_name)` carries the Wrap dispatch
/// without losing the rule name (`JsonStructBuilder::begin_compound`
/// only needs the name to fall through to `OpenFrame::Wrap`).
fn resolve_layout_meta<'a>(rule: &IrRule, ir: &'a GrammarIR) -> (LayoutKind, &'a str) {
    if let Some(layout) = ir.struct_registry.layout(rule.id) {
        return (layout.kind, layout.rule_name.as_str());
    }
    (LayoutKind::TaggedEnum, ir.get_string(rule.name))
}

/// Splice a [`LayoutKind`] discriminator into emitted code as
/// `::bbnf_ir::registry::LayoutKind::<variant>`.
fn quote_layout_kind(kind: LayoutKind) -> TokenStream {
    match kind {
        LayoutKind::Struct => {
            quote! { ::bbnf_ir::registry::LayoutKind::Struct }
        }
        LayoutKind::TaggedEnum => {
            quote! { ::bbnf_ir::registry::LayoutKind::TaggedEnum }
        }
        LayoutKind::UntaggedEnum => {
            quote! { ::bbnf_ir::registry::LayoutKind::UntaggedEnum }
        }
        LayoutKind::NewtypeWrapper => {
            quote! { ::bbnf_ir::registry::LayoutKind::NewtypeWrapper }
        }
    }
}

/// Construct a runtime `bbnf_ir::StructLayout` literal carrying the
/// rule's registered metadata. Allocated once per parse-fn invocation
/// (the Wrap rule fires once per occurrence in the input — JSON
/// `value` is the canonical case, fired per rooted JSON value). The
/// `fields` / `rule_type` slots are defaulted because
/// `JsonStructBuilder::begin_compound`'s dispatch only consults
/// `kind` + `rule_name` for the Wrap routing.
fn quote_layout_literal(rule: &IrRule, ir: &GrammarIR) -> TokenStream {
    let (kind, rule_name) = resolve_layout_meta(rule, ir);
    let kind_tokens = quote_layout_kind(kind);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name;
    quote! {
        ::bbnf_ir::registry::StructLayout {
            rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
            rule_name: ::std::string::String::from(#rule_name_lit),
            kind: #kind_tokens,
            rule_type: ::bbnf_ir::TypeDesc::Span,
            fields: ::std::vec::Vec::new(),
        }
    }
}

/// Resolve the call expression for a Wrap-Alt branch on the
/// struct-direct path. Returns `(call_tokens, first_byte_set)`;
/// an empty first-byte set routes the branch as the linear-try
/// fallback.
fn emit_wrap_branch_call_struct_direct(
    inner: &IrNode,
    grammar_suffix: &str,
    ir: &GrammarIR,
) -> Option<(TokenStream, Vec<u8>)> {
    use bbnf_ir::passes::recognizers::shape_dispatch::ShapeTag;
    match inner {
        IrNode::Ref(rid) => {
            let target = ir.rules.iter().find(|r| r.id == *rid)?;
            let tag = ir.shape_assignments.get(*rid);
            let shape_name = shape_tag_name(tag)?;
            let target_fn = shape_fn_ident(shape_name, grammar_suffix, ir.get_string(target.name));
            let call = match tag {
                ShapeTag::Number => quote! {
                    #target_fn(input, p, first, builder)
                },
                ShapeTag::Keyword => quote! {
                    #target_fn(input, p, first, state, builder)
                },
                _ => quote! {
                    #target_fn(input, p, state, builder)
                },
            };
            let first_bytes: Vec<u8> = target.meta.first_set.iter().collect();
            if first_bytes.len() > 16 {
                Some((call, Vec::new()))
            } else {
                Some((call, first_bytes))
            }
        }
        // Regex / Seq / other Alt-branch shapes are not admitted on
        // JSON's Wrap rule (`value = object | array | string | number
        // | bool | null` — every branch is a Ref). Future grammars
        // (Sheets / CSS L4 in W2.B / W3) extend this match.
        _ => None,
    }
}

/// Emit one branch attempt body for the StructDirect linear-try
/// dispatch. Saves `*p`, calls into the target shape fn; on Ok records
/// the branch index and breaks the dispatch loop; on Err restores
/// `*p` and falls through to the next candidate.
fn emit_struct_branch_attempt(call: &TokenStream, branch_idx: u32) -> TokenStream {
    quote! {
        {
            let attempt_p = *p;
            let attempt_builder = builder.checkpoint();
            match #call {
                ::core::result::Result::Ok(_) => {
                    __wrap_branch_idx = #branch_idx;
                    builder.commit(attempt_builder);
                    break 'try_branches;
                }
                ::core::result::Result::Err(_) => {
                    *p = attempt_p;
                    builder.rollback(attempt_builder);
                }
            }
        }
    }
}

/// Emit the StructDirect Wrap-shape parse function. Mirrors the
/// byte-dispatch + linear-try fallback structure while routing
/// structural emission through
/// `builder.begin_compound(&__wrap_layout)` /
/// `builder.push_branch_tag(__wrap_branch_idx)` /
/// `builder.end_compound(__wrap_handle)`.
pub(super) fn emit_parse_wrap_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("wrap", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_outer(&rule.body);

    // AZ-I.W2-act.B3 — splice the builder type from the SubstrateBinding.
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    // For non-Alt Wrap bodies we emit a single Ref delegation; the
    // outer wrap compound is unnecessary because the delegated body
    // owns its own layout. JSON's `value` rule is Alt-rooted; this arm
    // is defensive (single-Ref aliases would not be classified as
    // Wrap by W3.1's classifier).
    let dispatch = match body {
        IrNode::Alt(branches, _) if rule.meta.is_transparent => {
            // AZ-II.cutover.H Phase 1 — transparent rules emit
            // passthrough dispatch only. The pre-cutover.H emitter
            // gate `if rule.meta.is_transparent { continue; }` skipped
            // emission entirely; cutover.H lifts the gate so call
            // sites can resolve to the per-shape fn, but the body
            // must NOT open a compound — the consumer of the
            // transparent rule (the Ref's host) does not expect a
            // record for the alias itself; it expects the chosen
            // branch's record to bubble up directly.
            emit_alt_struct_dispatch_transparent(branches, grammar_suffix, ir, strategy)
        }
        IrNode::Alt(branches, _) => {
            emit_alt_struct_dispatch(branches, grammar_suffix, ir, rule, strategy)
        }
        IrNode::Ref(rid) => {
            // Single-Ref Wrap — emit a direct delegation. No outer
            // begin/end_compound; the target shape fn carries its own
            // layout opening.
            match super::super::dispatcher::emit_ref_call_shape(grammar_suffix, *rid, ir) {
                Some(call) => quote! {
                    #call
                },
                None => quote! {
                    ::core::result::Result::Err(
                        crate::runtime::DtaError::Syntax {
                            offset: *p as u32,
                        },
                    )
                },
            }
        }
        _ => quote! {
            ::core::result::Result::Err(
                crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                },
            )
        },
    };

    quote! {
        /// AZ-I.W2.RD — struct-direct Wrap-shape parse function.
        ///
        /// Opens a Wrap frame on the builder, dispatches to the matched
        /// branch's shape fn (which carries its own
        /// begin_compound/end_compound for compound branches and the
        /// matching push_leaf_with_* for scalar branches), stamps the
        /// chosen branch index via push_branch_tag, then closes the
        /// Wrap frame. Mirrors `JsonStructBuilder::OpenFrame::Wrap`'s
        /// forward-the-single-child semantics.
        ///
        /// Returns unit for StructDirect composition
        /// with sibling shape fns under struct-direct mode; the
        /// offset is unused by struct-direct callers.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            use crate::runtime::builder::StructBuilder as _;

            #dispatch
        }
    }
}

/// Emit the StructDirect Alt-dispatch body. Partitions branches into
/// byte-dispatch (Refs with bounded first-byte set ≤ 16) and
/// linear-try fallback; opens a Wrap frame on the builder, runs the
/// dispatch, stamps the chosen branch index, then closes the frame.
fn emit_alt_struct_dispatch(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    ir: &GrammarIR,
    rule: &IrRule,
    strategy: &EmitStrategy,
) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let layout_literal = quote_layout_literal(rule, ir);
    let builder_ty_e = builder_ty_elided(strategy);

    let mut per_byte: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut linear_arms: Vec<TokenStream> = Vec::new();

    for (ord, branch) in branches.iter().enumerate() {
        let inner = unwrap_outer(&branch.node);
        let Some((call, first_bytes)) =
            emit_wrap_branch_call_struct_direct(inner, grammar_suffix, ir)
        else {
            continue;
        };
        let branch_idx = ord as u32;
        let attempt_body = emit_struct_branch_attempt(&call, branch_idx);
        if first_bytes.is_empty() {
            linear_arms.push(attempt_body);
        } else {
            for b in first_bytes {
                per_byte.entry(b).or_default().push(attempt_body.clone());
            }
        }
    }

    let byte_arms: Vec<TokenStream> = per_byte
        .into_iter()
        .map(|(byte, bodies)| {
            let byte_lit = byte;
            quote! {
                #byte_lit => {
                    #(#bodies)*
                }
            }
        })
        .collect();

    quote! {
        // AZ-I.W2.RD — open the Wrap compound. The layout literal is
        // built inline from the rule's registered metadata; the
        // builder routes any name not in (array | object | pair) to
        // OpenFrame::Wrap per the JsonStructBuilder dispatch.
        let __wrap_checkpoint = builder.checkpoint();
        let __wrap_layout: ::bbnf_ir::registry::StructLayout = #layout_literal;
        let __wrap_handle = <
            #builder_ty_e as crate::runtime::StructBuilder
        >::begin_compound(builder, &__wrap_layout);
        let mut __wrap_branch_idx: u32 = 0;
        // AZ-II.cutover.K Phase 2 — wrap the leading skip_space
        // surface in an IIFE so the `?` propagation can't bypass the
        // matching `end_compound`. Pre-fix `skip_space?` returned
        // `Err` directly from the rule body, leaking the open Wrap
        // frame onto the builder's stack.
        let __body_result: ::core::result::Result<
            (),
            crate::runtime::DtaError,
        > = (|| {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            'try_branches: loop {
                match first {
                    #(#byte_arms)*
                    _ => {}
                }
                #(#linear_arms)*
                // No branch succeeded — bubble the error; the trailer
                // closes the open compound after observing the IIFE
                // result.
                return ::core::result::Result::Err(
                    crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    },
                );
            }
            ::core::result::Result::Ok(())
        })();
        match __body_result {
            ::core::result::Result::Ok(()) => {
                <
                    #builder_ty_e as crate::runtime::StructBuilder
                >::push_branch_tag(builder, __wrap_branch_idx);
                <
                    #builder_ty_e as crate::runtime::StructBuilder
                >::end_compound(builder, __wrap_handle);
                ::core::result::Result::Ok(())
            }
            ::core::result::Result::Err(e) => {
                <
                    #builder_ty_e as crate::runtime::StructBuilder
                >::rollback(builder, __wrap_checkpoint);
                ::core::result::Result::Err(e)
            }
        }
    }
}

/// AZ-II.cutover.H Phase 1 — transparent-rule wrap dispatch.
///
/// Emits the Alt-dispatch body without an outer `begin_compound` /
/// `end_compound` pair. The transparent rule's compound is elided
/// per the runtime contract: the Ref's host expects the chosen
/// branch's record to bubble up directly, not a wrapper compound
/// for the alias rule itself.
fn emit_alt_struct_dispatch_transparent(
    branches: &[bbnf_ir::AltBranch],
    grammar_suffix: &str,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let _ = strategy;

    let mut per_byte: std::collections::BTreeMap<u8, Vec<TokenStream>> = Default::default();
    let mut linear_arms: Vec<TokenStream> = Vec::new();

    for branch in branches.iter() {
        let inner = unwrap_outer(&branch.node);
        let Some((call, first_bytes)) =
            emit_wrap_branch_call_struct_direct(inner, grammar_suffix, ir)
        else {
            continue;
        };
        let attempt_body = emit_struct_branch_attempt_transparent(&call);
        if first_bytes.is_empty() {
            linear_arms.push(attempt_body);
        } else {
            for b in first_bytes {
                per_byte.entry(b).or_default().push(attempt_body.clone());
            }
        }
    }

    let byte_arms: Vec<TokenStream> = per_byte
        .into_iter()
        .map(|(byte, bodies)| {
            let byte_lit = byte;
            quote! {
                #byte_lit => {
                    #(#bodies)*
                }
            }
        })
        .collect();

    quote! {
        // AZ-II.cutover.H Phase 1 — transparent-rule passthrough.
        // No outer compound. Dispatch to the chosen branch; the
        // branch's per-shape fn carries its own record emission.
        let first = #support_mod::skip_space(input, p, state)
            .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                offset: *p as u32,
            })?;
        'try_branches: loop {
            match first {
                #(#byte_arms)*
                _ => {}
            }
            #(#linear_arms)*
            return ::core::result::Result::Err(
                crate::runtime::DtaError::Syntax {
                    offset: *p as u32,
                },
            );
        }
        ::core::result::Result::Ok(())
    }
}

/// Branch attempt for the transparent-rule wrap path: same shape as
/// `emit_struct_branch_attempt` but does NOT touch `__wrap_branch_idx`
/// since no wrap compound is open.
fn emit_struct_branch_attempt_transparent(call: &TokenStream) -> TokenStream {
    quote! {
        {
            let attempt_p = *p;
            let attempt_builder = builder.checkpoint();
            match #call {
                ::core::result::Result::Ok(_) => {
                    builder.commit(attempt_builder);
                    break 'try_branches;
                }
                ::core::result::Result::Err(_) => {
                    *p = attempt_p;
                    builder.rollback(attempt_builder);
                }
            }
        }
    }
}
