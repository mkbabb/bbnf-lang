//! AltDispatch-shape emitter — `parse_altdispatch_<grammar>_<rule>`.
//!
//! # Role — AX.W0a.2.b
//!
//! Emits per-grammar AltDispatch-shape parse functions for
//! generalized `Alt` byte-dispatchers (Wrap's strict superset).
//! Handles the mixed-leaf Alt pattern CSS `value`, BBNF `type_name`,
//! CSS `keyframeStop`, EBNF `letter`, and similar rules use.
//!
//! # Emission shape
//!
//! Byte-dispatch on the first non-whitespace byte:
//!
//! - Computes each branch's first-byte set via the branch body's
//!   structural head (Literal's first byte, Regex's NFA start set,
//!   Ref target's rule `meta.first_set`).
//! - Emits one match-arm per branch, keyed on the branch's first-
//!   byte set; overlapping sets fall through to a default arm that
//!   tries each branch in Alt order.
//! - For `Ref` branches the arm calls the target's shape fn via
//!   `dispatcher::emit_ref_call_tape` — no `__value` indirection.
//! - For `Literal` branches the arm matches the literal's bytes
//!   inline and pushes a `Literal` leaf.
//! - For `Regex` branches the arm performs a canonical non-whitespace
//!   scan (CSS `/[^\s;!}]+/` fallback pattern).
//! - For `Seq` branches (prefix-tree-factored keyword chains) the
//!   arm matches the flattened literal sequence inline.
//!
//! # Walker-parity compound
//!
//! Each admitted branch emits a `Rule`-kind outer compound stamping
//! `variant_idx = rule.id & 0xFF` so downstream `TapeView` readers
//! see the same shape the walker's Alt frame produces. The per-
//! branch leaf / compound pushes land as children of that Rule.

use bbnf_ir::{GrammarIR, IrNode, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use bbnf_ir::registry::EmitStrategy;
use super::dispatcher::shape_fn_ident;
use super::substrate::builder_ty_with_lifetime;

mod branches;
mod payload;
mod visitor;

pub use visitor::emit_parse_alt_dispatch_visitor;

use branches::{emit_dispatch_arms, unwrap_trivia};

/// Emit `pub fn parse_altdispatch_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>` for
/// [`EmitStrategy::TapeDirect`], or the matching `JsonStructBuilder`-
/// targeted body for [`EmitStrategy::StructDirect`].
///
/// AZ-I.W2.RB — strategy match is at codegen time. Per
/// `feedback_no-orthogonal-codepaths` ONE function body is emitted per
/// `(grammar, rule)`; the strategy selects which.
pub fn emit_parse_alt_dispatch(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    match strategy {
        EmitStrategy::StructDirect { .. } => {
            emit_parse_alt_dispatch_struct_direct(grammar_suffix, rule, ir, strategy)
        }
        EmitStrategy::TapeDirect => emit_parse_alt_dispatch_tape(grammar_suffix, rule, ir),
    }
}

fn emit_parse_alt_dispatch_tape(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("altdispatch", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    // Build (first-byte-set, branch-arm-body) pairs.
    // AX.W0a.2.q — pass `rule` so typed-Alt Literal branches can emit
    // the owning rule's payload (CSS `namedColor` → u32; analogous U8
    // / F64 shapes). Untyped rules fall through to the pre-W0a.2.q
    // plain-literal emission.
    let dispatch_arms = emit_dispatch_arms(branches, grammar_suffix, rule, ir);

    quote! {
        /// AX.W0a.2.b — per-grammar AltDispatch-shape parse function.
        ///
        /// Generalized byte-dispatch over `Alt(leaf, leaf, …)` bodies.
        /// Each branch is a classified-Ref, a Literal, a Regex, or a
        /// leaf-only Seq (prefix-tree factoring). No recursion through
        /// `__value` — per-Ref routing emits direct shape-fn calls.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;
            let alt_lo = *p as u32;
            // AY-II.W0.b — alt compound is walker-parity POST-ORDER:
            // branch records emit first, then the outer Rule row lands
            // after them. Capture the first-child index before branch
            // emission; allocate the compound row at post-branch
            // position via begin_compound_post; close immediately;
            // override child_off to name the first branch record.
            //
            // B5.W6 — bracket the post-order children scope so child
            // records stamp `frame_depth` at the correct (parent + 1)
            // depth at push time. Branch-failure paths inside the
            // dispatch arms call `exit_post_order_children` paired with
            // `rollback_to` per the alt_dispatch/branches.rs retry
            // template; the success path absorbs the bracket bump via
            // `end_compound_post_order` below.
            let alt_child = builder.enter_post_order_children();
            let _ = alt_child;
            #dispatch_arms
            let alt_hi = *p as u32;
            let off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Rule,
                alt_lo,
                #variant_idx,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                off,
                alt_hi,
                crate::runtime::tape::TapeOffset(alt_child),
            );
            Ok(crate::runtime::tape::TapeOffset(off))
        }
    }
}

/// AZ-I.W2.RB — struct-direct body for the AltDispatch shape.
///
/// JSON does not exercise AltDispatch (its `value` rule is classified
/// `Wrap`), so this body is reachable only when a future StructDirect
/// grammar admits AltDispatch (CSS L4 in W3, Sheets in W2.B). The
/// emission contract is: `begin_compound(&__layout)` opens the alt
/// frame, the dispatch picks a branch, `push_branch_tag(idx)` records
/// the discriminator, the branch body fires (Ref calls + builder
/// pushes), and `end_compound(handle)` closes.
///
/// Per the W2-EMITTER-REWIRE plan §3 hard gates, this body emits zero
/// `tape.*` calls; per-branch emission for struct-direct rewrites the
/// inline leaf pushes through `builder.push_leaf_with_*` matching
/// JsonStructBuilder's API.
fn emit_parse_alt_dispatch_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("altdispatch", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    // Per-branch first-byte sets + struct-direct dispatch arm bodies.
    let dispatch_arms = branches::emit_dispatch_arms_struct_direct(
        branches, grammar_suffix, rule, ir,
    );

    quote! {
        /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
        /// **struct-direct body**. Targets [`JsonStructBuilder`] (or
        /// the future per-grammar concrete builder for AltDispatch-
        /// admitting grammars).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident<'p>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::tape::DtaError> {
            use crate::runtime::builder::StructBuilder;

            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::tape::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;

            // AZ-I.W2.RB — open the alt compound. Layout is W1.A-
            // guaranteed for StructDirect.
            let __layout = ir_struct_registry_layout(#rule_id_lit)
                .expect("StructDirect requires populated layout for AltDispatch rule");
            let __handle = builder.begin_compound(__layout);

            #dispatch_arms

            builder.end_compound(__handle);
            Ok(())
        }
    }
}
