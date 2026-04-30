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
//!   `dispatcher::emit_ref_call_shape` — no `__value` indirection.
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

use super::dispatcher::shape_fn_ident;
use super::substrate::builder_ty_with_lifetime;
use bbnf_ir::registry::EmitStrategy;

mod branches;
mod visitor;

pub use visitor::emit_parse_alt_dispatch_visitor;

use branches::unwrap_trivia;

/// Emit the `parse_altdispatch_<grammar>_<rule>` body for the
/// resolved struct-builder substrate.
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
    emit_parse_alt_dispatch_struct_direct(grammar_suffix, rule, ir, strategy)
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
    let rule_name_lit = rule_name.to_string();
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let body = unwrap_trivia(&rule.body);
    let IrNode::Alt(branches, _) = body else {
        return quote! {};
    };

    // Per-branch first-byte sets + struct-direct dispatch arm bodies.
    let dispatch_arms =
        branches::emit_dispatch_arms_struct_direct(branches, grammar_suffix, rule, ir);

    // AZ-I.W2-act.recovery — inline `StructLayout` literal mirroring
    // the per-shape pattern used by `array/struct_direct`,
    // `flat/struct_direct`, `wrap/struct_direct`,
    // `keyword/struct_direct`, and `pratt/struct_direct`. The
    // pre-recovery body emitted a call to `ir_struct_registry_layout`
    // — a helper that does not exist in `bbnf_ir` or any consumed
    // crate; the resulting generated code did not compile on CSS L4
    // (which exercises 6 AltDispatch rules under StructDirect).
    // `JsonStructBuilder::begin_compound` / `CssStructBuilder::begin_compound`
    // / `SheetsStructBuilder::begin_compound` consult `(layout.kind,
    // layout.rule_name)` only; defaults suffice for `fields` /
    // `rule_type`.
    quote! {
        /// AZ-I.W2.RB — per-grammar AltDispatch-shape parse function,
        /// **struct-direct body**. Targets the grammar's concrete
        /// `StructBuilder` (JSON / Sheets / CSS L4 per the resolver's
        /// `SubstrateBinding`).
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments, unreachable_code)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<(), crate::runtime::DtaError> {
            use crate::runtime::builder::StructBuilder;

            let first = #support_mod::skip_space(input, p, state)
                .ok_or(crate::runtime::DtaError::UnexpectedEnd {
                    offset: *p as u32,
                })?;

            // AZ-I.W2-act.recovery — inline layout literal. The
            // builder routes `(kind, rule_name)` through its
            // OpenFrame dispatch; for AltDispatch under CSS L4 the
            // unmatched-rule-name path collapses to OpenFrame::Wrap,
            // which is the right semantics for an Alt-of-leaves
            // (every branch's scalar lands as the wrap's single
            // child via push_branch_tag + the per-branch leaf push).
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::TaggedEnum,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __dispatch_checkpoint = builder.checkpoint();
            let __handle = builder.begin_compound(&__layout);
            let __dispatch_result: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                #dispatch_arms
                ::core::result::Result::Ok(())
            })();
            match __dispatch_result {
                ::core::result::Result::Ok(()) => {
                    builder.end_compound(__handle);
                    Ok(())
                }
                ::core::result::Result::Err(__err) => {
                    builder.rollback(__dispatch_checkpoint);
                    ::core::result::Result::Err(__err)
                }
            }
        }
    }
}
