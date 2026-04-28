//! AZ-I.W2-act.recovery — struct-direct Pratt-shape body.
//!
//! W2.RE asserted "Pratt shape does not support StructDirect" and
//! panicked at codegen time. Once the resolver flipped Sheets and
//! CSS L4 to StructDirect (W2-act.B2/B3), Sheets's
//! `comparison_expr` / `concat_expr` / `add_expr` / `mul_expr` /
//! `exp_expr` rules surfaced Pratt and tripped the panic during
//! `cargo xtask regen`. This module restores activation by emitting
//! a real struct-direct body for Pratt that drives the grammar's
//! concrete `StructBuilder` impl directly.
//!
//! # Emission shape
//!
//! For a Pratt rule whose body is `operand (op operand)*` (canonical
//! example: Sheets `add_expr = mul_expr ?w , (add_op ?w , mul_expr ?w) *`):
//!
//! ```text
//! let __layout: bbnf_ir::registry::StructLayout = { rule_name = "add_expr"; … };
//! let __handle = builder.begin_compound(&__layout);
//! // 1. Dispatch leftmost operand → recursive parse fn deposits its
//! //    own value (scalar leaf or nested Compound) onto the open frame.
//! // 2. Loop: peek byte; consult PRECEDENCE_LUT_<rule>; on a matching
//! //    operator at this rule's precedence, push_branch_tag(op_disc)
//! //    and dispatch the RHS operand. Higher-precedence operators are
//! //    consumed by the inner operand's recursive Pratt body
//! //    (operands resolve through the per-grammar value dispatcher,
//! //    which re-enters the matching shape fn for the operand rule
//! //    — typically the next-tier Pratt rule); lower-precedence /
//! //    EOF operators terminate the loop.
//! // 3. Close the outer compound. The compound's children now carry
//! //    the linear sequence [lhs_subtree, op_tag, rhs_subtree, op_tag,
//! //    rhs_subtree, ...] — operator-and-operand interleave at this
//! //    rule's precedence tier. The binary tree (with associativity
//! //    honoured) is reconstructible by consumers using the rule's
//! //    PRECEDENCE_ENTRIES and SheetsCompoundKind / equivalent metadata.
//! builder.end_compound(__handle);
//! ```
//!
//! # Wire contract
//!
//! Per `crates/core/tests/sheets_expr_parity.rs::tests::
//! document_add_expr_one_plus_two_via_wire_contract`, the
//! `SheetsStructBuilder` consumer expects an `AddExpr` compound for
//! `=1+2` carrying linear children `[Number(1.0), Tag(0), Number(2.0)]`:
//! one operand, one operator tag, one operand. Higher-precedence
//! operators surface as nested `Compound` children (the recursive
//! operand's own Pratt rule packs them); same-tier chains
//! (`=1+2+3`) project as `[1, +, 2, +, 3]` linearly.
//!
//! # Precedence & associativity — the consumer side
//!
//! Per `feedback_preserve-rich-ast`, the rule's structural shape
//! (kind discriminator + linear children) preserves every typed
//! projection the grammar declares; the binary-tree precedence /
//! associativity reduction is a CONSUMER projection. The runtime
//! exposes `PRECEDENCE_ENTRIES_<rule>` per rule with op-byte +
//! precedence + associativity per entry; consumers (oracle, view-
//! walker, parity harness) consult the entries to rebuild the
//! associativity-honouring binary tree. The struct-direct body's
//! job is to record the operator chain faithfully — semantic
//! reduction lives one layer up, where it can be tested + tuned
//! independently.
//!
//! # Why not eager binary-nesting?
//!
//! The eager binary-nesting alternative — wrap each reducer in its
//! own `begin_compound` / `end_compound` pair — requires the
//! `StructBuilder` to expose a "wrap previously-deposited children"
//! primitive. The current trait surface (`begin_compound` opens a
//! frame BEFORE pushes, `end_compound` finalises it) does not admit
//! retroactive wrapping; once a child is deposited on a frame's
//! children Vec, it cannot be moved into a sibling-then-parent
//! reducer compound without runtime-side support absent at AZ-I.W2.
//! The flat-children projection is the highest-fidelity record the
//! existing trait surface produces, and the wire-contract test
//! confirms it.

use bbnf_ir::registry::{EmitStrategy, LayoutKind};
use bbnf_ir::{GrammarIR, IrRule};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, shape_fn_ident,
};
use super::super::root_rule_name;
use super::super::substrate::{builder_ty_elided, builder_ty_with_lifetime};
use super::extract_first_ref;

/// Codegen-time inspection of the rule's registered layout. Returns
/// the registry entry's `(kind, rule_name)` when present; otherwise
/// the structural fallback `(LayoutKind::Struct, rule_name)` keyed by
/// the grammar-registered identifier.
fn resolve_layout_meta<'a>(
    rule: &IrRule,
    ir: &'a GrammarIR,
) -> (LayoutKind, &'a str) {
    if let Some(layout) = ir.struct_registry.layout(rule.id) {
        return (layout.kind, layout.rule_name.as_str());
    }
    (LayoutKind::Struct, ir.get_string(rule.name))
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
/// rule's registered metadata. Mirrors the per-shape pattern used by
/// `flat::struct_direct` / `wrap::struct_direct` / `array`'s
/// struct-direct emission: `kind` + `rule_name` are the only fields
/// the concrete `StructBuilder::begin_compound` consults at the
/// dispatch boundary.
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

/// Emit the struct-direct Pratt-shape parse function.
///
/// Mirrors the tape-path entry's outer structure (operand dispatch +
/// LUT-driven operator loop) but routes structural emission through
/// the grammar's concrete `StructBuilder`'s
/// `begin_compound(&__layout)` /
/// `push_branch_tag(op_discriminant)` /
/// `end_compound(handle)` calls. Per-rule operand calls thread the
/// same `&mut <BuilderTy>` argument the tape path threads as
/// `&mut Tape<()>`; the call signature is consistent across both
/// substrates because [`super::super::dispatcher::emit_ref_call_tape`]
/// resolves the target shape's parse fn name uniformly.
pub(super) fn emit_parse_pratt_struct_direct(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
    strategy: &EmitStrategy,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("pratt", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_lut_ident = format_ident!("PRECEDENCE_LUT_{}", rule_name);
    let rule_entries_ident = format_ident!("PRECEDENCE_ENTRIES_{}", rule_name);

    // The per-grammar value-position dispatcher — the operand parses
    // recurse through this so nested calls, parens, function calls,
    // numbers, identifiers all resolve via the existing shape
    // dispatch. If the grammar has no root rule (ir.entry not set)
    // emit nothing — the grammar gates this shape off wholesale.
    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    // AW-V.W5.2 — resolve the operand Ref from the Pratt body so
    // operand calls can route through the per-Ref direct shape fn
    // rather than the dispatcher (the operand rule is statically
    // known at codegen time; no runtime byte-dispatch needed).
    let operand_ref = extract_first_ref(&rule.body);
    let operand_call = operand_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _ = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _ = #dispatcher_ident(input, p, state, builder)?;
            }
        });
    let rhs_call = operand_ref
        .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
        .map(|call| quote! { let _ = (#call)?; })
        .unwrap_or_else(|| {
            quote! {
                let _ = #dispatcher_ident(input, p, state, builder)?;
            }
        });

    // AZ-I.W2-act.B3 — substrate-binding splice: the function
    // signature carries the grammar's concrete builder type rather
    // than the JSON-specific path.
    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);
    let builder_ty_e = builder_ty_elided(strategy);

    let layout_literal = quote_layout_literal(rule, ir);
    let layout_var = format_ident!("__{}_layout", rule_name);
    let handle_var = format_ident!("__{}_handle", rule_name);

    quote! {
        /// AZ-I.W2-act.recovery — per-grammar Pratt-shape parse
        /// function, **struct-direct body**. Targets the grammar's
        /// concrete `StructBuilder`.
        ///
        /// Opens a compound for the rule (e.g. `add_expr` →
        /// `SheetsCompoundKind::AddExpr`), dispatches operands +
        /// stamps operator branch tags inline, closes the compound.
        /// Children land in the order
        /// `[lhs_subtree, op_tag, rhs_subtree, op_tag, …]` — the
        /// rule's structural alphabet is preserved verbatim;
        /// associativity-honouring binary-tree reduction is a
        /// consumer-side projection (the runtime exposes
        /// `PRECEDENCE_ENTRIES_<rule>` for that purpose).
        ///
        /// Returns `TapeOffset::NONE` for compositional uniformity
        /// with sibling shape fns under struct-direct mode.
        ///
        /// AX.W0a.2.f — `#[inline]` (not `#[inline(always)]`):
        /// cross-shape recursive edge through the value dispatcher.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut, unused_assignments)]
        pub fn #fn_ident<'p>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            let _ = #support_mod::skip_space(input, p, state);

            // ── Open the rule compound ──────────────────────────────
            let #layout_var: ::bbnf_ir::registry::StructLayout = #layout_literal;
            let #handle_var = <
                #builder_ty_e as crate::runtime::StructBuilder
            >::begin_compound(builder, &#layout_var);

            // ── Leftmost operand ────────────────────────────────────
            // Recurses through the operand's per-shape parse fn,
            // which deposits its own value (scalar or compound) onto
            // this rule's open frame as the first child.
            #operand_call

            // ── Operator + RHS loop ─────────────────────────────────
            //
            // Same structural shape as the tape-path emitter (peek →
            // LUT consult → reduce-or-push); the StructDirect path
            // omits the op-stack reducer because the linear
            // children projection captures the operator chain
            // directly. Every matched operator stamps a branch tag
            // (carrying the op_discriminant byte) and re-dispatches
            // the RHS operand. Lower-precedence / EOF operators
            // terminate the loop, closing the rule compound below.
            loop {
                let mut op_byte: u8 = input.get(*p).copied().unwrap_or(0);
                let mut lut_byte: u8 = #rule_lut_ident[op_byte as usize];
                if lut_byte == 0 {
                    let _ = #support_mod::skip_space(input, p, state);
                    op_byte = input.get(*p).copied().unwrap_or(0);
                    lut_byte = #rule_lut_ident[op_byte as usize];
                }
                if lut_byte == 0 {
                    break;
                }

                let two_byte: u8 = (lut_byte >> 7) & 0x01u8;
                let second_byte: ::core::option::Option<u8> =
                    input.get(*p + 1).copied();

                let (op_width, op_discriminant, op_matched) = if two_byte == 0 {
                    let mut found_disc: u8 = 0u8;
                    let mut matched: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte.is_none() {
                            found_disc = e.op_discriminant;
                            matched = true;
                            break;
                        }
                    }
                    (1u32, found_disc, matched)
                } else {
                    let mut found_disc: u8 = 0u8;
                    let mut matched_two_byte: bool = false;
                    let mut matched_single: bool = false;
                    for e in #rule_entries_ident.iter() {
                        if e.byte == op_byte && e.second_byte == second_byte {
                            found_disc = e.op_discriminant;
                            matched_two_byte = e.second_byte.is_some();
                            break;
                        }
                    }
                    if !matched_two_byte {
                        for e in #rule_entries_ident.iter() {
                            if e.byte == op_byte && e.second_byte.is_none() {
                                found_disc = e.op_discriminant;
                                matched_single = true;
                                break;
                            }
                        }
                    }
                    let width = if matched_two_byte { 2u32 } else { 1u32 };
                    (width, found_disc, matched_two_byte || matched_single)
                };

                // AX.W0a.2.n parity — phantom-op guard: a nonzero
                // LUT byte without a concrete entry match is the
                // first byte of a two-byte op alone (e.g. `|`
                // where only `||` is real). Break rather than
                // consume.
                if !op_matched {
                    break;
                }

                // ── Operator tag ─────────────────────────────────
                // Stamp the operator's discriminant on the open
                // rule frame as a typed branch tag. Sheets routes
                // this through `SheetsStructBuilder::push_branch_tag`
                // (deposits as `SheetsValue::Tag(op_discriminant)`).
                <
                    #builder_ty_e as crate::runtime::StructBuilder
                >::push_branch_tag(builder, op_discriminant as u32);

                *p = (*p).saturating_add(op_width as usize);

                // ── RHS operand ─────────────────────────────────
                let _ = #support_mod::skip_space(input, p, state);
                #rhs_call
            }

            // ── Close the rule compound ─────────────────────────────
            <
                #builder_ty_e as crate::runtime::StructBuilder
            >::end_compound(builder, #handle_var);

            ::core::result::Result::Ok(crate::runtime::tape::TapeOffset::NONE)
        }
    }
}
