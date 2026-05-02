//! Unordered-shape emitter — `parse_unordered_<grammar>_<rule>`.
//!
//! # Role — AW-V.W4-fix
//!
//! Emits per-grammar Unordered-shape parse functions for
//! `Repeat { lo: 1, .. }` over disjoint-FIRST Alts. The emitted body
//! runs a byte-dispatch sub-loop:
//!
//! 1. Capture the outer span / children offset.
//! 2. Skip leading whitespace.
//! 3. Loop — test `input[*p]` against each branch's mined FIRST byte
//!    set. First match routes into the branch dispatch (the per-
//!    grammar value-position dispatcher, same call site the W3 Array /
//!    Object shape emitters use for nested dispatch). A byte that no
//!    branch's FIRST admits exits the Kleene loop.
//! 4. Enforce `iters >= lo` — an Unordered with `lo: 1` that saw no
//!    branches is a syntax error.
//! 5. Close the outer StructDirect compound after all admitted
//!    branches have been consumed.
//!
//! Canonical: CSS `compoundSelector = (classSelector | idSelector |
//!   attrSelector | colonSelector | typeSelector) +` per
//! `grammar/css/l4/selectors.bbnf:87-88`. Five branches with disjoint
//! FIRST bytes.
//!
//! # StructDirect parity
//!
//! The builder opens one compound for the Repeat rule. The Alt state
//! under it is a byte dispatch that adds no separate frame, so each
//! iteration's record stream is whatever the chosen branch Ref emits.

use std::collections::HashSet;

use bbnf_ir::{CharSet128, GrammarIR, IrNode, IrRule, RuleId};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{dispatcher_fn_ident, emit_ref_call_shape, shape_fn_ident};
use super::root_rule_name;
use super::substrate::builder_ty_with_lifetime;
use bbnf_ir::registry::EmitStrategy;

// ─── Unordered body introspection ───────────────────────────────────

/// Walker-parity decomposition of an Unordered rule's body.
///
/// Extracted once at emit time so the StructDirect emitter has a
/// single branch projection + FIRST byte computation.
/// The FIRST byte set per branch is projected from the IR by the same
/// structural walk the Unordered detector uses (see
/// [`bbnf_ir::passes::recognizers::shape_dispatch::unordered`] — the
/// projections agree).
struct UnorderedBody {
    /// Per-Alt-branch FIRST byte sets. Every set is non-empty and
    /// pairwise-disjoint — the detector's admission invariant.
    first_sets: Vec<CharSet128>,
    /// Per-Alt-branch target Ref (when the branch is a Ref node).
    /// `None` for non-Ref branches (e.g. inline Literal / Regex /
    /// Seq). Used by AW-V.W5.2 per-Ref routing to emit direct calls.
    branch_refs: Vec<Option<RuleId>>,
    /// Minimum iteration count from the Repeat's `lo` field (≥ 1 per
    /// the detector admission).
    iters_lo: u32,
}

/// Introspect an Unordered rule body. Returns `None` when the body
/// isn't actually Unordered-shaped — the emitter degrades to a
/// walker-fallback delegate in that case.
fn introspect_unordered(rule: &IrRule, ir: &GrammarIR) -> Option<UnorderedBody> {
    let body = unwrap_map_ow(&rule.body);
    let IrNode::Repeat { inner, lo, .. } = body else {
        return None;
    };
    let lo = *lo;
    if lo < 1 {
        return None;
    }
    let inner_body = unwrap_map_ow(inner);
    let IrNode::Alt(branches, _) = inner_body else {
        return None;
    };
    let mut first_sets = Vec::with_capacity(branches.len());
    let mut branch_refs: Vec<Option<RuleId>> = Vec::with_capacity(branches.len());
    let mut visited: HashSet<RuleId> = HashSet::new();
    for branch in branches {
        let fs = node_first(&branch.node, ir, &mut visited)?;
        if fs.is_empty() {
            return None;
        }
        first_sets.push(fs);
        // AW-V.W5.2 — capture the branch's target Ref if present.
        let branch_ref = match unwrap_map_ow(&branch.node) {
            IrNode::Ref(rid) => Some(*rid),
            _ => None,
        };
        branch_refs.push(branch_ref);
    }
    Some(UnorderedBody {
        first_sets,
        branch_refs,
        iters_lo: lo,
    })
}

/// Mirror of [`bbnf_ir::passes::inspect::unwrap_map_ow`] — strips
/// `Map { .. }` and `OptionalWhitespace(..)` wrappers. Inlined to
/// avoid a cross-crate re-export from the emitter path.
fn unwrap_map_ow(node: &IrNode) -> &IrNode {
    match node {
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => unwrap_map_ow(inner),
        other => other,
    }
}

/// Check if a node structurally matches empty input — used by the
/// FIRST-set projection to decide whether a Seq's FIRST carries
/// through past the head element.
fn is_nullable(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> bool {
    match node {
        IrNode::Epsilon => true,
        IrNode::Repeat { lo: 0, .. } => true,
        IrNode::Literal(sid) => ir.get_string(*sid).is_empty(),
        IrNode::Regex(_) | IrNode::Negate(_) => false,
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return false;
            }
            let res = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .map(|r| is_nullable(&r.body, ir, visited))
                .unwrap_or(false);
            visited.remove(rid);
            res
        }
        IrNode::Seq(children) => children.iter().all(|c| is_nullable(c, ir, visited)),
        IrNode::Alt(branches, _) => branches.iter().any(|b| is_nullable(&b.node, ir, visited)),
        IrNode::Repeat { lo, .. } => *lo == 0,
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            is_nullable(a, ir, visited) && is_nullable(b, ir, visited)
        }
        IrNode::Minus(a, _) => is_nullable(a, ir, visited),
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => {
            is_nullable(inner, ir, visited)
        }
        IrNode::TokenDispatch { token, .. } => is_nullable(token, ir, visited),
    }
}

/// Project the FIRST byte set of a node by structural walk with a
/// cycle guard on Ref descent. Matches the sibling detector's
/// [`bbnf_ir::passes::recognizers::shape_dispatch::unordered`]
/// projection — the two walks produce the same set per branch so the
/// emitter's byte-dispatch arms cover the same byte space the
/// detector admitted.
fn node_first(node: &IrNode, ir: &GrammarIR, visited: &mut HashSet<RuleId>) -> Option<CharSet128> {
    match node {
        IrNode::Literal(sid) => {
            let bytes = ir.get_string(*sid).as_bytes();
            if bytes.is_empty() {
                None
            } else {
                let mut cs = CharSet128::new();
                cs.add(bytes[0]);
                Some(cs)
            }
        }
        IrNode::Regex(sid) => {
            let pattern = ir.get_string(*sid);
            Some(bbnf_ir::regex_first::regex_first_chars(pattern).unwrap_or_default())
        }
        IrNode::Epsilon => Some(CharSet128::new()),
        IrNode::Ref(rid) => {
            if !visited.insert(*rid) {
                return None;
            }
            let result = ir
                .rules
                .iter()
                .find(|r| r.id == *rid)
                .and_then(|r| node_first(&r.body, ir, visited));
            visited.remove(rid);
            result
        }
        IrNode::Seq(children) => {
            let mut acc = CharSet128::new();
            for c in children {
                let part = node_first(c, ir, visited)?;
                acc.union(&part);
                if !is_nullable(c, ir, visited) {
                    return Some(acc);
                }
            }
            Some(acc)
        }
        IrNode::Alt(branches, _) => {
            let mut acc = CharSet128::new();
            for b in branches {
                let part = node_first(&b.node, ir, visited)?;
                acc.union(&part);
            }
            Some(acc)
        }
        IrNode::Skip(a, b) | IrNode::Next(a, b) => {
            let mut acc = node_first(a, ir, visited)?;
            if is_nullable(a, ir, visited) {
                let part = node_first(b, ir, visited)?;
                acc.union(&part);
            }
            Some(acc)
        }
        IrNode::Minus(a, _) => node_first(a, ir, visited),
        IrNode::Map { inner, .. } | IrNode::OptionalWhitespace(inner) => {
            node_first(inner, ir, visited)
        }
        IrNode::Repeat { inner, .. } => node_first(inner, ir, visited),
        IrNode::Negate(_) | IrNode::TokenDispatch { .. } => None,
    }
}

/// Build a byte-match arm literal `b0 | b1 | …` from a `CharSet128`.
/// Every byte in `set` appears as its own literal in the arm pattern.
fn emit_byte_match_arm(set: &CharSet128) -> TokenStream {
    let bytes: Vec<u8> = set.iter().collect();
    if bytes.is_empty() {
        return quote! { _ if false };
    }
    let mut arms = Vec::with_capacity(bytes.len());
    for b in bytes {
        let lit = proc_macro2::Literal::u8_unsuffixed(b);
        arms.push(quote! { #lit });
    }
    quote! { #(#arms)|* }
}

// ─── StructDirect emitter ──────────────────────────────────────────

/// Emit `pub fn parse_unordered_<grammar>_<rule>(input, p, state,
/// builder) -> Result<(), DtaError>`.
///
/// # AZ-I.W2.RE — strategy gate
///
/// `strategy` is the codegen-time substrate selector resolved by
/// [`EmitStrategy::for_grammar`] in `shapes/mod.rs`. AZ-II O5 leaves
/// only the StructDirect substrate.
pub fn emit_parse_unordered(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    emit_parse_unordered_struct_direct(strategy, grammar_suffix, rule, ir)
}

// ─────────────────────────────────────────────────────────────────────
// AZ-I.W2-act.B3 — Unordered struct-direct body.
// ─────────────────────────────────────────────────────────────────────

/// Emit the struct-direct Unordered body. Byte-matches the Repeat's
/// branch FIRST sets and routes the outer compound through
/// `begin_compound(&__layout)` / `end_compound(handle)`. Each
/// branch's per-Ref call passes the `&mut <builder_ty>` argument.
fn emit_parse_unordered_struct_direct(
    strategy: &EmitStrategy,
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("unordered", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);
    let rule_id_lit = rule.id;
    let rule_name_lit = rule_name.to_string();

    let p_lt = format_ident!("p");
    let builder_ty = builder_ty_with_lifetime(strategy, &p_lt);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => return quote! {},
    };

    let Some(body_info) = introspect_unordered(rule, ir) else {
        // Defensive fallback: a malformed Unordered rule under
        // StructDirect emits a stub that delegates to the dispatcher.
        return quote! {
            #[inline]
            #[allow(non_snake_case, clippy::too_many_arguments, unused_variables)]
            pub fn #fn_ident<'p, __P>(
                input: &'p [u8],
                p: &mut usize,
                state: &mut #support_mod::ScanState,
                builder: &mut #builder_ty,
                cursor: &mut crate::path::cursor::PathCursor<'p, __P>,
            ) -> ::core::result::Result<(), crate::runtime::DtaError>
            where
                __P: crate::path::schema::PathSchema<'p>,
            {
                let _ = #dispatcher_ident(input, p, state, builder, cursor)?;
                Ok(())
            }
        };
    };

    let mut branch_arms = Vec::with_capacity(body_info.first_sets.len());
    for (first_set, branch_ref) in body_info
        .first_sets
        .iter()
        .zip(body_info.branch_refs.iter())
    {
        let pattern = emit_byte_match_arm(first_set);
        let call = branch_ref
            .and_then(|rid| emit_ref_call_shape(grammar_suffix, rid, ir))
            .map(|call| quote! { let _ = (#call)?; })
            .unwrap_or_else(|| {
                quote! { let _ = #dispatcher_ident(input, p, state, builder, cursor)?; }
            });
        branch_arms.push(quote! {
            #pattern => {
                #call
                __iters += 1;
            }
        });
    }
    let iters_lo_lit = proc_macro2::Literal::u32_unsuffixed(body_info.iters_lo);

    quote! {
        /// AZ-I.W2-act.B3 — per-grammar Unordered-shape parse function,
        /// **struct-direct body**.
        ///
        /// Opens a compound on the StructBuilder, runs the byte-
        /// dispatch sub-loop iterating the Repeat with `lo: N`
        /// admission, closes via `end_compound`. CSS L4
        /// `compoundSelector` (selector list under a Repeat) is the
        /// canonical case.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments, unused_variables, unused_mut)]
        pub fn #fn_ident<'p, __P>(
            input: &'p [u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut #builder_ty,
            cursor: &mut crate::path::cursor::PathCursor<'p, __P>,
        ) -> ::core::result::Result<(), crate::runtime::DtaError>
        where
            __P: crate::path::schema::PathSchema<'p>,
        {
            let _ = cursor;
            let __layout: ::bbnf_ir::registry::StructLayout =
                ::bbnf_ir::registry::StructLayout {
                    rule_id: #rule_id_lit as ::bbnf_ir::RuleId,
                    rule_name: ::std::string::String::from(#rule_name_lit),
                    kind: ::bbnf_ir::registry::LayoutKind::Struct,
                    rule_type: ::bbnf_ir::TypeDesc::Span,
                    fields: ::std::vec::Vec::new(),
                };
            let __unordered_checkpoint = <
                #builder_ty as crate::runtime::StructBuilder
            >::checkpoint(builder);
            let __handle = <
                #builder_ty as crate::runtime::StructBuilder
            >::begin_compound(builder, &__layout);
            let __unordered_result: ::core::result::Result<
                (),
                crate::runtime::DtaError,
            > = (|| {
                let _ = #support_mod::skip_space(input, p, state);

                let mut __iters: u32 = 0;
                'unordered: loop {
                    let __byte = match input.get(*p).copied() {
                        Some(b) => b,
                        None => break 'unordered,
                    };
                    match __byte {
                        #(#branch_arms)*
                        _ => break 'unordered,
                    }
                    let _ = #support_mod::skip_space(input, p, state);
                }

                if __iters < #iters_lo_lit {
                    return Err(crate::runtime::DtaError::Syntax {
                        offset: *p as u32,
                    });
                }

                ::core::result::Result::Ok(())
            })();

            match __unordered_result {
                ::core::result::Result::Ok(()) => {
                    <
                        #builder_ty as crate::runtime::StructBuilder
                    >::end_compound(builder, __handle);
                    Ok(())
                }
                ::core::result::Result::Err(__err) => {
                    <
                        #builder_ty as crate::runtime::StructBuilder
                    >::rollback(builder, __unordered_checkpoint);
                    ::core::result::Result::Err(__err)
                }
            }
        }
    }
}
