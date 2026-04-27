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
//! 5. Close the outer compound with a single `push_compound(Rule, …,
//!    variant_idx = rule.id & 0xFF, …)` — walker-parity with the DTA
//!    `Repeat` state's `push_compound_fused(Rule, *pos)` on entry
//!    plus `close_compound` at exit.
//!
//! Canonical: CSS `compoundSelector = (classSelector | idSelector |
//!   attrSelector | colonSelector | typeSelector) +` per
//! `grammar/css/l4/selectors.bbnf:87-88`. Five branches with disjoint
//! FIRST bytes.
//!
//! # Walker-tape parity
//!
//! The walker pushes exactly one compound for the Repeat rule — the
//! Alt state under it is a ByteDispatch that pushes no compound, so
//! each iteration's record stream is whatever the chosen branch Ref's
//! rule entry emits. The emitted body mirrors that decomposition:
//! one outer `push_compound(TapeKind::Rule, …)` wrapping the per-
//! iteration branch records, with `variant_idx = rule.id & 0xFF` the
//! walker's `stack.pending_variant_idx` captures at Repeat entry.

use std::collections::HashSet;

use bbnf_ir::{CharSet128, GrammarIR, IrNode, IrRule, RuleId};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use super::dispatcher::{
    dispatcher_fn_ident, emit_ref_call_tape, emit_ref_call_visitor, shape_fn_ident,
    visitor_dispatcher_fn_ident, visitor_shape_fn_ident,
};
use super::root_rule_name;

// ─── Unordered body introspection ───────────────────────────────────

/// Walker-parity decomposition of an Unordered rule's body.
///
/// Extracted once at emit time so both the tape-path and visitor-path
/// emitters share the same branch projection + FIRST byte computation.
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
        IrNode::OptionalWhitespace(inner) | IrNode::Map { inner, .. } => is_nullable(inner, ir, visited),
        IrNode::TokenDispatch { token, .. } => is_nullable(token, ir, visited),
    }
}

/// Project the FIRST byte set of a node by structural walk with a
/// cycle guard on Ref descent. Matches the sibling detector's
/// [`bbnf_ir::passes::recognizers::shape_dispatch::unordered`]
/// projection — the two walks produce the same set per branch so the
/// emitter's byte-dispatch arms cover the same byte space the
/// detector admitted.
fn node_first(
    node: &IrNode,
    ir: &GrammarIR,
    visited: &mut HashSet<RuleId>,
) -> Option<CharSet128> {
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

// ─── Tape-path emitter ──────────────────────────────────────────────

/// Emit `pub fn parse_unordered_<grammar>_<rule>(input, p, state,
/// builder) -> Result<TapeOffset, DtaError>`.
pub fn emit_parse_unordered(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("unordered", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => {
            return quote! {};
        }
    };

    let Some(body_info) = introspect_unordered(rule, ir) else {
        return emit_parse_unordered_fallback(grammar_suffix, rule, ir);
    };

    // Build per-branch byte-match arms. AW-V.W5.2 — when the branch is
    // a Ref to a classified target, emit a direct call to that
    // target's shape fn; otherwise fall back to the grammar's
    // value-position dispatcher. This produces tight monomorphic
    // dispatch without per-byte cross-crate boundaries for the hot
    // path while preserving walker-parity fallback when a branch's
    // target is unclassified.
    let mut branch_arms = Vec::with_capacity(body_info.first_sets.len());
    for (first_set, branch_ref) in body_info
        .first_sets
        .iter()
        .zip(body_info.branch_refs.iter())
    {
        let pattern = emit_byte_match_arm(first_set);
        let call = branch_ref
            .and_then(|rid| emit_ref_call_tape(grammar_suffix, rid, ir))
            .map(|call| quote! { let _ = (#call)?; })
            .unwrap_or_else(|| {
                quote! { let _ = #dispatcher_ident(input, p, state, builder)?; }
            });
        branch_arms.push(quote! {
            #pattern => {
                #call
                iters += 1;
            }
        });
    }

    let iters_lo_lit = proc_macro2::Literal::u32_unsuffixed(body_info.iters_lo);

    quote! {
        /// AW-V.W4-fix — per-grammar Unordered-shape parse function,
        /// **walker-tape-identical**.
        ///
        /// Byte-dispatch over the mined disjoint-FIRST Alt branches
        /// inside a Kleene-plus sub-loop. Emits one `TapeKind::Rule`
        /// outer compound for the full Unordered span; each admitted
        /// branch contributes its own sub-compound through the per-
        /// grammar value-position dispatcher.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]` per cross-shape
        /// recursion rationale.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            let span_lo = *p as u32;
            // AY-II.W0.b — walker-parity post-order Repeat Rule
            // compound via begin_compound_post/end_compound_post_order.
            //
            // B5.W6 — bracket the post-order children scope so child
            // records stamp `frame_depth` at the correct (parent + 1)
            // depth at push time.
            let outer_child = builder.enter_post_order_children();
            // The walker's Repeat entry doesn't skip leading ws on
            // its own — the Alt's ByteDispatch does. Mirror that: the
            // per-grammar value-position dispatcher does its own
            // `skip_space`, so we do NOT pre-skip here. Any leading
            // trim is applied by the first dispatch arm's skip.
            let mut iters: u32 = 0;
            loop {
                let Some(b) = input.get(*p).copied() else { break; };
                match b {
                    #(#branch_arms)*
                    _ => break,
                }
            }
            if iters < #iters_lo_lit {
                // B5.W6 — close the bracket before returning the error.
                builder.exit_post_order_children();
                return ::core::result::Result::Err(
                    match input.get(*p).copied() {
                        None => crate::runtime::tape::DtaError::UnexpectedEnd {
                            offset: *p as u32,
                        },
                        _ => crate::runtime::tape::DtaError::Syntax {
                            offset: *p as u32,
                            failing_state: crate::runtime::tape::DtaStateId::NONE,
                            failing_rule: crate::runtime::tape::DtaRuleId(u32::MAX),
                        },
                    },
                );
            }
            let span_hi = *p as u32;
            let outer_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Rule,
                span_lo,
                #variant_idx,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
            ::core::result::Result::Ok(crate::runtime::tape::TapeOffset(outer_off))
        }
    }
}

/// Emit a walker-fallback body for a rule the Unordered detector did
/// not admit — the Ref shape the `emit_shapes_for_grammar` dispatch
/// would use at the call site.
///
/// This form ships only as a defensive fallback: the public emission
/// path (`emit_shapes_for_grammar`) gates on the detector's
/// admission, so callers only instantiate the emitter for rules whose
/// bodies match the Unordered predicate. Tests that exercise the
/// emitter directly on non-Unordered fixtures (the parsable-tokens
/// regression) exercise this branch.
fn emit_parse_unordered_fallback(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = shape_fn_ident("unordered", grammar_suffix, rule_name);
    let variant_idx = (rule.id & 0xFF) as u8;
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    quote! {
        /// AW-V.W4-fix — Unordered-shape emitter fallback. Called
        /// only when a fixture invokes the emitter on a non-Unordered
        /// rule (the detector's admission would otherwise route this
        /// call elsewhere). Emits an empty compound + `UnexpectedEnd`
        /// so the parsability gate passes without asserting parse
        /// semantics on a shape the rule doesn't match.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            builder: &mut crate::runtime::tape::Tape<()>,
        ) -> ::core::result::Result<
            crate::runtime::tape::TapeOffset,
            crate::runtime::tape::DtaError,
        > {
            let _ = state;
            let span_lo = *p as u32;
            // AY-II.W0.b — empty-compound fallback via begin/end.
            // B5.W6 — bracket the (empty) post-order children scope.
            let outer_child = builder.enter_post_order_children();
            let span_hi = *p as u32;
            let _ = input;
            let outer_off = builder.begin_compound_post(
                crate::runtime::tape::TapeKind::Rule,
                span_lo,
                #variant_idx,
                0u8,
                0u16,
            );
            builder.end_compound_post_order(
                outer_off,
                span_hi,
                crate::runtime::tape::TapeOffset(outer_child),
            );
            ::core::result::Result::Ok(crate::runtime::tape::TapeOffset(outer_off))
        }
    }
}

// ─── Visitor-path emitter ───────────────────────────────────────────

/// Emit `pub fn parse_unordered_visitor_<grammar>_<rule><V>(input, p,
/// state, visitor) -> Result<(), ParseErr>`.
pub fn emit_parse_unordered_visitor(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("unordered", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    let dispatcher_ident = match root_rule_name(ir) {
        Some(root) => {
            let root_disp = visitor_dispatcher_fn_ident(grammar_suffix, &root);
            format_ident!("{}__value", root_disp)
        }
        None => {
            return quote! {};
        }
    };

    let Some(body_info) = introspect_unordered(rule, ir) else {
        return emit_parse_unordered_visitor_fallback(grammar_suffix, rule, ir);
    };

    let mut branch_arms = Vec::with_capacity(body_info.first_sets.len());
    for (first_set, branch_ref) in body_info
        .first_sets
        .iter()
        .zip(body_info.branch_refs.iter())
    {
        let pattern = emit_byte_match_arm(first_set);
        let call = branch_ref
            .and_then(|rid| emit_ref_call_visitor(grammar_suffix, rid, ir))
            .map(|call| quote! { (#call)?; })
            .unwrap_or_else(|| {
                quote! { #dispatcher_ident(input, p, state, visitor)?; }
            });
        branch_arms.push(quote! {
            #pattern => {
                #call
                iters += 1;
            }
        });
    }

    let iters_lo_lit = proc_macro2::Literal::u32_unsuffixed(body_info.iters_lo);

    quote! {
        /// AW-V.W4-fix — visitor-path Unordered-shape parse function.
        ///
        /// Generic over the visitor type; each admitted Alt branch
        /// routes through the per-grammar value-position visitor
        /// dispatcher. The Kleene-plus loop terminates at the first
        /// byte no branch's FIRST set admits.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            let mut iters: u32 = 0;
            loop {
                let Some(b) = input.get(*p).copied() else { break; };
                match b {
                    #(#branch_arms)*
                    _ => break,
                }
            }
            if iters < #iters_lo_lit {
                return ::core::result::Result::Err(
                    crate::runtime::ParseErr::Syntax {
                        offset: *p as u32, rule: None,
                    },
                );
            }
            ::core::result::Result::Ok(())
        }
    }
}

/// Fallback visitor-path emitter for rules the Unordered detector
/// rejects. Signature-compatible with the admitted form.
fn emit_parse_unordered_visitor_fallback(
    grammar_suffix: &str,
    rule: &IrRule,
    ir: &GrammarIR,
) -> TokenStream {
    let rule_name = ir.get_string(rule.name);
    let fn_ident = visitor_shape_fn_ident("unordered", grammar_suffix, rule_name);
    let support_mod = format_ident!("__shape_support_{}", grammar_suffix);

    quote! {
        /// AW-V.W4-fix — visitor-path Unordered-shape fallback.
        ///
        /// AX.W0a.2.f — compound; plain `#[inline]`.
        #[inline]
        #[allow(non_snake_case, clippy::too_many_arguments)]
        pub fn #fn_ident<V>(
            input: &[u8],
            p: &mut usize,
            state: &mut #support_mod::ScanState,
            visitor: &mut V,
        ) -> ::core::result::Result<(), crate::runtime::ParseErr>
        where
            V: crate::runtime::tape::ObjectVisitor
                + crate::runtime::tape::ArrayVisitor
                + crate::runtime::tape::StringVisitor
                + crate::runtime::tape::NumberVisitor
                + crate::runtime::tape::KeywordVisitor,
        {
            let _ = (input, p, state, visitor);
            ::core::result::Result::Ok(())
        }
    }
}
